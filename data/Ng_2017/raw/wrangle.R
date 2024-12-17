# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)



# Import data -------------------------------------------------------------
filename <- "data/Ng_2017/raw/DATA_LEAF.xlsx"
# excel_sheets(filename)
index   <- read_xlsx(filename, sheet = "Index")
species   <- read_xlsx(filename, sheet = "Species")
area      <- read_xlsx(filename, sheet = "Area")
weight    <- read_xlsx(filename, sheet = "Weight")
thickness <- read_xlsx(filename, sheet = "Thickness")
sc        <- read_xlsx(filename, sheet = "Stomatal Conductance")



# Leaf --------------------------------------------------------------------
## aggregate multiple scans to the LeafPart level
area.agg <- 
    area %>% 
    group_by(LeafID, LeafPart) %>% 
    summarise(Pixels = sum(`Area (Pixels)`, na.rm = TRUE)) %>% 
    # summarise() returns NAs as zeroes, must correct back to "NA"
    mutate(Pixels = ifelse(Pixels == 0, NA, Pixels)) %>% 
    filter(LeafPart == "L")

## aggregate leaf thickness into mean thickness
thickness.agg <- 
    thickness %>% 
    group_by(LeafID) %>% 
    summarise(Lth = round(mean(Lth, na.rm = TRUE), 3))

## aggregate weights into total weights
weight.agg <- 
    weight %>% 
    group_by(LeafID, LeafPart) %>% 
    summarise_at(vars(`WW (g)`, `DW (g)`), sum)

## aggregate stomatal conductance into means
sc.agg <- 
    sc %>% 
    group_by(`Leaf ID`) %>% 
    summarise(SC = mean(`Stomatal Conductance`)) %>% 
    rename(LeafID = `Leaf ID`)


# Consolidate -------------------------------------------------------------
out <- 
    expand.grid(LeafID = unique(index$LeafID),
                # select LAMINA only
                LeafPart = "L") %>% 
    left_join(area.agg) %>% 
    left_join(weight.agg) %>% 
    left_join(thickness.agg) %>% 
    left_join(sc.agg) %>% 
    # incorporate species names
    left_join(index %>% select(LeafID, `Field ID`, CrownPos)) %>% 
    mutate(SpecID = str_remove(`Field ID`, "[0-9]")) %>% 
    left_join(species %>% rename(SpecID = `Field ID`)) %>% 
    # change area pixel to mm^2
    mutate(Area_mm2 = Pixels / (118.110236^2) * 100,
           # SLA: divide area mm^2 by DW mg^-1
           SLA = Area_mm2 / (`DW (g)` * 1000),
           LDMC = `DW (g)` / `WW (g)`) %>% 
    select(
        LeafID,
        FullTag = `Field ID`,
        CrownPos,
        Species = Name,
        Area_mm2,
        SLA,
        LDMC,
        Th = Lth,
        SC
    ) %>% 
    # long format
    pivot_longer(cols = c(Area_mm2, Area_mm2, SLA, LDMC, Th, SC),
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) %>% 
    # other columns
    mutate(Dataset1 = "SG06",
           Dataset2 = NA,
           MID = LeafID,
           IID = FullTag,
           SID = NA,
           OrgName = Species,
           AccName = NA) %>% 
    # rearrange columns
    select(Dataset1, Dataset2, MID, IID, SID, OrgName, AccName, Trait, OrgVal) %>% 
    # aggregate to individual level following austraits standards
    group_by(Dataset1, Dataset2, IID, SID, OrgName, AccName, Trait) %>% 
    summarise(OrgVal = mean(OrgVal),
              n = n())

# export output
write_csv(out, "data/Ng_2017/data.csv")
