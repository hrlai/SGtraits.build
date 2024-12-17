# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)



# Import data -------------------------------------------------------------
filename <- "data/Lai_2021/raw/Novel leaf data_24Dec2015.xlsx"
# excel_sheets(filename)
species   <- read_excel(filename, sheet = "Index")
area      <- read_excel(filename, sheet = "Area")
weight    <- read_excel(filename, sheet = "Mass")
thickness <- read_excel(filename, sheet = "Th")



# Leaf --------------------------------------------------------------------
## aggregate multiple scans to the LeafPart level
area.agg <- 
    area %>% 
    group_by(UniqID, LeafPart) %>% 
    summarise(AreaCm2 = sum(AreaCm2, na.rm = TRUE)) %>% 
    # summarise() returns NAs as zeroes, must correct back to "NA"
    mutate(AreaCm2 = ifelse(AreaCm2 == 0, NA, AreaCm2)) %>% 
    filter(LeafPart == "L")

## aggregate leaf thickness into mean thickness
thickness.agg <- 
    thickness %>% 
    group_by(UniqID) %>% 
    summarise(Th = round(mean(Th, na.rm = TRUE), 3))

## aggregate weights into total weights
weight.agg <- 
    weight %>% 
    group_by(UniqID, LeafPart) %>% 
    summarise_at(vars(WetMass, DryMass), sum)


# Consolidate -------------------------------------------------------------
out.leaf <- 
    expand.grid(UniqID = unique(species$UniqID),
                # select LAMINA only
                LeafPart = "L") %>% 
    left_join(area.agg) %>% 
    left_join(weight.agg) %>% 
    left_join(thickness.agg) %>% 
    # incorporate species names
    left_join(species %>% select(UniqID, TreeID, Species)) %>% 
    # change area pixel to mm^2
    mutate(Area_mm2 = AreaCm2 * 100,
           # SLA: divide area mm^2 by DW mg^-1
           SLA = Area_mm2 / (DryMass * 1000),
           LDMC = DryMass / WetMass) %>% 
    select(UniqID, TreeID, Species, Area_mm2, SLA, LDMC, Th) %>% 
    # long format
    pivot_longer(cols = c(Area_mm2, SLA, LDMC, Th),
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) %>% 
    # standardise long format column names
    rename(MID = UniqID,
           IID = TreeID,
           OrgName = Species)

# export output
# write.csv(out.leaf, "clean/novel_leaf.csv", row.names = FALSE)



# Twig --------------------------------------------------------------------
# excel_sheets("raw/Novel twigwood data_24Dec2015.xlsx")
twig <- read_excel("data/Lai_2021/raw/Novel twigwood data_24Dec2015.xlsx")
out.twig <- 
    twig %>% 
    mutate(MainStem = str_detect(Comment, "stem")) %>% 
    group_by(UniqID, TreeID, Species) %>% 
    summarise_at(vars(MainStem, Volume, DryMass, Diameter), sum) %>% 
    mutate(WD = DryMass / Volume) %>% 
    select(UniqID, TreeID, Species, MainStem, WD) %>% 
    # long format
    gather(Trait, OrgVal, WD, na.rm = TRUE) %>% 
    mutate(Trait = ifelse(is.na(MainStem), "WD_twig", Trait)) %>% 
    select(-MainStem) %>% 
    # standardise long format column names
    rename(MID = UniqID,
           IID = TreeID,
           OrgName = Species)

# export output
# write.csv(out.twig, "clean/novel_twig.csv", row.names = FALSE)




# Condolidate leaf and twig wood ------------------------------------------
out <- 
    bind_rows(out.leaf, out.twig) %>% 
    # other columns
    mutate(Dataset1 = "SG04",
           Dataset2 = NA,
           SID = NA,
           AccName = NA) %>% 
    # rearrange columns
    select(Dataset1, Dataset2, MID, IID, SID, OrgName, AccName, Trait, OrgVal)%>% 
    # aggregate to individual level following austraits standards
    group_by(Dataset1, Dataset2, IID, SID, OrgName, AccName, Trait) %>% 
    summarise(OrgVal = mean(OrgVal),
              n = n())

write_csv(out, "data/Lai_2021/data.csv")
