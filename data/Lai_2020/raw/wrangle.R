library(tidyverse)
library(readxl)



# FER lab data -------------------------------------------------------------
# excel_sheets("data/Lai_2020/raw/DATA_LEAF_new.xlsx")
index     <- read_xlsx("data/Lai_2020/raw/DATA_LEAF_new.xlsx", sheet = "Leaf")
species   <- read_xlsx("data/Lai_2020/raw/FieldID_Species_index.xlsx", sheet = "index")
area      <- read_xlsx("data/Lai_2020/raw/DATA_LEAF_new.xlsx", sheet = "Area")
weight    <- read_xlsx("data/Lai_2020/raw/DATA_LEAF_new.xlsx", sheet = "Weight")
thickness <- read_xlsx("data/Lai_2020/raw/DATA_LEAF_new.xlsx", sheet = "Lth")
petiole   <- read_xlsx("data/Lai_2020/raw/DATA_LEAF_new.xlsx", sheet = "Petiole")



# Tidy --------------------------------------------------------------------
## Tidy index df
index <- 
    index %>% 
    mutate(LeafID = str_remove(LeafID, fixed(".0")),
           `Plant ID` = str_remove(`Plant ID`, fixed(".0")),
           FullTag = paste(`Plot ID/Location`, `Plant ID`, sep="T")) %>% 
    filter(`Plant ID`!="Paper 70gsm") %>% 
    droplevels()

## aggregate multiple scans to the LeafPart level
area.agg <- 
    area %>% 
    filter(!is.na(`Area (Pixels)`),
           LeafPart != "") %>% 
    mutate(LeafID = str_remove(LeafID, fixed(".0"))) %>% 
    group_by(LeafID, LeafPart) %>% 
    summarise(Pixels = sum(`Area (Pixels)`)) %>% 
    ungroup()

## aggregate leaf thickness into mean thickness
thickness.agg <- 
    thickness %>% 
    mutate(LeafID = str_remove(LeafID, fixed(".0"))) %>%
    group_by(LeafID) %>% 
    summarise(Th = round(mean(LTh, na.rm = TRUE), 3)) %>% 
    mutate(LeafPart = "L")

## aggregate weights into total weights
weight.agg <- 
    weight %>% 
    mutate(LeafID = str_remove(LeafID, fixed(".0"))) %>%
    group_by(LeafID, LeafPart) %>% 
    summarise_at(vars(WW, DW), sum)

## aggregate multiple petiole measurements into mean per petiole
# petiole dataset only had scan filenames (I do not remember why)
# so we need to match the filenames to the leaf ID (in the area dataset)
# however, it turns out that some leaf ID share the same scan filenames
# (I also don't remember why)
# I will remove these cases to minimise matching error (only 17 duplicates)
duplicated_filenames <- 
    area %>% 
    filter(!is.na(`Area (Pixels)`),
           LeafPart != "") %>% 
    distinct(LeafID, ScanNrFull) %>% 
    mutate(LeafID = str_remove(LeafID, fixed(".0"))) %>% 
    group_by(ScanNrFull) %>% 
    count() %>% 
    filter(n > 1)

petiole.agg <- 
    petiole %>% 
    mutate(Label = gsub(".jpg", "", Label)) %>% 
    left_join(
        area %>% 
            filter(!ScanNrFull %in% duplicated_filenames$ScanNrFull,
                   !is.na(`Area (Pixels)`),
                   LeafPart != "") %>% 
            distinct(LeafID, ScanNrFull) %>% 
            mutate(LeafID = str_remove(LeafID, fixed(".0"))), 
        by = c("Label" = "ScanNrFull")) %>%
    group_by(LeafID) %>% 
    summarise(Length = mean(Length, na.rm = TRUE)) %>% 
    mutate(Length_mm = Length / 118.110236 * 10) %>% 
    left_join(weight %>% 
                  mutate(LeafID = str_remove(LeafID, fixed(".0"))) %>% 
                  filter(LeafPart == "P") %>% 
                  group_by(LeafID) %>% 
                  summarise(DW = mean(DW, na.rm = TRUE))) %>% 
    mutate(SPL = Length_mm / (DW*1000)) %>% 
    select(LeafID, PL = Length_mm, SPL)


# Consolidate -------------------------------------------------------------
out.FER <- 
    expand.grid(LeafID = unique(index$LeafID),
                LeafPart = unique(area.agg$LeafPart)) %>% 
    left_join(area.agg) %>% 
    left_join(weight.agg) %>% 
    left_join(thickness.agg) %>% 
    left_join(petiole.agg) %>% 
    # incorporate species names
    left_join(index %>% 
                  # remove a leaf ID that is duplicated for two different trees
                  filter(LeafID != "2792") %>% 
                  select(LeafID, FullTag, CrownPos)) %>% 
    # remove trailing ".0" from Leaf and Tree IDs
    # mutate_at(vars(LeafID, FullTag), list(~str_replace(., ".0", ""))) %>% 
    left_join(species %>% distinct(FullTag, Species)) %>% 
    # select LAMINA only
    filter(LeafPart == "L") %>% 
    # change area pixel to mm^2
    mutate(Area_mm2 = Pixels / (118.110236^2) * 100,
           # SLA: divide area mm^2 by DW mg^-1
           SLA = Area_mm2 / (DW*1000),
           LDMC = DW / WW) %>% 
    select(LeafID, FullTag, CrownPos, Species, Area_mm2, SLA, LDMC, Th, PL) %>% 
    # long format
    pivot_longer(cols = c(Area_mm2, SLA, LDMC, Th, PL),
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) %>% 
    # standardise long format column names
    rename(MID = LeafID,
           IID = FullTag,
           OrgName = Species) %>% 
    # assign Dataset2 name
    mutate(Dataset2 = "SG01_HR")

# export output
# write.csv(out.FER, "clean/mandai_leaf_FER.csv", row.names = FALSE)




# Pin Jia's leaf data from Mandai (2017) ----------------------------------
## basically ditto the above as they're in the same format
# excel_sheets("raw/DATA_LEAF_PinJia.xlsx")
index     <- read_excel("data/Lai_2020/raw/DATA_LEAF_PinJia.xlsx", sheet = "Index")
species   <- read_excel("data/Lai_2020/raw/DATA_LEAF_PinJia.xlsx", sheet = "Species")
area      <- read_excel("data/Lai_2020/raw/DATA_LEAF_PinJia.xlsx", sheet = "Area")
weight    <- read_excel("data/Lai_2020/raw/DATA_LEAF_PinJia.xlsx", sheet = "Weight")
thickness <- read_excel("data/Lai_2020/raw/DATA_LEAF_PinJia.xlsx", sheet = "Thickness")

## aggregate multiple scans to the LeafPart level
area.agg <- 
    area %>% 
    group_by(LeafID, LeafPart) %>% 
    summarise(Pixels = sum(`Area (Pixels)`, na.rm = TRUE)) %>% 
    # summarise() returns NAs as zeroes, must correct back to "NA"
    mutate(Pixels = ifelse(Pixels == 0, NA, Pixels)) %>% 
    filter(LeafPart != "")

## aggregate leaf thickness into mean thickness
thickness.agg <- 
    thickness %>% 
    group_by(LeafID) %>% 
    summarise(Th = round(mean(Lth, na.rm = TRUE), 3))

## aggregate weights into total weights
weight.agg <- 
    weight %>% 
    group_by(LeafID, LeafPart) %>% 
    summarise_at(vars(WW, DW), sum)

## Consolidate
out.PJ <- 
    expand.grid(LeafID = unique(index$LeafID),
                LeafPart = "L") %>% 
    left_join(area.agg) %>% 
    left_join(weight.agg) %>% 
    left_join(thickness.agg) %>% 
    # incorporate species names
    left_join(index %>% select(LeafID, `Field ID`, CrownPos)) %>% 
    mutate(SpecID = str_remove(`Field ID`, "[0-9]")) %>% 
    left_join(species %>% select(SpecID = `Field ID`,
                                 Species = `Full name`)) %>% 
    # special case: fill in species name for 'ALSTONIA ANGUSTILOBA'
    mutate(Species = ifelse(SpecID == "ALSTONIA ANGUSTILOBA",
                            "Alstonia angustiloba",
                            Species)) %>% 
    # change area pixel to mm^2
    mutate(Area_mm2 = Pixels / (118.110236^2) * 100,
           # SLA: divide area mm^2 by DW mg^-1
           SLA = Area_mm2 / (DW*1000),
           LDMC = DW / WW,
           LeafID = as.character(LeafID)) %>% 
    select(LeafID, FullTag = `Field ID`, CrownPos, Species, Area_mm2, SLA, LDMC, Th) %>% 
    # long format
    pivot_longer(cols = c(Area_mm2, SLA, LDMC, Th),
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) %>% 
    # standardise long format column names
    rename(MID = LeafID,
           IID = FullTag,
           OrgName = Species) %>% 
    # assign Dataset2 name
    mutate(Dataset2 = "SG01_PJ")

# export output
# write.csv(out.PJ, "clean/mandai_leaf_PJ.csv", row.names = FALSE)



# Combine HR and PJ data --------------------------------------------------
out <- 
    bind_rows(out.FER, out.PJ) %>% 
    # fill in missing species name matches
    mutate(OrgName = ifelse(
        is.na(OrgName),
        trimws(str_remove_all(IID, "UHallT|UTownT|UtownT|KRT|Outside D23T|[[:digit:]]+")),
        OrgName
    )) %>% 
    # other columns
    mutate(Dataset1 = "SG01",
           SID = NA,
           AccName = NA) %>% 
    # rearrange columns
    select(Dataset1, Dataset2, MID, IID, SID, OrgName, AccName, Trait, OrgVal) %>% 
    # aggregate to individual level following austraits standards
    group_by(Dataset1, Dataset2, IID, SID, OrgName, AccName, Trait) %>% 
    summarise(OrgVal = mean(OrgVal),
              n = n())

write_csv(out, "data/Lai_2020/data.csv")
