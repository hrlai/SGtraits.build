# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)



# FER lab data -------------------------------------------------------------
index     <- read_excel("data/Brouwer_2016/raw/DATA_LEAF_new.xlsx", sheet = "Leaf")
species   <- read_excel("data/Brouwer_2016/raw/DATA_LEAF_new.xlsx", sheet = "Species")
# indent    <- read_excel("data/Brouwer_2016/raw/DATA_LEAF_new.xlsx", sheet = "Blunt Indent")
flat      <- read_excel("data/Brouwer_2016/raw/DATA_LEAF_new.xlsx", sheet = "Flat Punch")
concave   <- read_excel("data/Brouwer_2016/raw/DATA_LEAF_new.xlsx", sheet = "Concave Punch")
# plunger   <- read_excel("data/Brouwer_2016/raw/DATA_LEAF_new.xlsx", sheet = "Punch Plunger")
scissor   <- read_excel("data/Brouwer_2016/raw/DATA_LEAF_new.xlsx", sheet = "Scissor")



# Tidy --------------------------------------------------------------------
## Tidy index df
index <- 
    index %>% 
    mutate(LeafID = str_remove(LeafID, fixed(".0")),
           `Plant ID` = str_remove(`Plant ID`, fixed(".0")),
           FullTag = paste(`Plot ID/Location`, `Plant ID`, sep="T")) %>% 
    filter(`Plant ID`!="Paper 70gsm",
           # remove two Acacia perpendicullarly cut
           !LeafID %in% c("2304","2305")) %>% 
    droplevels()


## tidy all measurements
# NOT DOING MEMBRANE/INDENT now

flat <- 
    flat %>% 
    # remove data that are likely erroneous
    filter(is.na(comment)) %>% 
    mutate(leaf_work_to_punch_flat = `Peak Force` / (pi*1.94),
           leaf_work_to_punch_adjusted_flat = leaf_work_to_punch_flat / Thickness,
           MID = as.character(LeafID)) %>% 
    select(MID, leaf_work_to_punch_flat, leaf_work_to_punch_adjusted_flat, Thickness) %>% 
    # long format
    pivot_longer(cols = c(leaf_work_to_punch_flat, leaf_work_to_punch_adjusted_flat, Thickness),
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) 

concave <- 
    concave %>% 
    # remove data that are likely erroneous
    filter(is.na(Comment)) %>% 
    mutate(leaf_work_to_punch_concave = `Peak Force` / (pi*1.94),
           leaf_work_to_punch_adjusted_concave = leaf_work_to_punch_concave / Thickness,
           MID = as.character(LeafID)) %>% 
    select(MID, leaf_work_to_punch_concave, leaf_work_to_punch_adjusted_concave, Thickness) %>% 
    # long format
    pivot_longer(cols = c(leaf_work_to_punch_concave, leaf_work_to_punch_adjusted_concave, Thickness),
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) 

scissor <- 
    scissor %>% 
    # remove data that are likely erroneous
    filter(is.na(comment)) %>% 
    mutate(leaf_work_to_shear = `Fracture toughness` / (`Cut length` / 100),
           MID = as.character(LeafID)) %>% 
    select(MID, leaf_work_to_shear, Thickness) %>% 
    # long format
    pivot_longer(cols = c(leaf_work_to_shear, Thickness),
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) 


# Consolidate -------------------------------------------------------------
out <- 
    index %>%
    left_join(species %>% select(FullTag, Species)) %>% 
    select(Dataset2 = Project,
           MID = LeafID,
           IID = FullTag,
           OrgName = Species) %>% 
    left_join(
        bind_rows(flat, concave, scissor)
    ) %>% 
    filter(!is.na(OrgVal)) %>% 
    # fill in missing species name matches
    mutate(OrgName = ifelse(
        is.na(OrgName),
        trimws(str_remove_all(IID, "UHallT|UTownT|UtownT|KRT|[[:digit:]]+")),
        OrgName
    )) %>% 
    # assign Dataset name
    mutate(Dataset1 = "SG07",
           SID = NA,
           AccName = NA) %>% 
    # rearrange columns
    select(Dataset1, Dataset2, MID, IID, SID, OrgName, AccName, Trait, OrgVal) %>% 
    # aggregate to individual level following austraits standards
    group_by(Dataset1, Dataset2, IID, SID, OrgName, AccName, Trait) %>% 
    summarise(OrgVal = mean(OrgVal),
              n = n())

write_csv(out, "data/Brouwer_2016/data.csv")
