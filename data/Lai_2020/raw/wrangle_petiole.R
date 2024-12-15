# Packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)



# FER lab data -------------------------------------------------------------
# excel_sheets("raw/DATA_LEAF_new.xlsx")
index     <- read_excel("raw/DATA_LEAF_new.xlsx", sheet = "Leaf")
species   <- read_excel("raw/FieldID_Species_index.xlsx", sheet = "index")
area      <- read_excel("raw/DATA_LEAF_new.xlsx", sheet = "Area")
weight    <- read_excel("raw/DATA_LEAF_new.xlsx", sheet = "Weight")
# thickness <- read_excel("raw/DATA_LEAF_new.xlsx", sheet = "Lth")
petiole   <- read_excel("raw/DATA_LEAF_new.xlsx", sheet = "Petiole")



# Tidy --------------------------------------------------------------------
## Tidy index df
index <- 
  index %>% 
  mutate(LeafID = str_remove(LeafID, fixed(".0")),
         `Plant ID` = str_remove(`Plant ID`, fixed(".0")),
         FullTag = paste(`Plot ID/Location`, `Plant ID`, sep="T")) %>% 
  filter(`Plant ID`!="Paper 70gsm") %>% 
  droplevels()

# ## aggregate multiple scans to the LeafPart level
# area.agg <- 
#   area %>% 
#   mutate(LeafID = str_remove(LeafID, fixed(".0"))) %>% 
#   group_by(LeafID, LeafPart) %>% 
#   summarise(Pixels = sum(`Area (Pixels)`, na.rm = TRUE)) %>% 
#   # summarise() returns NAs as zeroes, must correct back to "NA"
#   ungroup() %>% 
#   mutate(Pixels = ifelse(Pixels == 0, NA, Pixels)) %>% 
#   filter(LeafPart != "")
# 
# ## aggregate leaf thickness into mean thickness
# thickness.agg <- 
#   thickness %>% 
#   mutate(LeafID = str_remove(LeafID, fixed(".0"))) %>%
#   group_by(LeafID) %>% 
#   summarise(Th = round(mean(LTh, na.rm = TRUE), 3)) %>% 
#   mutate(LeafPart = "L")

## aggregate weights into total weights
weight.agg <- 
  weight %>% 
  mutate(LeafID = str_remove(LeafID, fixed(".0"))) %>%
  group_by(LeafID, LeafPart) %>% 
  summarise_at(vars(WW, DW), sum)

# Petiole dry-matter content
pdmc.agg <- 
  weight.agg %>% 
  filter(LeafPart == "P") %>% 
  mutate(PDMC = DW / WW) %>% 
  filter(!is.na(PDMC),
         PDMC <= 1)

## aggregate multiple petiole measurements into mean per petiole
petiole.agg <- 
  petiole %>% 
  mutate(Label = gsub(".jpg", "", Label)) %>% 
  left_join(area %>% 
              select(LeafID, ScanNrFull) %>% 
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
  select(LeafID, SPL)


# Consolidate -------------------------------------------------------------
out.PDMC <- 
  expand.grid(LeafID = unique(index$LeafID)) %>% 
  left_join(pdmc.agg) %>% 
  left_join(petiole.agg) %>% 
  # incorporate species names
  left_join(index %>% select(LeafID, FullTag, CrownPos)) %>% 
  # remove trailing ".0" from Leaf and Tree IDs
  # mutate_at(vars(LeafID, FullTag), list(~str_replace(., ".0", ""))) %>% 
  left_join(species %>% select(FullTag, Species)) %>% 
  select(LeafID, FullTag, CrownPos, Species, PDMC, SPL) %>% 
  # long format
  gather(Trait, OrgVal, PDMC, SPL, na.rm = TRUE) %>% 
  # standardise long format column names
  rename(MID = LeafID,
         IID = FullTag,
         OrgName = Species) %>% 
  # assign Dataset2 name
  mutate(Dataset2 = "SG01_HR_PDMC")

# export output
# write.csv(out.FER, "clean/mandai_leaf_FER.csv", row.names = FALSE)


library(ggplot2)

tmp <- 
  out.PDMC %>% 
  spread(Trait, OrgVal)
ggplot(out.PDMC, aes(OrgVal, OrgName)) +
  facet_wrap(~ Trait, scales = "free_x") +
  geom_boxplot()




# Combine HR and PJ data --------------------------------------------------
out <- 
  bind_rows(out.FER, out.PJ) %>% 
  # other columns
  mutate(Dataset1 = "SG01",
         SID = NA,
         AccName = NA) %>% 
  # rearrange columns
  select(Dataset1, Dataset2, MID, IID, SID, OrgName, AccName, Trait, OrgVal)

write.csv(out, "clean/SG01.csv", row.names = FALSE)
