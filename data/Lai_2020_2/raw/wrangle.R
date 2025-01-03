# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)



# Import data -------------------------------------------------------------
# Zhongyu Chiam's twig data
ind <- read_xlsx("data/Lai_2020_2/raw/WD_Zhongyu.xlsx", sheet = "Individual")
twig <- read_xlsx("data/Lai_2020_2/raw/WD_Zhongyu.xlsx", sheet = "Main Twig")
twig_terminal <- read_xlsx("data/Lai_2020_2/raw/WD_Zhongyu.xlsx", sheet = "Terminal Twig")

wd_zy <- 
    twig %>% 
    mutate(`MT_WD_twig` = as.numeric(`MT_WD_twig`)) %>% 
    select(`MTwig ID`, `Plant ID`, `MT_WD_twig`) %>% 
    left_join(ind %>% select(`Plant ID`, Species)) %>% 
    rename(MID = `MTwig ID`,
           IID = `Plant ID`,
           OrgName = Species,
           WD_twig = MT_WD_twig) %>% 
    pivot_longer(cols = WD_twig,
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) %>% 
    # assign Dataset1 name
    mutate(Dataset1 = "SG05")

terminal_area <- 
    twig_terminal %>% 
    mutate(`TT_Wood_TA` = as.numeric(`TT_Wood_TA`)) %>% 
    select(`TTwig ID`, `Plant ID`, `TT_Wood_TA`) %>% 
    left_join(ind %>% select(`Plant ID`, Species)) %>% 
    pivot_longer(cols = TT_Wood_TA,
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) %>% 
    # assign Dataset1 name
    mutate(Dataset1 = "SG05")


# Hao Ran Lai's twig data
wd_hr <- 
    read_xlsx("data/Lai_2020_2/raw/WD.xlsx", sheet = "twig_wd") %>% 
    filter(is.na(Outlier)) %>% 
    mutate(WD_twig = DW / Volume,
           IID = paste(PlotID, FieldID, sep = "_")) %>% 
    group_by(IID, Species) %>% 
    summarise(WD_twig = mean(WD_twig, na.rm = TRUE)) %>% 
    mutate(MID = paste("HR_MT", cur_group_id(), sep = "_")) %>% 
    pivot_longer(cols = WD_twig,
                 names_to = "Trait",
                 values_to = "OrgVal",
                 values_drop_na = TRUE) %>% 
    select(MID, IID, OrgName = Species, Trait, OrgVal) %>% 
    # assign Dataset1 name
    mutate(Dataset1 = "SG02")






# Consolidate -------------------------------------------------------------
out.twig <- 
    bind_rows(wd_zy, wd_hr, terminal_area) %>% 
    # fill in missing species name matches
    mutate(OrgName = ifelse(
        is.na(OrgName),
        trimws(str_remove_all(IID, "NSSF_")),
        OrgName
    )) %>% 
    # other columns
    mutate(Dataset2 = NA,
           SID = NA,
           AccName = NA) %>% 
    # rearrange columns
    select(Dataset1, Dataset2, MID, IID, SID, OrgName, AccName, Trait, OrgVal) %>% 
    # aggregate to individual level following austraits standards
    group_by(Dataset1, Dataset2, IID, SID, OrgName, AccName, Trait) %>% 
    summarise(OrgVal = mean(OrgVal),
              n = n())

# export output
write_csv(out.twig, "data/Lai_2020_2/data.csv")
