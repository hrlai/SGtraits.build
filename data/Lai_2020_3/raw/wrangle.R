library(tidyverse)
library(readxl)



## Seed mass collected by Hao Ran Lai from field, SING and SINU ============
seeds <- 
    read_xlsx("data/Lai_2020_3/raw/Seed_Fruit.xlsx", 
              sheet = "seed", 
              col_types = c(rep("text", 4),
                            rep("date", 2),
                            rep("numeric", 6),
                            rep("text", 2))) %>% 
    mutate(SDM = DryMassInd * 1000) %>%   # in mg
    select(Project:MeasureDate, Ripen, Comment, SDM) %>% 
    pivot_longer(cols = SDM,
                 names_to = "Trait",
                 values_to = "OrgVal") %>% 
    mutate(Trait = paste(Trait, "seed"))


# Fruits collected by Hao Ran Lai
fruits <- 
    read_xlsx("data/Lai_2020_3/raw/Seed_Fruit.xlsx", 
              sheet = "fruit", 
              col_types = c(rep("text", 4),
                            rep("date", 2),
                            rep("numeric", 6),
                            rep("text", 2))) %>%   # in mg
    select(Project:MeasureDate, Ripen, Comment, Width, Length, DryMassInd) %>% 
    pivot_longer(cols = c(Width, Length, DryMassInd),
                 names_to = "Trait",
                 values_to = "OrgVal") %>% 
    mutate(Trait = paste(Trait, "fruit"))


# combine
seeds_fruits <- 
    bind_rows(seeds, fruits) %>% 
    mutate(IID = row_number())

write_csv(seeds_fruits, "data/Lai_2020_3/data.csv")
