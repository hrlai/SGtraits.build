library(tidyverse)
library(readxl)



## Seed mass from Metcalfe and Grubb (1995) =====================
sdm.dan <- 
    read_excel("data/Metcalfe_1995/raw/Metcalfe and Grubb 1995.xlsx") %>% 
    group_by(SpeciesOld) %>% 
    # there are some species that are repeated with the same seed mass
    # I think they just repeated them because they could be found in multiple
    # vegeation types, so I'll just take the mean, which should be the same
    # as removing the duplicates
    summarise(`Seed dry mass (mg)` = mean(`Seed dry mass (mg)`))

write_csv(sdm.dan, "data/Metcalfe_1995/data.csv")
