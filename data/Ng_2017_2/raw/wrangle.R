library(tidyverse)
library(readxl)



photosynthesis <- 
    read_xlsx("data/Ng_2017_2/raw/UPDATED COMPILED DATA SHEET.xlsx") %>% 
    distinct(Species, LCP, Rd)


write_csv(photosynthesis, "data/Ng_2017_2/data.csv")
