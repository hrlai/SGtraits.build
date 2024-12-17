library(tidyverse)

ky_seeds <- 
    read_csv("data/Chong_2010/raw/KY_seeds.csv") %>% 
    mutate(IID = row_number())

write_csv(ky_seeds, "data/Chong_2010/data.csv")
