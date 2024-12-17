library(tidyverse)
library(lubridate)

ky_seeds <- 
    read_csv("data/Chong_2010/raw/KY_seeds.csv") %>% 
    mutate(`Collection Date` = mdy(`Collection Date`),
           IID = row_number())

write_csv(ky_seeds, "data/Chong_2010/data.csv")
