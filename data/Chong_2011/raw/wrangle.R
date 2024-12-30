library(tidyverse)
library(lubridate)
library(readxl)

KYleaves <- 
    read_xlsx("data/Chong_2011/raw/KYleaves.xlsx") %>% 
    mutate(Date1 = mdy(`Collection Date`))

write_csv(KYleaves, "data/Chong_2011/data.csv") 
