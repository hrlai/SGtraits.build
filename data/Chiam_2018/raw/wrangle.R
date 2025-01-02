library(tidyverse)


chiam <- read_csv("data/Chiam_2018/raw/Overall Data.csv") 

# nothing to clean, for now

write_csv(chiam, "data/Chiam_2018/data.csv")
