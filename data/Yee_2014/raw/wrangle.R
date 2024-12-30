library(tidyverse)
library(readr)


mandaiSLA2014 <- 
    read_xlsx("data/Yee_2014/raw/mandaiSLA2014.xlsx") %>% 
    mutate(SLA = Area_cm2 / Weight1) %>% 
    group_by(Plot, `Tree_ID`, Species1) %>% 
    summarise(n = sum(`factor(Leaf)`),
              Area_cm2 = mean(Area_cm2 ),
              SLA = mean(SLA),
              Date.Dried = min(Date.Dried))

write_csv(mandaiSLA2014, "data/Yee_2014/data.csv")
