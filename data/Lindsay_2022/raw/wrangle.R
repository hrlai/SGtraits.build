library(tidyverse)


habit <- 
    read_delim("data/Lindsay_2022/raw/main.txt", delim = "\t") %>% 
    select(`Full Names with Authors`,
           `Full Name without Authors`,
           Habit) %>% 
    filter(!is.na(Habit)) %>% 
    mutate(plant_growth_form = case_match(
        Habit,
        "climber/herb" ~ "climber",
        "shrub or tree" ~ "shrub",
        "shrub/tree" ~ "shrub",
        .default = Habit
    )) %>% 
    distinct(`Full Names with Authors`, plant_growth_form)

write_csv(habit, "data/Lindsay_2022/data.csv")
