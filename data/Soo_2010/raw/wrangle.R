# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)



# Import dasta -------------------------------------------------------------
filename <- "data/Soo_2010/raw/SooWaiKit_native fruit size.xls"
# excel_sheets(filename)
fruit <- read_excel(filename, sheet = "Forest Species (emp+gen sub)")

# Tidy up
out <- 
    fruit %>% 
    # remove comment row
    slice(-1) %>% 
    select(
        Name.auth,
        Dispersal,  # dispersal_syndrome
        Pollination,   # pollination_syndrome
        `Fruit type by Corlett`:`No. Seed`,   # fruit_type
        -`Fruit Width (cm)...27`, # this is a rounding column
        `Fruit Width (cm)` = `Fruit Width (cm)...26`
    ) %>% 
    # match numeric key to descriptions
    mutate(
        Dispersal = 
            case_match(Dispersal,
                       1 ~ "adhesion (spines, hooks, bars or callus hairs)",
                       2 ~ "ingestion (berries, drupes or aggregate fruits)",
                       3 ~ "wind (wing-like structures)",                   
                       4 ~ "water (fibrous endocarp or viviparous)",         
                       5 ~ "others"),
        Pollination = 
            case_match(Pollination,
                       1 ~ "insect",
                       2 ~ "mammal",
                       3 ~ "bird",                   
                       4 ~ "wind",         
                       5 ~ "others")
    ) %>% 
    # split range variables to two columns
    separate(`Fruit Length (cm)`,
             c("Fruit Length (cm) min",
               "Fruit Length (cm) max"),
             sep = "-")

# export output
