# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)



# Import dasta -------------------------------------------------------------
fruit <- 
    read_excel("data/Soo_2010/raw/SooWaiKit_native fruit size.xls", 
               sheet = "Forest Species (emp+gen sub)") %>% 
    # remove comment row
    slice(-1) %>% 
    select(
        Name.auth,
        Dispersal,  # dispersal_syndrome
        Pollination,   # pollination_syndrome
        `Fruit type by Corlett`:`No. Seed`,   # fruit_type
        -`Fruit Width (cm)...27`, # this is a rounding column
        `Fruit Width (cm)` = `Fruit Width (cm)...26`
    )  %>% 
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
    ) 

# Tidy up

# IMPROVE ME: number extracted from string below may not be 100% foolproof

# handle numeric variables which are a mixture of range and mean separately
# function to extract min and max from string
get_number <- function(text, type) {
    num <- str_extract_all(text, "\\d+(\\.\\d+)?")
    num <- as.numeric(unlist(num))
    switch(type,
           min = min(num),
           max = max(num))
}

fruit_cont <- 
    fruit %>% 
    select(Name.auth, 
           `Fruit Length (cm)`,
           `Fruit Width (cm)`,
           `Seed Length (mm)`,
           `Seed width (mm)`,
           `No. Seed`) %>% 
    pivot_longer(cols = -Name.auth,
                 names_to = "Trait",
                 values_drop_na = TRUE) %>% 
    rowwise() %>% 
    mutate(min = get_number(value, "min"),
           max = get_number(value, "max")) %>% 
    ungroup() %>% 
    filter(is.finite(min),
           is.finite(max)) %>% 
    mutate(mean = ifelse(min == max, min, NA),
           min  = ifelse(min == max, NA, min),
           max  = ifelse(min == max, NA, max)) %>% 
    select(-value) %>% 
    pivot_wider(names_from = Trait,
                values_from = c(min, max, mean),
                names_glue = "{Trait} {.value}")

# export output
out <- 
    fruit %>% 
    select(Name.auth, 
           Dispersal:`Fruit type by Corlett`,
           `Fruit Colour`:`Fruit Description`,
           Aril:`Seed Colour`) %>% 
    left_join(fruit_cont)

write_csv(out, "data/Soo_2010/data.csv")
