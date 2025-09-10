# This script is a testing ground to match species names in Soo 2010 to
# floraSG. I anticipate this script to be a temporary file, which will later
# be moved over to R/build_update_taxon_list.R



# Load Soo 2010 dataset from the SGtraits package -------------------------

# Install the package following steps on https://github.com/hrlai/SGtraits

library(tidyverse)
library(SGtraits)

# download (for the first time) and load the SGtraits database
SGtraits <- load_database(version = "0.0.2", path = "tmp/SGtraits")

# extract Soo 2010
Soo_2010 <- extract_dataset(SGtraits, "Soo_2010")

# extract taxa in Soo 2010
taxa_test <- Soo_2010$taxa$taxon_name

# Match with floraSG ------------------------------------------------------

# The following lines of code are modified from R/splitnames.R in the
# floraSG repo
# Summary of steps:
# neutralize the filial f. to avoid confusion with forma
# split forma by " f. "
# split variety by " var. "
# split subspecies by " subsp. "
# split the remaining first part by " " and save the first element as genus
# save the rest as specific epithet with authorities
# split all the epithets and authors
# reinstate filial f.

reinstate_f <- function(x) {
  str_replace_all(x, c(
    "Hallier_f." = "Hallier f.",
    "Forsyth_f." = "Forsyth f."
  ))
}

taxa_test_df <- 
  taxa_test %>%
  str_replace_all(c(
    "Hook. f." = "Hook.f.",
    "L. f." = "L.f.",
    "Rchb. f." = "Rchb.f.",
    "Burm. f." = "Burm.f.",
    "Hallier f." = "Hallier_f.",
    "Forsyth f." = "Forsyth_f.",
    "ssp." = "subsp."
  )) %>%
  data.frame(fullName_wAuth = .) %>%
  separate(fullName_wAuth,
    into = c("allElse", "forma_wAuth"),
    sep = " f. ", remove = FALSE
  ) %>%
  separate(allElse,
    into = c("allElse", "variety_wAuth"),
    sep = " var. ", remove = TRUE
  ) %>%
  separate(allElse,
    into = c("allElse", "subspecies_wAuth"),
    sep = " subsp. ", remove = TRUE
  ) %>%
  mutate(genus = str_split_i(allElse, " ", 1)) %>%
  mutate(species_wAuth = str_replace(allElse, paste0(genus, " "), "")) %>%
  select(-allElse) %>%
  mutate(
    species_woAuth = str_split_i(species_wAuth, " ", 1),
    subspecies_woAuth = str_split_i(subspecies_wAuth, " ", 1),
    variety_woAuth = str_split_i(variety_wAuth, " ", 1),
    forma_woAuth = str_split_i(forma_wAuth, " ", 1)
  ) %>%
  mutate(fullName_woAuth = paste(
    genus, species_woAuth,
    "subsp.", subspecies_woAuth,
    "var.", variety_woAuth,
    "f.", forma_woAuth
  )) %>%
  mutate(fullName_woAuth = str_replace_all(
    fullName_woAuth,
    c(
      " subsp. NA " = " ",
      " var. NA " = " ",
      " f. NA" = ""
    )
  )) %>%
  mutate(across(contains("_wAuth"), reinstate_f))

# Match to floraSG nomenclature

## floraSG repo is in a sister folder to SGtraits.build
## yours might vary
## ideal is to get floraSG published as a package so it can be loaded as one
here <- getwd()
setwd("../floraSG")
source("./R/database_check.R")

taxa_test_out <- database_check(taxa_test_df, colname_SppOnly = "fullName_woAuth")

# check with the original data

taxa_test_out2 <- database_check(data.frame(fullName_wAuth = taxa_test),
  colname_SppAuthor = "fullName_wAuth"
)
