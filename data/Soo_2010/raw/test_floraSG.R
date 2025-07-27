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

# Kwek Yan please help match taxa_test to floraSG nomenclature
# in principal most if not all should have a match because Soo Wai Kit
# followed Chong et al. 2009?
# Delete this comment after reading