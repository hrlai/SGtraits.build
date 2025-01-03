library(bdc)
library(fuzzyjoin)

# install gnparser here (read the following link)
# the required gnparser.exe is gitignored
# https://brunobrr.github.io/bdc/articles/help/installing_gnparser.html

synrefs_tab <- read_delim("config/floraSG/syn_refs.txt", delim = "\t")
synrefs_clean <-
    synrefs_tab %>%
    distinct(`Full name with authors`) %>% 
    filter(!is.na(`Full name with authors`)) %>% 
    pull(`Full name with authors`) %>% 
    bdc_clean_names(.) %>% 
    select(`Full name with authors` = scientificName,
           names_clean) %>% 
    right_join(
        synrefs_tab %>%
            distinct(main_ID, `Full name with authors`)
    ) %>% 
    filter(!is.na(names_clean)) %>% 
    distinct(main_ID, names_clean)

taxon_main_ID <- 
    SGtraits$taxonomic_updates %>% 
    distinct(original_name) %>% 
    filter(!is.na(original_name)) %>% 
    pull(original_name) %>% 
    bdc_clean_names(.) %>% 
    filter(!is.na(names_clean)) %>% 
    stringdist_left_join(synrefs_clean, 
                         by = c(names_clean = "names_clean"),
                         max_dist = 1) %>% 
    distinct(main_ID, scientificName) %>% 
    rename(aligned_name = scientificName)

main_tab <- 
    read_delim("config/floraSG/main.txt", delim = "\t") %>% 
    select(main_ID,
           taxon_name = `Full Name without Authors`,
           scientific_name = `Full Names with Authors`,
           division = Division,
           class = Class,
           order = Order,
           family = `Family (Current)`,
           status = `Status (2022)`,
           `Infraspecific taxon`,
           IPNI_ID,
           IPNI_url) %>% 
    mutate(taxon_rank = case_match(
        `Infraspecific taxon`,
        "f." ~ "subspecies",
        "n/a" ~ "species",
        "no" ~ "species",
        "no subsp." ~ "species",
        "var." ~ "subspecies",
        "yes" ~ "subspecies"),
        .keep = "unused")

taxon_list <- 
    taxon_main_ID %>% 
    left_join(main_tab)

write_csv(taxon_list, "config/taxon_list.csv")
