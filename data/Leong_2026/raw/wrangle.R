library(tidyverse)




# Read raw data -----------------------------------------------------------

sla <- 
  read_csv("data/Leong_2026/raw/SLA_individual.csv") |> 
  select(ID, species, lamina_SLA) |> 
  pivot_longer(cols = c(lamina_SLA),
               names_to = "trait_name",
               values_to = "value") 
  
ldmc <- 
  read_csv("data/Leong_2026/raw/LDMC_individual.csv") |> 
  select(ID, species, lamina_ldmc) |> 
  pivot_longer(cols = c(lamina_ldmc),
               names_to = "trait_name",
               values_to = "value") 

chem <- 
  read_csv("data/Leong_2026/raw/leafchem_individual.csv") |> 
  select(ID, species, K, P, N, C, S) |> 
  pivot_longer(cols = c(K, P, N, C, S),
               names_to = "trait_name",
               values_to = "value")

twd <- 
  read_csv("data/Leong_2026/raw/TWD_individual.csv") |> 
  select(ID, species, TWD) |> 
  pivot_longer(cols = TWD,
               names_to = "trait_name",
               values_to = "value") 




# Combine datasets --------------------------------------------------------

out <-
  bind_rows(sla, ldmc, chem, twd)

write_csv(out, "data/Leong_2026/data.csv")
