library(tidyverse)


# Read raw data -----------------------------------------------------------

rahman <-
  read_csv(
    "data/Rahman_2023/raw/functional_traits_untransformed_decomposition_flammability.csv"
  ) |>
  # remove traits that are not part of AusTraits' dictionary; worth revisiting
  # fuel moisture content later (FMC) and other flammability traits
  select(
    -matches(
      "CNR|NPR|^LA|SAV|FMC|^DW|Si|_PC|decomp|dpyr_sen|dsmo_sen|log|_sd_|Tpyr|Tsmo|rate|Tmax|Tcum|tsmo"
    ),
    -ends_with("_res")
  ) |>
  pivot_longer(cols = -species, names_to = "trait_name", values_drop_na = TRUE)


# Export cleaned data -----------------------------------------------------

write_csv(rahman, "data/Rahman_2023/data.csv")
