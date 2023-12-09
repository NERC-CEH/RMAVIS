# Read raw data -----------------------------------------------------------
storm_2021_raw <- read.csv(file = "./data/raw_data/example_data/storm_flora_2021.csv",
                           header = TRUE)

obs_per_site <- storm_2021_raw |>
  dplyr::filter(Storm == 1) |>
  dplyr::group_by(SITE_NO) |>
  dplyr::summarise("Count" = dplyr::n()) |>
  dplyr::ungroup()

stormWoods_2021 <- storm_2021_raw |>
  dplyr::filter(SITE_NO %in% c(2))


stormWoods_2021_prepped <- stormWoods_2021 |>
  dplyr::select("Quadrat" = Plot_no,
                "Species" = species) |>
  dplyr::mutate("Group" = "A",
                "Cover" = NA,
                "Site" = "Pickreed Wood",
                "Year" = 2021) |>
  dplyr::select(Site, Year, Group, Quadrat, Species, Cover)
  

# Compile Data ------------------------------------------------------------
exampleData_stormWoods <- rbind(stormWoods_2021_prepped)

# Save Data ---------------------------------------------------------------
saveRDS(object = exampleData_stormWoods, file = "./data/bundled_data/exampleData_stormWoods.rds")