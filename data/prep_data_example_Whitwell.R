# Read raw data -----------------------------------------------------------
whitwell_raw <- read.csv(file = "./data/raw_data/example_data/Whitwell_common_SSSI_1994_1997.csv",
                         header = TRUE)


# Prepare Data ------------------------------------------------------------
whitwell_prepped <- whitwell_raw |>
  dplyr::select("Quadrat" = Quad,
                "Species" = Name,
                "Year") |>
  dplyr::mutate("Group" = "A",
                "Cover" = NA,
                "Site" = "Whitwell Common") |>
  dplyr::select(Site, Year, Group, Quadrat, Species, Cover)

# Clean Names -------------------------------------------------------------

# Compile Data ------------------------------------------------------------
exampleData_whitwell <- rbind(whitwell_prepped)

# Save Data ---------------------------------------------------------------
saveRDS(object = exampleData_whitwell, file = "./data/bundled_data/exampleData_whitwell.rds")
