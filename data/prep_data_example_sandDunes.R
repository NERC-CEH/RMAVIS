# Retrieve utilities ------------------------------------------------------
source("R/create_constants.R")

# Read raw data -----------------------------------------------------------
newboroughWarren_raw <- read.csv(file = "./data/raw_data/example_data/NRestSlack_fulldata_LJ2_prepped.csv")


# Prepare Newborough Warren Data --------------------------------------------------
newboroughWarren_prepped <- newboroughWarren_raw |>
  base::t() |>
  janitor::row_to_names(row_number = 1) |>
  tibble::as_tibble(rownames = "Year.Quadrat") |>
  dplyr::mutate("Year" = stringr::str_extract(string = Year.Quadrat, pattern = "\\d{4}") |> as.integer(), .before = "Year.Quadrat") |>
  dplyr::mutate("Quadrat" = stringr::str_extract(string = Year.Quadrat, pattern = "\\d{2}$"), .after = "Year") |>
  dplyr::select(-Year.Quadrat) |>
  tidyr::pivot_longer(cols = -c(Year, Quadrat),
                      names_to = "Species",
                      values_to = "Cover") |>
  dplyr::filter(!(Cover %in% c("."))) |>
  dplyr::mutate("Cover" = as.double(Cover) / 100) |>
  dplyr::mutate("Site" = "Newborough Warren") |>
  dplyr::mutate("Group" = "A") |>
  dplyr::select(Site, Year, Group, Quadrat, Species, Cover)

exampleData_newboroughWarren <- newboroughWarren_prepped

# Clean Names -------------------------------------------------------------


# Save Data ---------------------------------------------------------------
saveRDS(object = exampleData_newboroughWarren, file = "./data/bundled_data/exampleData_newboroughWarren.rds")
