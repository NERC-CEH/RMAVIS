# Retrieve utilities ------------------------------------------------------
source("R/create_constants.R")

dominCoverMidPointPerc_df <- tibble::enframe(dominCoverMidPointPerc) |>
  dplyr::rename("Domin" = name,
                "Cover" = value)


# Read raw data -----------------------------------------------------------
pd_1970_raw <- read.csv(file = "./data/raw_data/example_data/PD_SurveyData1970.csv",
                        header = FALSE)

pd_1990_raw <- read.csv(file = "./data/raw_data/example_data/PD_SurveyData1990.csv",
                        header = FALSE)

pd_2016_raw <- read.csv(file = "./data/raw_data/example_data/PD_SurveyData2016.csv",
                        header = FALSE)


# Prep 1970 ---------------------------------------------------------------
pd_1970_prepped <- pd_1970_raw |>
  dplyr::filter(!(V1 %in% c("Vegetation height (cm):", "Recorder:", "Species:"))) |>
  base::t() |>
  tibble::as_tibble() |>
  janitor::row_to_names(row = 1) |>
  dplyr::rename("Group" = "Transect number:",
                "Quadrat" = "Quadrat no:") |>
  tidyr::pivot_longer(cols = -c(Group, Quadrat),
                      names_to = "Species",
                      values_to = "Domin") |>
  dplyr::filter(Domin != "") |>
  dplyr::left_join(dominCoverMidPointPerc_df, by = "Domin") |>
  dplyr::select(-Domin) |>
  dplyr::mutate("Year" = 1970, .before = "Group") |>
  # Add transect prefix to quadrat name to ensure each quadratID is unique
  dplyr::mutate("Quadrat" = paste0(Quadrat, ".", Group))


# Prep 1990 ---------------------------------------------------------------
pd_1990_prepped <- pd_1990_raw |>
  dplyr::filter(!(V1 %in% c("Vegetation height (cm):", "Recorder:", "Species:"))) |>
  base::t() |>
  tibble::as_tibble() |>
  janitor::row_to_names(row = 1) |>
  dplyr::rename("Group" = "Transect number:",
                "Quadrat" = "Quadrat no:") |>
  tidyr::pivot_longer(cols = -c(Group, Quadrat),
                      names_to = "Species",
                      values_to = "Domin") |>
  dplyr::filter(Domin != "") |>
  dplyr::left_join(dominCoverMidPointPerc_df, by = "Domin") |>
  dplyr::select(-Domin) |>
  dplyr::mutate("Year" = 1990, .before = "Group") |>
  # Add transect prefix to quadrat name to ensure each quadratID is unique
  dplyr::mutate("Quadrat" = paste0(Quadrat, ".", Group))

# Prep 2016 ---------------------------------------------------------------
pd_2016_prepped <- pd_2016_raw |>
  dplyr::filter(!(V1 %in% c("Vegetation height (cm):", "Recorder:", "Species:"))) |>
  base::t() |>
  tibble::as_tibble() |>
  janitor::row_to_names(row = 1) |>
  dplyr::rename("Group" = "Transect number:",
                "Quadrat" = "Quadrat no:") |>
  tidyr::pivot_longer(cols = -c(Group, Quadrat),
                      names_to = "Species",
                      values_to = "Domin") |>
  dplyr::filter(Domin != "") |>
  dplyr::left_join(dominCoverMidPointPerc_df, by = "Domin") |>
  dplyr::select(-Domin) |>
  dplyr::mutate("Year" = 2016, .before = "Group") |>
  # Add transect prefix to quadrat name to ensure each quadratID is unique
  dplyr::mutate("Quadrat" = paste0(Quadrat, ".", Group))


# Clean Species Names -----------------------------------------------------


# Check Data --------------------------------------------------------------

# Check all quadrats are present in each years data
quadrats_1970 <- pd_1970_prepped$Quadrat |> unique()
quadrats_1990 <- pd_1990_prepped$Quadrat |> unique()
quadrats_2016 <- pd_2016_prepped$Quadrat |> unique()

identical(x = quadrats_1970, y = quadrats_1990)
identical(x = quadrats_1970, y = quadrats_2016)
identical(x = quadrats_1990, y = quadrats_2016)

# Check all groups are identical in each years data
groups_1970 <- pd_1970_prepped$Group |> unique()
groups_1990 <- pd_1990_prepped$Group |> unique()
groups_2016 <- pd_2016_prepped$Group |> unique()

identical(x = groups_1970, y = groups_1990)
identical(x = groups_1970, y = groups_2016)
identical(x = groups_1990, y = groups_2016)


# Check all group - quadrat combinations are unique
quadratGroup_1970 <- pd_1970_prepped |>
  dplyr::select(Year, Group, Quadrat) |>
  dplyr::distinct()

quadratGroup_1990 <- pd_1990_prepped |>
  dplyr::select(Year, Group, Quadrat) |>
  dplyr::distinct()

quadratGroup_2016 <- pd_2016_prepped |>
  dplyr::select(Year, Group, Quadrat) |>
  dplyr::distinct()

# Compile Data ------------------------------------------------------------
exampleData_pd <- rbind(pd_1970_prepped, pd_1990_prepped, pd_2016_prepped)

# Save Data ---------------------------------------------------------------
saveRDS(object = exampleData_pd, file = "./data/bundled_data/exampleData_pd.rds")


