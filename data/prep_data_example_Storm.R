# Read raw data -----------------------------------------------------------
storm_2021_raw <- read.csv(file = "./data/raw_data/example_data/storm_flora_2021.csv",
                           header = TRUE)

storm_1971_2001_raw <- read.csv(file = "./data/raw_data/example_data/storm_flora_1971_2001.csv",
                                header = TRUE)


# Check the number of observations per site -------------------------------
obs_per_site <- storm_2021_raw |>
  dplyr::filter(Storm == 1) |>
  dplyr::group_by(SITE_NO) |>
  dplyr::summarise("Count" = dplyr::n()) |>
  dplyr::ungroup()


obs_per_site <- storm_1971_2001_raw |>
  dplyr::filter(Storm == 1) |>
  dplyr::group_by(SITE.x) |>
  dplyr::summarise("Count" = dplyr::n()) |>
  dplyr::ungroup()



# Select sites to prepare -------------------------------------------------
storm_2021 <- storm_2021_raw |>
  dplyr::filter(SITE_NO %in% c(88))

storm_1971_2001 <- storm_1971_2001_raw |>
  dplyr::filter(SITE.x %in% c(88))



# Prepare data ------------------------------------------------------------
storm_2021_prepped <- storm_2021 |>
  dplyr::select("Quadrat" = Plot_no,
                "Species" = species) |>
  dplyr::mutate("Group" = "A",
                "Cover" = NA,
                "Site" = "Leith Hill Place Wood",
                "Year" = 2021) |>
  dplyr::select(Site, Year, Group, Quadrat, Species, Cover) |>
  dplyr::mutate("Year" = as.integer(Year)) |>
  dplyr::arrange(Year, Group, Quadrat)
  

storm_1971_2001_prepped <- storm_1971_2001|>
  dplyr::select("Quadrat" = PLOT.x,
                "Species" = BRC_NAMES,
                "Year" = Yr_2,
                "BRC.Code" = BRC_NUMBER) |>
  dplyr::mutate(
    "Year" = 
      dplyr::case_when(
        Year == 1 ~ 1971,
        Year == 2 ~ 2001,
        TRUE ~ NA
      )
  ) |>
  dplyr::mutate("Group" = "A",
                "Cover" = NA,
                "Site" = "Leith Hill Place Wood") |>
  dplyr::select(Site, Year, Group, Quadrat, Species, Cover) |>
  dplyr::mutate("Year" = as.integer(Year)) |>
  dplyr::arrange(Year, Group, Quadrat)
  
  

# Check Species Names -----------------------------------------------------

storm_1971_2001_prepped_species <- unique(storm_1971_2001_prepped$Species)
storm_2021_prepped_species <- unique(storm_2021_prepped$Species)

setdiff(storm_1971_2001_prepped_species, storm_2021_prepped_species)
setdiff(storm_2021_prepped_species, storm_1971_2001_prepped_species)


# Check all quadrats are present in all years -----------------------------

storm_1971_2001_prepped_quadrats <- unique(storm_1971_2001_prepped$Quadrat)
storm_2021_prepped_quadrats <- unique(storm_2021_prepped$Quadrat)

  
# Clean Names -------------------------------------------------------------


# Compile Data ------------------------------------------------------------
exampleData_stormWoods <- rbind(storm_2021_prepped, storm_1971_2001_prepped) |>
  dplyr::arrange(Year, Group, Quadrat) |>
  dplyr::distinct()

# Save Data ---------------------------------------------------------------
saveRDS(object = exampleData_stormWoods, file = "./data/bundled_data/exampleData_stormWoods.rds")

