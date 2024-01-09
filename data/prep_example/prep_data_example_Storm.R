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

# Compile Data ------------------------------------------------------------
exampleData_stormWoods <- rbind(storm_2021_prepped, storm_1971_2001_prepped) |>
  dplyr::arrange(Year, Group, Quadrat) |>
  dplyr::distinct()

# Clean Names -------------------------------------------------------------
stormWoods_cleanNames <- exampleData_stormWoods |>
  dplyr::mutate(
    "Species" = 
      dplyr::case_when(
        Species == "Pellia sp." ~ "Pellia",
        Species == "Lophocolea sp." ~ "Lophocolea",
        Species == "Poa nemoralis/trivialis" ~ "Poa",
        Species == "Quercus seedling/sp" ~ "Quercus",
        Species == "Rubus fruticosus agg." ~ "Rubus fruticosus",
        Species == "Viola riviniana/reichenbiana" ~ "Viola",
        Species == "Ulmus sp." ~ "Ulmus",
        Species == "Betula seedling/sp" ~ "Betula",
        Species == "Abies sp." ~ "Abies",
        Species == "Thuja spp." ~ "Thuja",
        Species == "Populus sp" ~ "Populus",
        Species == "Hypnum cupressiforme sens.lat." ~ "Hypnum cupressiforme",
        Species == "Isopterygium elegans" ~ "Pseudotaxiphyllum elegans",
        Species == "Dryopteris dilatata/carthusiana" ~ "Dryopteris",
        Species == "Sphagnum (green/thin)" ~ "Sphagnum",
        Species == "Sphagnum sp." ~ "Sphagnum",
        Species == "Pseudotsuga spp." ~ "Pseudotsuga menziesii (c)",
        Species == "Plagiochila sp" ~ "Plagiochila",
        Species == "Dryopteris affinis/f-mas" ~ "Dryopteris",
        Species == "Fraxinus excelsior" ~ "Fraxinus excelsior (c)",
        Species == "Hedera helix" ~ "Hedera helix (g)",
        Species == "Fissidens sp." ~ "Fissidens",
        # Species == "Hypericum androsaemum" ~ "",
        Species == "Agrostis canina sens.lat." ~ "Agrostis canina",
        Species == "Galeopsis tetrahit agg." ~ "Galeopsis tetrahit",
        Species == "Cardamine hirsuta/flexuosa" ~ "Cardamine",
        Species == "Brachythecium sp." ~ "Brachythecium",
        Species == "Rumex conglomeratus/sanguineus" ~ "Rumex",
        Species == "Sorbus sp." ~ "Sorbus",
        Species == "Ribes nigrum/rubrum" ~ "Ribes",
        # Species == "Impatiens parviflora" ~ "",
        Species == "Juncus bufonius sens.lat." ~ "Juncus bufonius",
        Species == "Salix cin/cap" ~ "Salix",
        Species == "Sphagnum (green/fat)" ~ "Sphagnum",
        Species == "Cladonia sp." ~ "Cladonia",
        Species == "Arctium agg." ~ "Arctium",
        Species == "Prunus avium" ~ "Prunus avium (c)",
        Species == "Larix sp." ~ "Larix",
        Species == "Viburnum opulus" ~ "Viburnum opulus (s)",
        Species == "Impatiens sp." ~ "Impatiens",
        Species == "Taraxacum agg." ~ "Taraxacum",
        Species == "Callitriche stagnalis s.l." ~ "Callitriche stagnalis",
        Species == "Senecio erucifolius" ~ "Jacobaea erucifolia",
        Species == "Festuca gigantea" ~ "Schedonorus giganteus",
        Species == "Deschampsia flexuosa" ~ "Avenella flexuosa",
        TRUE ~ as.character(Species)
      )
  )

# Save Data ---------------------------------------------------------------
saveRDS(object = stormWoods_cleanNames, file = "./data/bundled_data/exampleData_stormWoods.rds")

