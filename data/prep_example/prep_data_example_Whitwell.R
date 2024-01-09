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
whitwell_cleanNames <- whitwell_prepped |>
  dplyr::mutate(
    "Species" = 
      dplyr::case_when(
        Species == "Lotus uliginosus" ~ "Lotus pedunculatus",
        Species == "Calliergon cuspidatum" ~ "Calliergonella cuspidata",
        Species == "Polygonum amphibium" ~ "Persicaria amphibia",
        Species == "Dactylorhiza sp." ~ "Dactylorhiza",
        Species == "Cerastium fontanum triviale" ~ "Cerastium fontanum",
        Species == "Rubus fruticosus agg (g)" ~ "Rubus fruticosus",
        Species == "Prunus spinosa (s)" ~ "Prunus spinosa",
        Species == "Galium uligonosum" ~ "Galium uliginosum",
        Species == "Lotus uligonosum" ~ "Lotus pedunculatus",
        Species == "Juncus articulata" ~ "Juncus articulatus",
        Species == "Pulicaria dysentrica" ~ "Pulicaria dysenterica",
        Species == "Rhiananthus minor" ~ "Rhinanthus minor",
        Species == "Lynchis flos-cuculi" ~ "Silene flos-cuculi",
        Species == "Lychnis flos-cuculi" ~ "Silene flos-cuculi",
        Species == "Tortilis japonica" ~ "Torilis japonica",
        Species == "Taraxacum officinalis" ~ "Taraxacum",
        Species == "Eurynchium praelongum" ~ "Eurhynchium praelongum",
        Species == "Festuca arundinacea" ~  "Schedonorus arundinaceus",
        Species == "Vicia hirsuta" ~  "Ervilia hirsuta",
        TRUE ~ as.character(Species)
      )
  )

# Compile Data ------------------------------------------------------------
exampleData_whitwell <- rbind(whitwell_cleanNames)

# Save Data ---------------------------------------------------------------
saveRDS(object = exampleData_whitwell, file = "./data/bundled_data/exampleData_whitwell.rds")
