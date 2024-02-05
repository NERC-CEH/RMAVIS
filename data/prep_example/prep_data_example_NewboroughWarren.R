# Retrieve utilities ------------------------------------------------------
source("R/create_constants.R")

# Read raw data -----------------------------------------------------------
newboroughWarren_raw <- read.csv(file = "./data/raw_data/example_data/NRestSlack_fulldata_LJ2_prepped.csv")


# Prepare Newborough Warren Data --------------------------------------------------
newboroughWarren_prepped <- newboroughWarren_raw |>
  base::t() |>
  janitor::row_to_names(row_number = 1) |>
  tibble::as_tibble(rownames = "Year.Quadrat") |>
  dplyr::mutate("Year" = stringr::str_extract(string = Year.Quadrat, pattern = "(\\d{4})") |> as.integer(), .before = "Year.Quadrat") |>
  dplyr::mutate("Group" = stringr::str_extract(string = Year.Quadrat, pattern = "(\\d{1})(\\d{1})$", group = 1), .after = "Year") |>
  dplyr::mutate("Group" = paste0("Slack ", Group)) |>
  dplyr::mutate("Quadrat" = stringr::str_extract(string = Year.Quadrat, pattern = "(\\d{2}$)"), .after = "Year") |>
  dplyr::select(-Year.Quadrat) |>
  tidyr::pivot_longer(cols = -c(Year, Group, Quadrat),
                      names_to = "Species",
                      values_to = "Cover") |>
  dplyr::filter(!(Cover %in% c("."))) |>
  dplyr::mutate("Cover" = as.double(Cover) / 100) |>
  dplyr::mutate("Site" = "Newborough Warren") |>
  dplyr::select(Site, Year, Group, Quadrat, Species, Cover)

# Clean Names -------------------------------------------------------------
newboroughWarren_cleanNames <- newboroughWarren_prepped |>
  dplyr::mutate(
    "Species" = 
      dplyr::case_when(
        Species == "Festuca pratensis" ~ "Schedonorus pratensis", 
        Species == "Leontodon autumnalis" ~ "Scorzoneroides autumnalis", 
        Species == "Senecio jacobaea" ~ "Jacobaea vulgaris", 
        Species == "Anagallis arvensis" ~ "Lysimachia arvensis", 
        Species == "Vicia hirsuta" ~ "Ervilia hirsuta", 
        Species == "Chamerion angustifolium" ~ "Chamaenerion angustifolium", 
        Species == "Pellia sp" ~ "Pellia", 
        Species == "Thymus polytrichus" ~ "Thymus drucei", 
        Species == "Thymus praecox arcticus" ~ "Thymus drucei", 
        Species == "Listera ovata" ~ "Neottia ovata", 
        Species == "Carex serotina" ~ "Carex viridula subsp. viridula",
        Species == "Poa subcaerulea" ~ "Poa humilis",
        Species == "Dactylorhiza sp." ~ "Dactylorhiza",
        Species == "Cardamine sp." ~ "Cardamine",
        Species == "Taraxacum officinale agg." ~ "Taraxacum",
    
        Species == "Anagallis tenella" ~ "Lysimachia tenella",
        Species == "Hypochoeris radicata" ~ "Hypochaeris radicata",
        Species == "Leontodon taraxacoides" ~ "Leontodon saxatilis",
        Species == "Salix repens agg." ~ "Salix repens",
        Species == "Calliergon cuspidatum" ~ "Calliergonella cuspidata",
        Species == "Taraxacum officinale" ~ "Taraxacum",
        Species == "Drepanocladus sp" ~ "Drepanocladus",
        Species == "Scirpus setaceus" ~ "Isolepis setacea",
        Species == "Barbula fallax" ~ "Didymodon fallax",
        Species == "Filaginella uliginosa" ~ "Gnaphalium uliginosum",
        Species == "Euphrasia officinalis agg" ~ "Euphrasia officinalis",
        Species == "Pilosella officinarum agg" ~ "Pilosella officinarum",
        Species == "Taraxacum sect. erythrosperma" ~ "Taraxacum",
        Species == "Barbula tophacea" ~ "Didymodon tophaceus",
        Species == "Campylium polygamum" ~ "Drepanocladus polygamus",
        Species == "Viola tricolor curtisii" ~ "Viola tricolor subsp. curtisii",
        Species == "Bryum algovicum var rutheanum" ~ "Bryum algovicum",
        Species == "Hieracium sp" ~ "Hieracium",
        Species == "Daucus carota" ~ "Daucus carota subsp. carota",
        Species == "Tortula ruralis ssp ruraliformis" ~ "Syntrichia ruraliformis",
        Species == "Salix aurita (s)" ~ "Salix aurita",
        Species == "Bryum sp" ~ "Bryum",
        Species == "Scleropodium purum" ~ "Pseudoscleropodium purum",
        Species == "Rhytidiadelphus triquetrus" ~ "Hylocomiadelphus triquetrus",
        TRUE ~ as.character(Species)
      )
  ) |>
  dplyr::group_by(Site, Year, Group, Quadrat, Species) |>
  dplyr::summarise("Cover" = sum(Cover)) |>
  dplyr::ungroup()

# Save Data ---------------------------------------------------------------
saveRDS(object = newboroughWarren_cleanNames, file = "./data/bundled_data/exampleData_newboroughWarren.rds")
