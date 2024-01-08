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
  dplyr::mutate("Quadrat" = stringr::str_extract(string = Year.Quadrat, pattern = "(\\d{1}$)"), .after = "Year") |>
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
        # Species == "Centaurium littorale" ~ "",
        Species == "Hypochoeris radicata" ~ "Hypochaeris radicata",
        Species == "Leontodon taraxacoides" ~ "Leontodon saxatilis",
        Species == "Salix repens agg." ~ "Salix repens",
        # Species == "Carex demissa" ~ "",
        Species == "Calliergon cuspidatum" ~ "Calliergonella cuspidata",
        Species == "Taraxacum officinale agg." ~ "Taraxacum",
        Species == "Drepanocladus sp" ~ "Drepanocladus sp.",
        Species == "Pseudoscleropodium purum" ~ "Scleropodium purum",
        Species == "Scirpus setaceus" ~ "Isolepis setacea",
        Species == "Barbula fallax" ~ "Didymodon fallax",
        # Species == "Veronica anagallis-aquatica" ~ "",
        Species == "Filaginella uliginosa" ~ "Gnaphalium uliginosum",
        # Species == "Carex serotina" ~ "",
        Species == "Euphrasia officinalis agg" ~ "Euphrasia officinalis",
        Species == "Pilosella officinarum agg" ~ "Hieracium pilosella",
        Species == "Taraxacum sect. erythrosperma" ~ "Taraxacum",
        Species == "Barbula tophacea" ~ "Didymodon tophaceus",
        # Species == "Poa subcaerulea" ~ "",
        Species == "Campylium polygamum" ~ "Drepanocladus polygamus",
        # Species == "Aira caryophyllea" ~ "",
        Species == "Viola tricolor curtisii" ~ "Viola tricolor subsp. curtisii",
        Species == "Ceratodon purpureus" ~ "Ceratodon purpureus s.l.",
        Species == "Bryum algovicum var rutheanum" ~ "Bryum algovicum",
        # Species == "Cardamine sp." ~ "",
        # Species == "Dactylorhiza purpurella" ~ "",
        Species == "Hieracium sp" ~ "Hieracium",
        Species == "Daucus carota" ~ "Daucus carota subsp. carota",
        Species == "Avenula pubescens" ~ "Helictotrichon pubescens",
        # Species == "Pellia sp" ~ "Pellia sp.",
        # Species == "Campylium chrysophyllum" ~ "",
        Species == "Thymus praecox arcticus" ~ "Thymus polytrichus",
        Species == "Bryum bicolor" ~ "Bryum dichotomum s.l.",
        Species == "Tortula ruralis ssp ruraliformis" ~ "Syntrichia ruraliformis",
        Species == "Salix aurita (s)" ~ "Salix aurita",
        # Species == "Dactylorhiza sp." ~ "",
        Species == "Bryum sp" ~ "Bryum",
        TRUE ~ as.character(Species)
      )
  )

# Save Data ---------------------------------------------------------------
saveRDS(object = newboroughWarren_cleanNames, file = "./data/bundled_data/exampleData_newboroughWarren.rds")
