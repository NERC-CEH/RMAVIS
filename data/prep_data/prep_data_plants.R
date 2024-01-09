concordance_all <- readRDS(file = "./data/bundled_data/concordance_all.rds")
concordance_all_trimmed <- concordance_all |>
  dplyr::select("proposedSpecies" = proposedSpecies,
                "taxonId" = bsbiTaxonId)


# Stace v3 ----------------------------------------------------------------
bsbiChecklistStace <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_stace3.csv",
                                        delim = "\t", escape_double = FALSE,
                                        trim_ws = TRUE,
                                        show_col_types = FALSE)



# Taxon concepts ----------------------------------------------------------
bsbiTaxonConcepts <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_BRC_taxonConcepts.csv",
                                       delim = "\t", escape_double = FALSE,
                                       trim_ws = TRUE,
                                       show_col_types = FALSE)


bsbiTaxonConcepts_ready <- bsbiTaxonConcepts |>
  dplyr::filter(key == preferredQualifiedTaxonName) |>
  dplyr::select(-c(key, dataValue, origTaxonId, preferredTaxonQualifier, origTaxon, origQualifier, notes, referenceSummary, `reference entity id`)) |>
  dplyr::distinct()

# Check there are no duplicate taxonIds
bsbiTaxonConcepts_ready |>
  dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(taxonId)


# Hill-Ellenberg F --------------------------------------------------------
HE_F <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_F.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::filter(!(key %in% c("Cerastium arcticum auct., non Lange", "Asparagus officinalis L. subsp. officinalis", "Callitriche hamulata s.l."))) |>
  dplyr::select("key" = key, 
                "taxonId" = taxonId, 
                "F" = dataValue)

length(unique(HE_F$taxonId)) == nrow(HE_F)

HE_F |> dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()



# Hill-Ellenberg L --------------------------------------------------------
HE_L <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_L.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::filter(!(key %in% c("Cerastium arcticum auct., non Lange", "Asparagus officinalis L. subsp. officinalis", "Callitriche hamulata s.l."))) |>
  dplyr::select(#"key" = key, 
                "taxonId" = taxonId, 
                "L" = dataValue)

length(unique(HE_L$taxonId)) == nrow(HE_L)

HE_L |> dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()

# Hill-Ellenberg N --------------------------------------------------------
HE_N <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_N.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::filter(!(key %in% c("Cerastium arcticum auct., non Lange", "Asparagus officinalis L. subsp. officinalis", "Callitriche hamulata s.l."))) |>
  dplyr::select(#"key" = key, 
                "taxonId" = taxonId, 
                "N" = dataValue)

length(unique(HE_N$taxonId)) == nrow(HE_N)

HE_N |> dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()

# Hill-Ellenberg R --------------------------------------------------------
HE_R <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_R.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::filter(!(key %in% c("Cerastium arcticum auct., non Lange", "Asparagus officinalis L. subsp. officinalis", "Callitriche hamulata s.l."))) |>
  dplyr::select(#"key" = key, 
                "taxonId" = taxonId, 
                "R" = dataValue)

length(unique(HE_R$taxonId)) == nrow(HE_R)

HE_R |> dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()

# Hill-Ellenberg S --------------------------------------------------------
HE_S <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_S.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::filter(!(key %in% c("Cerastium arcticum auct., non Lange", "Asparagus officinalis L. subsp. officinalis", "Callitriche hamulata s.l."))) |>
  dplyr::select(#"key" = key, 
                "taxonId" = taxonId, 
                "S" = dataValue)

length(unique(HE_S$taxonId)) == nrow(HE_S)

HE_S |> dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()

# Rarity GB ---------------------------------------------------------------
rarityNewAtlas_GB <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_rarityNewAtlas_GB.csv", 
                                       delim = "\t", escape_double = FALSE, 
                                       trim_ws = TRUE,
                                       show_col_types = FALSE) |>
  dplyr::select(#"key" = key, 
                "taxonId" = taxonId, 
                "rarityGB" = dataValue)

length(unique(rarityNewAtlas_GB$taxonId)) == nrow(rarityNewAtlas_GB)

rarityNewAtlas_GB |> dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()

# Rarity Ireland ----------------------------------------------------------
rarityNewAtlas_Ireland <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_rarityNewAtlas_Ireland.csv", 
                                            delim = "\t", escape_double = FALSE, 
                                            trim_ws = TRUE,
                                            show_col_types = FALSE) |>
  dplyr::select(#"key" = key, 
                "taxonId" = taxonId, 
                "rarityIr" = dataValue)

length(unique(rarityNewAtlas_Ireland$taxonId)) == nrow(rarityNewAtlas_Ireland)

rarityNewAtlas_Ireland |> dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup()

# Compile data ------------------------------------------------------------
bsbiChecklistData <- HE_F |>
  dplyr::left_join(HE_L, by = "taxonId") |>
  dplyr::left_join(HE_N, by = "taxonId") |>
  dplyr::left_join(HE_R, by = "taxonId") |>
  dplyr::left_join(HE_S, by = "taxonId") |>
  dplyr::left_join(rarityNewAtlas_GB, by = "taxonId") |>
  dplyr::left_join(rarityNewAtlas_Ireland, by = "taxonId")

# Check there are no duplicate taxonId values in bsbiChecklistData
length(bsbiChecklistData$taxonId) == nrow(bsbiChecklistData)
length(unique(bsbiChecklistData$taxonId)) == length(bsbiChecklistData$taxonId)

bsbiChecklistData |> 
  dplyr::group_by(taxonId) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(taxonId)

# Create bsbiChecklistData_ready
bsbiChecklistData_ready <- bsbiChecklistData



# Compose plants_data -----------------------------------------------------
plants_data <- bsbiChecklistData_ready |>
  dplyr::left_join(concordance_all_trimmed, by = "taxonId") |>
  dplyr::mutate(
    "proposedSpecies" = 
      dplyr::case_when(
        is.na(proposedSpecies) == FALSE ~ proposedSpecies,
        is.na(proposedSpecies) == TRUE ~ key,
        TRUE ~ as.character(key)
      )
  ) |>
  dplyr::rename("species" = "proposedSpecies") |>
  dplyr::distinct() |>
  dplyr::filter(!(taxonId %in% c("2cd4p9h.7mxs2v", "2cd4p9h.cew",
                                 "2cd4p9h.v4k", "2cd4p9h.w2r"))) |>
  dplyr::select(-key)

plants_data_check <- plants_data |> 
  dplyr::group_by(species) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(species)

plants_data_check

length(unique(plants_data$species)) == nrow(plants_data)


