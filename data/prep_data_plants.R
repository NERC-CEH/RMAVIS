concordance_all <- readRDS(file = "./data/bundled_data/concordance_all.rds")
concordance_all_trimmed <- concordance_all |>
  dplyr::select("proposedSpecies" = proposedSpecies,
                "taxonId" = bsbiTaxonId)

bsbiChecklistStace <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_stace3.csv",
                                        delim = "\t", escape_double = FALSE,
                                        trim_ws = TRUE,
                                        show_col_types = FALSE)

bsbiTaxonConcepts <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_BRC_taxonConcepts.csv",
                                       delim = "\t", escape_double = FALSE,
                                       trim_ws = TRUE,
                                       show_col_types = FALSE)

HE_F <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_F.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "F")

length(HE_F$taxonId) == nrow(HE_F)

HE_L <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_L.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "L")

length(HE_L$taxonId) == nrow(HE_L)

HE_N <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_N.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "N")

length(HE_N$taxonId) == nrow(HE_N)

HE_R <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_R.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "R")

length(HE_R$taxonId) == nrow(HE_R)

HE_S <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_HE_S.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "S")

length(HE_S$taxonId) == nrow(HE_S)

rarityNewAtlas_GB <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_rarityNewAtlas_GB.csv", 
                                       delim = "\t", escape_double = FALSE, 
                                       trim_ws = TRUE,
                                       show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Rarity - GB")

length(rarityNewAtlas_GB$taxonId) == nrow(rarityNewAtlas_GB)

rarityNewAtlas_Ireland <- readr::read_delim("./data/raw_data/bsbi_checklist/bsbi_checklist_rarityNewAtlas_Ireland.csv", 
                                            delim = "\t", escape_double = FALSE, 
                                            trim_ws = TRUE,
                                            show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Rarity - Ireland")

length(rarityNewAtlas_Ireland$taxonId) == nrow(rarityNewAtlas_Ireland)

bsbiChecklistData <- rbind(
  HE_F,
  HE_L,
  HE_N,
  HE_R,
  HE_S,
  rarityNewAtlas_GB,
  rarityNewAtlas_Ireland
) |>
  tidyr::pivot_wider(names_from = dataType, values_from = dataValue) |>
  dplyr::mutate("F" = as.numeric(`F`),
                "L" = as.numeric(L),
                "N" = as.numeric(N),
                "R" = as.numeric(R),
                "S" = as.numeric(S)) |>
  dplyr::select(-key)


bsbiChecklistData_ready <- bsbiChecklistData

bsbiTaxonConcepts_ready <- bsbiTaxonConcepts |>
  dplyr::select(-c(key, dataValue, origTaxonId, preferredTaxonQualifier, origTaxon, origQualifier, notes, referenceSummary, `reference entity id`)) |>
  dplyr::distinct()

# Expect a many to many join because of the "Acer campestre (c)" strata taxa
plants_data <- bsbiTaxonConcepts_ready |>
  dplyr::left_join(bsbiChecklistData_ready, by = "taxonId") |>
  dplyr::right_join(concordance_all_trimmed, by = "taxonId") |>
  dplyr::select(-taxonId, -preferredQualifiedTaxonName, -preferredTaxon) |>
  dplyr::rename("species" = "proposedSpecies")

length(plants_data$species) == nrow(plants_data)


