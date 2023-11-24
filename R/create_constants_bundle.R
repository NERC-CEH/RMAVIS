# Example Sample Data -----------------------------------------------------
example_data_df <- readxl::read_xlsx("data/raw_data/example_data.xlsx", sheet = "df")
saveRDS(object = example_data_df, file = "data/bundled_data/example_data_df.rds")

example_data_matrix <- readxl::read_xlsx("data/raw_data/example_data.xlsx", sheet = "matrix")
saveRDS(object = example_data_matrix, file = "data/bundled_data/example_data_matrix.rds")

# Data Entry Format Options -----------------------------------------------
dataEntryFormat_options <- c("Table" = "table",
                             "Matrix" = "matrix")
saveRDS(object = dataEntryFormat_options, file = "data/bundled_data/dataEntryFormat_options.rds")

# Domin Cover -------------------------------------------------------------
dominCoverVals <- c("91-100%" = 10,
                    "76-90%" = 9,
                    "51-75%" = 8,
                    "34-50%" = 7,
                    "26-33%" = 6,
                    "11-25%" = 5,
                    "4-10%" = 4,
                    "<4% (many individuals)" = 3,
                    "<4% (several individuals)" = 2,
                    "<4% (few individuals)" = 1)
saveRDS(object = dominCoverVals, file = "data/bundled_data/dominCoverVals.rds")

dominCoverValsRev <- c("10" = "91-100%",
                       "9" = "76-90%",
                       "8" = "51-75%",
                       "7" = "34-50%",
                       "6" = "26-33%",
                       "5" = "11-25%",
                       "4" = "4-10%",
                       "3" = "<4% (many individuals)",
                       "2" = "<4% (several individuals)",
                       "1" = "<4% (few individuals)")
saveRDS(object = dominCoverValsRev , file = "data/bundled_data/dominCoverValsRev.rds")

# Cover Method Options ----------------------------------------------------
coverMethod_options <- list(
  "Direct Percentage" = "directPercentage",
  "Domin Class" = "dominCover"
)
saveRDS(object = coverMethod_options, file = "data/bundled_data/coverMethod_options.rds")

# Habitat Restriction Options ---------------------------------------------
habitatRestriction_options <- list(
  "Woodland and scrub (W)" = "W",
  "Mires (M)" = "M",
  "Heaths (H)" = "H",
  "Mesotrophic grasslands (MG)" = "MG",
  "Calcicolous grasslands (CG)" = "CG",
  "Calcifugous grasslands and montane communities (U)" = "U",
  "Aquatic communities (A)" = "A",
  "Swamps and tall-herb fens (S)" = "S",
  "Salt-marsh communities (SM)" = "SM",
  "Shingle, strandline and sand-dune communities (SD)" = "SD",
  "Maritime cliff communities (MC)" = "MC",
  "Vegetation of open habitats (OV)" = "OV"
)
saveRDS(object = habitatRestriction_options, file = "data/bundled_data/habitatRestriction_options.rds")

# Species Name Options ----------------------------------------------------
sppName_to_brcCode <- readr::read_delim("data/raw_data/bsbi_checklist_sppName_to_brcCode.csv", 
                                        delim = "\t", escape_double = FALSE, 
                                        trim_ws = TRUE,
                                        show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue, origTaxon, origTaxonId) |>
  dplyr::rename("BRC" = "dataValue")

saveRDS(object = sppName_to_brcCode, file = "data/bundled_data/sppName_to_brcCode.rds")

speciesNames <- assignNVC::nvc_pquads |> # sppName_to_brcCode |>
  dplyr::pull(species) |>
  unique() |>
  sort()

saveRDS(object = speciesNames, file = "data/bundled_data/speciesNames.rds")


# assignNVC::nvc_pquads - Tidied names ------------------------------------

nvc_pquads_tidied <- assignNVC::nvc_pquads

# assignNVC::nvc_pquads BRC Code and BSBI Checklist BRC Codes don't match...
# nvc_pquads_tidied <- assignNVC::nvc_pquads |>
#   dplyr::mutate("BRC" = as.character(BRC)) |>
#   dplyr::left_join(sppName_to_brcCode, by = "BRC")
  

saveRDS(object = nvc_pquads_tidied, file = "data/bundled_data/nvc_pquads_tidied.rds")

# BSBI Checklist DF -------------------------------------------------------
HE_F <- readr::read_delim("data/raw_data/bsbi_checklist_HE_F.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Hill-Ellenberg F")

HE_L <- readr::read_delim("data/raw_data/bsbi_checklist_HE_L.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Hill-Ellenberg L")

HE_N <- readr::read_delim("data/raw_data/bsbi_checklist_HE_N.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Hill-Ellenberg N")

HE_R <- readr::read_delim("data/raw_data/bsbi_checklist_HE_R.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Hill-Ellenberg R")

HE_S <- readr::read_delim("data/raw_data/bsbi_checklist_HE_S.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE,
                          show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Hill-Ellenberg S")

rarityNewAtlas_GB <- readr::read_delim("data/raw_data/bsbi_checklist_rarityNewAtlas_GB.csv", 
                                       delim = "\t", escape_double = FALSE, 
                                       trim_ws = TRUE,
                                       show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Rarity - GB")

rarityNewAtlas_Ireland <- readr::read_delim("data/raw_data/bsbi_checklist_rarityNewAtlas_Ireland.csv", 
                                            delim = "\t", escape_double = FALSE, 
                                            trim_ws = TRUE,
                                            show_col_types = FALSE) |>
  dplyr::select(key, taxonId, dataValue) |>
  dplyr::mutate("dataType" = "Rarity - Ireland")

bsbiChecklistData <- rbind(
  HE_F,
  HE_L,
  HE_N,
  HE_R,
  HE_S,
  rarityNewAtlas_GB,
  rarityNewAtlas_Ireland
)


# Create Correspondence Data ----------------------------------------------
suppressWarnings(
  suppressMessages(
    raw_JNCC_habCor <- readxl::read_xls(path = "data/raw_data/Habitat-correspondences-2008.xls",
                                        sheet = "master table - sheet protected",
                                        col_types = c("text", "text", "text",
                                                      "text", "text", "text",
                                                      "text", "text", "text",
                                                      "text"))
  )
)

suppressWarnings(
  suppressMessages(
    nvc_floristic_tables <- read.csv(file = "data/raw_data/NVC-floristic-tables.csv")
  )
)

suppressWarnings(
  suppressMessages(
    ukHab_habCor_raw <- readxl::read_xlsx(path = "data/raw_data/UK Habitat Classification V1-1 including Correspondences_7 Sep 2020_ZMCleaned.xlsx",
                                      sheet = "NVC to UKHab",
                                      skip = 6,
                                      .name_repair = "minimal")
  )
)

suppressWarnings(
  suppressMessages(
    ukHab_namesCodes_raw <- readxl::read_xlsx(path = "data/raw_data/UK Habitat Classification V1-1 including Correspondences_7 Sep 2020_ZMCleaned.xlsx",
                                              sheet = "Professional Edition Hierarchy",
                                              skip = 0,
                                              .name_repair = "minimal")
  )
)

ukHab_namesCodes_level2 <- ukHab_namesCodes_raw |>
  dplyr::select(`Level 2 code`, `Level 2 Label`) |>
  dplyr::rename("Code" = "Level 2 code", "Label" = "Level 2 Label") |>
  dplyr::filter(!is.na(Code)) |>
  dplyr::mutate("Level" = "2")

ukHab_namesCodes_level3 <- ukHab_namesCodes_raw |>
  dplyr::select(`Level 3 code`, `Level 3 Label`) |>
  dplyr::rename("Code" = "Level 3 code", "Label" = "Level 3 Label") |>
  dplyr::filter(!is.na(Code)) |>
  dplyr::mutate("Level" = "3")

ukHab_namesCodes_level4 <- ukHab_namesCodes_raw |>
  dplyr::select(`Level 4 code`, `Level 4 Label\r\n(Priority Habitats in Bold)`) |>
  dplyr::rename("Code" = "Level 4 code", "Label" = "Level 4 Label\r\n(Priority Habitats in Bold)") |>
  dplyr::filter(!is.na(Code)) |>
  dplyr::mutate("Level" = "4")

ukHab_namesCodes_level5 <- ukHab_namesCodes_raw |>
  dplyr::select(`Level 5 code`, `Level 5 Label\r\n(Including Annex 1 Habitats)`) |>
  dplyr::rename("Code" = "Level 5 code", "Label" = "Level 5 Label\r\n(Including Annex 1 Habitats)") |>
  dplyr::filter(!is.na(Code)) |>
  dplyr::mutate("Level" = "5")

ukHab_namesCodes <- rbind(
  ukHab_namesCodes_level2,
  ukHab_namesCodes_level3,
  ukHab_namesCodes_level4,
  ukHab_namesCodes_level5
)

# Create NVC to UKHab correspondence df -----------------------------------
ukHab_habCor_renamed <- ukHab_habCor_raw

names(ukHab_habCor_renamed) <- c("NVC.Code",
                                 "UKHab.Secondary.Code",
                                 "Level1_Variant1",
                                 "Level2_Variant1",
                                 "Level3_Variant1",
                                 "Level4_Variant1",
                                 "Level5_Variant1",
                                 
                                 "Level1_Variant2",
                                 "Level2_Variant2",
                                 "Level3_Variant2",
                                 "Level4_Variant2",
                                 "Level5_Variant2",
                                 
                                 "Level1_Variant3",
                                 "Level2_Variant3",
                                 "Level3_Variant3",
                                 "Level4_Variant3",
                                 "Level5_Variant3",
                                 
                                 "Level1_Variant4",
                                 "Level2_Variant4",
                                 "Level3_Variant4",
                                 "Level4_Variant4",
                                 "Level5_Variant4",
                                 
                                 "Level1_Variant5",
                                 "Level2_Variant5",
                                 "Level3_Variant5",
                                 "Level4_Variant5",
                                 "Level5_Variant5",
                                 
                                 "Level1_Variant6",
                                 "Level2_Variant6",
                                 "Level3_Variant6",
                                 "Level4_Variant6",
                                 "Level5_Variant6",
                                 
                                 "Level1_Variant7",
                                 "Level2_Variant7",
                                 "Level3_Variant7",
                                 "Level4_Variant7",
                                 "Level5_Variant7",
                                 
                                 "Level1_Variant8",
                                 "Level2_Variant8",
                                 "Level3_Variant8",
                                 "Level4_Variant8",
                                 "Level5_Variant8",
                                 
                                 "Level1_Variant9",
                                 "Level2_Variant9",
                                 "Level3_Variant9",
                                 "Level4_Variant9",
                                 "Level5_Variant9"
                         
)

ukHab_habCor_final <- ukHab_habCor_renamed |>
  dplyr::select(-UKHab.Secondary.Code) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
  tidyr::pivot_longer(cols = !NVC.Code,
                      names_pattern = "(.*)_(.*)",
                      names_to = c("Level", "Variant"),
                      values_to = "Code") |>
  dplyr::distinct() |>
  tidyr::pivot_wider(names_from = Level,
                     values_from = Code) |>
  dplyr::filter(!is.na(Level1)) |>
  dplyr::mutate(
    Level2.Code = stringr::str_c(Level2),
    Level3.Code = stringr::str_c(Level2, Level3),
    Level4.Code = stringr::str_c(Level2, Level3, Level4),
    Level5.Code = stringr::str_c(Level2, Level3, Level4, Level5),
    
  ) |>
  dplyr::select(-c(Variant, Level1, Level2, Level3, Level4, Level5)) |>
  tidyr::pivot_longer(cols = c(Level2.Code, Level3.Code, Level4.Code, Level5.Code),
                      names_to = "Level",
                      values_to = "Code") |>
  dplyr::filter(!is.na(Code)) |>
  dplyr::mutate(
    Level = stringr::str_remove(string = Level,
                                pattern = stringr::fixed(".Code"))
  ) |>
  dplyr::distinct() |>
  dplyr::mutate("Relationship" = "Associated with", .after = "NVC.Code") |>
  dplyr::mutate("Classification" = paste0("UKHab - ", Level), .keep = "unused") |>
  dplyr::left_join(ukHab_namesCodes, by = "Code") |>
  dplyr::select(-Level) |>
  dplyr::select(NVC.Code, Relationship, Code, Label, Classification)



# Get NVC names from floristic tables -------------------------------------
nvc_name_to_code <- nvc_floristic_tables |>
  dplyr::select("Community.or.sub.community.name", "Community.or.sub.community.code") |>
  dplyr::distinct()



# Create JNCC correspondence df -------------------------------------------
tidied_JNCC_habCor_CLASSN1 <- raw_JNCC_habCor |>
  dplyr::filter(CLASSN1 == "National Vegetation Classification") |>
  dplyr::select(-BIOTOPE_SHORT_TERM1,
                -CLASSN1,
                -`...10`) |>
  dplyr::relocate(RELATIONSHIP, .after = BIOTOPE_FULL_TERM1) |>
  dplyr::distinct()
  
tidied_JNCC_habCor_CLASSN1$BIOTOPE_FULL_TERM1 <- gsub("<[^>]+>", "", tidied_JNCC_habCor_CLASSN1$BIOTOPE_FULL_TERM1)
tidied_JNCC_habCor_CLASSN1$BIOTOPE_FULL_TERM2 <- gsub("<[^>]+>", "", tidied_JNCC_habCor_CLASSN1$BIOTOPE_FULL_TERM2)

# tidied_JNCC_habCor_CLASSN2 <- raw_JNCC_habCor |>
#   dplyr::filter(CLASSN2 == "National Vegetation Classification") |>
#   dplyr::select(-BIOTOPE_SHORT_TERM2,
#                 -CLASSN2,
#                 -`...10`) |>
#   dplyr::relocate(RELATIONSHIP, .after = BIOTOPE_FULL_TERM1) |>
#   dplyr::distinct()
# 
# tidied_JNCC_habCor_CLASSN2$BIOTOPE_FULL_TERM1 <- gsub("<[^>]+>", "", tidied_JNCC_habCor_CLASSN2$BIOTOPE_FULL_TERM1)
# 
# 
# tidied_JNCC_habCor_CLASSN1_nvc_names <- unique(tidied_JNCC_habCor_CLASSN1$BIOTOPE_CODE1) |>
#   sort()
# tidied_JNCC_habCor_CLASSN2_nvc_names <- unique(tidied_JNCC_habCor_CLASSN2$BIOTOPE_CODE2) |>
#   sort()
# nvc_floristic_tables_nvc_names <- unique(nvc_name_to_code$Community.or.sub.community.code) |>
#   sort()
# 
# setdiff(tidied_JNCC_habCor_CLASSN1_nvc_names, tidied_JNCC_habCor_CLASSN2_nvc_names)
# 
# # NVC codes in JNCC correspondance but not NVC floristic tables 
# setdiff(tidied_JNCC_habCor_CLASSN1_nvc_names, nvc_floristic_tables_nvc_names)
# 
# # NVC codes in NVC floristic tables but not JNCC correspondance
# setdiff(nvc_floristic_tables_nvc_names, tidied_JNCC_habCor_CLASSN1_nvc_names)


jncc_habCor_final <- tidied_JNCC_habCor_CLASSN1 |>
  dplyr::select(BIOTOPE_CODE1, RELATIONSHIP, BIOTOPE_CODE2, BIOTOPE_FULL_TERM2, CLASSN2) |>
  dplyr::rename("NVC.Code" = "BIOTOPE_CODE1",
                "Relationship" = "RELATIONSHIP",
                "Code" = "BIOTOPE_CODE2",
                "Label" = "BIOTOPE_FULL_TERM2",
                "Classification" = "CLASSN2")

all_habCor_final <- rbind(
  ukHab_habCor_final,
  jncc_habCor_final
)
saveRDS(object = all_habCor_final, file = "data/bundled_data/all_habCor_final.rds")

all_habCor_classifications <- all_habCor_final |>
  dplyr::pull(Classification) |>
  unique()
saveRDS(object = all_habCor_classifications, file = "data/bundled_data/all_habCor_classifications.rds")