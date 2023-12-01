raw_JNCC_habCor <- readxl::read_xls(path = "../data/raw_data/Habitat-correspondences-2008.xls",
                                    sheet = "master table - sheet protected",
                                    col_types = c("text", "text", "text",
                                                  "text", "text", "text",
                                                  "text", "text", "text",
                                                  "text"))

ukHab_habCor_raw <- readxl::read_xlsx(path = "../data/raw_data/UK Habitat Classification V1-1 including Correspondences_7 Sep 2020_ZMCleaned.xlsx",
                                      sheet = "NVC to UKHab",
                                      skip = 6,
                                      .name_repair = "minimal")

ukHab_namesCodes_raw <- readxl::read_xlsx(path = "../data/raw_data/UK Habitat Classification V1-1 including Correspondences_7 Sep 2020_ZMCleaned.xlsx",
                                          sheet = "Professional Edition Hierarchy",
                                          skip = 0,
                                          .name_repair = "minimal")


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

ukHab_habCor_renamed <- ukHab_habCor_raw

names(ukHab_habCor_renamed) <- c("NVC.Code",
                                 "UKHab.Secondary.Code",
                                 "Level1_Variant1", "Level2_Variant1", "Level3_Variant1", "Level4_Variant1", "Level5_Variant1",
                                 "Level1_Variant2", "Level2_Variant2", "Level3_Variant2", "Level4_Variant2", "Level5_Variant2",
                                 "Level1_Variant3", "Level2_Variant3", "Level3_Variant3", "Level4_Variant3", "Level5_Variant3",
                                 "Level1_Variant4", "Level2_Variant4", "Level3_Variant4", "Level4_Variant4", "Level5_Variant4",
                                 "Level1_Variant5", "Level2_Variant5", "Level3_Variant5", "Level4_Variant5", "Level5_Variant5",
                                 "Level1_Variant6", "Level2_Variant6", "Level3_Variant6", "Level4_Variant6", "Level5_Variant6",
                                 "Level1_Variant7", "Level2_Variant7", "Level3_Variant7", "Level4_Variant7", "Level5_Variant7",
                                 "Level1_Variant8", "Level2_Variant8", "Level3_Variant8", "Level4_Variant8", "Level5_Variant8",
                                 "Level1_Variant9", "Level2_Variant9", "Level3_Variant9", "Level4_Variant9", "Level5_Variant9"
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


nvc_community_codes <- nvc_name_to_code |>
  dplyr::pull("Community.or.sub.community.code")

saveRDS(object = nvc_community_codes, file = "../data/bundled_data/nvc_community_codes.rds")

tidied_JNCC_habCor_CLASSN1 <- raw_JNCC_habCor |>
  dplyr::filter(CLASSN1 == "National Vegetation Classification") |>
  dplyr::select(-BIOTOPE_SHORT_TERM1,
                -CLASSN1,
                -`...10`) |>
  dplyr::relocate(RELATIONSHIP, .after = BIOTOPE_FULL_TERM1) |>
  dplyr::distinct()

tidied_JNCC_habCor_CLASSN1$BIOTOPE_FULL_TERM1 <- gsub("<[^>]+>", "", tidied_JNCC_habCor_CLASSN1$BIOTOPE_FULL_TERM1)
tidied_JNCC_habCor_CLASSN1$BIOTOPE_FULL_TERM2 <- gsub("<[^>]+>", "", tidied_JNCC_habCor_CLASSN1$BIOTOPE_FULL_TERM2)

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



saveRDS(object = all_habCor_final, file = "../data/bundled_data/all_habCor_final.rds")


all_habCor_classifications <- all_habCor_final |>
  dplyr::pull(Classification) |>
  unique()


saveRDS(object = all_habCor_classifications, file = "../data/bundled_data/all_habCor_classifications.rds")

