nvc_pquads_uniqSpecies_charophytes <- nvc_pquads_noMissCodes |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[7]"))

concordance_charophytes <- nvc_pquads_uniqSpecies_charophytes |>
  dplyr::mutate(
    "proposedSpecies" =
      dplyr::case_when(
        species == "Nitella sp." ~ "Nitella",
        species == "Chara sp." ~ "Chara",
        species == "Bostrychia scorpioides" ~ "Bostrychia scorpioides"
      )
  ) |>
  dplyr::mutate(
    "BRC_new" =
      dplyr::case_when(
        species == "Nitella sp." ~ "Cha_153",
        species == "Chara sp." ~ "Cha_123",
        species == "Bostrychia scorpioides" ~ "Alg_1168"
      )
  ) |>
  dplyr::mutate(
    "TVK" =
      dplyr::case_when(
        species == "Nitella sp." ~ "NHMSYS0000461036",
        species == "Chara sp." ~ "NHMSYS0000457125",
        species == "Bostrychia scorpioides" ~ "NHMSYS0021059913"
      )
  ) |>
  dplyr::rename("assignNVCSpecies" = "species",
                "BRC_old" = "BRC")


# Check that the number of plant species in nvc_pquads_uniqSpecies_charophytes is equal to the length of concordance_charophytes
nrow(nvc_pquads_uniqSpecies_charophytes) - nrow(concordance_charophytes)

# Check whether there is any missing data
concordance_charophytes_naRows <- concordance_charophytes |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))

# Save concordance
saveRDS(object = concordance_charophytes, file = "./data/bundled_data/concordance_charophytes.rds")
