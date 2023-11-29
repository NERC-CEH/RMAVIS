nvc_pquads_uniqSpecies_charophytes <- nvc_pquads_noMissCodes |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[7]")) 



concordance_charophytes <- nvc_pquads_uniqSpecies_algae |>
  dplyr::mutate(
    "proposedSpecies" = 
      dplyr::case_when(
        species == "Nitella sp." ~ "Nitella",
        species == "Chara sp." ~ "Chara",
        species == "Bostrychia scorpioides" ~ "Bostrychia scorpioides"
      )
  ) |>
  # dplyr::mutate(
  #   "BRC_new" = 
  #     dplyr::case_when(
  #       species == "Nitella sp." ~ ,
  #       species == "Turf fucoids" ~ ,
  #       species == "Bostrychia scorpioides" ~ 
  #     )
  # ) |>
  dplyr::rename("assignNVCSpecies" = species)


# Check that the number of plant species in nvc_pquads_uniqSpecies_charophytes is equal to the length of concordance_charophytes
nrow(nvc_pquads_uniqSpecies_charophytes) - nrow(concordance_charophytes)

# Check whether there is any missing data
concordance_charophytes_naRows <- concordance_charophytes |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))