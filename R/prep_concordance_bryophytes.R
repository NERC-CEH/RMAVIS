nvc_pquads_uniqSpecies_bryophytes <- nvc_pquads_noMissCodes |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[8]")) |>
  dplyr::rename("BRC_old" = BRC)

bryoatt_raw <- readxl::read_xls(path = "./data/raw_data/Bryoatt_updated_2017.xls", sheet = "BRYOATT")

bryoatt_names <- bryoatt_raw |>
  dplyr::select("bryoattSpecies" = `Taxon name`, 
                "BRC_old" = BRC_num,
                "BRC_new" = Concept) |>
  dplyr::mutate("BRC_old" = stringr::str_replace_all(string = BRC_old, pattern = "\\s*", replacement = "")) |>
  dplyr::mutate("BRC_old" = as.numeric(BRC_old))


nvc_pquads_uniqSpecies_bryophytes_joined <- nvc_pquads_uniqSpecies_bryophytes |> 
  dplyr::left_join(bryoatt_names, by = "BRC_old")

nvc_pquads_uniqSpecies_bryophytes_joined_present <- nvc_pquads_uniqSpecies_bryophytes_joined |>
  dplyr::filter(!is.na(BRC_new))


# 12 species from nvc_pquads_uniqSpecies_bryophytes could not be matched up to BRYOATT, resolve manually.
nvc_pquads_uniqSpecies_bryophytes_missing <- nvc_pquads_uniqSpecies_bryophytes_joined |>
  dplyr::filter(is.na(BRC_new))
  
  
nvc_pquads_uniqSpecies_bryophytes_missing_fixed1 <- nvc_pquads_uniqSpecies_bryophytes_missing |>
  dplyr::mutate(
    "BRC_old_fixed" = 
      dplyr::case_when(
        species == "Hypnum cupressiforme sens.lat." ~ 8203901.0,
        # species == "Weissia sp." ~ ,
        # species == "Fissidens sp." ~ ,
        # species == "Calypogeia sp." ~ ,
        # species == "Cephaloziella sp." ~ ,
        species == "Racomitrium heterostichum sens.lat." ~ 8201072,
        # species == "Bryum erythrocarpum" ~ ,
        species == "Pottia starkeana subsp.conica" ~ 8201781,
        species == "Lophocolea bidentata var.bidentata" ~ 8101056,
        # species == "Riccia sp." ~ ,
        # species == "Barbula sp." ~ ,
        species == "Pohlia proligera sensu Smith (1978)" ~ 820466,
        TRUE ~ NA
      )
  ) |>
  dplyr::filter(!is.na(BRC_old_fixed)) |>
  dplyr::select("species" = species,
                "BRC_old" = BRC_old_fixed) |>
  dplyr::left_join(bryoatt_names, by = "BRC_old")

# nvc_pquads_uniqSpecies_bryophytes_fixed2 <- nvc_pquads_uniqSpecies_bryophytes_missing |>
#   dplyr::mutate(
#     "BRC_old_fixed" = 
#       dplyr::case_when(
#         # species == "Hypnum cupressiforme sens.lat." ~ 8203901.0,
#         species == "Weissia sp." ~ ,
#         species == "Fissidens sp." ~ ,
#         species == "Calypogeia sp." ~ ,
#         species == "Cephaloziella sp." ~ ,
#         # species == "Racomitrium heterostichum sens.lat." ~ 8201072,
#         # species == "Bryum erythrocarpum" ~ ,
#         # species == "Pottia starkeana subsp.conica" ~ 8201781,
#         # species == "Lophocolea bidentata var.bidentata" ~ 8101056,
#         species == "Riccia sp." ~ ,
#         species == "Barbula sp." ~ ,
#         # species == "Pohlia proligera sensu Smith (1978)" ~ 820466,
#         TRUE ~ NA
#       )
#   )


concordance_bryophytes <- nvc_pquads_uniqSpecies_bryophytes_joined_present |>
  dplyr::bind_rows(nvc_pquads_uniqSpecies_bryophytes_missing_fixed1) #|>
  # dplyr::bind_rows(nvc_pquads_uniqSpecies_bryophytes_missing_fixed2)


# Check that the number of plant species in nvc_pquads_uniqSpecies_bryophytes is equal to the length of concordance_bryophytes
nrow(nvc_pquads_uniqSpecies_bryophytes) - nrow(concordance_bryophytes)

# Check whether there is any missing data
concordance_bryophytes_naRows <- concordance_bryophytes |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))
