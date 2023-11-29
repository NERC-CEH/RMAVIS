nvc_pquads_uniqSpecies_algae <- nvc_pquads_noMissCodes |>
  dplyr::select(species, BRC) |>
  dplyr::distinct() |>
  dplyr::filter(stringr::str_detect(string = BRC, pattern = "^[2]")) 



concordance_algae <- nvc_pquads_uniqSpecies_algae |>
  dplyr::mutate(
    "proposedSpecies" = 
      dplyr::case_when(
        species == "Algal mat" ~ "Algae",
        species == "Turf fucoids" ~ "Fucales",
        species == "[Alga]" ~ "Algae"
      )
    ) |>
  # dplyr::mutate(
  #   "BRC_new" = 
  #     dplyr::case_when(
  #       species == "Algal mat" ~ ,
  #       species == "Turf fucoids" ~ ,
  #       species == "[Alga]" ~ 
  #     )
  # ) |>
  dplyr::rename("assignNVCSpecies" = species)


# Check that the number of plant species in nvc_pquads_uniqSpecies_algae is equal to the length of concordance_algae
nrow(nvc_pquads_uniqSpecies_algae) - nrow(concordance_algae)

# Check whether there is any missing data
concordance_algae_naRows <- concordance_algae |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))
