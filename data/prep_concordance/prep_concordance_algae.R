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
  dplyr::mutate(
    "BRC_new" =
      dplyr::case_when(
        species == "Algal mat" ~ "Alg_2335",
        species == "Turf fucoids" ~ "Alg_1534",
        species == "[Alga]" ~ "Alg_2335"
      )
  ) |>
  dplyr::mutate(
    "TVK" =
      dplyr::case_when(
        species == "Algal mat" ~ "NHMSYS0000352587",
        species == "Turf fucoids" ~ "NBNSYS0000175377",
        species == "[Alga]" ~ "NHMSYS0000352587"
      )
  ) |>
  dplyr::rename("assignNVCSpecies" = species,
                "BRC_old" = "BRC")


# Check that the number of plant species in nvc_pquads_uniqSpecies_algae is equal to the length of concordance_algae
nrow(nvc_pquads_uniqSpecies_algae) - nrow(concordance_algae)

# Check whether there is any missing data
concordance_algae_naRows <- concordance_algae |>
  dplyr::filter(is.na(dplyr::if_any(dplyr::everything(), is.na)))

# Save concordance
saveRDS(object = concordance_algae, file = "./data/bundled_data/concordance_algae.rds")
