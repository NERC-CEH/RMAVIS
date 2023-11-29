nvc_pquads_uniqSpecies <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct()

concordance_all <- concordance_plants |>
  dplyr::bind_rows(concordance_algae) |>
  # dplyr::bind_rows(concordance_bryophytes) |>
  dplyr::bind_rows(concordance_charophytes) #|>
  # dplyr::bind_rows(concordance_lichens)

# Check that the number of plant species in nvc_pquads_uniqSpecies is equal to the length of concordance_all
nrow(nvc_pquads_uniqSpecies) - nrow(concordance_all)

# Check whether there is any missing data in the following columns only:
# proposedName
# TVK
# BRC_old
# BRC_new
# assignNVCSpecies

concordance_plants_naRows <- concordance_plants |>
  dplyr::filter(is.na(dplyr::if_any(c(proposedName, TVK, BRC_old, BRC_new, assignNVCSpecies), is.na)))