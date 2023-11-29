nvc_pquads_uniqSpecies <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct()

nvc_comms_uniqSpecies <- assignNVC::NVC_communities |>
  dplyr::select(Species, BRC) |>
  dplyr::distinct()

length(assignNVC::nvc_pquads$species |> unique())
length(assignNVC::NVC_communities$Species |> unique())


setdiff(nvc_pquads_uniqSpecies$species, nvc_comms_uniqSpecies$Species)
setdiff(nvc_comms_uniqSpecies$Species, nvc_pquads_uniqSpecies$species)



colnames(concordance_plants)
colnames(concordance_algae)
colnames(concordance_bryophytes)
colnames(concordance_charophytes)
colnames(concordance_lichens)

concordance_all <- concordance_plants |>
  dplyr::bind_rows(concordance_algae) |>
  dplyr::bind_rows(concordance_bryophytes) |>
  dplyr::bind_rows(concordance_charophytes) |>
  dplyr::bind_rows(concordance_lichens)

# Check that the number of plant species in nvc_pquads_uniqSpecies is equal to the length of concordance_all
nrow(nvc_pquads_uniqSpecies) - nrow(concordance_all)

# NO missing assignNVC species
setdiff(nvc_pquads_uniqSpecies$species, concordance_all$assignNVCSpecies) 
setdiff(concordance_all$assignNVCSpecies, nvc_pquads_uniqSpecies$species) 

concordance_all_missing_assignNVCSpecies <- concordance_all |>
  dplyr::filter(is.na(assignNVCSpecies))

# 35 lichens with no BRC_new code
concordance_all_missing_BRC_new <- concordance_all |>
  dplyr::filter(is.na(BRC_new))

# NO missing TVK codes
concordance_all_missing_TVK <- concordance_all |>
  dplyr::filter(is.na(TVK))

# NO missing old BRC codes
concordance_all_missing_BRC_old <- concordance_all |>
  dplyr::filter(is.na(BRC_old))

# NO missing proposed species
concordance_all_missing_proposedSpecies <- concordance_all |>
  dplyr::filter(is.na(proposedSpecies))

# Save concordance
saveRDS(object = concordance_all, file = "./data/bundled_data/concordance_all.rds")

