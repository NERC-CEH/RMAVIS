concordance_plants <- readRDS(file = "./data/bundled_data/concordance_plants.rds")
concordance_bryophytes <- readRDS(file = "./data/bundled_data/concordance_bryophytes.rds")
concordance_lichens <- readRDS(file = "./data/bundled_data/concordance_lichens.rds")
concordance_charophytes <- readRDS(file = "./data/bundled_data/concordance_charophytes.rds")
concordance_algae <- readRDS(file = "./data/bundled_data/concordance_algae.rds")

nvc_pquads_uniqSpecies <- assignNVC::nvc_pquads |>
  dplyr::select(species, BRC) |>
  dplyr::distinct()

nvc_psquad_uniqSpecies <- assignNVC::ps_quad |>
  dplyr::select(name, spp) |>
  dplyr::distinct()

nvc_comms_uniqSpecies <- assignNVC::NVC_communities |>
  dplyr::select(Species, BRC) |>
  dplyr::distinct()

all_species <- c(nvc_comms_uniqSpecies$Species, nvc_pquads_uniqSpecies$species, nvc_psquad_uniqSpecies$name) |> unique()

length(assignNVC::nvc_pquads$species |> unique())
length(assignNVC::ps_quad$name |> unique())
length(assignNVC::NVC_communities$Species |> unique())
length(all_species)

setdiff(nvc_psquad_uniqSpecies$name, nvc_comms_uniqSpecies$Species)
setdiff(nvc_comms_uniqSpecies$Species, nvc_psquad_uniqSpecies$name)

setdiff(nvc_pquads_uniqSpecies$species, nvc_comms_uniqSpecies$Species)
setdiff(nvc_pquads_uniqSpecies$species, all_species)

setdiff(nvc_comms_uniqSpecies$Species, nvc_pquads_uniqSpecies$species)
setdiff(nvc_comms_uniqSpecies$Species, all_species)

colnames(concordance_plants)
colnames(concordance_algae)
colnames(concordance_bryophytes)
colnames(concordance_charophytes)
colnames(concordance_lichens)

# Bind each taxon groups concordence data into a single data frame
concordance_all <- concordance_plants |>
  dplyr::bind_rows(concordance_algae) |>
  dplyr::bind_rows(concordance_bryophytes) |>
  dplyr::bind_rows(concordance_charophytes) |>
  dplyr::bind_rows(concordance_lichens)

# Check that the number of plant species in nvc_pquads_uniqSpecies is equal to the length of concordance_all
nrow(nvc_pquads_uniqSpecies) - nrow(concordance_all)

# NO missing assignNVC species
setdiff(concordance_all$assignNVCSpecies, all_species) 
setdiff(all_species, concordance_all$assignNVCSpecies) # "" empty string missing, present in assignNVC::nvc_pquads and assignNVC::ps_quad

concordance_all_missing_assignNVCSpecies <- concordance_all |>
  dplyr::filter(is.na(assignNVCSpecies)) |>
  print()

# 35 lichens with no BRC_new code
concordance_all_missing_BRC_new <- concordance_all |>
  dplyr::filter(is.na(BRC_new)) |>
  print()

# NO missing TVK codes
concordance_all_missing_TVK <- concordance_all |>
  dplyr::filter(is.na(TVK)) |>
  print()

# NO missing old BRC codes
concordance_all_missing_BRC_old <- concordance_all |>
  dplyr::filter(is.na(BRC_old)) |>
  print()

# NO missing proposed species
concordance_all_missing_proposedSpecies <- concordance_all |>
  dplyr::filter(is.na(proposedSpecies)) |>
  print()

# Is every proposed species unique
concordance_all_nonUniqpropSpecies <- concordance_all |>
  dplyr::group_by(proposedSpecies, assignNVCSpecies) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(proposedSpecies)|>
  print()

# Save concordance
saveRDS(object = concordance_all, file = "./data/bundled_data/concordance_all.rds")

# Accepted Species --------------------------------------------------------
acceptedSpecies <- concordance_all |>
  dplyr::select(
    "Accepted_Species" = "proposedSpecies",
    "BSBI_taxonId" = "bsbiTaxonId",
    # "BRC_new" = "BRC_new",
    # "BRC_old" = "BRC_old",
    "TVK" = "TVK",
  )

saveRDS(object = acceptedSpecies, file = "./data/bundled_data/acceptedSpecies.rds")

