# Load concordance data ---------------------------------------------------
concordance_all <- readRDS(file = "./data/bundled_data/concordance_all.rds")
concordance_all_trimmed <- concordance_all |>
  dplyr::select("species" = assignNVCSpecies,
                "proposedSpecies" = proposedSpecies)


# Species names -----------------------------------------------------------
speciesNames <- concordance_all_trimmed$proposedSpecies |> unique()

saveRDS(object = speciesNames, file = "./data/bundled_data/speciesNames.rds")



# assignNVC::nvc_pquads ---------------------------------------------------
nvc_pquads_final <- assignNVC::nvc_pquads |>
  dplyr::left_join(concordance_all_trimmed, by = "species") |>
  dplyr::select(-species) |>
  dplyr::rename("species" = "proposedSpecies")

nrow(nvc_pquads_final) == nrow(assignNVC::nvc_pquads)

saveRDS(object = nvc_pquads_final, file = "./data/bundled_data/nvc_pquads_final.rds")



# assignNVC::NVC_communities ----------------------------------------------
nvc_communities_final <- concordance_all_trimmed |>
  dplyr::rename("Species" = species) |>
  dplyr::right_join(assignNVC::NVC_communities, by = "Species") |>
  dplyr::select(-Species) |>
  dplyr::rename("Species" = "proposedSpecies")

nrow(nvc_communities_final) == nrow(assignNVC::NVC_communities)

saveRDS(object = nvc_communities_final, file = "./data/bundled_data/nvc_floristic_tables.rds")

nvc_community_codes <- assignNVC::NVC_communities |>
  dplyr::pull(NVC) |>
  unique()

saveRDS(object = nvc_community_codes, file = "./data/bundled_data/nvc_community_codes.rds")

