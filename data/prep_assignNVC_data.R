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
  dplyr::rename("Species" = "species") |>
  dplyr::right_join(assignNVC::NVC_communities, by = "Species") |>
  dplyr::select(-Species, -freq, -BRC) |>
  dplyr::rename("Species" = "proposedSpecies") |>
  dplyr::rename("NVC.Code" = "NVC")|>
  dplyr::mutate(
    "Constancy" = 
      dplyr::case_when(
        Constancy == 1 ~ "I",
        Constancy == 2 ~ "II",
        Constancy == 3 ~ "III",
        Constancy == 4 ~ "IV",
        Constancy == 5 ~ "V",
        TRUE ~ as.character(Constancy)
      )
  )

nrow(nvc_communities_final) == nrow(assignNVC::NVC_communities)

saveRDS(object = nvc_communities_final, file = "./data/bundled_data/nvc_floristic_tables.rds")

nvc_community_codes <- assignNVC::NVC_communities |>
  dplyr::pull(NVC) |>
  unique()

saveRDS(object = nvc_community_codes, file = "./data/bundled_data/nvc_community_codes.rds")


nvc_community_namesCodes <- read.csv(file = "./data/raw_data/NVC-floristic-tables.csv") |>
  dplyr::select("NVC.Code" = Community.or.sub.community.code, 
                "NVC.Name" = Community.or.sub.community.name) |>
  dplyr::distinct()

saveRDS(object = nvc_community_namesCodes, file = "./data/bundled_data/nvc_community_namesCodes.rds")

