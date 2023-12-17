# Load concordance data ---------------------------------------------------
concordance_all <- readRDS(file = "./data/bundled_data/concordance_all.rds")
concordance_all_trimmed <- concordance_all |>
  dplyr::select("species" = assignNVCSpecies,
                "proposedSpecies" = proposedSpecies)


# Species names -----------------------------------------------------------
speciesNames <- concordance_all_trimmed$proposedSpecies |> unique() |> sort()

saveRDS(object = speciesNames, file = "./data/bundled_data/speciesNames.rds")



# assignNVC::nvc_pquads ---------------------------------------------------
nvc_pquads_final <- assignNVC::nvc_pquads |>
  dplyr::left_join(concordance_all_trimmed, by = "species") |>
  dplyr::select(-species) |>
  dplyr::rename("species" = "proposedSpecies") |>
  dplyr::mutate(
    "NVC" =
      dplyr::case_when(
        NVC == "SM 1a" ~ "SM1a",
        TRUE ~ as.character(NVC)
      )
  )

nrow(nvc_pquads_final) == nrow(assignNVC::nvc_pquads)

saveRDS(object = nvc_pquads_final, file = "./data/bundled_data/nvc_pquads_final.rds")

# !!! when replacing the names in assignNVC::nvc_pquads several Pid3-Species duplications are introduced:
duplictate_Pid3_species <- nvc_pquads_final |>
  dplyr::group_by(Pid3, species) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n > 1L) 



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
  ) |>
  dplyr::mutate(
    "NVC.Code" =
      dplyr::case_when(
        NVC.Code == "MG7A" ~ "MG7a",
        NVC.Code == "MG7B" ~ "MG7b",
        NVC.Code == "MG7C" ~ "MG7c",
        NVC.Code == "MG7D" ~ "MG7d",
        NVC.Code == "MG7E" ~ "MG7e",
        NVC.Code == "MG7F" ~ "MG7f",
        NVC.Code == "SM 1a" ~ "SM1a",
        TRUE ~ as.character(NVC.Code)
      )
  )

nrow(nvc_communities_final) == nrow(assignNVC::NVC_communities)

saveRDS(object = nvc_communities_final, file = "./data/bundled_data/nvc_floristic_tables.rds")

nvc_community_codes <- nvc_communities_final |>
  dplyr::pull(NVC.Code) |>
  unique()

saveRDS(object = nvc_community_codes, file = "./data/bundled_data/nvc_community_codes.rds")


# NVC Name-Code Information -----------------------------------------------
nvc_community_namesCodes <- read.csv(file = "./data/raw_data/NVC-names-codes.csv") |>
  dplyr::select("NVC.Code" = code, 
                "NVC.Name" = community.sub.community.name, 
                "Rodwell.Synonym" = Synonym.or.shortened.community.name.as.used.in.British.Plant.Communities.Volumes)

saveRDS(object = nvc_community_namesCodes, file = "./data/bundled_data/nvc_community_namesCodes.rds")


# Calculate mean unweighted Hill-Ellenberg values for selected pse --------
nvc_pquads_mean_unweighted_eivs <- nvc_pquads_final |>
  dplyr::left_join(master_data, by = "species") |>
  dplyr::group_by(Pid3) |>
  dplyr::summarise("F" = mean(`F`, na.rm = TRUE),
                   "L" = mean(`L`, na.rm = TRUE),
                   "N" = mean(`N`, na.rm = TRUE),
                   "R" = mean(`R`, na.rm = TRUE),
                   "S" = mean(`S`, na.rm = TRUE)) |>
  tibble::as_tibble() |>
  # Replace NaN's with NA's. Required in a limited number of cases where there are pseudo-quadrats without any species with Hill-Ellenberg values?
  dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, NA)))

# Check all pseudo-quadrats are present
nrow(nvc_pquads_mean_unweighted_eivs) == length(unique(nvc_pquads_final$Pid3))

saveRDS(object = nvc_pquads_mean_unweighted_eivs, file = "./data/bundled_data/nvc_pquads_mean_unweighted_eivs.rds")
