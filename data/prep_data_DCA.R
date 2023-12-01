nvc_pquads_final <- readRDS(file = "./data/bundled_data/nvc_pquads_final.rds")

nvc_pquads_final_noDupes <- nvc_pquads_final |>
  dplyr::group_by(Pid3, species) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup()
  
# Check whether there are any duplicate Pid3 - species combinations
# which left unchecked would result in list-cols when pivoting wide
duplictate_Pid3_species <- nvc_pquads_final_noDupes |>
  dplyr::group_by(Pid3, species) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n > 1L) |>
  print()
  
# Convert ot matric format with binary presence/absence values
nvc_pquads_final_wide <- nvc_pquads_final_noDupes  |>
  dplyr::mutate("Present" = 1) |>
  dplyr::select(species, Pid3, Present) |>
  tidyr::pivot_wider(id_cols = Pid3,
                     names_from = species,
                     values_from = Present)  |>
  tibble::column_to_rownames(var = "Pid3") |>
  dplyr::mutate_all(~replace(., is.na(.), 0))
  
# Bundle data
saveRDS(object = nvc_pquads_final_wide, file = "./data/bundled_data/nvc_pquads_final_wide.rds")

# saveRDS(object = nvc_pquads_final_dca, file = "./data/bundled_data/nvc_pquads_final_dca.rds")
# 
# nvc_pquads_final_dca_psquad_axisScores <- nvc_pquads_final_dca$rproj
# nvc_pquads_final_dca_species_axisScores <- nvc_pquads_final_dca$cproj
# 
# saveRDS(object = nvc_pquads_final_dca_psquad_axisScores, file = "./data/bundled_data/nvc_pquads_final_dca_psquad_axisScores.rds")
# saveRDS(object = nvc_pquads_final_dca_species_axisScores, file = "./data/bundled_data/nvc_pquads_final_dca_species_axisScores.rds")


