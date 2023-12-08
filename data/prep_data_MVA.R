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



# Check that each nvc code in nvc_community_codes has a set of pseudo-quadrats  --------
# First get rownames and string quadrat IDs
# nvc_pquads_final_wide_rownames_raw <- rownames(nvc_pquads_final_wide) |> unique()
# nvc_pquads_final_wide_rownames_nvccodes <- nvc_pquads_final_wide_rownames_raw |>
#   stringr::str_extract(pattern = ".+?(?=P)") |>
#   unique()
# 
# identical(nvc_community_codes, nvc_pquads_final_wide_rownames_nvccodes)
# setdiff(nvc_community_codes, nvc_pquads_final_wide_rownames_nvccodes)
# NVC codes with no pseudo-quadrats
# "MG7A"  "MG7B"  "MG7C"  "MG7D"  "MG7E"  "MG7F"  "S23"   "SM 1a" "SM2"   "SM3"   "SM4"   "SM5"
# nvc_floristic_tables |>
#   dplyr::filter(NVC.Code %in% c("S23")) 
# 
# nvc_floristic_tables |>
#   dplyr::filter(NVC.Code %in% c("MG7A", "MG7B", "MG7C", "MG7D", "MG7E", "MG7F", "S23", "SM 1a", "SM2", "SM3", "SM4", "SM5")) |>
#   dplyr::group_by(NVC.Code) |>
#   dplyr::summarise("n" = dplyr::n(), .groups = "drop")
# 
# nvc_pquads_final |>
#   dplyr::filter(NVC %in% c("MG7a", "MG7b", "MG7c", "MG7d", "MG7e", "MG7f", "S23", "SM1a", "SM2", "SM3", "SM4", "SM5")) |>
#   dplyr::group_by(NVC) |>
#   dplyr::summarise("n" = dplyr::n(), .groups = "drop")

# Perform DCA for each communities pseudo-quadrats ------------------------
nvc_pquad_dca_list <- list()

for(nvcCode in setdiff(nvc_community_codes, c("S23", "SM1a", "SM2", "SM3", "SM4", "SM5"))){
  
  # Subset communities
  nvc_pquads_final_wide_trimmed <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide),
                                                                             pattern = paste0(nvcCode, "+?(?=P)")), ]
  # Remove columns (species) that are absent in all selected communities
  nvc_pquads_final_wide_prepped <- nvc_pquads_final_wide_trimmed[, colSums(abs(nvc_pquads_final_wide_trimmed)) != 0] |>
    as.data.frame()
  
  # Run DCA
  dca_result <- vegan::decorana(veg = nvc_pquads_final_wide_prepped)
  
  # Append to list
  nvc_pquad_dca_list[[nvcCode]] <- dca_result
  
  
}

saveRDS(object = nvc_pquad_dca_list, file = "./data/bundled_data/nvc_pquad_dca_list.rds")

# Perform DCA across all communities --------------------------------------
codes_regex <- c()
codes_to_remove <- c("SM1", "SM1a", "SM1b")
for(code in codes_to_remove){
  
  regex <- paste0("^(", code, ")(?<=)P")
  
  codes_regex <- c(codes_regex, regex)
  
  codes_regex <- stringr::str_c(codes_regex, collapse = "|")
  
}

# Subset pseudo-quadrats for selected communities
nvc_pquads_final_wide_trimmed <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide), pattern = codes_regex, negate = TRUE), ]

nvc_pquad_dca_all <- vegan::decorana(nvc_pquads_final_wide_trimmed)

plot(nvc_pquad_dca_all)

saveRDS(object = nvc_pquad_dca_all, file = "./data/bundled_data/nvc_pquad_dca_all.rds")



# Produce hulls for each sub-community ------------------------------------
nvc_pquad_dca_all_hulls_subComm <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA2)) |>
  dplyr::ungroup()

# ggplot2::ggplot() +
#   ggplot2::geom_polygon(data = nvc_pquad_dca_all_hulls_subComm, alpha = 0.2, 
#                         mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.SubComm)) +
#   ggplot2::theme_minimal()

# saveRDS(object = nvc_pquad_dca_all_hulls_subComm, file = "./data/bundled_data/nvc_pquad_dca_all_hulls_subComm.rds")

# Produce hulls for each Community ----------------------------------------
nvc_pquad_dca_all_hulls_Comm <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = "^[A-Z]{1,}\\d{1,}+(?![a-z*][P])")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA2)) |>
  dplyr::filter(!is.na(NVC))

# ggplot2::ggplot() +
#   ggplot2::geom_polygon(data = nvc_pquad_dca_all_hulls_Comm, alpha = 0.2, 
#                         mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Comm)) +
#   ggplot2::theme_minimal()

# saveRDS(object = nvc_pquad_dca_all_hulls_Comm, file = "./data/bundled_data/nvc_pquad_dca_all_hulls_Comm.rds")

# Produce hulls for each broad habitat ------------------------------------
nvc_pquad_dca_all_hulls_habitat <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=\\d)")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA2)) |>
  dplyr::ungroup()

# ggplot2::ggplot() +
#   ggplot2::geom_polygon(data = nvc_pquad_dca_all_hulls_habitat, alpha = 0.2, 
#                         mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Habitat)) +
#   ggplot2::theme_minimal()

# saveRDS(object = nvc_pquad_dca_all_hulls_habitat, file = "./data/bundled_data/nvc_pquad_dca_all_hulls_habitat.rds")


# Compile all hulls -------------------------------------------------------

nvc_pquad_dca_all_hulls <- rbind(nvc_pquad_dca_all_hulls_subComm,
                                 nvc_pquad_dca_all_hulls_Comm,
                                 nvc_pquad_dca_all_hulls_habitat)

saveRDS(object = nvc_pquad_dca_all_hulls, file = "./data/bundled_data/nvc_pquad_dca_all_hulls.rds")

# Pre-calculate CCA axis scores for all pseudo-quadrats -------------------


