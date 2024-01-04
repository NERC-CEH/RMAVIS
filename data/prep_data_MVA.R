# Load constants ----------------------------------------------------------
source("R/create_constants.R", local = TRUE)

# Check whether there are any duplicate Pid3 - species combinations
# which left unchecked would result in list-cols when pivoting wide
nvc_pquads_final <- readRDS(file = "./data/bundled_data/nvc_pquads_final.rds")

nvc_pquads_final_noDupes <- nvc_pquads_final |>
  dplyr::group_by(Pid3, species) |>
  dplyr::filter(dplyr::n() == 1) |>
  dplyr::ungroup()

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


# Perform DCA across all communities --------------------------------------
codes_regex <- c()
codes_to_remove <- c("SM1", "SM1a", "SM1b")
for(code in codes_to_remove){
  
  regex <- paste0("^(", code, ")(?<=)P")
  
  codes_regex <- c(codes_regex, regex)
  
  codes_regex <- stringr::str_c(codes_regex, collapse = "|")
  
}

nvc_pquads_final_wide_trimmed <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide), pattern = codes_regex, negate = TRUE), ]

nvc_pquad_dca_all <- vegan::decorana(nvc_pquads_final_wide_trimmed)

plot(nvc_pquad_dca_all)

saveRDS(object = nvc_pquad_dca_all, file = "./data/bundled_data/nvc_pquad_dca_all.rds")



# Produce hulls for each sub-community ------------------------------------
nvc_pquad_dca_all_hulls_subComm_dca1dca2 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA2)) |>
  dplyr::ungroup() |>
  dplyr::mutate("dcaAxes" = "dca1dca2")

nvc_pquad_dca_all_hulls_subComm_dca1dca3 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA3)) |>
  dplyr::ungroup() |>
  dplyr::mutate("dcaAxes" = "dca1dca3")

nvc_pquad_dca_all_hulls_subComm_dca2dca3 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA2, DCA3)) |>
  dplyr::ungroup() |>
  dplyr::mutate("dcaAxes" = "dca2dca3")

# ggplot2::ggplot() +
#   ggplot2::geom_polygon(data = nvc_pquad_dca_all_hulls_subComm, alpha = 0.2, 
#                         mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.SubComm)) +
#   ggplot2::theme_minimal()

# Produce hulls for each Community ----------------------------------------
nvc_pquad_dca_all_hulls_Comm_dca1dca2 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = "^[A-Z]{1,}\\d{1,}+(?![a-z*][P])")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA2)) |>
  dplyr::filter(!is.na(NVC)) |>
  dplyr::mutate("dcaAxes" = "dca1dca2")

nvc_pquad_dca_all_hulls_Comm_dca1dca3 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = "^[A-Z]{1,}\\d{1,}+(?![a-z*][P])")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA3)) |>
  dplyr::filter(!is.na(NVC)) |>
  dplyr::mutate("dcaAxes" = "dca1dca3")

nvc_pquad_dca_all_hulls_Comm_dca2dca3 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = "^[A-Z]{1,}\\d{1,}+(?![a-z*][P])")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA2, DCA3)) |>
  dplyr::filter(!is.na(NVC)) |>
  dplyr::mutate("dcaAxes" = "dca2dca3")

# ggplot2::ggplot() +
#   ggplot2::geom_polygon(data = nvc_pquad_dca_all_hulls_Comm, alpha = 0.2, 
#                         mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Comm)) +
#   ggplot2::theme_minimal()

# Produce hulls for each broad habitat ------------------------------------
nvc_pquad_dca_all_hulls_habitat_dca1dca2 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=\\d)")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA2)) |>
  dplyr::ungroup() |>
  dplyr::mutate("dcaAxes" = "dca1dca2")

nvc_pquad_dca_all_hulls_habitat_dca1dca3 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=\\d)")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA1, DCA3)) |>
  dplyr::ungroup() |>
  dplyr::mutate("dcaAxes" = "dca1dca3")

nvc_pquad_dca_all_hulls_habitat_dca2dca3 <- vegan::scores(nvc_pquad_dca_all, tidy = TRUE) |>
  dplyr::filter(score == "sites") |>
  dplyr::select(-score, -weight) |>
  dplyr::rename("Quadrat" = label) |>
  dplyr::mutate("NVC" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=\\d)")) |>
  dplyr::group_by(NVC) |>
  dplyr::slice(grDevices::chull(DCA2, DCA3)) |>
  dplyr::ungroup() |>
  dplyr::mutate("dcaAxes" = "dca2dca3")

# ggplot2::ggplot() +
#   ggplot2::geom_polygon(data = nvc_pquad_dca_all_hulls_habitat, alpha = 0.2, 
#                         mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Habitat)) +
#   ggplot2::theme_minimal()


# Compile all hulls -------------------------------------------------------
nvc_pquad_dca_all_hulls <- rbind(nvc_pquad_dca_all_hulls_subComm_dca1dca2,
                                 nvc_pquad_dca_all_hulls_subComm_dca1dca3,
                                 nvc_pquad_dca_all_hulls_subComm_dca2dca3,
                                 nvc_pquad_dca_all_hulls_Comm_dca1dca2,
                                 nvc_pquad_dca_all_hulls_Comm_dca1dca3,
                                 nvc_pquad_dca_all_hulls_Comm_dca2dca3,
                                 nvc_pquad_dca_all_hulls_habitat_dca1dca2,
                                 nvc_pquad_dca_all_hulls_habitat_dca1dca3,
                                 nvc_pquad_dca_all_hulls_habitat_dca2dca3)

saveRDS(object = nvc_pquad_dca_all_hulls, file = "./data/bundled_data/nvc_pquad_dca_all_hulls.rds")


# Pre-calculate CCA axis scores for all pseudo-quadrats -------------------
nvc_pquads_mean_unweighted_eivs <- readRDS(file = "./data/bundled_data/nvc_pquads_mean_unweighted_eivs.rds")
nvc_pquads_final_wide <- readRDS(file = "./data/bundled_data/nvc_pquads_final_wide.rds")

nvc_pquads_cca_list <- list()

for(ccaVars in names(ccaVars_vals)){
  
  nvc_pquads_cca <- vegan::cca(as.formula(paste0("nvc_pquads_final_wide ~ ", paste0(c(ccaVars_vals[[ccaVars]]), collapse = " + "))),
                               data = nvc_pquads_mean_unweighted_eivs,
                               na.action = na.exclude)
  
  nvc_pquads_cca_scores <- vegan::scores(nvc_pquads_cca, display = "bp")
  # nvc_pquads_cca_multiplier <- vegan:::ordiArrowMul(nvc_pquads_cca_scores)
  
  nvc_pquads_cca_list[[paste0(ccaVars, collapse = "")]] <- nvc_pquads_cca_scores
  
  # Collect garbage
  base::gc()
  
}

saveRDS(object = nvc_pquads_cca_list, file = "./data/bundled_data/nvc_pquads_cca_list.rds")

