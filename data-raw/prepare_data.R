# Example data ------------------------------------------------------------
example_data_all <- readRDS(file = "./inst/extdata/example_data_all.rds")
usethis::use_data(example_data_all, overwrite = TRUE)

# NVC floristic tables and community codes --------------------------------
nvc_floristic_tables <- readRDS(file = "./inst/extdata/nvc_floristic_tables.rds")
usethis::use_data(nvc_floristic_tables, overwrite = TRUE)
nvc_floristic_tables_numeric <- readRDS(file = "./inst/extdata/nvc_floristic_tables_numeric.rds")
usethis::use_data(nvc_floristic_tables_numeric, overwrite = TRUE)
nvc_community_codes <- readRDS(file = "./inst/extdata/nvc_community_codes.rds")
usethis::use_data(nvc_community_codes, overwrite = TRUE)
nvc_community_namesCodes <- readRDS("./inst/extdata/nvc_community_namesCodes.rds")
usethis::use_data(nvc_community_namesCodes, overwrite = TRUE)

# Habitat Correspondence --------------------------------------------------
all_habCor_final <- readRDS(file = "./inst/extdata/all_habCor_final.rds")
usethis::use_data(all_habCor_final, overwrite = TRUE)
all_habCor_classifications <- readRDS(file = "./inst/extdata/all_habCor_classifications.rds")
usethis::use_data(all_habCor_classifications, overwrite = TRUE)

# Master data -------------------------------------------------------------
master_data <- readRDS(file = "./inst/extdata/master_data.rds")
usethis::use_data(master_data, overwrite = TRUE)

# NVC Pseudo-quadrat data -------------------------------------------------
nvc_pquads_final <- readRDS(file = "./inst/extdata/nvc_pquads_final.rds")
usethis::use_data(nvc_pquads_final, overwrite = TRUE)
nvc_pquads_final_wide <- readRDS(file = "./inst/extdata/nvc_pquads_final_wide.rds")
usethis::use_data(nvc_pquads_final_wide, overwrite = TRUE)

# Accepted Species data ---------------------------------------------------
# Supplied to the user via download button
acceptedSpecies <- readRDS(file = "./inst/extdata/acceptedSpecies.rds")
usethis::use_data(acceptedSpecies, overwrite = TRUE)

# Species names -----------------------------------------------------------
speciesNames <- readRDS(file = "./inst/extdata/speciesNames.rds")
usethis::use_data(speciesNames, overwrite = TRUE)

# Concordance data --------------------------------------------------------
concordance_all <- readRDS(file = "./inst/extdata/concordance_all.rds")
usethis::use_data(concordance_all, overwrite = TRUE)
concordance_plants <- readRDS(file = "./inst/extdata/concordance_plants.rds")
usethis::use_data(concordance_plants, overwrite = TRUE)
concordance_bryophytes <- readRDS(file = "./inst/extdata/concordance_bryophytes.rds")
usethis::use_data(concordance_bryophytes, overwrite = TRUE)

# Pseudo-quadrat DCA scores -----------------------------------------------
nvc_pquad_dca_all <- readRDS(file = "./inst/extdata/nvc_pquad_dca_all.rds")
usethis::use_data(nvc_pquad_dca_all, overwrite = TRUE)
nvc_pquad_dca_noBryophytes <- readRDS(file = "./inst/extdata/nvc_pquad_dca_noBryophytes.rds")
usethis::use_data(nvc_pquad_dca_noBryophytes, overwrite = TRUE)

# Pseudo-quadrat hulls ----------------------------------------------------
nvc_pquad_dca_all_hulls <- readRDS(file = "./inst/extdata/nvc_pquad_dca_all_hulls.rds")
usethis::use_data(nvc_pquad_dca_all_hulls, overwrite = TRUE)
nvc_pquad_dca_noBryophytes_hulls <- readRDS(file = "./inst/extdata/nvc_pquad_dca_noBryophytes_hulls.rds")
usethis::use_data(nvc_pquad_dca_noBryophytes_hulls, overwrite = TRUE)

# Pseudo-quadrat centroids ------------------------------------------------
nvc_pquad_dca_all_centroids <- readRDS(file = "./inst/extdata/nvc_pquad_dca_all_centroids.rds")
usethis::use_data(nvc_pquad_dca_all_centroids, overwrite = TRUE)
nvc_pquad_dca_noBryophytes_centroids <- readRDS(file = "./inst/extdata/nvc_pquad_dca_noBryophytes_centroids.rds")
usethis::use_data(nvc_pquad_dca_noBryophytes_centroids, overwrite = TRUE)

# Pseudo-quadrat mean unweighted EIVs -------------------------------------
nvc_pquads_mean_unweighted_eivs <- readRDS(file = "./inst/extdata/nvc_pquads_mean_unweighted_eivs.rds")
usethis::use_data(nvc_pquads_mean_unweighted_eivs, overwrite = TRUE)
nvc_pquads_mean_unweighted_eivs_noBryophytes <- readRDS(file = "./inst/extdata/nvc_pquads_mean_unweighted_eivs_noBryophytes.rds")
usethis::use_data(nvc_pquads_mean_unweighted_eivs_noBryophytes, overwrite = TRUE)