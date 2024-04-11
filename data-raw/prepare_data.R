# Example data ------------------------------------------------------------
example_data <- readRDS(file = "./inst/extdata/example_data.rds")
usethis::use_data(example_data, overwrite = TRUE)

# NVC floristic tables and community codes --------------------------------
nvc_floristic_tables <- readRDS(file = "./inst/extdata/nvc_floristic_tables.rds")
usethis::use_data(nvc_floristic_tables, overwrite = TRUE)

nvc_floristic_tables_numeric <- readRDS(file = "./inst/extdata/nvc_floristic_tables_numeric.rds")
usethis::use_data(nvc_floristic_tables_numeric, overwrite = TRUE)

nvc_community_namesCodes <- readRDS("./inst/extdata/nvc_community_namesCodes.rds")
usethis::use_data(nvc_community_namesCodes, overwrite = TRUE)

# Habitat Correspondence --------------------------------------------------
habCor_data <- readRDS(file = "./inst/extdata/habCor_data.rds")
usethis::use_data(habCor_data, overwrite = TRUE)

habCor_classifications <- readRDS(file = "./inst/extdata/habCor_classifications.rds")
usethis::use_data(habCor_classifications, overwrite = TRUE)

# Master data -------------------------------------------------------------
master_data <- readRDS(file = "./inst/extdata/master_data.rds")
usethis::use_data(master_data, overwrite = TRUE)

# NVC Pseudo-quadrat data -------------------------------------------------
nvc_pquads_final <- readRDS(file = "./inst/extdata/nvc_pquads_final.rds")
usethis::use_data(nvc_pquads_final, overwrite = TRUE)

nvc_pquads_final_wide <- readRDS(file = "./inst/extdata/nvc_pquads_final_wide.rds")
usethis::use_data(nvc_pquads_final_wide, overwrite = TRUE)

# Accepted Species data ---------------------------------------------------
acceptedSpecies <- readRDS(file = "./inst/extdata/acceptedSpecies.rds")
usethis::use_data(acceptedSpecies, overwrite = TRUE)

# Concordance data --------------------------------------------------------
concordance <- readRDS(file = "./inst/extdata/concordance.rds")
usethis::use_data(concordance, overwrite = TRUE)

# Pseudo-quadrat DCA scores -----------------------------------------------
nvc_pquad_dca <- readRDS(file = "./inst/extdata/nvc_pquad_dca.rds")
usethis::use_data(nvc_pquad_dca, overwrite = TRUE)

nvc_pquad_dca_noBryophytes <- readRDS(file = "./inst/extdata/nvc_pquad_dca_noBryophytes.rds")
usethis::use_data(nvc_pquad_dca_noBryophytes, overwrite = TRUE)

# Pseudo-quadrat hulls ----------------------------------------------------
nvc_pquad_dca_hulls <- readRDS(file = "./inst/extdata/nvc_pquad_dca_hulls.rds")
usethis::use_data(nvc_pquad_dca_hulls, overwrite = TRUE)

nvc_pquad_dca_noBryophytes_hulls <- readRDS(file = "./inst/extdata/nvc_pquad_dca_noBryophytes_hulls.rds")
usethis::use_data(nvc_pquad_dca_noBryophytes_hulls, overwrite = TRUE)

# Pseudo-quadrat centroids ------------------------------------------------
nvc_pquad_dca_centroids <- readRDS(file = "./inst/extdata/nvc_pquad_dca_centroids.rds")
usethis::use_data(nvc_pquad_dca_centroids, overwrite = TRUE)

nvc_pquad_dca_noBryophytes_centroids <- readRDS(file = "./inst/extdata/nvc_pquad_dca_noBryophytes_centroids.rds")
usethis::use_data(nvc_pquad_dca_noBryophytes_centroids, overwrite = TRUE)

# Pseudo-quadrat mean unweighted EIVs -------------------------------------
nvc_pquads_mean_unweighted_eivs <- readRDS(file = "./inst/extdata/nvc_pquads_mean_unweighted_eivs.rds")
usethis::use_data(nvc_pquads_mean_unweighted_eivs, overwrite = TRUE)

nvc_pquads_mean_unweighted_eivs_noBryophytes <- readRDS(file = "./inst/extdata/nvc_pquads_mean_unweighted_eivs_noBryophytes.rds")
usethis::use_data(nvc_pquads_mean_unweighted_eivs_noBryophytes, overwrite = TRUE)