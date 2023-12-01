# Example data ------------------------------------------------------------
example_data_df <- readRDS(file = "data/bundled_data/example_data_df.rds")
exampleDataOptions <- readRDS(file = "data/bundled_data/exampleDataOptions.rds")


# NVC floristic tables and community codes --------------------------------
nvc_floristic_tables <- readRDS(file = "data/bundled_data/nvc_floristic_tables.rds")
nvc_community_codes <- readRDS(file = "data/bundled_data/nvc_community_codes.rds")


# Habitat Correspondence --------------------------------------------------
all_habCor_final <- readRDS(file = "data/bundled_data/all_habCor_final.rds")
all_habCor_classifications <- readRDS(file = "data/bundled_data/all_habCor_classifications.rds")


# Master data -------------------------------------------------------------
master_data <- readRDS(file = "data/bundled_data/master_data.rds")


# NVC Pseudo-quadrat data -------------------------------------------------
nvc_pquads_final <- readRDS(file = "data/bundled_data/nvc_pquads_final.rds")


# Species names -----------------------------------------------------------
speciesNames <- readRDS(file = "data/bundled_data/speciesNames.rds")


# Concordance data --------------------------------------------------------
concordance_all <- readRDS(file = "./data/bundled_data/concordance_all.rds")


# Pseudo-quadrat DCA scores -----------------------------------------------
nvc_pquads_final_wide <- readRDS(file = "./data/bundled_data/nvc_pquads_final_wide.rds")
# nvc_pquads_final_dca_psquad_axisScores <-readRDS(file = "./data/bundled_data/nvc_pquads_final_dca_psquad_axisScores.rds")
# nvc_pquads_final_dca_species_axisScores <-readRDS(file = "./data/bundled_data/nvc_pquads_final_dca_species_axisScores.rds")
