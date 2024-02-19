# Example data ------------------------------------------------------------
example_data_all <- readRDS(file = "./data/example_data_all.rds")

# NVC floristic tables and community codes --------------------------------
nvc_floristic_tables <- readRDS(file = "./data/nvc_floristic_tables.rds")
nvc_floristic_tables_numeric <- readRDS(file = "./data/nvc_floristic_tables_numeric.rds")
nvc_community_codes <- readRDS(file = "./data/nvc_community_codes.rds")
nvc_community_namesCodes <- readRDS("./data/nvc_community_namesCodes.rds")


# Habitat Correspondence --------------------------------------------------
all_habCor_final <- readRDS(file = "./data/all_habCor_final.rds")
all_habCor_classifications <- readRDS(file = "./data/all_habCor_classifications.rds")


# Master data -------------------------------------------------------------
master_data <- readRDS(file = "./data/master_data.rds")


# NVC Pseudo-quadrat data -------------------------------------------------
nvc_pquads_final <- readRDS(file = "./data/nvc_pquads_final.rds")


# Accepted Species data ---------------------------------------------------
# Supplied to the user via download button
acceptedSpecies <- readRDS(file = "./data/acceptedSpecies.rds")

# Species names -----------------------------------------------------------
speciesNames <- readRDS(file = "./data/speciesNames.rds")


# Concordance data --------------------------------------------------------
concordance_all <- readRDS(file = "./data/concordance_all.rds")
concordance_plants <- readRDS(file = "./data/concordance_plants.rds")
concordance_bryophytes <- readRDS(file = "./data/concordance_bryophytes.rds")

# Pseudo-quadrat DCA scores -----------------------------------------------
nvc_pquads_final_wide <- readRDS(file = "./data/nvc_pquads_final_wide.rds")
nvc_pquad_dca_all <- readRDS(file = "./data/nvc_pquad_dca_all.rds")


# Pseudo-quadrat hulls ----------------------------------------------------
nvc_pquad_dca_all_hulls <- readRDS(file = "./data/nvc_pquad_dca_all_hulls.rds")


# Pseudo-quadrat mean unweighted EIVs -------------------------------------
nvc_pquads_mean_unweighted_eivs <- readRDS(file = "./data/nvc_pquads_mean_unweighted_eivs.rds")

