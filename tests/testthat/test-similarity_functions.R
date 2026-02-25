testthat::test_that("similarityCzekanowski works using the GB-NVC", {
  
  actual <- RMAVIS::similarityCzekanowski(samp_df = RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
                                                                                  group_cols = c("Year", "Group"), 
                                                                                  species_col_name = "Species", 
                                                                                  plot_col_name = "Quadrat",
                                                                                  numeral_constancy = FALSE,
                                                                                  factor_constancy = FALSE), 
                                          comp_df = RMAVIS::subset_vcData(vc_data = RMAVIS::nvc_floristic_tables, 
                                                                          habitatRestriction = c("CG"), 
                                                                          col_name = "nvc_code"), 
                                          samp_species_col = "Species", 
                                          comp_species_col = "nvc_taxon_name", 
                                          samp_group_name = "ID", 
                                          comp_group_name = "nvc_code",
                                          samp_weight_name = "Constancy", 
                                          comp_weight_name = "constancy",
                                          downweight_threshold = 1, 
                                          downweight_value = 0.1)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("ID", "nvc_code", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})

testthat::test_that("similarityJaccard works using the GB-NVC", {
  
  actual <- RMAVIS::similarityJaccard(samp_df = RMAVIS::example_data[["Parsonage Down"]], 
                                      comp_df = RMAVIS::subset_vcData(vc_data = RMAVIS::nvc_pquads, 
                                                                      habitatRestriction = c("CG"), 
                                                                      col_name = "psq_id"),
                                      samp_species_col = "Species", 
                                      comp_species_col = "nvc_taxon_name",
                                      samp_group_name = "Quadrat", 
                                      comp_group_name = "psq_id",
                                      comp_groupID_name = "nvc_code", 
                                      remove_zero_matches = TRUE, 
                                      average_comp = TRUE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("Quadrat", "nvc_code", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})

testthat::test_that("similarityJaccard works with one species using the GB-NVC", {
  
  actual <- RMAVIS::similarityJaccard(samp_df = RMAVIS::example_data[["Parsonage Down"]][1,], 
                                      comp_df = RMAVIS::subset_vcData(vc_data = RMAVIS::nvc_pquads, 
                                                                      habitatRestriction = c("CG"), 
                                                                      col_name = "psq_id"),
                                      samp_species_col = "Species", 
                                      comp_species_col = "nvc_taxon_name",
                                      samp_group_name = "Quadrat", 
                                      comp_group_name = "psq_id",
                                      comp_groupID_name = "nvc_code", 
                                      remove_zero_matches = TRUE, 
                                      average_comp = TRUE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("Quadrat", "nvc_code", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})

testthat::test_that("similarityCzekanowski works using the MNNPC", {
  
  actual <- RMAVIS::similarityCzekanowski(samp_df = RMAVIS::composeSyntopicTables(surveyData = MNNPC::mnnpc_example_data[["St. Croix State Forest"]], 
                                                                                  group_cols = c("Year", "Group"), 
                                                                                  species_col_name = "Species", 
                                                                                  plot_col_name = "Quadrat",
                                                                                  numeral_constancy = FALSE,
                                                                                  factor_constancy = FALSE), 
                                          comp_df = MNNPC::mnnpc_floristic_tables, 
                                          samp_species_col = "Species", 
                                          comp_species_col = "npc_taxon_name", 
                                          samp_group_name = "ID", 
                                          comp_group_name = "npc_code",
                                          samp_weight_name = "Constancy", 
                                          comp_weight_name = "constancy",
                                          downweight_threshold = 1, 
                                          downweight_value = 0.1)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("ID", "npc_code", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})

testthat::test_that("similarityJaccard works using the MNNPC", {
  
  actual <- RMAVIS::similarityJaccard(samp_df = MNNPC::mnnpc_example_data[["St. Croix State Forest"]],
                                      comp_df = MNNPC::mnnpc_pquads,
                                      samp_species_col = "Species", 
                                      comp_species_col = "taxon_name",
                                      samp_group_name = "Quadrat", 
                                      comp_group_name = "psq_id",
                                      comp_groupID_name = "npc_code", 
                                      remove_zero_matches = TRUE, 
                                      average_comp = TRUE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("Quadrat", "npc_code", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})

testthat::test_that("similarityJaccard works with one species using the MNNPC", {
  
  actual <- RMAVIS::similarityJaccard(samp_df = MNNPC::mnnpc_example_data[["St. Croix State Forest"]][1,],
                                      comp_df = MNNPC::mnnpc_pquads,
                                      samp_species_col = "Species", 
                                      comp_species_col = "taxon_name",
                                      samp_group_name = "Quadrat", 
                                      comp_group_name = "psq_id",
                                      comp_groupID_name = "npc_code", 
                                      remove_zero_matches = TRUE, 
                                      average_comp = TRUE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("Quadrat", "npc_code", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})