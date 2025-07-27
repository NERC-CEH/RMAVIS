testthat::test_that("similarityCzekanowski works", {
  
  actual <- RMAVIS::similarityCzekanowski(samp_df = RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
                                                                                  group_cols = c("Year", "Group"), 
                                                                                  species_col_name = "Species", 
                                                                                  plot_col_name = "Quadrat",
                                                                                  numeric_constancy = TRUE), 
                                          comp_df = RMAVIS::subset_nvcData(nvc_data = RMAVIS::nvc_floristic_tables_numeric, 
                                                                           habitatRestriction = c("CG"), 
                                                                           col_name = "NVC.Code"), 
                                          samp_species_col = "Species", 
                                          comp_species_col = "Species", 
                                          samp_group_name = "ID", 
                                          comp_group_name = "NVC.Code",
                                          samp_weight_name = "Constancy", 
                                          comp_weight_name = "Constancy",
                                          downweight_threshold = 1, 
                                          downweight_value = 0.1)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("ID", "NVC.Code", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})

testthat::test_that("similarityJaccard works", {
  
  actual <- RMAVIS::similarityJaccard(samp_df = RMAVIS::example_data[["Parsonage Down"]], 
                                      comp_df = RMAVIS::subset_nvcData(nvc_data = RMAVIS::nvc_pquads, 
                                                                       habitatRestriction = c("CG"), 
                                                                       col_name = "Pid3"),
                                      samp_species_col = "Species", 
                                      comp_species_col = "species",
                                      samp_group_name = "Quadrat", 
                                      comp_group_name = "Pid3",
                                      comp_groupID_name = "NVC", 
                                      remove_zero_matches = TRUE, 
                                      average_comp = TRUE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("Quadrat", "NVC", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})

testthat::test_that("similarityJaccard works with one species", {
  
  actual <- RMAVIS::similarityJaccard(samp_df = RMAVIS::example_data[["Parsonage Down"]][1,], 
                                      comp_df = RMAVIS::subset_nvcData(nvc_data = RMAVIS::nvc_pquads, 
                                                                       habitatRestriction = c("CG"), 
                                                                       col_name = "Pid3"),
                                      samp_species_col = "Species", 
                                      comp_species_col = "species",
                                      samp_group_name = "Quadrat", 
                                      comp_group_name = "Pid3",
                                      comp_groupID_name = "NVC", 
                                      remove_zero_matches = TRUE, 
                                      average_comp = TRUE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("Quadrat", "NVC", "Similarity")
  expected_coltypes <- c("character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Similarity <= 1))
  testthat::expect_true(all(actual$Similarity >= 0))
  
})
