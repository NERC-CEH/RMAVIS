testthat::test_that("composeSyntopicTables works", {
  
  actual <- RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
                                          group_cols = c("Year", "Group"), 
                                          species_col_name = "Species", 
                                          plot_col_name = "Quadrat",
                                          numeric_constancy = FALSE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("ID", "Species", "Constancy")
  expected_coltypes <- c("character", "character", "factor")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Constancy %in% c("I", "II", "III", "IV", "V")))
  
  
})
