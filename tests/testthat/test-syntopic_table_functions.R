testthat::test_that("composeSyntopicTables works with numeral constancy false and factor constancy false", {
  
  actual <- RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
                                          group_cols = c("Year", "Group"), 
                                          species_col_name = "Species", 
                                          plot_col_name = "Quadrat",
                                          factor_constancy = FALSE,
                                          numeral_constancy = FALSE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("ID", "Species", "Constancy", "Min.Cover", "Mean.Cover", "Max.Cover")
  expected_coltypes <- c("character", "character", "numeric", "integer", "numeric", "integer")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Constancy %in% c(1, 2, 3, 4, 5)))
  
  
})

testthat::test_that("composeSyntopicTables works with numeral constancy false and factor constancy true", {
  
  actual <- RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
                                          group_cols = c("Year", "Group"), 
                                          species_col_name = "Species", 
                                          plot_col_name = "Quadrat",
                                          factor_constancy = TRUE,
                                          numeral_constancy = FALSE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("ID", "Species", "Constancy", "Min.Cover", "Mean.Cover", "Max.Cover")
  expected_coltypes <- c("character", "character", "factor", "integer", "numeric", "integer")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Constancy %in% c(1, 2, 3, 4, 5)))
  
  
})

testthat::test_that("composeSyntopicTables works with numeral constancy true and factor constancy false", {
  
  actual <- RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
                                          group_cols = c("Year", "Group"), 
                                          species_col_name = "Species", 
                                          plot_col_name = "Quadrat",
                                          factor_constancy = FALSE,
                                          numeral_constancy = TRUE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("ID", "Species", "Constancy", "Min.Cover", "Mean.Cover", "Max.Cover")
  expected_coltypes <- c("character", "character", "character", "integer", "numeric", "integer")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Constancy %in% c("I", "II", "III", "IV", "V")))
  
  
})

testthat::test_that("composeSyntopicTables works with numeral constancy true and factor constancy true", {
  
  actual <- RMAVIS::composeSyntopicTables(surveyData = RMAVIS::example_data[["Parsonage Down"]], 
                                          group_cols = c("Year", "Group"), 
                                          species_col_name = "Species", 
                                          plot_col_name = "Quadrat",
                                          factor_constancy = TRUE,
                                          numeral_constancy = TRUE)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("ID", "Species", "Constancy", "Min.Cover", "Mean.Cover", "Max.Cover")
  expected_coltypes <- c("character", "character", "factor", "integer", "numeric", "integer")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
  testthat::expect_true(all(actual$Constancy %in% c("I", "II", "III", "IV", "V")))
  
  
})
