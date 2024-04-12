testthat::test_that("read_mavis_data works", {

  actual <- RMAVIS::read_mavis_data(filepath = system.file("testdata", "mavis_testdata.txt", package = "RMAVIS"))
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- c("Year", "Group", "Quadrat", "Species", "Cover")
  expected_coltypes <- c("integer", "character", "character", "character", "numeric")
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  testthat::expect_true(all(actual$Cover <= 100))
  testthat::expect_true(all(actual$Cover >= 0))
  
})
