testthat::test_that("subset_nvcData works", {
  
  actual <- RMAVIS::subset_nvcData(nvc_data = RMAVIS::nvc_pquads, habitatRestriction = c("U", "H"), col_name = "Pid3")
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- colnames(RMAVIS::nvc_pquads)
  expected_coltypes <- sapply(RMAVIS::nvc_pquads, class) |> unname()
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
})
