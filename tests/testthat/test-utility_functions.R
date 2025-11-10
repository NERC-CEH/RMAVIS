testthat::test_that("subset_nvcData works with the GB-NVC", {
  
  actual <- RMAVIS::subset_vcData(vc_data = RMAVIS::nvc_pquads, habitatRestriction = c("U", "H"), col_name = "psq_id", habitatRestrictionPrefixes = RMAVIS:::habitatRestrictionPrefixes)
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- colnames(RMAVIS::nvc_pquads)
  expected_coltypes <- sapply(RMAVIS::nvc_pquads, class) |> unname()
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
})

testthat::test_that("subset_nvcData works with the MNNPC", {
  
  actual <- RMAVIS::subset_vcData(vc_data = MNNPC::mnnpc_pquads, habitatRestriction = c("AP"), col_name = "psq_id", habitatRestrictionPrefixes = as.list(MNNPC::mnnpc_vc_types_named))
  
  actual_colnames <- colnames(actual)
  actual_coltypes <- sapply(actual, class) |> unname()
  
  expected_colnames <- colnames(MNNPC::mnnpc_pquads)
  expected_coltypes <- sapply(MNNPC::mnnpc_pquads, class) |> unname()
  
  testthat::expect_equal(actual_colnames, expected_colnames)
  testthat::expect_equal(actual_coltypes, expected_coltypes)
  
})
