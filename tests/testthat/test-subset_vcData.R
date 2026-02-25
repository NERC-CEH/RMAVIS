testthat::test_that("subset_vcData works on GB-NVC data", {
  
  actual <- RMAVIS::subset_vcData(vc_data = RMAVIS::nvc_pquads, 
                                  habitatRestriction = c("U", "H"), 
                                  col_name = "psq_id",
                                  habitatRestrictionPrefixes = RMAVIS:::habitatRestrictionPrefixes)
  actual_system_region <- stringr::str_extract(string = actual$nvc_code, pattern = "\\w{1}") |> unique()
  
  testthat::expect_true(setequal(c("U", "H"), actual_system_region))
  
})

testthat::test_that("subset_vcData works on MNNPC data", {
  
  actual <- RMAVIS::subset_vcData(vc_data = MNNPC::mnnpc_pquads, 
                                  habitatRestriction = c("MH"), 
                                  col_name = "psq_id",
                                  habitatRestrictionPrefixes = MNNPC::mnnpc_vc_systems_flreg_nested)
  
  actual_system_region <- stringr::str_extract(string = actual$npc_code, pattern = "\\w{3}") |> unique()
  
  testthat::expect_equal(actual_system_region, c("MHc", "MHn", "MHs", "MHw"))
  
})
