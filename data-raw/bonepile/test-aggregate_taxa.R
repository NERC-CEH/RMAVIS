testthat::test_that("aggregate_taxa works", {
  
  test_plot_data <- MNNPC::mnnpc_example_data$`Earthworm-Invaded Forests`
  test_agg_lookup <- MNNPC::mnnpc_taxa_lookup
  
  actual <- RMAVIS::aggregate_taxa(plot_data = test_plot_data, 
                                   agg_lookup = test_agg_lookup,
                                   plot_data_taxon_col = "taxon",
                                   agg_lookup_taxon_col = "taxon_name")
  
  expected_rows <- nrow(test_plot_data)
  
  testthat::expect_equal(nrow(actual), expected_rows)
  
})
