setupData <- function(input, output, session, deSidebar_options) {
  
  ns <- session$ns
  
# Establish reactive objects ----------------------------------------------
  setupData_init <- list(
    "species_names" = RMAVIS::accepted_taxa[["taxon_name"]],
    "accepted_species" = RMAVIS::accepted_taxa,
    "example_data" = RMAVIS::example_data,
    "nvc_floristic_tables" = RMAVIS::nvc_floristic_tables,
    "nvc_pquads" = RMAVIS::nvc_pquads,
    "nvc_pquad_dca" = RMAVIS::nvc_pquad_dca,
    "nvc_pquad_dca_hulls" = RMAVIS::nvc_pquad_dca_hulls,
    "nvc_pquad_dca_centroids" = RMAVIS::nvc_pquad_dca_centroids,
    "nvc_pquads_mean_unweighted_eivs" = RMAVIS::nvc_psquad_cm_he
  )
  
  setupData <- reactiveVal(setupData_init)

# Retrieve sidebar options ------------------------------------------------
  # observe({
  #   
  # }) |>
  #   bindEvent(deSidebar_options(), 
  #             ignoreInit = FALSE)


# Update input data -------------------------------------------------------
  observe({
    
    species_names_selected <- RMAVIS::accepted_taxa[["taxon_name"]]
    accepted_species_selected <- RMAVIS::accepted_taxa
    example_data_selected <- RMAVIS::example_data
    nvc_floristic_tables_selected <- RMAVIS::nvc_floristic_tables
    nvc_pquads_selected <- RMAVIS::nvc_pquads
    nvc_pquad_dca_selected <- RMAVIS::nvc_pquad_dca
    nvc_pquad_dca_hulls_selected <- RMAVIS::nvc_pquad_dca_hulls
    nvc_pquad_dca_centroids_selected <- RMAVIS::nvc_pquad_dca_centroids
    nvc_pquads_mean_unweighted_eivs_selected <- RMAVIS::nvc_psquad_cm_he
    
    setupData_list <- list(
      "species_names" = species_names_selected,
      "accepted_species" = accepted_species_selected,
      "example_data" = example_data_selected,
      "nvc_floristic_tables" = nvc_floristic_tables_selected,
      "nvc_floristic_tables_numeric" = nvc_floristic_tables_numeric_selected,
      "nvc_pquads" = nvc_pquads_selected,
      "nvc_pquads_wide" = nvc_pquads_wide_selected,
      "nvc_pquad_dca" = nvc_pquad_dca_selected,
      "nvc_pquad_dca_hulls" = nvc_pquad_dca_hulls_selected,
      "nvc_pquad_dca_centroids" = nvc_pquad_dca_centroids_selected,
      "nvc_pquads_mean_unweighted_eivs" = nvc_pquads_mean_unweighted_eivs_selected
    )
    
    setupData(setupData_list)
    
  })
  
# Return Setup Data -------------------------------------------------------
  return(setupData)
  
}
