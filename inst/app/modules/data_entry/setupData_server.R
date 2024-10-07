setupData <- function(input, output, session, deSidebar_options) {
  
  ns <- session$ns
  
# Establish reactive objects ----------------------------------------------
  setupData_init <- list(
    "species_names" = RMAVIS::acceptedSpecies[["Accepted_Species"]],
    "accepted_species" = RMAVIS::acceptedSpecies,
    "example_data" = RMAVIS::example_data,
    "nvc_floristic_tables" = RMAVIS::nvc_floristic_tables,
    "nvc_floristic_tables_numeric" = RMAVIS::nvc_floristic_tables_numeric,
    "nvc_pquads" = RMAVIS::nvc_pquads,
    "nvc_pquads_wide" = RMAVIS::nvc_pquads_wide,
    "nvc_pquad_dca" = RMAVIS::nvc_pquad_dca,
    "nvc_pquad_dca_hulls" = RMAVIS::nvc_pquad_dca_hulls,
    "nvc_pquad_dca_centroids" = RMAVIS::nvc_pquad_dca_centroids,
    "nvc_pquads_mean_unweighted_eivs" = RMAVIS::nvc_pquads_mean_unweighted_eivs
  )
  
  setupData <- reactiveVal(setupData_init)

# Retrieve sidebar options ------------------------------------------------
  includeBryophytes <- reactiveVal()
  
  observe({
    
    includeBryophytes(deSidebar_options()$includeBryophytes)
    
  }) |>
    bindEvent(deSidebar_options(), 
              ignoreInit = FALSE)

# Update Input Data Based On includeBryophytes ----------------------------
  observe({
    
    includeBryophytes <- includeBryophytes()
    
    if(includeBryophytes == TRUE){
      
      species_names_selected <- RMAVIS::acceptedSpecies[["Accepted_Species"]]
      accepted_species_selected <- RMAVIS::acceptedSpecies
      example_data_selected <- RMAVIS::example_data
      nvc_floristic_tables_selected <- RMAVIS::nvc_floristic_tables
      nvc_floristic_tables_numeric_selected <- RMAVIS::nvc_floristic_tables_numeric
      nvc_pquads_selected <- RMAVIS::nvc_pquads
      nvc_pquads_wide_selected <- RMAVIS::nvc_pquads_wide
      nvc_pquad_dca_selected <- RMAVIS::nvc_pquad_dca
      nvc_pquad_dca_hulls_selected <- RMAVIS::nvc_pquad_dca_hulls
      nvc_pquad_dca_centroids_selected <- RMAVIS::nvc_pquad_dca_centroids
      nvc_pquads_mean_unweighted_eivs_selected <- RMAVIS::nvc_pquads_mean_unweighted_eivs
      
    } else if(includeBryophytes == FALSE){
      
      plant_species <- RMAVIS::concordance |>
        dplyr::filter(TaxonGroup == "Vascular Plants") |>
        dplyr::pull(rmavisTaxonName)
     
      species_names_selected <- RMAVIS::acceptedSpecies[["Accepted_Species"]][RMAVIS::acceptedSpecies[["Accepted_Species"]] %in% plant_species] 
      
      accepted_species_selected <- RMAVIS::acceptedSpecies |>
        dplyr::filter(Accepted_Species %in% plant_species)
      
      example_data_selected <- RMAVIS::example_data |>
        purrr::map(~dplyr::filter(., Species %in% plant_species))
      
      nvc_floristic_tables_selected <- RMAVIS::nvc_floristic_tables |>
        dplyr::filter(Species %in% plant_species)
      
      nvc_floristic_tables_numeric_selected <- RMAVIS::nvc_floristic_tables_numeric |>
        dplyr::filter(Species %in% plant_species)
      
      nvc_pquads_selected <- RMAVIS::nvc_pquads |>
        dplyr::filter(species %in% plant_species)
      
      nvc_pquads_wide_selected <- RMAVIS::nvc_pquads_wide[, (names(RMAVIS::nvc_pquads_wide) %in% plant_species)]
      
      nvc_pquad_dca_selected <- RMAVIS::nvc_pquad_dca_noBryophytes
      
      nvc_pquad_dca_hulls_selected <- RMAVIS::nvc_pquad_dca_noBryophytes_hulls
      
      nvc_pquad_dca_centroids_selected <- RMAVIS::nvc_pquad_dca_noBryophytes_centroids

      nvc_pquads_mean_unweighted_eivs_selected <- RMAVIS::nvc_pquads_mean_unweighted_eivs_noBryophytes
      
    }
    
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
    
  }) |>
    bindEvent(includeBryophytes(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  
# Return Setup Data -------------------------------------------------------
  return(setupData)
  
}
