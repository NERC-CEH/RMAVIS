setupData <- function(input, output, session, sidebar_options) {
  
  ns <- session$ns
  
# Establish reactive objects ----------------------------------------------
  # uploadedTaxonomicBackbone <- reactiveVal()
  # finalTaxonomicBackbone <- reactiveVal()
  
  setupData_init <- list(
    "species_names" = RMAVIS::speciesNames,
    "accepted_species" = RMAVIS::acceptedSpecies,
    "example_data" = RMAVIS::example_data_all,
    "nvc_floristic_tables" = RMAVIS::nvc_floristic_tables,
    "nvc_floristic_tables_numeric" = RMAVIS::nvc_floristic_tables_numeric,
    "nvc_pquads_final" = RMAVIS::nvc_pquads_final,
    "nvc_pquads_final_wide" = RMAVIS::nvc_pquads_final_wide,
    "nvc_pquad_dca_all" = RMAVIS::nvc_pquad_dca_all,
    "nvc_pquad_dca_all_hulls" = RMAVIS::nvc_pquad_dca_all_hulls,
    "nvc_pquad_dca_all_centroids" = RMAVIS::nvc_pquad_dca_all_centroids,
    "nvc_pquads_mean_unweighted_eivs" = RMAVIS::nvc_pquads_mean_unweighted_eivs
  )
  
  setupData <- reactiveVal(setupData_init)

# Retrieve sidebar options ------------------------------------------------
  # selectedTaxonomicBackboneMethod <- reactiveVal()
  # selectedTaxonomicBackbone <- reactiveVal()
  # wcvpCountries <- reactiveVal()
  includeBryophytes <- reactiveVal()
  
  observe({
    
    includeBryophytes(sidebar_options()$includeBryophytes)
    # selectedTaxonomicBackboneMethod(sidebar_options()$taxonomicBackboneMethod)
    # selectedTaxonomicBackbone(sidebar_options()$bundledTaxonomicBackbone)
    # wcvpCountries(sidebar_options()$wcvpCountries)
    
  }) |>
    bindEvent(sidebar_options(), 
              ignoreInit = FALSE)

# Update Input Data Based On includeBryophytes ----------------------------
  observe({
    
    includeBryophytes <- includeBryophytes()
    
    if(includeBryophytes == TRUE){
      
      species_names_selected <- RMAVIS::speciesNames
      accepted_species_selected <- RMAVIS::acceptedSpecies
      example_data_selected <- RMAVIS::example_data_all
      nvc_floristic_tables_selected <- RMAVIS::nvc_floristic_tables
      nvc_floristic_tables_numeric_selected <- RMAVIS::nvc_floristic_tables_numeric
      nvc_pquads_final_selected <- RMAVIS::nvc_pquads_final
      nvc_pquads_final_wide_selected <- RMAVIS::nvc_pquads_final_wide
      nvc_pquad_dca_all_selected <- RMAVIS::nvc_pquad_dca_all
      nvc_pquad_dca_all_hulls_selected <- RMAVIS::nvc_pquad_dca_all_hulls
      nvc_pquad_dca_all_centroids_selected <- RMAVIS::nvc_pquad_dca_all_centroids
      nvc_pquads_mean_unweighted_eivs_selected <- RMAVIS::nvc_pquads_mean_unweighted_eivs
      
    } else if(includeBryophytes == FALSE){
     
      species_names_selected <- RMAVIS::speciesNames[RMAVIS::speciesNames %in% RMAVIS::concordance_plants$proposedSpecies] 
      
      accepted_species_selected <- RMAVIS::acceptedSpecies |>
        dplyr::filter(Accepted_Species %in% RMAVIS::concordance_plants$proposedSpecies)
      
      example_data_selected <- RMAVIS::example_data_all |>
        purrr::map(~dplyr::filter(., Species %in% RMAVIS::concordance_plants$proposedSpecies))
      
      nvc_floristic_tables_selected <- RMAVIS::nvc_floristic_tables |>
        dplyr::filter(Species %in% RMAVIS::concordance_plants$proposedSpecies)
      
      nvc_floristic_tables_numeric_selected <- RMAVIS::nvc_floristic_tables_numeric |>
        dplyr::filter(Species %in% RMAVIS::concordance_plants$proposedSpecies)
      
      nvc_pquads_final_selected <- RMAVIS::nvc_pquads_final |>
        dplyr::filter(species %in% RMAVIS::concordance_plants$proposedSpecies)
      
      nvc_pquads_final_wide_selected <- RMAVIS::nvc_pquads_final_wide[, (names(RMAVIS::nvc_pquads_final_wide) %in% RMAVIS::concordance_plants$proposedSpecies)]
      
      nvc_pquad_dca_all_selected <- RMAVIS::nvc_pquad_dca_noBryophytes
      
      nvc_pquad_dca_all_hulls_selected <- RMAVIS::nvc_pquad_dca_noBryophytes_hulls
      
      nvc_pquad_dca_all_centroids_selected <- RMAVIS::nvc_pquad_dca_noBryophytes_centroids

      nvc_pquads_mean_unweighted_eivs_selected <- RMAVIS::nvc_pquads_mean_unweighted_eivs_noBryophytes
      
    }
    
    setupData_list <- list(
      "species_names" = species_names_selected,
      "accepted_species" = accepted_species_selected,
      "example_data" = example_data_selected,
      "nvc_floristic_tables" = nvc_floristic_tables_selected,
      "nvc_floristic_tables_numeric" = nvc_floristic_tables_numeric_selected,
      "nvc_pquads_final" = nvc_pquads_final_selected,
      "nvc_pquads_final_wide" = nvc_pquads_final_wide_selected,
      "nvc_pquad_dca_all" = nvc_pquad_dca_all_selected,
      "nvc_pquad_dca_all_hulls" = nvc_pquad_dca_all_hulls_selected,
      "nvc_pquad_dca_all_centroids" = nvc_pquad_dca_all_centroids_selected,
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