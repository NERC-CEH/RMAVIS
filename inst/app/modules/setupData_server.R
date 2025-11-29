setupData <- function(input, output, session, region, deSidebar_options, sidebar_options) {
  
  ns <- session$ns
  
# Establish reactive objects ----------------------------------------------
  setupData_init <- list(
    "region" = "gbnvc",
    "regional_availability" = as.list(tibble::deframe(RMAVIS:::regional_availability[, c("module", "gbnvc")])),
    "species_names" = RMAVIS::accepted_taxa[["taxon_name"]],
    "accepted_species" = RMAVIS::accepted_taxa,
    "example_data" = RMAVIS::example_data,
    "floristic_tables" = RMAVIS::nvc_floristic_tables,
    "community_attributes" = RMAVIS::nvc_community_attributes,
    "pquads" = RMAVIS::nvc_pquads,
    "psquad_cm_he" = RMAVIS::nvc_psquad_cm_he,
    "example_data_options" = RMAVIS:::example_data_options,
    "habitat_correspondences" = RMAVIS::habitat_correspondences,
    "ft_taxon_name_col" = "nvc_taxon_name",
    "psq_taxon_name_col" = "nvc_taxon_name",
    "unit_name_col" = "nvc_code",
    "hab_rest_pref" = RMAVIS:::habitatRestrictionPrefixes,
    "agg_lookup" = NULL
  )

# Initialise objets -------------------------------------------------------
  setupData <- reactiveVal(setupData_init)

# Retrieve sidebar options ------------------------------------------------
  selectVCtypes <- reactiveVal("Original")

  observe({

    sidebar_options <- sidebar_options()

    selectVCtypes <- sidebar_options$selectVCtypes

    selectVCtypes(selectVCtypes)

  }) |>
    bindEvent(sidebar_options(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
# Update input data -------------------------------------------------------
  observe({
    
    selected_vc_types <- selectVCtypes()

    regional_availability_selected <- as.list(tibble::deframe(RMAVIS:::regional_availability[, c("module", region())]))
    
    if(region() == "gbnvc"){
      
      # Establish setup data which doesn't vary based on NVC type
      species_names_selected <- RMAVIS::accepted_taxa[["taxon_name"]]
      accepted_species_selected <- RMAVIS::accepted_taxa
      example_data_selected <- RMAVIS::example_data
      example_data_options_selected <- RMAVIS:::example_data_options
      habitat_correspondences_selected <-  RMAVIS::habitat_correspondences
      ft_taxon_name_col_selected <- "nvc_taxon_name"
      psq_taxon_name_col_selected <- "nvc_taxon_name"
      unit_name_col_selected <- "nvc_code"
      hab_rest_pref_selected <- RMAVIS:::habitatRestrictionPrefixes
      agg_lookup_selected <- NULL
      
      # Compose setup data from selected NVC types
      floristic_tables_selected <- tibble::tibble()
      community_attributes_selected <- tibble::tibble()
      pquads_selected <- tibble::tibble()
      psquad_cm_he_selected <- tibble::tibble()
      comm_he_selected <- tibble::tibble()
      
      if("Original" %in% selected_vc_types){
        
        floristic_tables_selected <- floristic_tables_selected |>
          dplyr::bind_rows(RMAVIS::nvc_floristic_tables)
        
        community_attributes_selected <- community_attributes_selected |>
          dplyr::bind_rows(RMAVIS::nvc_community_attributes)
        
        pquads_selected <- pquads_selected |>
          dplyr::bind_rows(RMAVIS::nvc_pquads)
        
        psquad_cm_he_selected <- psquad_cm_he_selected |>
          dplyr::bind_rows(RMAVIS::nvc_psquad_cm_he)
        
        comm_he_selected <- comm_he_selected |>
          dplyr::bind_rows(RMAVIS::nvc_cm_he)
        
      }
      
      if("Calthion" %in% selected_vc_types){
        
        floristic_tables_selected <- floristic_tables_selected |>
          dplyr::bind_rows(RMAVIS::calthion_floristic_tables)
        
        community_attributes_selected <- community_attributes_selected |>
          dplyr::bind_rows(RMAVIS::calthion_community_attributes)
        
        pquads_selected <- pquads_selected |>
          dplyr::bind_rows(RMAVIS::calthion_pquads)
        
        psquad_cm_he_selected <- psquad_cm_he_selected |>
          dplyr::bind_rows(RMAVIS::calthion_psquad_cm_he)
        
        comm_he_selected <- comm_he_selected |>
          dplyr::bind_rows(RMAVIS::calthion_cm_he)
        
      }
      
      if("SOWG" %in% selected_vc_types){
        
        floristic_tables_selected <- floristic_tables_selected |>
          dplyr::bind_rows(RMAVIS::sowg_floristic_tables)
        
        community_attributes_selected <- community_attributes_selected |>
          dplyr::bind_rows(RMAVIS::sowg_community_attributes)
        
        pquads_selected <- pquads_selected |>
          dplyr::bind_rows(RMAVIS::sowg_pquads)
        
        psquad_cm_he_selected <- psquad_cm_he_selected |>
          dplyr::bind_rows(RMAVIS::sowg_psquad_cm_he)
        
        comm_he_selected <- comm_he_selected |>
          dplyr::bind_rows(RMAVIS::sowg_cm_he)
        
      }
      
    } else if(region() == "mnnpc"){
      
      species_names_selected <- MNNPC::mnnpc_accepted_taxa[["taxon_name"]]
      accepted_species_selected <- MNNPC::mnnpc_accepted_taxa
      example_data_selected <- MNNPC::mnnpc_example_data
      floristic_tables_selected <- MNNPC::mnnpc_floristic_tables
      community_attributes_selected <- MNNPC::mnnpc_community_attributes
      pquads_selected <- MNNPC::mnnpc_pquads
      psquad_cm_he_selected <- NULL
      comm_he_selected <- NULL
      example_data_options_selected <- MNNPC:::example_data_options
      habitat_correspondences_selected <-  NULL
      ft_taxon_name_col_selected <- "npc_taxon_name"
      psq_taxon_name_col_selected <- "taxon_name"
      unit_name_col_selected <- "npc_code"
      hab_rest_pref_selected <- MNNPC::mnnpc_vc_types
      agg_lookup_selected <- MNNPC::mnnpc_taxa_conv
      
    }
    
    # Compose final setup data
    setupData_list <- list(
      "region" = region(),
      "regional_availability" = regional_availability_selected,
      "species_names" = species_names_selected,
      "accepted_species" = accepted_species_selected,
      "example_data" = example_data_selected,
      "floristic_tables" = floristic_tables_selected,
      "community_attributes" = community_attributes_selected,
      "pquads" = pquads_selected,
      "psquad_cm_he" = psquad_cm_he_selected,
      "comm_cm_he" = comm_he_selected,
      "example_data_options" = example_data_options_selected,
      "habitat_correspondences" = habitat_correspondences_selected,
      "ft_taxon_name_col" = ft_taxon_name_col_selected,
      "psq_taxon_name_col" = psq_taxon_name_col_selected,
      "unit_name_col" = unit_name_col_selected,
      "hab_rest_pref" = hab_rest_pref_selected,
      "agg_lookup" = agg_lookup_selected
    )
    
    setupData(setupData_list)
    
  }) |>
    bindEvent(region(),
              selectVCtypes(),
              ignoreNULL = TRUE,
              ignoreInit = FALSE)
  
# Return Setup Data -------------------------------------------------------
  return(setupData)
  
}
