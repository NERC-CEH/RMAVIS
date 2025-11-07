setupData <- function(input, output, session, region, deSidebar_options, sidebar_options) {
  
  ns <- session$ns
  
# Establish reactive objects ----------------------------------------------
  setupData_init <- list(
    "species_names" = RMAVIS::accepted_taxa[["taxon_name"]],
    "accepted_species" = RMAVIS::accepted_taxa,
    "example_data" = RMAVIS::example_data,
    "floristic_tables" = RMAVIS::nvc_floristic_tables,
    "community_attributes" = RMAVIS::nvc_community_attributes,
    "pquads" = RMAVIS::nvc_pquads,
    "psquad_cm_he" = RMAVIS::nvc_psquad_cm_he,
    "example_data_options" = RMAVIS:::example_data_options,
    "ft_taxon_name_col" = "nvc_taxon_name",
    "psq_taxon_name_col" = "nvc_taxon_name",
    "unit_name_col" = "nvc_code",
    "hab_rest_pref" = RMAVIS:::habitatRestrictionPrefixes
  )

# Initialise objets -------------------------------------------------------
  selected_region <- reactiveVal("gbnvc")
  setupData <- reactiveVal(setupData_init)

# Retrieve sidebar options ------------------------------------------------
  selectNVCtypes <- reactiveVal()
  
  observe({
    
    sidebar_options <- sidebar_options()
    
    selectNVCtypes <- sidebar_options$selectNVCtypes
    
    selectNVCtypes(selectNVCtypes)

  }) |>
    bindEvent(sidebar_options(),
              ignoreInit = FALSE)
  

# Update region -----------------------------------------------------------
  observe({
    
    selected_region(region())
    
  }) |>
    shiny::bindEvent(region(),
                     ignoreInit = FALSE)
  


# Update input data -------------------------------------------------------
  observe({
    
    selected_region <- selected_region()
    selected_nvc_types <- selectNVCtypes()
    
    if(selected_region == "gbnvc"){
      
      # Establish setup data which doesn't vary based on NVC type
      species_names_selected <- RMAVIS::accepted_taxa[["taxon_name"]]
      accepted_species_selected <- RMAVIS::accepted_taxa
      example_data_selected <- RMAVIS::example_data
      example_data_options_selected <- RMAVIS:::example_data_options
      ft_taxon_name_col_selected <- "nvc_taxon_name"
      psq_taxon_name_col_selected <- "nvc_taxon_name"
      unit_name_col_selected <- "nvc_code"
      hab_rest_pref_selected <- RMAVIS:::habitatRestrictionPrefixes
      
      # Compose setup data from selected NVC types
      floristic_tables_selected <- tibble::tibble()
      community_attributes_selected <- tibble::tibble()
      pquads_selected <- tibble::tibble()
      psquad_cm_he_selected <- tibble::tibble()
      comm_he_selected <- tibble::tibble()
      
      if("Original" %in% selected_nvc_types){
        
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
      
      if("Calthion" %in% selected_nvc_types){
        
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
      
      if("SOWG" %in% selected_nvc_types){
        
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
      
    } else if(selected_region == "mnnpc"){
      
      species_names_selected <- MNNPC::mnnpc_accepted_taxa[["taxon_name"]]
      accepted_species_selected <- MNNPC::mnnpc_accepted_taxa
      example_data_selected <- MNNPC::mnnpc_example_data
      floristic_tables_selected <- MNNPC::mnnpc_floristic_tables
      community_attributes_selected <- MNNPC::mnnpc_community_attributes
      pquads_selected <- MNNPC::mnnpc_pquads
      psquad_cm_he_selected <- NULL
      comm_he_selected <- NULL
      example_data_options_selected <- MNNPC:::example_data_options
      ft_taxon_name_col_selected <- "npc_taxon_name"
      psq_taxon_name_col_selected <- "taxon_name"
      unit_name_col_selected <- "npc_code"
      hab_rest_pref_selected <- MNNPC::mnnpc_vc_types
      
    }
    
    # Compose final setup data
    setupData_list <- list(
      "species_names" = species_names_selected,
      "accepted_species" = accepted_species_selected,
      "example_data" = example_data_selected,
      "floristic_tables" = floristic_tables_selected,
      "community_attributes" = community_attributes_selected,
      "pquads" = pquads_selected,
      "psquad_cm_he" = psquad_cm_he_selected,
      "comm_cm_he" = comm_he_selected,
      "example_data_options" = example_data_options_selected,
      "ft_taxon_name_col" = ft_taxon_name_col_selected,
      "psq_taxon_name_col" = psq_taxon_name_col_selected,
      "unit_name_col" = unit_name_col_selected,
      "hab_rest_pref" = hab_rest_pref_selected
    )
    
    setupData(setupData_list)
    
  }) |>
    bindEvent(selected_region(),
              selectNVCtypes(),
              ignoreNULL = TRUE,
              ignoreInit = FALSE)
  
# Return Setup Data -------------------------------------------------------
  return(setupData)
  
}
