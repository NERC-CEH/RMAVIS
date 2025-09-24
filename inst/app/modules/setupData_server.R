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
    "psquad_cm_he" = RMAVIS::nvc_psquad_cm_he
  )
  
  region <- reactiveVal("gbnvc")
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
    
    region(region())
    
  }) |>
    shiny::bindEvent(region(),
                     ignoreInit = FALSE)
  


# Update input data -------------------------------------------------------
  observe({
    
    region <- region()
    selected_nvc_types <- selectNVCtypes()
    
    if(region == "gbnvc"){
      
      # Establish setup data which doesn't change at present
      species_names_selected <- RMAVIS::accepted_taxa[["taxon_name"]]
      accepted_species_selected <- RMAVIS::accepted_taxa
      example_data_selected <- RMAVIS::example_data
      
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
      
    } else if(region == "mnnpc"){
      
      # species_names_selected <- MNNPC::
      # accepted_species_selected <- MNNPC::
      # example_data_selected <- MNNPC::
      # floristic_tables_selected <- MNNPC::
      # community_attributes_selected <- MNNPC::
      # pquads_selected <- MNNPC::
      # psquad_cm_he_selected <- NULL
      # comm_he_selected <- NULL
      
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
      "comm_cm_he" = comm_he_selected
    )
    
    setupData(setupData_list)
    
  }) |>
    bindEvent(region(),
              selectNVCtypes(),
              ignoreNULL = TRUE,
              ignoreInit = TRUE)
  
# Return Setup Data -------------------------------------------------------
  return(setupData)
  
}
