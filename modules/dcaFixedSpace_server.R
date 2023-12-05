dcaFixedSpace <- function(input, output, session, surveyTable, nvcAverageSim, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaVars <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaVars(sidebar_options()$dcaVars)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  dcaFixedSpaceResults <- reactiveVal()
  
  observe({
    
    # Require selected objects are not NULL
    shiny::req(surveyTable())
    shiny::req(nvcAverageSim())
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Performing DCA Analysis"
    )
    
    # Peform analysis in a reactive context without creating a reactive relationship
    shiny::isolate({
      
      # Get all NVC communities and sub-communities from nvc assignment results
      NVC_communities_all <- nvcAverageSim() |>
        dplyr::pull(NVC.Code)
      
      # Get all NVC communities from community and sub-community codes
      NVC_communities_fromSubCom <- stringr::str_replace(string = NVC_communities_all, 
                                                         pattern = "(\\d)[^0-9]+$", 
                                                         replace = "\\1") |>
        unique()
      
      NVC_communities_final <- unique(c(NVC_communities_all, NVC_communities_fromSubCom))
      
      
      # Create pattern to subset matrix rows
      codes_regex <- c()
      
      for(code in NVC_communities_final){
        
        regex <- paste0("(", code, ")(?<=)P")
        
        codes_regex <- c(codes_regex, regex)
        
        codes_regex <- stringr::str_c(codes_regex, collapse = "|")
        
      }
      
      # Subset pseudo-quadrats for selected communities
      selected_pquads <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide), pattern = codes_regex), ]
      
      # Remove columns (species) that are absent in all selected communities
      selected_pquads_prepped <- selected_pquads[, colSums(abs(selected_pquads)) != 0] |>
        as.data.frame()
      
      # Perform a DCA on the selected pseudo-quadrats
      selected_pquads_dca_results <- vegan::decorana(veg = selected_pquads_prepped)
      
      # Extract the DCA results species axis scores
      selected_pquads_dca_results_species <- selected_pquads_dca_results$cproj |>
        tibble::as_tibble(rownames = "Species")
      
      # Extract the DCA results quadrat axis scores
      selected_pquads_dca_results_quadrats <- selected_pquads_dca_results$rproj |>
        tibble::as_tibble(rownames = "Quadrat")
      
      # Prepare the pseudo-quadrat DCA results quadrat axis scores
      selected_pquads_dca_results_quadrats_final <- selected_pquads_dca_results_quadrats  |>
        dplyr::mutate("Year" = "Reference", .before  = "Quadrat") |>
        dplyr::mutate("Group" = "Reference", .before  = "Quadrat") |>
        dplyr::mutate("NVC.Comm" = stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)"), .before  = "Quadrat")
      
    
      # Calculate the surveyTable DCA results using the pseudo-quadrat species scores
      surveyTable_dca_results_quadrats <- surveyTable() |>
        tibble::as_tibble() |>
        dplyr::filter(!is.na(Cover)) |>
        dplyr::select(-Cover) |>
        dplyr::left_join(selected_pquads_dca_results_species, by = "Species") |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE) |>
        dplyr::group_by(ID, Year, Group, Quadrat) |>
        dplyr::summarise("DCA1" = mean(DCA1, na.rm = TRUE),
                         "DCA2" = mean(DCA2, na.rm = TRUE),
                         "DCA3" = mean(DCA3, na.rm = TRUE),
                         "DCA4" = mean(DCA4, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::mutate("NVC.Comm" = "Sample", .before  = "Quadrat")
      
      # print(surveyTable_dca_results_quadrats)
      
      # Create convex hulls around the pseudo-quadrat DCA points.
      selected_pquads_dca_results_quadrats_final_hull <- selected_pquads_dca_results_quadrats_final |>
        dplyr::group_by(NVC.Comm) |>
        dplyr::slice(grDevices::chull(DCA1, DCA2))
      
      # Prepare the data required to draw arrows between points, ordered by Year
      
      if(length(unique(surveyTable_dca_results_quadrats$Year)) > 1){
        
        arrow_plot_data <- surveyTable_dca_results_quadrats |>
          dplyr::arrange(Year) |>
          dplyr::select("Year" = Year, 
                        # "Group" = Group,
                        "Quadrat" = Quadrat, 
                        "x" = DCA1, 
                        "y" = DCA2) |>
          dplyr::group_by(Quadrat) |>
          dplyr::mutate("endX" = dplyr::lead(x),
                        "endY" = dplyr::lead(y)) |>
          dplyr::filter(!is.na(endX)) |>
          dplyr::ungroup()
        
      } else {
        
        arrow_plot_data <- NULL
        
      }
      
    }) # close isolate
    
    # Create an interactive plot of the DCA results
    output$dcaFixedSpacePlot <- plotly::renderPlotly({
      
      # Create ggplot2 plot
      dcaFixedSpacePlot_plot <- ggplot2::ggplot() +
        {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = selected_pquads_dca_results_quadrats_final_hull, alpha = 0.2, 
                                                                  mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Comm))} +
        {if("species" %in% dcaVars())ggplot2::geom_point(data = selected_pquads_dca_results_species,
                                                         color = '#32a87d',
                                                         shape = 18,
                                                         mapping = ggplot2::aes(x = DCA1, 
                                                                                y = DCA2,
                                                                                Species = Species))} +
        {if("pseudoQuadrats" %in% dcaVars())ggplot2::geom_point(data = selected_pquads_dca_results_quadrats_final,
                                                                mapping = ggplot2::aes(color = NVC.Comm,
                                                                                       Quadrat = Quadrat,
                                                                                       x = DCA1,
                                                                                       y = DCA2))} +
        {if("surveyQuadrats" %in% dcaVars())ggplot2::geom_point(data = surveyTable_dca_results_quadrats,
                                                                color = 'black',
                                                                mapping = ggplot2::aes(Year = Year,
                                                                                       # Group = Group,
                                                                                       Quadrat = Quadrat,
                                                                                       x = DCA1,
                                                                                       y = DCA2))} +
        ggplot2::theme_minimal()
      
      if("surveyQuadratChange" %in% dcaVars() & !is.null(arrow_plot_data)){
        
        if(nrow(arrow_plot_data) > 0){
          
          dcaFixedSpacePlot_plotly <- plotly::ggplotly(p = dcaFixedSpacePlot_plot) |>
            plotly::add_annotations(data = arrow_plot_data,
                                    showarrow = TRUE,
                                    text = "",
                                    xref = "x", axref = "x",
                                    yref = "y", ayref = "y",
                                    x = ~endX,
                                    ax = ~x,
                                    y = ~endY,
                                    ay = ~y)
          
        }
        
      } else {
        
        dcaFixedSpacePlot_plotly <- plotly::ggplotly(p = dcaFixedSpacePlot_plot)
      
      }
      
      
      return(dcaFixedSpacePlot_plotly)  
      
    })
    
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()
    
    
    # Compose list of DCA results objects
    dcaFixedSpaceResults_list <- list("selected_pquads_dca_results_species_final" = selected_pquads_dca_results_species,
                                      "selected_pquads_dca_results_quadrats_final" = selected_pquads_dca_results_quadrats_final,
                                      "surveyTable_dca_results_quadrats" = surveyTable_dca_results_quadrats,
                                      "selected_pquads_dca_results_quadrats_final_hull" = selected_pquads_dca_results_quadrats_final_hull,
                                      "arrow_plot_data" = arrow_plot_data)
    
    dcaFixedSpaceResults(dcaFixedSpaceResults_list)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  # Return list of DCA results objects
  return(dcaFixedSpaceResults)
  
}
