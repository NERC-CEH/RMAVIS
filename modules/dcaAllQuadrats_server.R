dcaAllQuadrats <- function(input, output, session, surveyTable, nvcAverageSim, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  dcaVars <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    dcaVars(sidebar_options()$dcaVars)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  dcaAllQuadratsResults <- reactiveVal()
  
  observe({
    
    shiny::req(surveyTable())
    shiny::req(nvcAverageSim())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Performing DCA Analysis"
    )
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
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
      nvc_pquads_final_wide_trimmed <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide), pattern = codes_regex), ]
      
      # Remove columns (species) that are absent in all selected communities
      nvc_pquads_final_wide_prepped <- nvc_pquads_final_wide_trimmed[, colSums(abs(nvc_pquads_final_wide_trimmed)) != 0] |>
        as.data.frame()
      
      # Prepare survey data
      surveyTable_prepped <- surveyTable() |>
        # Convert cover estimates to presence/absence binary values
        dplyr::mutate(
          "Cover" = 
            dplyr::case_when(
              Cover > 0 ~ 1,
              is.na(Cover) ~ 0,
              TRUE ~ as.numeric(0)
            )
          ) |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE) |>
        dplyr::select(ID, Species, Cover)  |>
        dplyr::filter(!is.na(Cover)) |>
        tidyr::pivot_wider(id_cols = ID,
                           names_from = Species,
                           values_from = Cover) |>
        tibble::column_to_rownames(var = "ID")
    
      # Combine the pseudo-quadrats and survey data into a single matrix
      nvc_pquads_final_wide_prepped_wSurveyTable_prepped <- nvc_pquads_final_wide_prepped |>
        dplyr::bind_rows(surveyTable_prepped) |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix()
      
      # Perform a DCA on the combined pseudo-quadrat and survey data
      pquads_surveyTable_dca_results <- vegan::decorana(veg = nvc_pquads_final_wide_prepped_wSurveyTable_prepped)
      
      # Extract the DCA results species axis scores
      pquads_surveyTable_dca_results_species <- pquads_surveyTable_dca_results$cproj |>
        tibble::as_tibble(rownames = "Species")
      
      # Extract the DCA results quadrat axis scores
      pquads_surveyTable_dca_results_quadrats <- pquads_surveyTable_dca_results$rproj |>
        tibble::as_tibble(rownames = "Quadrat")
      
      pquads_surveyTable_dca_results_quadrats <- pquads_surveyTable_dca_results_quadrats |>
        dplyr::mutate(
          "Year" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ "Reference",
              TRUE ~ stringr::str_extract(string = Quadrat, pattern = "(\\d{4})")
            ),
          .before  = "Quadrat"
        ) |>
        dplyr::mutate(
          "NVC.Comm" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)"),
              TRUE ~ as.character("Sample")
            ),
          .before  = "Quadrat"
        ) |>
        dplyr::mutate(
          "Group" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) == TRUE ~ "Reference",
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) == FALSE ~ stringr::str_extract(string = Quadrat, pattern = "(?<=\\d{4}\\s-\\s)([[:alnum:]]*)"),
              TRUE ~ as.character("")
            ),
          .before  = "Quadrat"
        )
      
      
      pquads_surveyTable_dca_results_quadrats_pquads <- pquads_surveyTable_dca_results_quadrats |>
        dplyr::filter(NVC.Comm != "Sample")
        
      pquads_surveyTable_dca_results_quadrats_sample <- pquads_surveyTable_dca_results_quadrats |>
        dplyr::filter(NVC.Comm == "Sample") |>
        dplyr::mutate("ID" = Quadrat, .before = "Year") |>
        dplyr::mutate("Quadrat" = stringr::str_extract(string = Quadrat, pattern = "[[:alnum:]]*$"))
        
        
      
      
      # Create convex hulls around the pseudo-quadrat DCA points.
      pquads_surveyTable_dca_results_quadrats_hull <- pquads_surveyTable_dca_results_quadrats_pquads |>
        dplyr::group_by(NVC.Comm) |>
        dplyr::slice(grDevices::chull(DCA1, DCA2))
      
      # Prepare the data required to draw arrows between points, ordered by Year
      if(length(unique(pquads_surveyTable_dca_results_quadrats_sample$Year)) > 1){
        
        arrow_plot_data <- pquads_surveyTable_dca_results_quadrats_sample |>
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
      
    }) # Close isolate
    
    output$dcaAllQuadratsPlot <- plotly::renderPlotly({
      
      suppressWarnings(
        
        dcaAllQuadratsPlot_plot <- ggplot2::ggplot() +
          {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = pquads_surveyTable_dca_results_quadrats_hull, alpha = 0.2, 
                                                                    mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Comm))} +
          {if("species" %in% dcaVars())ggplot2::geom_point(data = pquads_surveyTable_dca_results_species,
                                                           color = '#32a87d',
                                                           shape = 18,
                                                           mapping = ggplot2::aes(x = DCA1,
                                                                                  y = DCA2,
                                                                                  Species = Species))} +
          {if("pseudoQuadrats" %in% dcaVars())ggplot2::geom_point(data = pquads_surveyTable_dca_results_quadrats_pquads,
                                                                  mapping = ggplot2::aes(color = NVC.Comm,
                                                                                         Quadrat = Quadrat,
                                                                                         x = DCA1,
                                                                                         y = DCA2))} +
          {if("surveyQuadrats" %in% dcaVars())ggplot2::geom_point(data = pquads_surveyTable_dca_results_quadrats_sample,
                                                                  color = 'black',
                                                                  mapping = ggplot2::aes(Year = Year,
                                                                                         # Group = Group,
                                                                                         Quadrat = Quadrat,
                                                                                         x = DCA1,
                                                                                         y = DCA2))} +
          ggplot2::theme_minimal()
        
      )
      
      if("surveyQuadratChange" %in% dcaVars() & !is.null(arrow_plot_data)){
        
        if(nrow(arrow_plot_data) > 0){
          
          dcaAllQuadratsPlot_plotly <- plotly::ggplotly(p = dcaAllQuadratsPlot_plot) |>
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
        
        dcaAllQuadratsPlot_plotly <- plotly::ggplotly(p = dcaAllQuadratsPlot_plot)
      
      }
      
      
      return(dcaAllQuadratsPlot_plotly)  
      
    })
    
    shinybusy::remove_modal_spinner()
    
    # Compose list of DCA results objects
    dcaAllQuadratsResults_list <- list("pquads_surveyTable_dca_results_species" = pquads_surveyTable_dca_results_species,
                                       "pquads_surveyTable_dca_results_quadrats_sample" = pquads_surveyTable_dca_results_quadrats_sample,
                                       "pquads_surveyTable_dca_results_quadrats_pquads" = pquads_surveyTable_dca_results_quadrats_pquads,
                                       "pquads_surveyTable_dca_results_quadrats_hull" = pquads_surveyTable_dca_results_quadrats_hull,
                                       "arrow_plot_data" = arrow_plot_data)
    
    dcaAllQuadratsResults(dcaAllQuadratsResults_list)
    
  }) |>
    bindEvent(runAnalysis(),
              # dcaVars(),
              # surveyTablePrepped(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  return(dcaAllQuadratsResults)
  
}
