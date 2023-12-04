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
    
      # Method 2
      nvc_pquads_final_wide_prepped_wSurveyTable_prepped <- nvc_pquads_final_wide_prepped |>
        dplyr::bind_rows(surveyTable_prepped) |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix()
      
      dca_results <- vegan::decorana(veg = nvc_pquads_final_wide_prepped_wSurveyTable_prepped)
      
      dca_results_psquad_axisScores <- dca_results$rproj |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "Quadrat") |>
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
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(NVC_communities_final, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = "^([A-Z]*)"),
              TRUE ~ as.character("Sample")
            ),
          .before  = "Quadrat"
        ) |>
        print()
      
      dca_results_psquad_axisScores_pquads <- dca_results_psquad_axisScores |>
        dplyr::filter(NVC.Comm != "Sample")
        
      dca_results_psquad_axisScores_sample <- dca_results_psquad_axisScores |>
        dplyr::filter(NVC.Comm == "Sample") |>
        print()
        
      
      
      # Create convex hulls around the pseudo-quadrat DCA points.
      selected_pquads_dca_results_quadrats_final_hull <- dca_results_psquad_axisScores_pquads |>
        dplyr::group_by(NVC.Comm) |>
        dplyr::slice(grDevices::chull(DCA1, DCA2))
      
      # Prepare the data required to draw arrows between points, ordered by Year
      arrow_plot_data <- dca_results_psquad_axisScores_sample |>
        dplyr::arrange(Quadrat) |>
        dplyr::select("Year" = Year, 
                      "Quadrat" = Quadrat, 
                      "x" = DCA1, 
                      "y" = DCA2) |>
        dplyr::mutate("endX" = dplyr::lead(x),
                      "endY" = dplyr::lead(y)) |>
        dplyr::filter(!is.na(endX)) |>
        print()
      
    }) # Close isolate
    
    output$dcaAllQuadratsPlot <- plotly::renderPlotly({
      
      dcaAllQuadratsPlot_plot <- ggplot2::ggplot() +
        {if("referenceSpace" %in% dcaVars())ggplot2::geom_polygon(data = selected_pquads_dca_results_quadrats_final_hull, alpha = 0.2, 
                                                                  mapping = ggplot2::aes(x = DCA1, y = DCA2, fill = NVC.Comm, colour = NVC.Comm))} +
        {if("pseudoQuadrats" %in% dcaVars())ggplot2::geom_point(data = dca_results_psquad_axisScores_pquads,
                                                                mapping = ggplot2::aes(color = NVC.Comm,
                                                                                       Quadrat = Quadrat,
                                                                                       x = DCA1,
                                                                                       y = DCA2))} +
        {if("surveyQuadrats" %in% dcaVars())ggplot2::geom_point(data = dca_results_psquad_axisScores_sample,
                                                                mapping = ggplot2::aes(color = NVC.Comm,
                                                                                       Quadrat = Quadrat,
                                                                                       x = DCA1,
                                                                                       y = DCA2))} +
        # {if("species" %in% dcaVars())ggplot2::geom_point(data = dca_results_m1_species,
        #                                                  mapping = ggplot2::aes(x = DCA1, 
        #                                                                         y = DCA2,
        #                                                                         Species = Species))} +
        # ggplot2::geom_path(data = method1_results1,
        #                    mapping = ggplot2::aes(x = DCA1,
        #                                           y = DCA2),
        #                    arrow = grid::arrow()) +
      ggplot2::theme_minimal()
      
      if("surveyQuadratChange" %in% dcaVars()){
        
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
        
      } else {
        
        dcaAllQuadratsPlot_plotly <- plotly::ggplotly(p = dcaAllQuadratsPlot_plot)
      
      }
      
      
      return(dcaAllQuadratsPlot_plotly)  
      
    })
    
    shinybusy::remove_modal_spinner()
    
    # Compose list of DCA results objects
    dcaAllQuadratsResults_list <- list(#"selected_pquads_dca_results_species_final" = selected_pquads_dca_results_species,
                                       "selected_pquads_dca_results_quadrats_final" = dca_results_psquad_axisScores_pquads,
                                       "surveyTable_dca_results_quadrats_final" = dca_results_psquad_axisScores_sample,
                                       "selected_pquads_dca_results_quadrats_final_hull" = selected_pquads_dca_results_quadrats_final_hull)
    
    dcaAllQuadratsResults(dcaAllQuadratsResults_list)
    
  }) |>
    bindEvent(runAnalysis(),
              # dcaVars(),
              # surveyTablePrepped(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  return(dcaAllQuadratsResults)
  
}
