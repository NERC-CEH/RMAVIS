ordinationAnalysis <- function(input, output, session, surveyTable, nvcAverageSim, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  dcaPSvsSamples_rval <- reactiveVal()
  
  observe({
    
    shiny::req(surveyTable())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Performing DCA Analysis"
    )
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      fitted_nvcs <- nvcAverageSim()$NVC.Code |> unique()
      
      # Subset communities
      nvc_pquads_final_wide_trimmed <- nvc_pquads_final_wide[stringr::str_detect(string = row.names(nvc_pquads_final_wide),
                                                                                 pattern = stringr::str_c(fitted_nvcs, collapse = "|")), ]
      # Remove columns (species) that are absent in all selected communities
      nvc_pquads_final_wide_prepped <- nvc_pquads_final_wide_trimmed[, colSums(abs(nvc_pquads_final_wide_trimmed)) != 0] |>
        as.data.frame()
      
      # Prepare survey data
      # surveyTable <- read.csv(file = "./data/bundled_data/example_data_long.csv")
      surveyTable <- surveyTable()
      
      surveyTable_prepped <- surveyTable |>
        # Convert cover estimates to presence/absence binary values
        dplyr::mutate(
          "Cover" = 
            dplyr::case_when(
              Cover > 0 ~ 1,
              is.na(Cover) ~ 0,
              TRUE ~ as.numeric(0)
            )
          ) |>
        tidyr::unite(col = "ID", c(Year, Site, Quadrat.Group, Quadrat), sep = " - ", remove = TRUE) |>
        dplyr::select(ID, Species, Cover)  |>
        dplyr::filter(!is.na(Cover)) |>
        tidyr::pivot_wider(id_cols = ID,
                           names_from = Species,
                           values_from = Cover) |>
        tibble::column_to_rownames(var = "ID")
      
      
      nvc_pquads_final_wide_prepped_wSurveyTable_prepped <- nvc_pquads_final_wide_prepped |>
        dplyr::bind_rows(surveyTable_prepped) |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix()
      
      dca_results <- vegan::decorana(veg = nvc_pquads_final_wide_prepped_wSurveyTable_prepped)
      
      dca_results_psquad_axisScores <- dca_results$rproj |>
        as.data.frame()|>
        tibble::rownames_to_column(var = "Quadrat")  |>
        dplyr::mutate(
          "NVC.Comm" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(fitted_nvcs, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = ".+?(?=P)"),
              TRUE ~ as.character("Sample")
              ),
          .before  = "Quadrat"
          ) |>
        dplyr::mutate(
          "NVC.Broad" =
            dplyr::case_when(
              stringr::str_detect(string = Quadrat, pattern = stringr::str_c(fitted_nvcs, collapse = "|")) ~ stringr::str_extract(string = Quadrat, pattern = "^([A-Z]*)"),
              TRUE ~ as.character("Sample")
            ),
          .before  = "NVC.Comm"
        )
      
      # print(dca_results_psquad_axisScores)
        
      
    })
    
    output$dcaPSvsSamples <- plotly::renderPlotly({ # shiny::renderPlot
      
      
      dcaPSvsSamples_plot <- ggplot2::ggplot(data = dca_results_psquad_axisScores) +
        ggplot2::geom_point(mapping = ggplot2::aes(color = NVC.Comm,
                                                   label2 = Quadrat,
                                                   x = DCA1, 
                                                   y = DCA2)) +
        ggplot2::theme_minimal()
      
      dcaPSvsSamples_plotly <- plotly::ggplotly(p = dcaPSvsSamples_plot)
      
      
      return(dcaPSvsSamples_plotly)  
      
    }#,
    # res = 300
    )
    
    
    # dcaPSvsSamples_rval(dcaPSvsSamples)
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              # surveyTablePrepped(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  # return(avgEIVsTable_rval)
  
}