heatmap <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  heatmap_rval <- reactiveVal()
  
  observe({
    
    shiny::req(surveyTable())
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      surveyTable <- surveyTable()
      
      surveyTable <- read.csv(file = "./data/bundled_data/example_data_long.csv")
      
      surveyTable_prepped <- surveyTable |>
        dplyr::select(Quadrat, Species, Cover)  |>
        dplyr::filter(!is.na(Cover)) |>
        tidyr::pivot_wider(id_cols = Quadrat,
                           names_from = Species,
                           values_from = Cover) |>
        tibble::column_to_rownames(var = "Quadrat") |>
        dplyr::mutate_all(~replace(., is.na(.), 0))
      
      # Transform abundances into integer values?
      
      # Perform DCA
      dca <- vegan::decorana(veg = surveyTable_prepped)
      
      # Transform abundances using a log scale to smooth out extremes
      surveyTable_transformed <- vegan::decostand(x = surveyTable_prepped, method = "log")
      
      # Plot transformed survey data, order by DCA results
      # heatmap <- vegan::tabasco(x = surveyTable_transformed, use = dca)
      
      
    })
    
    output$heatmap <- shiny::renderPlot({
      
      heatmap <- vegan::tabasco(x = surveyTable_transformed, use = dca)
      
      # heatmap <- ggplot2::ggplot(data = data.frame(surveyTable_prepped)) |>
      #   ggplot2::geom_tile(mapping = ggplot2::aes(x = site, y = species))
    
      
      return(heatmap)  
      
    }#,
    # res = 300
    )
    
    
    heatmap_rval(heatmap)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  # return(avgEIVsTable_rval)
  
}
