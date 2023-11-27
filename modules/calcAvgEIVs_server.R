calcAvgEIVs <- function(input, output, session, surveyTablePrepped, sidebar_options) {
  
  ns <- session$ns

# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
# Create initial habitat correspondance table -----------------------------
  avgEIVsTable_init <- data.frame("ID" = character(),
                                  "Sample" = character(),
                                  "Moisture.F" = character(),
                                  "Light.L" = character(),
                                  "Nitrogen.N" = character(),
                                  "Reaction.R" = character(),
                                  "Salinity.S" = character()
                                  )
  
  avgEIVsTable_rval <- reactiveVal(avgEIVsTable_init)
  
  output$avgEIVsTable <- rhandsontable::renderRHandsontable({
    
    avgEIVsTable <- rhandsontable::rhandsontable(data = avgEIVsTable_init,
                                                 rowHeaders = NULL,
                                                 width = "100%"#,
                                                 # overflow = "visible",
                                                 # stretchH = "all"
                                                 ) |>
      rhandsontable::hot_col(col = colnames(avgEIVsTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"analysis_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(avgEIVsTable)
    
  })
  
  observe({
    
    # shiny::req(input$avgEIVsTable) # Why do I not need this???????????????
    shiny::req(surveyTablePrepped())
    
    # print(surveyTablePrepped())
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      surveyTablePrepped <- surveyTablePrepped()
      
      print(str(surveyTablePrepped))
      
      avgEIVsTable <- surveyTablePrepped |>
        dplyr::rename("preferredTaxon" = "Species") |>
        dplyr::left_join(master_data, by = "preferredTaxon") |>
        dplyr::select(ID, Sample, Cover, `F`, L, N, R, S) |>
        dplyr::mutate("F" = `F` * Cover,
                      "L" = L * Cover, 
                      "N" = N * Cover, 
                      "R" = R * Cover, 
                      "S" = S * Cover) |>
        dplyr::group_by(ID, Sample) |>
        dplyr::summarise("Moisture.F" = sum(`F`, na.rm = TRUE),
                         "Light.L" = sum(L, na.rm = TRUE), 
                         "Nitrogen.N" = sum(N, na.rm = TRUE), 
                         "Reaction.R" = sum(R, na.rm = TRUE), 
                         "Salinity.S" = sum(S, na.rm = TRUE))

      print(avgEIVsTable)
      
      # avgEIVsTable <- data.frame("ID" = as.character("Test"),
      #                            "Moisture.F" = as.character("Test"),
      #                            "Light.L" = as.character("Test"),
      #                            "Nitrogen.N" = as.character("Test"),
      #                            "Reaction.R" = as.character("Test"),
      #                            "Salinity.S" = as.character("Test")
      #                            )
        
      
    })
    
    output$avgEIVsTable <- rhandsontable::renderRHandsontable({
      
      avgEIVsTable <- rhandsontable::rhandsontable(data = avgEIVsTable,
                                                   rowHeaders = NULL,
                                                   width = "100%"#,
                                                   # overflow = "visible",
                                                   # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(avgEIVsTable), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"analysis_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(avgEIVsTable)
      
    })
    
    print(rhandsontable::hot_to_r(input$avgEIVsTable))
    
    avgEIVsTable_rval(rhandsontable::hot_to_r(input$avgEIVsTable))
    
  }) |>
    bindEvent(runAnalysis(),
              # surveyTablePrepped(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  return(avgEIVsTable_rval)
  
}