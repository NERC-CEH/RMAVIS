calcAvgEIVs <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns

# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  

# Cover-weighted mean Hill-Ellenberg values -------------------------------
  avgWeightedEIVsTable_init <- data.frame("Year" = integer(),
                                          "Group" = character(),
                                          "Quadrat" = character(),
                                          "Moisture.F" = double(),
                                          "Light.L" = double(),
                                          "Nitrogen.N" = double(),
                                          "Reaction.R" = double(),
                                          "Salinity.S" = double()
                                          )
  
  avgWeightedEIVsTable_rval <- reactiveVal(avgWeightedEIVsTable_init)
  
  output$avgWeightedEIVsTable <- rhandsontable::renderRHandsontable({
    
    avgWeightedEIVsTable <- rhandsontable::rhandsontable(data = avgWeightedEIVsTable_init,
                                                         rowHeaders = NULL,
                                                         width = "100%"#,
                                                         # overflow = "visible",
                                                         # stretchH = "all"
                                                         ) |>
      rhandsontable::hot_col(col = colnames(avgWeightedEIVsTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"eivs_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(avgWeightedEIVsTable)
    
  })
  
  observe({
    
    # shiny::req(input$avgEIVsTable) # Why do I not need this???????????????
    shiny::req(surveyTable())
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      surveyTable <- surveyTable()
      
      avgWeightedEIVsTable <- surveyTable |>
        dplyr::rename("species" = "Species") |>
        dplyr::left_join(master_data, by = "species",
                         relationship = "many-to-many") |>
        dplyr::select(Year, Group, Quadrat, Cover, `F`, L, N, R, S) |>
        dplyr::mutate("F" = `F` * Cover,
                      "L" = L * Cover, 
                      "N" = N * Cover, 
                      "R" = R * Cover, 
                      "S" = S * Cover) |>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::summarise("Moisture.F" = sum(`F`, na.rm = TRUE),
                         "Light.L" = sum(L, na.rm = TRUE), 
                         "Nitrogen.N" = sum(N, na.rm = TRUE), 
                         "Reaction.R" = sum(R, na.rm = TRUE), 
                         "Salinity.S" = sum(S, na.rm = TRUE), .groups = "drop") |>
        dplyr::ungroup() |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE)
        
      
    })
    
    output$avgWeightedEIVsTable <- rhandsontable::renderRHandsontable({
      
      avgWeightedEIVsTable <- rhandsontable::rhandsontable(data = avgWeightedEIVsTable,
                                                           rowHeaders = NULL,
                                                           width = "100%"#,
                                                           # overflow = "visible",
                                                           # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(avgWeightedEIVsTable), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"eivs_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(avgWeightedEIVsTable)
      
    })
    
    avgWeightedEIVsTable_rval(rhandsontable::hot_to_r(input$avgWeightedEIVsTable))
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  

# Unweighted mean Hill-Ellenberg values -------------------------------
  avgUnweightedEIVsTable_init <- data.frame("Year" = integer(),
                                            "Group" = character(),
                                            "Quadrat" = character(),
                                            "Moisture.F" = double(),
                                            "Light.L" = double(),
                                            "Nitrogen.N" = double(),
                                            "Reaction.R" = double(),
                                            "Salinity.S" = double()
  )
  
  avgUnweightedEIVsTable_rval <- reactiveVal(avgUnweightedEIVsTable_init)
  
  output$avgUnweightedEIVsTable <- rhandsontable::renderRHandsontable({
    
    avgUnweightedEIVsTable <- rhandsontable::rhandsontable(data = avgUnweightedEIVsTable_init,
                                                           rowHeaders = NULL,
                                                           width = "100%"#,
                                                           # overflow = "visible",
                                                           # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(avgUnweightedEIVsTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"eivs_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(avgUnweightedEIVsTable)
    
  })
  
  observe({
    
    # shiny::req(input$avgEIVsTable) # Why do I not need this???????????????
    shiny::req(surveyTable())
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      surveyTable <- surveyTable()
      
      avgUnweightedEIVsTable <- surveyTable |>
        dplyr::rename("species" = "Species") |>
        dplyr::left_join(master_data, by = "species",
                         relationship = "many-to-many") |>
        dplyr::select(Year, Group, Quadrat, Cover, `F`, L, N, R, S) |>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::summarise("Moisture.F" = mean(`F`, na.rm = TRUE),
                         "Light.L" = mean(L, na.rm = TRUE), 
                         "Nitrogen.N" = mean(N, na.rm = TRUE), 
                         "Reaction.R" = mean(R, na.rm = TRUE), 
                         "Salinity.S" = mean(S, na.rm = TRUE), .groups = "drop") |>
        dplyr::ungroup() |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE)
      
      
    })
    
    output$avgUnweightedEIVsTable <- rhandsontable::renderRHandsontable({
      
      avgUnweightedEIVsTable <- rhandsontable::rhandsontable(data = avgUnweightedEIVsTable,
                                                             rowHeaders = NULL,
                                                             width = "100%"#,
                                                             # overflow = "visible",
                                                             # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(avgUnweightedEIVsTable), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"eivs_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(avgUnweightedEIVsTable)
      
    })
    
    # assign(x = "avgUnweightedEIVsTable", value = avgUnweightedEIVsTable, envir = .GlobalEnv)
    
    avgUnweightedEIVsTable_rval(avgUnweightedEIVsTable)
    
    # print(avgUnweightedEIVsTable())
    # print(avgUnweightedEIVsTable)
    
    # print(avgUnweightedEIVsTable_rval)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  
  outputOptions(output, "avgWeightedEIVsTable", suspendWhenHidden = FALSE)
  outputOptions(output, "avgUnweightedEIVsTable", suspendWhenHidden = FALSE)
  
  return(avgUnweightedEIVsTable_rval)
  
}
