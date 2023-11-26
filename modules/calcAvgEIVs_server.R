calcAvgEIVs <- function(input, output, session, surveyTablePrepped, sidebar_options) {
  
  ns <- session$ns

# Retrieve sidebar options ------------------------------------------------
  habCorClass <- reactiveVal()
  
  observe({
    
    habCorClass(sidebar_options()$habCorClass)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
# Create initial habitat correspondance table -----------------------------
  avgEIVsTable_init <- data.frame("ID" = character(),
                                  "Hill-Ellenberg F" = character(),
                                  "Hill-Ellenberg L" = character(),
                                  "Hill-Ellenberg N" = character(),
                                  "Hill-Ellenberg R" = character(),
                                  "Hill-Ellenberg S" = character()
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
    
    req(input$avgEIVsTable)
    req(surveyTablePrepped())
    
    print(input$avgEIVsTable)
    
    print(surveyTablePrepped())
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      surveyTablePrepped <- surveyTablePrepped()
      
      # print(surveyTablePrepped)
      
      bsbiChecklistData_he <- bsbiChecklistData |>
        dplyr::filter(dataType %in% c("Hill-Ellenberg F", "Hill-Ellenberg L", "Hill-Ellenberg N", "Hill-Ellenberg R", "Hill-Ellenberg S")) |>
        dplyr::rename("Species" = "key")
      
      print(bsbiChecklistData_he)
      
      avgEIVsTable <- surveyTablePrepped |>
        dplyr::left_join(bsbiChecklistData_he, by = Species)
      
      # print(avgEIVsTable)
        
      
    })
    
    output$avgEIVsTable <- rhandsontable::renderRHandsontable({
      
      avgEIVsTable <- rhandsontable::rhandsontable(data = avgEIVsTable,
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
    
    avgEIVsTable_rval(avgEIVsTable)
    
  }) |>
    bindEvent(surveyTablePrepped(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  return(avgEIVsTable_rval)
  
}