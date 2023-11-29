calcAvgEIVs <- function(input, output, session, surveyTable, surveyTablePrepped, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  observe({
    
    
    
  })
  
}