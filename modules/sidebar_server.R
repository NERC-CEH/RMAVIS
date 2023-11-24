sidebar <- function(input, output, session) {
  
  ns <- session$ns
  
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options(
      list(
        "dataEntryFormat" = input$dataEntryFormat,
        "runAnalysis" = input$runAnalysis,
        "coverMethod" = input$coverMethod, 
        "habitatRestriction" = input$habitatRestriction,
        "nTopResults" = input$nTopResults,
        "habCorClass" = input$habCorClass
      )
    )
    
  }) |>
    bindEvent(input$dataEntryFormat, input$runAnalysis, input$coverMethod, 
              input$habitatRestriction, input$nTopResults, input$habCorClass,
              ignoreInit = TRUE)
  
  return(sidebar_options)
  
}
