sidebar <- function(input, output, session, nvcAverageSim) {
  
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
        "groupSample" = input$groupSample,
        "habCorClass" = input$habCorClass,
        "nvcFloristicTable" = input$nvcFloristicTable,
        "crossTabulate" = input$crossTabulate,
        "restrictNVCFlorTablesOpts" = input$restrictNVCFlorTablesOpts
      )
    )
    
  }) |>
    bindEvent(input$dataEntryFormat, input$runAnalysis, input$coverMethod, 
              input$habitatRestriction, input$nTopResults, input$groupSample,
              input$habCorClass, input$nvcFloristicTable, input$crossTabulate,
              input$restrictNVCFlorTablesOpts,
              ignoreInit = TRUE)
  
  observe({
    
    if(input$restrictNVCFlorTablesOpts == TRUE){
      
      fitted_nvcs <- nvcAverageSim()$NVC.Code |> unique()
      
      print(fitted_nvcs)
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = ns("nvcFloristicTable"),
        choices = fitted_nvcs,
        selected = fitted_nvcs[1],
        server = FALSE
      )
      
    }
    
  }) |>
    bindEvent(input$restrictNVCFlorTablesOpts,
              nvcAverageSim(),
              ignoreInit = TRUE)
  
  
  
  return(sidebar_options)
  
}
