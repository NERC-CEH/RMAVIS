sidebar <- function(input, output, session, nvcAverageSim) {
  
  ns <- session$ns
  
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "exampleData" = input$exampleData,
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
    
    sidebar_options(sidebar_options_list)
    
  }) |>
    bindEvent(input$exampleData, input$dataEntryFormat, input$runAnalysis, 
              input$coverMethod, 
              input$habitatRestriction, input$nTopResults, input$groupSample,
              input$habCorClass, input$nvcFloristicTable, input$crossTabulate,
              input$restrictNVCFlorTablesOpts,
              ignoreInit = TRUE)
  
  observe({

    if(input$restrictNVCFlorTablesOpts == TRUE){

      fitted_nvcs <- nvcAverageSim()$NVC.Code |> unique()

      print(fitted_nvcs)

      shiny::updateSelectizeInput(
        inputId = ns("nvcFloristicTable"),
        choices = fitted_nvcs,
        selected = fitted_nvcs[1],
        server = TRUE
      )

    }

  }) |>
    bindEvent(input$restrictNVCFlorTablesOpts,
              nvcAverageSim(),
              ignoreInit = TRUE)
  
  return(sidebar_options)
  
}
