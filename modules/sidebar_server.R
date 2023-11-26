sidebar <- function(input, output, session, nvcAverageSim) {
  
  ns <- session$ns
  
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "inputMethod" = input$inputMethod,
      # "resetTable" = input$resetTable,
      "exampleData" = input$exampleData,
      "dataEntryFormat" = input$dataEntryFormat,
      "runAnalysis" = input$runAnalysis,
      "coverMethod" = input$coverMethod,
      "habitatRestriction" = input$habitatRestriction,
      "nTopResults" = input$nTopResults,
      "groupMethod" = input$groupMethod,
      "habCorClass" = input$habCorClass,
      "nvcFloristicTable" = input$nvcFloristicTable,
      "crossTabulate" = input$crossTabulate,
      "restrictNVCFlorTablesOpts" = input$restrictNVCFlorTablesOpts
    )
    
    sidebar_options(sidebar_options_list)
    
  }) |>
    bindEvent(input$inputMethod, 
              # input$resetTable, 
              input$exampleData, 
              input$dataEntryFormat, input$runAnalysis, input$coverMethod, 
              input$habitatRestriction, input$nTopResults, input$groupMethod,
              input$habCorClass, input$nvcFloristicTable, input$crossTabulate,
              input$restrictNVCFlorTablesOpts,
              ignoreInit = TRUE)
  
  observe({
    if (input$inputMethod == "manual") {
      shinyjs::hide(id = "exampleData")
      shinyjs::hide(id = "dataEntryFormat")
      shinyjs::hide(id = "uploadData")
    } else if (input$inputMethod == "example") {
      shinyjs::show(id = "exampleData")
      shinyjs::hide(id = "dataEntryFormat")
      shinyjs::hide(id = "uploadData")
    } else if (input$inputMethod == "upload") {
      shinyjs::hide(id = "exampleData")
      shinyjs::show(id = "dataEntryFormat")
      shinyjs::show(id = "uploadData")
    }
  }) |>
    bindEvent(input$inputMethod, ignoreInit = FALSE)
  
  observe({
    
    fitted_nvcs <- nvcAverageSim()$NVC.Code |> unique()

    if(input$restrictNVCFlorTablesOpts == TRUE){

      shiny::updateSelectizeInput(
        session = session,
        inputId = "nvcFloristicTable",
        choices = fitted_nvcs,
        selected = fitted_nvcs[1],
        server = TRUE
      )

    } else if(input$restrictNVCFlorTablesOpts == FALSE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "nvcFloristicTable",
        choices = nvc_community_codes,
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
