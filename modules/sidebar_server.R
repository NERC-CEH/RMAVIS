sidebar <- function(input, output, session, nvcAverageSim) {
  
  ns <- session$ns
  

# Compose list of inputs to return from module ----------------------------
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "inputMethod" = input$inputMethod,
      # "resetTable" = input$resetTable,
      "exampleData" = input$exampleData,
      "dataEntryFormat" = input$dataEntryFormat,
      "runAnalysis" = input$runAnalysis,
      # "coverMethod" = input$coverMethod,
      "habitatRestriction" = input$habitatRestriction,
      "nTopResults" = input$nTopResults,
      "groupMethod" = input$groupMethod,
      "habCorClass" = input$habCorClass,
      "composedFloristicTable" = input$composedFloristicTable,
      "nvcFloristicTable" = input$nvcFloristicTable,
      "crossTabulate" = input$crossTabulate,
      "restrictNVCFlorTablesOpts" = input$restrictNVCFlorTablesOpts#,
      # "generateReport" = input$generateReport
    )
    
    sidebar_options(sidebar_options_list)
    
  }) |>
    bindEvent(input$inputMethod, 
              # input$resetTable, 
              input$exampleData, 
              input$dataEntryFormat, input$runAnalysis, 
              # input$coverMethod, 
              input$habitatRestriction, input$nTopResults, input$groupMethod,
              input$habCorClass, input$composedFloristicTable,
              input$nvcFloristicTable, input$crossTabulate,
              input$restrictNVCFlorTablesOpts,
              # input$generateReport,
              ignoreInit = TRUE)
  

# Show/Hide inputMethod-related inputs ------------------------------------
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
  
  

# Reactively update nvcFloristicTable options -----------------------------
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


# Reactively update composedFloristicTable options ------------------------
  observe({
      
    if(nrow(nvcAverageSim()) != 0){
      
      uniq_IDs <- nvcAverageSim() |>
        dplyr::pull(ID) |>
        unique()
      
      names(uniq_IDs) <- uniq_IDs
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "composedFloristicTable",
        choices = uniq_IDs,
        selected = NULL,
        server = FALSE
      )
      
    }
    
  }) |>
    bindEvent(nvcAverageSim(),
              ignoreInit = TRUE)

  return(sidebar_options)
  
}



