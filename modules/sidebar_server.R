sidebar <- function(input, output, session, nvcAverageSim) {
  
  ns <- session$ns
  
  sidebar_options <- reactiveVal()
  
  observe({
    # sidebar_options <- list(
    #   "exampleData" = reactiveVal(input$exampleData),
    #   "dataEntryFormat" = reactiveVal(input$dataEntryFormat),
    #   "runAnalysis" = reactiveVal(input$runAnalysis),
    #   "coverMethod" = reactiveVal(input$coverMethod),
    #   "habitatRestriction" = reactiveVal(input$habitatRestriction),
    #   "nTopResults" = reactiveVal(input$nTopResults),
    #   "groupSample" = reactiveVal(input$groupSample),
    #   "habCorClass" = reactiveVal(input$habCorClass),
    #   "nvcFloristicTable" = reactiveVal(input$nvcFloristicTable),
    #   "crossTabulate" = reactiveVal(input$crossTabulate),
    #   "restrictNVCFlorTablesOpts" = reactiveVal(input$restrictNVCFlorTablesOpts)
    # )
    
    # sidebar_options <- list(
    #   "exampleData" = reactiveVal({input$exampleData}),
    #   "dataEntryFormat" = reactiveVal({input$dataEntryFormat}),
    #   "runAnalysis" = reactiveVal({input$runAnalysis}),
    #   "coverMethod" = reactiveVal({input$coverMethod}),
    #   "habitatRestriction" = reactiveVal({input$habitatRestriction}),
    #   "nTopResults" = reactiveVal({input$nTopResults}),
    #   "groupSample" = reactiveVal({input$groupSample}),
    #   "habCorClass" = reactiveVal({input$habCorClass}),
    #   "nvcFloristicTable" = reactiveVal({input$nvcFloristicTable}),
    #   "crossTabulate" = reactiveVal({input$crossTabulate}),
    #   "restrictNVCFlorTablesOpts" = reactiveVal({input$restrictNVCFlorTablesOpts})
    # )
    
    # sidebar_options <- list(
    #   "exampleData" = reactive({input$exampleData}),
    #   "dataEntryFormat" = reactive({input$dataEntryFormat}),
    #   "runAnalysis" = reactive({input$runAnalysis}),
    #   "coverMethod" = reactive({input$coverMethod}), 
    #   "habitatRestriction" = reactive({input$habitatRestriction}),
    #   "nTopResults" = reactive({input$nTopResults}),
    #   "groupSample" = reactive({input$groupSample}),
    #   "habCorClass" = reactive({input$habCorClass}),
    #   "nvcFloristicTable" = reactive({input$nvcFloristicTable}),
    #   "crossTabulate" = reactive({input$crossTabulate}),
    #   "restrictNVCFlorTablesOpts" = reactive({input$restrictNVCFlorTablesOpts})
    # )
    
    # sidebar_options <- list(
    #   "exampleData" = reactive(input$exampleData),
    #   "dataEntryFormat" = reactive(input$dataEntryFormat),
    #   "runAnalysis" = reactive(input$runAnalysis),
    #   "coverMethod" = reactive(input$coverMethod),
    #   "habitatRestriction" = reactive(input$habitatRestriction),
    #   "nTopResults" = reactive(input$nTopResults),
    #   "groupSample" = reactive(input$groupSample),
    #   "habCorClass" = reactive(input$habCorClass),
    #   "nvcFloristicTable" = reactive(input$nvcFloristicTable),
    #   "crossTabulate" = reactive(input$crossTabulate),
    #   "restrictNVCFlorTablesOpts" = reactive(input$restrictNVCFlorTablesOpts)
    # )
    
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
    
    # print(sidebar_options)
    
  }) |>
    bindEvent(input$exampleData, input$dataEntryFormat, input$runAnalysis, 
              input$coverMethod, 
              input$habitatRestriction, input$nTopResults, input$groupSample,
              input$habCorClass, input$nvcFloristicTable, input$crossTabulate,
              input$restrictNVCFlorTablesOpts,
              ignoreInit = TRUE)
  
  # observe({
  #   
  #   if(input$restrictNVCFlorTablesOpts == TRUE){
  #     
  #     fitted_nvcs <- nvcAverageSim()$NVC.Code |> unique()
  #     
  #     print(fitted_nvcs)
  #     
  #     shiny::updateSelectizeInput(
  #       session = session,
  #       inputId = ns("nvcFloristicTable"),
  #       choices = fitted_nvcs,
  #       selected = fitted_nvcs[1],
  #       server = FALSE
  #     )
  #     
  #   }
  #   
  # }) |>
  #   bindEvent(input$restrictNVCFlorTablesOpts,
  #             nvcAverageSim(),
  #             ignoreInit = TRUE)
  
  
  
  return(sidebar_options)
  
}
