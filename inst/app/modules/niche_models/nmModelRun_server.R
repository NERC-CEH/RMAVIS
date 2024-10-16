nmModelRun <- function(input, output, session, sidebar_nm_options, nmDataInput) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runNMAnalysis <- reactiveVal()
  focalSpecies <- reactiveVal()
  identifyPredDrivers <- reactiveVal()
  selectedModelPredict <- reactiveVal()
  
  observe({
    runNMAnalysis(sidebar_nm_options()$runNMAnalysis)
    focalSpecies(sidebar_nm_options()$focalSpecies)
    identifyPredDrivers(sidebar_nm_options()$identifyPredDrivers)
    selectedModelPredict(sidebar_nm_options()$selectedModelPredict)
    
  }) |>
    bindEvent(sidebar_nm_options(),
              ignoreInit = TRUE)
  

  # Retrieve Predictor Data -------------------------------------------------
  predictorData_rval <- reactiveVal(predictors <- tibble::tribble(
    ~id,           ~`F`,   ~L,    ~N,    ~R,    ~S,     ~DG,    ~DS,   ~H,
    "nvc_1000897",  4,     7.2,   7,   6.4,   0,      0.278,  0.177,  0.200,
    "nvc_1000898",  3.86,  7.57,  2.71,  6.57,  0.571,  0.271,  0.219,  0.226
  ))
  
  # observe({
  #   
  #   predictorData_rval()
  #   
  # }) |>
  #   bindEvent(ignoreInit = TRUE)

  # Retrieve Model Data ----------------------------------------------------
  selectedModel_rval <- reactiveVal()
  selectedExplainer_rval <- reactiveVal()
  
  observe({
    
    focalSpecies <- focalSpecies()
    selectedModelPredict <- selectedModelPredict()
    
    models <- targets::tar_read(name = "GAMModels", store = tar_store)
    explainers <- targets::tar_read(name = "GAMDALEXExplainer", store = tar_store)
    
    model <- retrieve_nested_element(nested_list = models, 
                                     focal_species = focalSpecies)
    
    explainer <- retrieve_nested_element(nested_list = explainers, 
                                         focal_species = focalSpecies)
    
    selectedModel_rval(model)
    selectedExplainer_rval(explainer)
    

  }) |>
    bindEvent(focalSpecies(),
              selectedModelPredict(),
              ignoreInit = TRUE)
  
  
  # Run Model ---------------------------------------------------------------
  modelPredBreakdown_rval <- reactiveVal()
  
  observe({
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Generating ENM Predictions"
    )
    
    # Model Prediction
    isolate({
      predictorData <- predictorData_rval()
      selectedModel <- selectedModel_rval()
      selectedExplainer <- selectedExplainer_rval()
      identifyPredDrivers <- identifyPredDrivers()
    })
    
    results <- predict(selectedModel, newdata = predictorData, predict_type = "prob") |>
      tibble::as_tibble() |>
      dplyr::bind_cols(dplyr::select(predictors, id)) |>
      dplyr::select("id" = "id",
                    "Prob.Presence" = "Present",
                    "Prob.Absence" = "Absent")
    
    
    # Identify model prediction drivers
    if(isTRUE(identifyPredDrivers)){
      
      # Update busy spinner
      shinybusy::update_modal_spinner(
        text = "Identifying Drivers"
      )
      
      # Model Breakdown
      predictorData_1 <- predictorData |>
        dplyr::slice(1)
      
      profile <- DALEX::predict_parts(explainer = selectedExplainer,
                                      new_observation = predictorData)
      
      modelPredBreakdown_rval(profile)
      
    }
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()

  }) |>
    bindEvent(runNMAnalysis(),
              ignoreInit = TRUE)
  

  # Break Down Plot --------------------------------------------------------
  observe({
    
    modelPredBreakdown <- modelPredBreakdown_rval()
    
    output$modelPredBreakdown_plot <- plotly::renderPlotly({
      
      modelPredBreakdown_plot <- plot_break_down(x = modelPredBreakdown)
      
      modelPredBreakdown_plotly <- plotly::ggplotly(modelPredBreakdown_plot)
      
      return(modelPredBreakdown_plotly)
      
    })
    
  }) |>
    bindEvent(modelPredBreakdown_rval(),
              ignoreInit = TRUE)
  
  
  # return(nmModelRunOutput_rval)
  
}