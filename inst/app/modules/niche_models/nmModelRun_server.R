nmModelRun <- function(input, output, session, sidebar_nm_options, nmDataInput) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runNMAnalysis <- reactiveVal()
  focalSpecies <- reactiveVal()
  
  observe({
    runNMAnalysis(sidebar_nm_options()$runNMAnalysis)
    focalSpecies(sidebar_nm_options()$focalSpecies)
    
  }) |>
    bindEvent(sidebar_nm_options(),
              ignoreInit = TRUE)
  

  # Retrieve Predictor Data -------------------------------------------------
  predictorData_rval <- reactiveVal(predictors <- tibble::tribble(
    ~id, ~ph, ~fert, ~wet, ~cov4, ~mju, ~mja, ~prec,
    "a", 4, 1.5, 2, 2, 19.16, 1.74, 605.66,
    "b", 7, 2, 2, 1, 17.4, -1.26, 1056.49
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
    focalSpecies <- "NBNSYS0000004288"
    
    models <- targets::tar_read(name = "WeightedEnsembleModel", store = tar_store)
    explainers <- targets::tar_read(name = "WeightedEnsembleDALEXExplainer", store = tar_store)
    
    model <- retrieve_nested_element(nested_list = models, 
                                     focal_species = focalSpecies)
    
    explainer <- retrieve_nested_element(nested_list = explainers, 
                                         focal_species = focalSpecies)
    
    selectedModel_rval(model)
    selectedExplainer_rval(explainer)
    

  }) |>
    bindEvent(focalSpecies(),
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
    predictorData <- predictorData_rval()
    
    results <- predict(model_species, newdata = predictorData, predict_type = "prob") |>
      tibble::as_tibble() |>
      dplyr::bind_cols(dplyr::select(predictors, id)) |>
      dplyr::select("id" = "id",
                    "Prob.Presence" = "1",
                    "Prob.Absence" = "0")
    
    # Model Breakdown
    selectedExplainer <- selectedExplainer_rval()
    
    predictorData <- predictorData_rval() |>
      dplyr::slice(1)
    
    profile <- DALEX::predict_parts(explainer = selectedExplainer,
                                    new_observation = predictorData)
    
    modelPredBreakdown_rval(profile)
    
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