nmModelDisplay <- function(input, output, session, sidebar_nm_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  focalSpecies <- reactiveVal()
  selectedModelDisplay <- reactiveVal()
  selectedVariablesDisplay <- reactiveVal()

  observe({

    focalSpecies(sidebar_nm_options()$focalSpecies)
    selectedModelDisplay(sidebar_nm_options()$selectedModelDisplay)
    selectedVariablesDisplay(sidebar_nm_options()$selectedVariablesDisplay)

  }) |>
    bindEvent(sidebar_nm_options(),
              ignoreInit = TRUE)
  
  
  # Retrieve Data -----------------------------------------------------------
  measures_rval <- reactiveVal()
  aleData_rval <- reactiveVal()
  featureImportance_rval <- reactiveVal()
  
  observe({
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Retrieving ENM Model Data"
    )
    
    focalSpecies <- focalSpecies()
    selectedModelDisplay <- selectedModelDisplay()
    selectedVariablesDisplay <- c("_full_model_", "_baseline_", selectedVariablesDisplay())
    
    measures <- targets::tar_read(name = "AllMeasures", store = tar_store) |>
      dplyr::filter(species == focalSpecies) |>
      dplyr::filter(model %in% selectedModelDisplay)
    
    aleData <- targets::tar_read(name = "AllALEData", store = tar_store) |>
      dplyr::filter(species == focalSpecies) |>
      dplyr::filter(model %in% selectedModelDisplay) |>
      dplyr::filter(variable %in% selectedVariablesDisplay)
    
    featureImportance <- targets::tar_read(name = "AllFeatureImportance", store = tar_store) |>
      dplyr::filter(species == focalSpecies) |>
      dplyr::filter(model %in% selectedModelDisplay) |>
      dplyr::filter(variable %in% selectedVariablesDisplay)
    
    assign(x = "featureImportance", value = featureImportance, envir = .GlobalEnv)
    
    measures_rval(measures)
    aleData_rval(aleData)
    featureImportance_rval(featureImportance)
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()

  }) |>
    bindEvent(focalSpecies(),
              selectedModelDisplay(),
              selectedVariablesDisplay(),
              ignoreInit = TRUE)
  

  # Model evaluation metrics ------------------------------------------------
  modelEvalMetricsTable_init <- data.frame("Model" = character(),
                                           "Binary Brier" = double(),
                                           "PRAUC" = double(),
                                           "Precision" = double(),
                                           "Sensitivity" = double(),
                                           "Specificity" = double(),
                                           "Balanced Accuracy" = double()
                                           )
  
  modelEvalMetricsTable_rval <- reactiveVal(modelEvalMetricsTable_init)
  
  output$modelEvalMetricsTable <- reactable::renderReactable({
    
    modelEvalMetricsTable <- reactable::reactable(data = modelEvalMetricsTable_init,
                                                  filterable = FALSE,
                                                  pagination = FALSE, 
                                                  highlight = TRUE,
                                                  bordered = TRUE,
                                                  sortable = TRUE, 
                                                  wrap = FALSE,
                                                  resizable = TRUE,
                                                  style = list(fontSize = "1rem"),
                                                  class = "my-tbl",
                                                  # style = list(fontSize = "1rem"),
                                                  rowClass = "my-row",
                                                  defaultColDef = reactable::colDef(
                                                    format = reactable::colFormat(digits = 3),
                                                    headerClass = "my-header",
                                                    class = "my-col",
                                                    align = "center" # Needed as alignment is not passing through to header
                                                  ))
    
    return(modelEvalMetricsTable)
    
  })
  
  observe({
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Rendering Performance Metrics Table"
    )
    
    measures <- measures_rval()
    
    modelEvalMetricsTable_data <- measures |>
      dplyr::mutate(
        "model_type" = dplyr::case_when(
          model %in% c("WE") ~ "WE",
          TRUE ~ "Individual"
        ),
        .before = model
      ) |>
      dplyr::arrange(model_type, dplyr::desc(bacc)) |>
      dplyr::select(-model_type) |>
      dplyr::select("Model" = "model",
                    "Binary Brier" = "bbrier",
                    # "Log Loss" = "logloss",
                    # "AUC" = "auc",
                    "PRAUC" = "prauc",
                    "Precision" = "precision",
                    # "Recall" = "recall",
                    "Sensitivity" = "sensitivity",
                    "Specificity" = "specificity",
                    # "Accuracy" = "acc",
                    "Balanced Accuracy" = "bacc"
      )
    
    output$modelEvalMetricsTable <- reactable::renderReactable({
      
      modelEvalMetricsTable <- reactable::reactable(data = modelEvalMetricsTable_data,
                                                    filterable = FALSE,
                                                    pagination = FALSE, 
                                                    highlight = TRUE,
                                                    bordered = TRUE,
                                                    sortable = TRUE, 
                                                    wrap = FALSE,
                                                    resizable = TRUE,
                                                    style = list(fontSize = "1rem"),
                                                    class = "my-tbl",
                                                    # style = list(fontSize = "1rem"),
                                                    rowClass = "my-row",
                                                    defaultColDef = reactable::colDef(
                                                      format = reactable::colFormat(digits = 3),
                                                      headerClass = "my-header",
                                                      class = "my-col",
                                                      align = "center" # Needed as alignment is not passing through to header
                                                      )
                                                    )
      
      return(modelEvalMetricsTable)
      
    })
    
    modelEvalMetricsTable_rval(modelEvalMetricsTable_data)
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(measures_rval())

  
  
  
  # Feature importance plot -------------------------------------------------
  observe({
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Rendering Feature Importance Plots"
    )
    
    featureImportance <- featureImportance_rval()
    
    output$feature_importance_plot <- plotly::renderPlotly({
      
      feature_importance_plot <- create_feature_importance_plot(fe_data = featureImportance)
      
      feature_importance_plotly <- plotly::ggplotly(feature_importance_plot)
      
      return(feature_importance_plotly)
      
    })
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()

  }) |>
    bindEvent(featureImportance_rval(),
              ignoreInit = FALSE)
  

  # ALE plot ----------------------------------------------------------------
  observe({
    
    # Start busy spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Rendering ALE Plot"
    )
    
    aleData <- aleData_rval()
    
    output$ale_plot <- plotly::renderPlotly({
      
      ale_plot <- create_ale_plot(ale_data = aleData)
      
      ale_plotly <- plotly::ggplotly(ale_plot)
      
      return(ale_plotly)
      
    })
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()
    
    
  }) |>
    bindEvent(aleData_rval(),
              ignoreInit = FALSE)
  
  
  # return(nmModelRunOutput_rval)
  
}