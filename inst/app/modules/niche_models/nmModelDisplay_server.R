nmModelDisplay <- function(input, output, session, sidebar_nm_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  focalSpecies <- reactiveVal()
  selectedModelDisplay <- reactiveVal()
  selectedVariablesDisplay <- reactiveVal()
  selectedMarginalEffectsPlot <- reactiveVal()

  observe({

    focalSpecies(sidebar_nm_options()$focalSpecies)
    selectedModelDisplay(sidebar_nm_options()$selectedModelDisplay)
    selectedVariablesDisplay(sidebar_nm_options()$selectedVariablesDisplay)
    selectedMarginalEffectsPlot(sidebar_nm_options()$selectedMarginalEffectsPlot)

  }) |>
    bindEvent(sidebar_nm_options(),
              ignoreInit = TRUE)
  
  
  # Retrieve Data -----------------------------------------------------------
  measures_rval <- reactiveVal()
  meData_rval <- reactiveVal()
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
    selectedMarginalEffectsPlot <- selectedMarginalEffectsPlot()
    selectedVariablesDisplay <- c("_full_model_", "_baseline_", selectedVariablesDisplay())
    
    # Open connection
    con <- DBI::dbConnect(duckdb::duckdb(),
                          dbdir = file.path(db_path, "biens-db.duckdb"),
                          read_only = TRUE)
    
    # Retrieve measures
    measures <- dplyr::tbl(src = con, "AllMeasures") |>
      dplyr::filter(species == focalSpecies) |>
      dplyr::filter(model %in% selectedModelDisplay) |>
      dplyr::collect()
    
    # Retrieve marginal effects
    if(selectedMarginalEffectsPlot == "ALE"){
      
      meData <- dplyr::tbl(src = con, "AllALEData") |>
        dplyr::filter(species == focalSpecies) |>
        dplyr::filter(model %in% selectedModelDisplay) |>
        dplyr::filter(variable %in% selectedVariablesDisplay) |>
        dplyr::collect()
      
    } else if(selectedMarginalEffectsPlot == "PDP"){
      
      meData <- dplyr::tbl(src = con, "AllPDPData") |>
        dplyr::filter(species == focalSpecies) |>
        dplyr::filter(model %in% selectedModelDisplay) |>
        dplyr::filter(variable %in% selectedVariablesDisplay) |>
        dplyr::collect()
      
    } else if(selectedMarginalEffectsPlot == "CP"){
      
      meData <- dplyr::tbl(src = con, "AllCDData") |>
        dplyr::filter(species == focalSpecies) |>
        dplyr::filter(model %in% selectedModelDisplay) |>
        dplyr::filter(variable %in% selectedVariablesDisplay) |>
        dplyr::collect()
      
    }
    
    # Retrieve feature importance
    featureImportance <- dplyr::tbl(src = con, "AllFeatureImportance") |>
      dplyr::filter(species == focalSpecies) |>
      dplyr::filter(model %in% selectedModelDisplay) |>
      dplyr::filter(variable %in% selectedVariablesDisplay) |>
      dplyr::collect()
    
    # Close connection
    DBI::dbDisconnect(conn = con)
    
    # assign(x = "aleData", value = aleData, envir = .GlobalEnv)
    
    # Store data in reactive objects
    measures_rval(measures)
    meData_rval(meData)
    featureImportance_rval(featureImportance)
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()

  }) |>
    bindEvent(focalSpecies(),
              selectedModelDisplay(),
              selectedVariablesDisplay(),
              selectedMarginalEffectsPlot(),
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
    
    meData <- meData_rval()
    
    output$ale_plot <- plotly::renderPlotly({
      
      ale_plot <- create_ale_plot(ale_data = meData)
      
      ale_plotly <- plotly::ggplotly(ale_plot)
      
      return(ale_plotly)
      
    })
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()
    
    
  }) |>
    bindEvent(meData_rval(),
              ignoreInit = FALSE)
  
  
  # return(nmModelRunOutput_rval)
  
}