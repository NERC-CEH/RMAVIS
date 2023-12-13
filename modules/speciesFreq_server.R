speciesFreq <- function(input, output, session, surveyTable, surveyTableWide, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  

# Intialise Species Frequency Table ---------------------------------------
  speciesFrequencyTable_init <- data.frame("Year" = integer(),
                                           "Species" = character())
  
  speciesFrequencyTable_rval <- reactiveVal(speciesFrequencyTable_init)
  
  
  output$speciesFrequencyTable <- reactable::renderReactable({
    
    speciesFrequencyTable <- reactable::reactable(data = speciesFrequencyTable_init)
    
    
    return(speciesFrequencyTable)
    
  })
  

# Compile Frequency Table -------------------------------------------------
  observe({
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Compiling Frequency Table"
    )
    
    shiny::req(surveyTable())
    shiny::req(surveyTableWide())
    
    surveyTable <- surveyTable()
    surveyTableWide <- surveyTableWide()
    
    isolate({
      
      # I need to find a better way to do this with tidy select
      max_year <- max(surveyTable$Year) |>
        as.character()
      min_year <- min(surveyTable$Year) |>
        as.character()
      
      speciesFrequency <- surveyTable |>
        dplyr::group_by(Year, Species) |>
        dplyr::summarise(Frequency = dplyr::n()) |>
        tidyr::pivot_wider(id_cols = Species,
                           names_from = Year,
                           values_from = Frequency) |>
        dplyr::mutate(
          "Difference" = 
            dplyr::case_when(
              is.na(get(min_year)) ~ as.numeric(get(max_year)),
              is.na(get(max_year)) ~ as.numeric(get(min_year)) * -1,
              !is.na(get(min_year)) & !is.na(get(max_year)) ~ as.numeric(get(max_year)) - as.numeric(get(min_year)),
              TRUE ~ 0
            )
        ) |>
        dplyr::mutate(
          "Change" = 
            dplyr::case_when(
              is.na(get(min_year)) & !is.na(get(max_year)) ~ "Gain",
              is.na(get(max_year)) & !is.na(get(min_year))~ "Loss",
              Difference > 0 ~ "Net Increase",
              Difference < 0 ~ "Net Decrease",
              Difference == 0 ~ "No Net Difference",
              TRUE ~ "Gain then Loss"
            )
        )
      
    })
    
    output$speciesFrequencyTable <- reactable::renderReactable({
      
      speciesFrequencyTable <- reactable::reactable(data = speciesFrequency, 
                                                    filterable = TRUE,
                                                    pagination = FALSE, 
                                                    highlight = TRUE,
                                                    bordered = TRUE,
                                                    sortable = TRUE, 
                                                    resizable = TRUE,
                                                    style = list(fontSize = "1rem"),
                                                    defaultColDef = reactable::colDef(
                                                      # header = function(value) gsub(".", " ", value, fixed = TRUE),
                                                      # cell = function(value) format(value, nsmall = 1),
                                                      align = "center",
                                                      # minWidth = 70,
                                                      headerStyle = list(background = "#f7f7f8",
                                                                         fontweight = "normal")
                                                      ),
                                                    # rowStyle = function(index) {
                                                    #   if(speciesFrequency[index, "Change"] %in% c("Gain", "Net Increase")) {
                                                    #     list(background = "lightgreen", alpha = 0.5)
                                                    #   } else if(speciesFrequency[index, "Change"] %in% c("Loss", "Net Decrease")){
                                                    #     list(background = "red", alpha = 0.5)
                                                    #   }
                                                    # },
                                                    theme = reactable::reactableTheme(
                                                      cellPadding = "2px 2px"
                                                    )
                                                    )
      
      
      return(speciesFrequencyTable)
      
    })
      
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "speciesFrequencyTable", suspendWhenHidden = FALSE)
  
  return(speciesFrequencyTable_rval)
  
}