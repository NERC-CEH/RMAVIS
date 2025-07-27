speciesFreq <- function(input, output, session, surveyData, sidebar_options) {
  
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
    
    speciesFrequencyTable <- reactable::reactable(data = speciesFrequencyTable_init,
                                                  filterable = FALSE,
                                                  pagination = FALSE, 
                                                  highlight = TRUE,
                                                  bordered = TRUE,
                                                  sortable = TRUE, 
                                                  wrap = FALSE,
                                                  resizable = TRUE, 
                                                  class = "my-tbl",
                                                  # style = list(fontSize = "1rem"),
                                                  rowClass = "my-row",
                                                  # columns = list(
                                                  #   Species = reactable::colDef(
                                                  #     sticky = "left",
                                                  #     style = list(borderRight = "1px solid #eee"),
                                                  #     headerStyle = list(borderRight = "1px solid #eee")
                                                  #   ),
                                                  #   Change = reactable::colDef(
                                                  #     sticky = "right"
                                                  #   )
                                                  # ),
                                                  defaultColDef = reactable::colDef(
                                                    headerClass = "my-header",
                                                    class = "my-col",
                                                    align = "center" # Needed as alignment is not passing through to header
                                                  ))
    
    
    return(speciesFrequencyTable)
    
  })
  

# Compile Frequency Table -------------------------------------------------
  observe({
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Compiling Frequency Table"
    )
    
    shiny::req(surveyData())
    
    surveyData <- surveyData()
    surveyData_long <- surveyData$surveyData_long
    surveyData_wide <- surveyData$wide
    
    isolate({
      
      # I need to find a better way to do this with tidyselect
      max_year <- max(surveyData_long$Year) |>
        as.character()
      min_year <- min(surveyData_long$Year) |>
        as.character()
      
      speciesFrequency <- surveyData_long |>
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
      
      speciesFrequencyTable_rval(speciesFrequency)
      
    }) # close isolate
    
    output$speciesFrequencyTable <- reactable::renderReactable({
      
      speciesFrequencyTable <- reactable::reactable(data = speciesFrequency, 
                                                    filterable = FALSE,
                                                    pagination = FALSE, 
                                                    highlight = TRUE,
                                                    bordered = TRUE,
                                                    sortable = TRUE, 
                                                    wrap = FALSE,
                                                    resizable = TRUE, 
                                                    # compact = TRUE,
                                                    class = "my-tbl",
                                                    # style = list(fontSize = "1rem"),
                                                    rowClass = "my-row",
                                                    defaultColDef = reactable::colDef(
                                                      headerClass = "my-header",
                                                      class = "my-col",
                                                      align = "center" # Needed as alignment is not passing through to header
                                                    ),
                                                    columns = list(
                                                      Species = reactable::colDef(
                                                        filterable = TRUE,
                                                        sticky = "left",
                                                        style = list(borderRight = "1px solid #eee"),
                                                        headerStyle = list(borderRight = "1px solid #eee")
                                                      ),
                                                      Change = reactable::colDef(
                                                        filterable = TRUE,
                                                        filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                       return rows.filter(function(row) {
                                                                                       return row.values[columnId] == filterValue
                                                                                       })
                                                                                       }"),
                                                        sticky = "right"
                                                      ),
                                                      Difference = reactable::colDef(
                                                        sticky = "right",
                                                        style = list(borderLeft = "1px solid #eee"),
                                                        headerStyle = list(borderLeft = "1px solid #eee")
                                                      )
                                                    ),
                                                    # rowStyle = function(index) {
                                                    #   if(speciesFrequency[index, "Change"] %in% c("Gain", "Net Increase")) {
                                                    #     list(background = "lightgreen", alpha = 0.5)
                                                    #   } else if(speciesFrequency[index, "Change"] %in% c("Loss", "Net Decrease")){
                                                    #     list(background = "red", alpha = 0.5)
                                                    #   }
                                                    # }
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