surveyDataSummary <- function(input, output, session, surveyData) {
  
  ns <- session$ns
  
  # Create Survey Data Structure Table data ------------------------------
  surveyDataStructure_rval <- reactiveVal()
  
  observe({
    
    shiny::req(surveyData())
    
    surveyData <- surveyData()
    surveyData_long <- surveyData$surveyData_long
    
    # Create a list of dataframes containing the number of quadrats, per group, per year
    quadratsPerYear <- surveyData_long |>
      dplyr::select(Year, Group, Quadrat) |>
      dplyr::distinct() |>
      dplyr::group_by(Year) |>
      dplyr::summarise(quadratsPerYear = dplyr::n())
    
    quadratsPerYearGroup <- surveyData_long |>
      dplyr::select(Year, Group, Quadrat) |>
      dplyr::distinct() |>
      dplyr::group_by(Year, Group) |>
      dplyr::summarise(quadratsPerYearGroup = dplyr::n())
    
    quadratsPerYearID <- quadratsPerYear |>
      dplyr::mutate("ID" = Year, .before = "Year", .keep = "unused") |>
      dplyr::select("ID" = ID,
                    "n" = quadratsPerYear)
    
    quadratsPerYearGroupID <- quadratsPerYearGroup |>
      tidyr::unite(col = "ID", c(Year, Group), sep = " - ", remove = TRUE) |>
      dplyr::select("ID" = ID,
                    "n" = quadratsPerYearGroup)
    
    quadratsPerID <- rbind(quadratsPerYearID, quadratsPerYearGroupID)
    
    surveyData_quadratsPerYearGroup <- list("quadratsPerYear" = quadratsPerYear,
                                            "quadratsPerYearGroup" = quadratsPerYearGroup,
                                            "quadratsPerID" = quadratsPerID)
    
    surveyDataStructure_rval(surveyData_quadratsPerYearGroup)
    
  }) |>
    bindEvent(surveyData(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  # Create Species Data Availability Table data ----------------------------
  speciesDataAvailability_rval <- reactiveVal()
  
  observe({
    
    shiny::req(surveyData())
    
    surveyData <- surveyData()
    surveyData_long <- surveyData$surveyData_long
    
    hill_ellenberg_w_names <- RMAVIS::hill_ellenberg |>
      dplyr::left_join(UKVegTB::taxa_lookup, by = "TVK") |>
      dplyr::select("Species" = "recommended_taxon_name", `F`, L, N, R, S)
    
    speciesDataAvailability <- surveyData_long |>
      dplyr::select("Species") |>
      dplyr::distinct() |>
      dplyr::mutate(
        "Hill-Ellenberg" = 
          dplyr::case_when(
            Species %in% unique(dplyr::filter(hill_ellenberg_w_names, !is.na(`F`)) |> dplyr::pull(Species)) ~ "Yes",
            TRUE ~ as.character("No")
          )
      ) |>
      dplyr::mutate(
        "NVC" = 
          dplyr::case_when(
            Species %in% unique(RMAVIS::nvc_floristic_tables$nvc_taxon_name) ~ "Yes",
            TRUE ~ as.character("No")
          )
      )
    
    
    speciesDataAvailability_rval(speciesDataAvailability)
    
  }) |>
    bindEvent(surveyData(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
# Initialise speciesDataAvailability Table ------------------------------
  speciesDataAvailabilityTable_init <- data.frame("Species" = integer(),
                                                  "Hill-Ellenberg" = character(),
                                                  "NVC" = character()
  )
  
  speciesDataAvailabilityTable_rval <- reactiveVal(speciesDataAvailabilityTable_init)
  
  output$speciesDataAvailabilityTable <- reactable::renderReactable({
    
    speciesDataAvailabilityTable <- reactable::reactable(data = speciesDataAvailabilityTable_init,
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
                                                           format = reactable::colFormat(digits = 0),
                                                           headerClass = "my-header",
                                                           class = "my-col",
                                                           align = "center" # Needed as alignment is not passing through to header
                                                         )
                                                         )
    
    return(speciesDataAvailabilityTable)
    
  })
  
  
  # Initialise quadratsPerYear Table ----------------------------------------
  quadratsPerYearTable_init <- data.frame("Year" = integer(),
                                          "n" = numeric(),
                                          "Similarities.Calculable?" = character()
  )
  
  quadratsPerYearTable_rval <- reactiveVal(quadratsPerYearTable_init)
  
  output$quadratsPerYearTable <- reactable::renderReactable({
    
    quadratsPerYearTable <- reactable::reactable(data = quadratsPerYearTable_init,
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
                                                   format = reactable::colFormat(digits = 0),
                                                   headerClass = "my-header",
                                                   class = "my-col",
                                                   align = "center" # Needed as alignment is not passing through to header
                                                 ))
    
    return(quadratsPerYearTable)
    
  })
  
  # Initialise quadratsPerYearGroup Table -----------------------------------
  quadratsPerYearGroupTable_init <- data.frame("Year" = integer(),
                                               "Group" = character(),
                                               "n" = numeric(),
                                               "Similarities.Calculable?" = character()
  )
  
  quadratsPerYearGroupTable_rval <- reactiveVal(quadratsPerYearGroupTable_init)
  
  output$quadratsPerYearGroupTable <- reactable::renderReactable({
    
    quadratsPerYearGroupTable <- reactable::reactable(data = quadratsPerYearGroupTable_init,
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
                                                        format = reactable::colFormat(digits = 0),
                                                        headerClass = "my-header",
                                                        class = "my-col",
                                                        align = "center" # Needed as alignment is not passing through to header
                                                      ))
    
    return(quadratsPerYearGroupTable)
    
  })
  
  
  # Update quadratsPerYear Table --------------------------------------------
  observe({
    
    req(surveyDataStructure_rval())
    
    quadratsPerYear <- surveyDataStructure_rval()$quadratsPerYear |>
      dplyr::mutate("n" = quadratsPerYear, .keep = "unused") |>
      dplyr::mutate(
        "Similarities.Calculable?" = 
          dplyr::case_when(
            n < 5 ~ "No",
            TRUE ~ as.character("Yes")
          )
      )
    
    output$quadratsPerYearTable <- reactable::renderReactable({
      
      quadratsPerYearTable <- reactable::reactable(data = quadratsPerYear,
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
                                                     format = reactable::colFormat(digits = 0),
                                                     headerClass = "my-header",
                                                     class = "my-col",
                                                     align = "center" # Needed as alignment is not passing through to header
                                                   ))
      
      return(quadratsPerYearTable)
      
    })
    
  }) |>
    bindEvent(surveyDataStructure_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  # Update quadratsPerYearGroup Table ---------------------------------------
  observe({
    
    req(surveyDataStructure_rval())
    
    quadratsPerYearGroup <- surveyDataStructure_rval()$quadratsPerYearGroup|>
      dplyr::mutate("n" = quadratsPerYearGroup, .keep = "unused") |>
      dplyr::mutate(
        "Similarities.Calculable?" = 
          dplyr::case_when(
            n < 5 ~ "No",
            TRUE ~ as.character("Yes")
          )
      )
    
    output$quadratsPerYearGroupTable <- reactable::renderReactable({
      
      quadratsPerYearGroupTable <- reactable::reactable(data = quadratsPerYearGroup,
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
                                                          format = reactable::colFormat(digits = 0),
                                                          headerClass = "my-header",
                                                          class = "my-col",
                                                          align = "center" # Needed as alignment is not passing through to header
                                                        ))
      
      return(quadratsPerYearGroupTable)
      
    })
    
  }) |>
    bindEvent(surveyDataStructure_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  # Update speciesDataAvailability Table ----------------------------------
  observe({
    
    req(speciesDataAvailability_rval())
    
    speciesDataAvailability <- speciesDataAvailability_rval()
    
    output$speciesDataAvailabilityTable <- reactable::renderReactable({
      
      speciesDataAvailabilityTable <- reactable::reactable(data = speciesDataAvailability,
                                                           height = 300,
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
                                                             format = reactable::colFormat(digits = 0),
                                                             headerClass = "my-header",
                                                             class = "my-col",
                                                             align = "center" # Needed as alignment is not passing through to header
                                                           ),
                                                           columns = list(
                                                             `Species` = reactable::colDef(
                                                               filterable = TRUE
                                                             ),
                                                             `Hill-Ellenberg` = reactable::colDef(
                                                               format = reactable::colFormat(digits = 0),
                                                               filterable = TRUE,
                                                               filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                                   return rows.filter(function(row) {
                                                                                                   return row.values[columnId] == filterValue
                                                                                                   })
                                                                                                   }")
                                                             ),
                                                             `NVC` = reactable::colDef(
                                                               format = reactable::colFormat(digits = 0),
                                                               filterable = TRUE,
                                                               filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                                   return rows.filter(function(row) {
                                                                                                   return row.values[columnId] == filterValue
                                                                                                   })
                                                                                                   }")
                                                             )
                                                           ))
      
      return(speciesDataAvailabilityTable)
      
    })
    
  }) |>
    bindEvent(speciesDataAvailability_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  # Compose Data Object to Return -------------------------------------------
  surveyDataSummary_rval <- reactiveVal()
  
  observe({
    
    surveyDataSummary <- list(
      "surveyDataStructure" = surveyDataStructure_rval()
    )
    
    surveyDataSummary_rval(surveyDataSummary)
    
  }) |>
    bindEvent(surveyDataStructure_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  return(surveyDataSummary_rval)
  
}
