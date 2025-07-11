calcAvgEIVs <- function(input, output, session, surveyData, sidebar_options) {
  
  ns <- session$ns

# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  resultsViewEIVs <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    resultsViewEIVs(sidebar_options()$resultsViewEIVs)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

# Show/Hide Results -------------------------------------------------------
  observe({
    
    shinyjs::show(id = "weightedMeanHEValuesSite_div")
    shinyjs::show(id = "unweightedMeanHEValuesSite_div")
    shinyjs::show(id = "weightedMeanHEValuesGroup_div")
    shinyjs::show(id = "unweightedMeanHEValuesGroup_div")
    shinyjs::show(id = "weightedMeanHEValuesQuadrat_div")
    shinyjs::show(id = "unweightedMeanHEValuesQuadrat_div")
    
  }) |>
    bindEvent(resultsViewEIVs(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    if("weightedMeanHEValuesSite" %in% resultsViewEIVs()){
      shinyjs::show(id = "weightedMeanHEValuesSite_div")
    } else {
      shinyjs::hide(id = "weightedMeanHEValuesSite_div")
    }
    
    if("unweightedMeanHEValuesSite" %in% resultsViewEIVs()){
      shinyjs::show(id = "unweightedMeanHEValuesSite_div")
    } else {
      shinyjs::hide(id = "unweightedMeanHEValuesSite_div")
    }
    
    if("weightedMeanHEValuesGroup" %in% resultsViewEIVs()){
      shinyjs::show(id = "weightedMeanHEValuesGroup_div")
    } else {
      shinyjs::hide(id = "weightedMeanHEValuesGroup_div")
    }
    
    if("unweightedMeanHEValuesGroup" %in% resultsViewEIVs()){
      shinyjs::show(id = "unweightedMeanHEValuesGroup_div")
    } else {
      shinyjs::hide(id = "unweightedMeanHEValuesGroup_div")
    }
    
    if("weightedMeanHEValuesQuadrat" %in% resultsViewEIVs()){
      shinyjs::show(id = "weightedMeanHEValuesQuadrat_div")
    } else {
      shinyjs::hide(id = "weightedMeanHEValuesQuadrat_div")
    }
    
    if("unweightedMeanHEValuesQuadrat" %in% resultsViewEIVs()){
      shinyjs::show(id = "unweightedMeanHEValuesQuadrat_div")
    } else {
      shinyjs::hide(id = "unweightedMeanHEValuesQuadrat_div")
    }
    
    
  }) |>
    bindEvent(resultsViewEIVs(),
              ignoreInit = FALSE,
              ignoreNULL = FALSE)
  
  observe({
    
    shinyjs::show(id = "unweightedMeanHEValuesSite_div")
    
  }) |>
    bindEvent(resultsViewEIVs(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  

# Initialise Results Tables -----------------------------------------------
  meanHEValuesTable_init <- data.frame("Year" = integer(),
                                       "Moisture.F" = double(),
                                       "Light.L" = double(),
                                       "Nitrogen.N" = double(),
                                       "Reaction.R" = double(),
                                       "Salinity.S" = double()
                                       )
  
# Initialise reactive objects to hold table data --------------------------
  weightedMeanHEValuesSite_rval <- reactiveVal(meanHEValuesTable_init)
  unweightedMeanHEValuesSite_rval <- reactiveVal(meanHEValuesTable_init)
  weightedMeanHEValuesGroup_rval <- reactiveVal(meanHEValuesTable_init)
  unweightedMeanHEValuesGroup_rval <- reactiveVal(meanHEValuesTable_init)
  weightedMeanHEValuesQuadrat_rval <- reactiveVal(meanHEValuesTable_init)
  unweightedMeanHEValuesQuadrat_rval <- reactiveVal(meanHEValuesTable_init)
  
# Initialise Weighted Mean HE Values by Site ------------------------------
  output$weightedMeanHEValuesSiteTable <- reactable::renderReactable({
    
    weightedMeanHEValuesSiteTable <- reactable::reactable(data = meanHEValuesTable_init,
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
                                                            format = reactable::colFormat(digits = 2),
                                                            headerClass = "my-header",
                                                            class = "my-col",
                                                            align = "center" # Needed as alignment is not passing through to header
                                                          ))
    
    return(weightedMeanHEValuesSiteTable)
    
  })

  
# Initialise Unweighted Mean HE Values by Site ----------------------------
  output$unweightedMeanHEValuesSiteTable <- reactable::renderReactable({
    
    unweightedMeanHEValuesSiteTable <- reactable::reactable(data = meanHEValuesTable_init,
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
                                                              format = reactable::colFormat(digits = 2),
                                                              headerClass = "my-header",
                                                              class = "my-col",
                                                              align = "center" # Needed as alignment is not passing through to header
                                                            ))
  
  return(unweightedMeanHEValuesSiteTable)
    
  })


# Initialise Weighted Mean HE Values by Group -----------------------------
  output$weightedMeanHEValuesGroupTable <- reactable::renderReactable({
    
    weightedMeanHEValuesGroupTable <- reactable::reactable(data = meanHEValuesTable_init,
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
                                                             format = reactable::colFormat(digits = 2),
                                                             headerClass = "my-header",
                                                             class = "my-col",
                                                             align = "center" # Needed as alignment is not passing through to header
                                                           ))
  
  return(weightedMeanHEValuesGroupTable)
    
  })


# Initialise Unweighted Mean HE Values by Group ---------------------------
  output$unweightedMeanHEValuesGroupTable <- reactable::renderReactable({
    
    unweightedMeanHEValuesGroupTable<- reactable::reactable(data = meanHEValuesTable_init,
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
                                                              format = reactable::colFormat(digits = 2),
                                                              headerClass = "my-header",
                                                              class = "my-col",
                                                              align = "center" # Needed as alignment is not passing through to header
                                                            ))
  
  return(unweightedMeanHEValuesGroupTable)
    
  })
  

# Initialise Weighted Mean HE Values by Quadrat ---------------------------
  output$weightedMeanHEValuesQuadratTable <- reactable::renderReactable({
    
    weightedMeanHEValuesQuadratTable <- reactable::reactable(data = meanHEValuesTable_init,
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
                                                               format = reactable::colFormat(digits = 2),
                                                               headerClass = "my-header",
                                                               class = "my-col",
                                                               align = "center" # Needed as alignment is not passing through to header
                                                             ))
  
  return(weightedMeanHEValuesQuadratTable)
    
  })

# Initialise Unweighted Mean HE Values by Quadrat -------------------------
  output$unweightedMeanHEValuesQuadratTable <- reactable::renderReactable({
    
    unweightedMeanHEValuesQuadratTable <- reactable::reactable(data = meanHEValuesTable_init,
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
                                                                 format = reactable::colFormat(digits = 2),
                                                                 headerClass = "my-header",
                                                                 class = "my-col",
                                                                 align = "center" # Needed as alignment is not passing through to header
                                                               ))
  
  return(unweightedMeanHEValuesQuadratTable)
    
  })
  

# Update tables -----------------------------------------------------------
  meanHEValuesTableAll_rval <- reactiveVal()
  
  observe({
    
    shiny::req(surveyData())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating Mean Hill-Ellenberg Values"
    )
    
    # Isolate reactive objects
    shiny::isolate({
      
      surveyData <- surveyData()
      surveyData_long <- surveyData$surveyData_long
      
    })
      

# Calculate Weighted Mean HE Values ---------------------------------------
      
    # By Quadrat
    weightedMeanHEValuesQuadrat <- surveyData_long |>
      dplyr::rename("taxon_name" = "Species") |>
      dplyr::left_join(RMAVIS::accepted_taxa, by = "taxon_name") |>
      dplyr::left_join(RMAVIS::hill_ellenberg, by = "TVK",
                       relationship = "many-to-many") |>
      dplyr::select(Year, Group, Quadrat, Cover, `F`, L, N, R, S) |>
      dplyr::mutate("F" = `F` * Cover,
                    "L" = L * Cover, 
                    "N" = N * Cover, 
                    "R" = R * Cover, 
                    "S" = S * Cover) |>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::summarise("Moisture.F" = sum(`F`, na.rm = TRUE),
                       "Light.L" = sum(L, na.rm = TRUE), 
                       "Nitrogen.N" = sum(N, na.rm = TRUE), 
                       "Reaction.R" = sum(R, na.rm = TRUE), 
                       "Salinity.S" = sum(S, na.rm = TRUE), .groups = "drop") |>
      dplyr::ungroup() |>
      dplyr::arrange(Year, Group, Quadrat)
    
    weightedMeanHEValuesQuadrat_prepped <- weightedMeanHEValuesQuadrat #|>
      # tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE)
    
    
    # By Group
    weightedMeanHEValuesGroup <- weightedMeanHEValuesQuadrat |>
      dplyr::group_by(Year, Group) |>
      dplyr::summarise("Moisture.F" = mean(`Moisture.F`, na.rm = TRUE),
                       "Light.L" = mean(Light.L, na.rm = TRUE), 
                       "Nitrogen.N" = mean(Nitrogen.N, na.rm = TRUE), 
                       "Reaction.R" = mean(Reaction.R, na.rm = TRUE), 
                       "Salinity.S" = mean(Salinity.S, na.rm = TRUE), .groups = "drop")|>
      dplyr::ungroup() |>
      dplyr::arrange(Year, Group)
    
    weightedMeanHEValuesGroup_prepped <- weightedMeanHEValuesGroup  #|>
      # tidyr::unite(col = "ID", c(Year, Group), sep = " - ", remove = TRUE)
    
    # By Site
    weightedMeanHEValuesSite <- weightedMeanHEValuesQuadrat |>
      dplyr::group_by(Year) |>
      dplyr::summarise("Moisture.F" = mean(`Moisture.F`, na.rm = TRUE),
                       "Light.L" = mean(Light.L, na.rm = TRUE), 
                       "Nitrogen.N" = mean(Nitrogen.N, na.rm = TRUE), 
                       "Reaction.R" = mean(Reaction.R, na.rm = TRUE), 
                       "Salinity.S" = mean(Salinity.S, na.rm = TRUE), .groups = "drop")|>
      dplyr::ungroup() |>
      dplyr::arrange(Year)
    
    weightedMeanHEValuesSite_prepped <- weightedMeanHEValuesSite
      

# Calculate Unweighted Mean HE Values -------------------------------------
      
    # By Quadrat
    unweightedMeanHEValuesQuadrat <- surveyData_long |>
      dplyr::rename("taxon_name" = "Species") |>
      dplyr::left_join(RMAVIS::accepted_taxa, by = "taxon_name") |>
      dplyr::left_join(RMAVIS::hill_ellenberg, by = "TVK",
                       relationship = "many-to-many") |>
      dplyr::select(Year, Group, Quadrat, Cover, `F`, L, N, R, S) |>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::summarise("Moisture.F" = mean(`F`, na.rm = TRUE),
                       "Light.L" = mean(L, na.rm = TRUE), 
                       "Nitrogen.N" = mean(N, na.rm = TRUE), 
                       "Reaction.R" = mean(R, na.rm = TRUE), 
                       "Salinity.S" = mean(S, na.rm = TRUE), .groups = "drop") |>
      dplyr::ungroup() |>
      dplyr::arrange(Year, Group, Quadrat)
    
    unweightedMeanHEValuesQuadrat_prepped <- unweightedMeanHEValuesQuadrat #|>
      # tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE)
    
    
    # By Group
    unweightedMeanHEValuesGroup <- unweightedMeanHEValuesQuadrat |>
      dplyr::group_by(Year, Group) |>
      dplyr::summarise("Moisture.F" = mean(`Moisture.F`, na.rm = TRUE),
                       "Light.L" = mean(Light.L, na.rm = TRUE), 
                       "Nitrogen.N" = mean(Nitrogen.N, na.rm = TRUE), 
                       "Reaction.R" = mean(Reaction.R, na.rm = TRUE), 
                       "Salinity.S" = mean(Salinity.S, na.rm = TRUE), .groups = "drop")|>
      dplyr::ungroup() |>
      dplyr::arrange(Year, Group)
    
    unweightedMeanHEValuesGroup_prepped <- unweightedMeanHEValuesGroup  #|>
      # tidyr::unite(col = "ID", c(Year, Group), sep = " - ", remove = TRUE)
    
    # By Site
    unweightedMeanHEValuesSite <- unweightedMeanHEValuesQuadrat |>
      dplyr::group_by(Year) |>
      dplyr::summarise("Moisture.F" = mean(`Moisture.F`, na.rm = TRUE),
                       "Light.L" = mean(Light.L, na.rm = TRUE), 
                       "Nitrogen.N" = mean(Nitrogen.N, na.rm = TRUE), 
                       "Reaction.R" = mean(Reaction.R, na.rm = TRUE), 
                       "Salinity.S" = mean(Salinity.S, na.rm = TRUE), .groups = "drop")|>
      dplyr::ungroup() |>
      dplyr::arrange(Year)
    
    unweightedMeanHEValuesSite_prepped <- unweightedMeanHEValuesSite
    

# Update Tables -----------------------------------------------------------

# Update Weighted Mean HE Values by Site ------------------------------
  output$weightedMeanHEValuesSiteTable <- reactable::renderReactable({

    weightedMeanHEValuesSiteTable <- reactable::reactable(data = weightedMeanHEValuesSite_prepped,
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
                                                            format = reactable::colFormat(digits = 2),
                                                            headerClass = "my-header",
                                                            class = "my-col",
                                                            align = "center" # Needed as alignment is not passing through to header
                                                          ),
                                                          columns = list(
                                                            Year = reactable::colDef(
                                                              format = reactable::colFormat(digits = 0),
                                                              filterable = TRUE,
                                                              filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                            )
                                                          ))

    return(weightedMeanHEValuesSiteTable)

  })

# Update Unweighted Mean HE Values by Site ----------------------------
  output$unweightedMeanHEValuesSiteTable <- reactable::renderReactable({

    unweightedMeanHEValuesSiteTable <- reactable::reactable(data = unweightedMeanHEValuesSite_prepped,
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
                                                              format = reactable::colFormat(digits = 2),
                                                              headerClass = "my-header",
                                                              class = "my-col",
                                                              align = "center" # Needed as alignment is not passing through to header
                                                            ),
                                                            columns = list(
                                                              Year = reactable::colDef(
                                                                format = reactable::colFormat(digits = 0),
                                                                filterable = TRUE,
                                                                filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                              )
                                                            ))

    return(unweightedMeanHEValuesSiteTable)

  })

# Update Weighted Mean HE Values by Group -----------------------------
  output$weightedMeanHEValuesGroupTable <- reactable::renderReactable({

    weightedMeanHEValuesGroupTable <- reactable::reactable(data = weightedMeanHEValuesGroup_prepped,
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
                                                             format = reactable::colFormat(digits = 2),
                                                             headerClass = "my-header",
                                                             class = "my-col",
                                                             align = "center" # Needed as alignment is not passing through to header
                                                           ),
                                                           columns = list(
                                                             Year = reactable::colDef(
                                                               format = reactable::colFormat(digits = 0),
                                                               filterable = TRUE,
                                                               filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                             ),
                                                             Group = reactable::colDef(
                                                               filterable = TRUE,
                                                               filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                             )
                                                           ))

    return(weightedMeanHEValuesGroupTable)

  })

# Update Unweighted Mean HE Values by Group ---------------------------
  output$unweightedMeanHEValuesGroupTable <- reactable::renderReactable({

    unweightedMeanHEValuesGroupTable<- reactable::reactable(data = unweightedMeanHEValuesGroup_prepped,
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
                                                              format = reactable::colFormat(digits = 2),
                                                              headerClass = "my-header",
                                                              class = "my-col",
                                                              align = "center" # Needed as alignment is not passing through to header
                                                            ),
                                                            columns = list(
                                                              Year = reactable::colDef(
                                                                format = reactable::colFormat(digits = 0),
                                                                filterable = TRUE,
                                                                filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                              ),
                                                              Group = reactable::colDef(
                                                                filterable = TRUE,
                                                                filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                              )
                                                            ))

    return(unweightedMeanHEValuesGroupTable)

  })

# # Update Weighted Mean HE Values by Quadrat ---------------------------
  output$weightedMeanHEValuesQuadratTable <- reactable::renderReactable({

    weightedMeanHEValuesQuadratTable <- reactable::reactable(data = weightedMeanHEValuesQuadrat_prepped,
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
                                                               format = reactable::colFormat(digits = 2),
                                                               headerClass = "my-header",
                                                               class = "my-col",
                                                               align = "center" # Needed as alignment is not passing through to header
                                                             ),
                                                             columns = list(
                                                               Year = reactable::colDef(
                                                                 format = reactable::colFormat(digits = 0),
                                                                 filterable = TRUE,
                                                                 filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                               ),
                                                               Group = reactable::colDef(
                                                                 filterable = TRUE,
                                                                 filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                               ),
                                                               Quadrat = reactable::colDef(
                                                                 filterable = TRUE,
                                                                 filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                               )
                                                             )
    )

    return(weightedMeanHEValuesQuadratTable)

  })

# Update Unweighted Mean HE Values by Quadrat -------------------------
  output$unweightedMeanHEValuesQuadratTable <- reactable::renderReactable({

    unweightedMeanHEValuesQuadratTable <- reactable::reactable(data = unweightedMeanHEValuesQuadrat_prepped,
                                                               # elementId = "unweightedMeanHEValuesQuadratTable",
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
                                                                 format = reactable::colFormat(digits = 2),
                                                                 headerClass = "my-header",
                                                                 class = "my-col",
                                                                 align = "center" # Needed as alignment is not passing through to header
                                                               ),
                                                               columns = list(
                                                                 Year = reactable::colDef(
                                                                   format = reactable::colFormat(digits = 0),
                                                                   filterable = TRUE,
                                                                   filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                                 ),
                                                                 Group = reactable::colDef(
                                                                   filterable = TRUE,
                                                                   filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                                 ),
                                                                 Quadrat = reactable::colDef(
                                                                   filterable = TRUE,
                                                                   # filterInput = dataListFilter("unweightedMeanHEValuesQuadratTable")
                                                                   filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                     return rows.filter(function(row) {
                                                                                     return row.values[columnId] == filterValue
                                                                                     })
                                                                                     }")
                                                                 )
                                                               )
                                                               )

    return(unweightedMeanHEValuesQuadratTable)

  })
    
  weightedMeanHEValuesSite_rval(weightedMeanHEValuesSite_prepped)
  unweightedMeanHEValuesSite_rval(unweightedMeanHEValuesSite_prepped)
  weightedMeanHEValuesGroup_rval(weightedMeanHEValuesGroup_prepped)
  unweightedMeanHEValuesGroup_rval(unweightedMeanHEValuesGroup_prepped)
  weightedMeanHEValuesQuadrat_rval(weightedMeanHEValuesQuadrat_prepped)
  unweightedMeanHEValuesQuadrat_rval(unweightedMeanHEValuesQuadrat_prepped)
  
  meanHEValuesTableAll <- list("weightedMeanHEValuesSite" = weightedMeanHEValuesSite_rval(),
                               "unweightedMeanHEValuesSite" = unweightedMeanHEValuesSite_rval(),
                               "weightedMeanHEValuesGroup" = weightedMeanHEValuesGroup_rval(),
                               "unweightedMeanHEValuesGroup" = unweightedMeanHEValuesGroup_rval(),
                               "weightedMeanHEValuesQuadrat" = weightedMeanHEValuesQuadrat_rval(),
                               "unweightedMeanHEValuesQuadrat" = unweightedMeanHEValuesQuadrat_rval()
                               )
  
  meanHEValuesTableAll_rval(meanHEValuesTableAll)
  
  shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE, 
              label = "updateEIVTables")
  

# Ensure tables are not suspended when hidden -----------------------------
  outputOptions(output, "weightedMeanHEValuesSiteTable", suspendWhenHidden = FALSE)
  outputOptions(output, "unweightedMeanHEValuesSiteTable", suspendWhenHidden = FALSE)
  outputOptions(output, "weightedMeanHEValuesGroupTable", suspendWhenHidden = FALSE)
  outputOptions(output, "unweightedMeanHEValuesGroupTable", suspendWhenHidden = FALSE)
  outputOptions(output, "weightedMeanHEValuesQuadratTable", suspendWhenHidden = FALSE)
  outputOptions(output, "unweightedMeanHEValuesQuadratTable", suspendWhenHidden = FALSE)
  

# Return data -------------------------------------------------------------
  return(meanHEValuesTableAll_rval)
  
}
