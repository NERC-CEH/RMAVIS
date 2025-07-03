diversityAnalysis <- function(input, output, session, surveyData, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  resultsViewDiversity <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    resultsViewDiversity(sidebar_options()$resultsViewDiversity)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

  

# Show/Hide Results -------------------------------------------------------
  observe({
    
    shinyjs::show(id = "diversitySummaryTable_div")
    shinyjs::show(id = "diversityIndicesTable_div")
    shinyjs::show(id = "speciesRichnessSiteTable_div")
    shinyjs::show(id = "speciesRichnessGroupTable_div")
    shinyjs::show(id = "speciesRichnessQuadratTable_div")
    
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    # diversitySummaryTable
    if("diversitySummaryTable" %in% resultsViewDiversity()){
      shinyjs::show(id = "diversitySummaryTable_div")
    } else {
      shinyjs::hide(id = "diversitySummaryTable_div")
    }
    
    # diversityIndicesTable
    if("diversityIndicesTable" %in% resultsViewDiversity()){
      shinyjs::show(id = "diversityIndicesTable_div")
    } else {
      shinyjs::hide(id = "diversityIndicesTable_div")
    }
    
    # speciesRichnessSiteTable
    if("speciesRichnessSite" %in% resultsViewDiversity()){
      shinyjs::show(id = "speciesRichnessSiteTable_div")
    } else {
      shinyjs::hide(id = "speciesRichnessSiteTable_div")
    }
    
    # speciesRichnessGroupTable
    if("speciesRichnessGroup" %in% resultsViewDiversity()){
      shinyjs::show(id = "speciesRichnessGroupTable_div")
    } else {
      shinyjs::hide(id = "speciesRichnessGroupTable_div")
    }
    
    # speciesRichnessQuadratTable
    if("speciesRichnessQuadrat" %in% resultsViewDiversity()){
      shinyjs::show(id = "speciesRichnessQuadratTable_div")
    } else {
      shinyjs::hide(id = "speciesRichnessQuadratTable_div")
    }

    
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreInit = FALSE,
              ignoreNULL = FALSE)

  observe({
    
    shinyjs::show(id = "diversitySummaryTable_div")
  
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)

# Initialise Summary Table ------------------------------------------------
  diversitySummaryTable_init <- data.frame("Year" = integer(),
                                           "Metric" = character(),
                                           "Value" = character())
  
  diversitySummaryTable_rval <- reactiveVal(diversitySummaryTable_init)
  
  output$diversitySummaryTable <- reactable::renderReactable({
    
    diversitySummaryTable <- reactable::reactable(data = diversitySummaryTable_init,
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
    
    return(diversitySummaryTable)
    
  })
  

# Intialise Diversity Indices Table ---------------------------------------
  diversityIndicesTable_init <- data.frame("Year" = integer(),
                                           "Group" = character(),
                                           "Quadrat" = character(),
                                           "Richness" = integer(),
                                           "Shannon.Diversity" = double(),
                                           "Simpson.Diversity" = double(),
                                           "InverseSimpson.Diversity" = double(),
                                           "Shannon.Evenness" = double(),
                                           "Simpson.Evenness" = double()
                                           )
  
  diversityIndicesTable_rval <- reactiveVal(diversityIndicesTable_init)
  
  output$diversityIndicesTable <- reactable::renderReactable({
    
    diversityIndicesTable <- reactable::reactable(data = diversityIndicesTable_init,
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
    
    return(diversityIndicesTable)
    
  })

  
# Initialise Species Richness Quadrat Table -------------------------------
  speciesRichnessQuadratTable_init <- data.frame("Year" = integer(),
                                                 "Group" = character(),
                                                 "Quadrat" = character(),
                                                 "Richness" = double())
  
  speciesRichnessQuadratTable_rval <- reactiveVal(speciesRichnessQuadratTable_init)

  output$speciesRichnessQuadratTable <- reactable::renderReactable({

    speciesRichnessQuadratTable <- reactable::reactable(data = speciesRichnessQuadratTable_init,
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

    return(speciesRichnessQuadratTable)

  })
  

# Initialise Species Richness Group Table ---------------------------------
  speciesRichnessGroupTable_init <- data.frame("Year" = integer(),
                                               "Group" = character(),
                                               "Richness" = double())
  
  speciesRichnessGroupTable_rval <- reactiveVal(speciesRichnessGroupTable_init)
  
  output$speciesRichnessGroupTable <- reactable::renderReactable({
    
    speciesRichnessGroupTable <- reactable::reactable(data = speciesRichnessGroupTable_init,
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
    
    return(speciesRichnessGroupTable)
    
  })


# Initialise Species Richness Site Table ----------------------------------
  speciesRichnessSiteTable_init <- data.frame("Year" = integer(),
                                              "Richness" = double())
  
  speciesRichnessSiteTable_rval <- reactiveVal(speciesRichnessSiteTable_init)
  
  output$speciesRichnessSiteTable <- reactable::renderReactable({
    
    speciesRichnessSiteTable <- reactable::reactable(data = speciesRichnessSiteTable_init,
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
    
    return(speciesRichnessSiteTable)
    
  })
  
  

# Calculate Diversity Metrics, Update Tables ------------------------------
  diversityDataAll_rval <- reactiveVal()
  
  observe({
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating Diversity Metrics"
    )
    
    shiny::req(surveyData())
    
    surveyData <- surveyData()
    surveyData_long <- surveyData$surveyData_long
    surveyData_mat <- surveyData$surveyData_mat

# Species Richness --------------------------------------------------------
  
    # Species Richness - Quadrat
    speciesRichness_quadrat <- surveyData_long |>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
      dplyr::ungroup()
    
    speciesRichness_quadrat_wide <- speciesRichness_quadrat |>
      tidyr::pivot_wider(id_cols = c(Group, Quadrat),
                         names_from = Year,
                         values_from = Richness)
    
    speciesRichness_quadrat_long <- speciesRichness_quadrat |>
      tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE)
    
    
    # Species Richness - Group
    speciesRichness_group <- surveyData_long |>
      dplyr::group_by(Year, Group) |>
      dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
      dplyr::ungroup()
    
    speciesRichness_group_wide <- speciesRichness_group |>
      tidyr::pivot_wider(id_cols = c(Group),
                         names_from = Year,
                         values_from = Richness)
    
    speciesRichness_group_long <- speciesRichness_group |>
      tidyr::unite(col = "ID", c(Year, Group), sep = " - ", remove = TRUE)
    
    # Species Richness - Site
    speciesRichness_site <- surveyData_long |>
      dplyr::group_by(Year) |>
      dplyr::summarise("Richness" = dplyr::n_distinct(Species)) |>
      dplyr::ungroup()
    
    speciesRichness_site_wide <- speciesRichness_site |>
      dplyr::mutate("Site" = "Site", .before = "Year") |>
      tidyr::pivot_wider(id_cols = Site,
                         names_from = Year,
                         values_from = Richness)
    
    speciesRichness_site_long <- speciesRichness_site
    
    # Summary table concordance
    surveyData_conc <- surveyData_long |>
      tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = FALSE) |>
      dplyr::select(ID, Year, Group, Quadrat) |>
      dplyr::distinct()
    
    # Summary Table
    summaryTable <- speciesRichness_quadrat |>
      dplyr::group_by(Year) |>
      dplyr::summarise("Alpha.Mean" = mean(Richness)) |>
      dplyr::left_join(speciesRichness_site_long, by = "Year") |>
      dplyr::rename("Gamma" = "Richness") |>
      dplyr::mutate("Beta" = (Gamma / Alpha.Mean) - 1) |>
      base::t() 
    
    colnames(summaryTable) <- summaryTable[1,]

    summaryTable <- summaryTable |>
      tibble::as_tibble(rownames = "Metric") |>
      dplyr::filter(Metric != "Year")
    
    # Shannon Diversity
    shannonDiversity <- surveyData_mat |>
      vegan::diversity(index = "shannon") |>
      tibble::as_tibble(rownames = "ID") |>
      dplyr::rename("Shannon.Diversity" = "value")
    
    # Simpson Diversity
    simpsonDiversity <- surveyData_mat |>
      vegan::diversity(index = "simpson") |>
      tibble::as_tibble(rownames = "ID") |>
      dplyr::rename("Simpson.Diversity" = "value")
    
    # Inverse Simpson Diversity
    inverseSimpsonDiversity <- surveyData_mat |>
      vegan::diversity(index = "invsimpson") |>
      tibble::as_tibble(rownames = "ID") |>
      dplyr::rename("InverseSimpson.Diversity" = "value")
    
    # Shannon's/Pielou’s J evenness
    shannonsEvenness <- shannonDiversity |>
      # dplyr::left_join(speciesRichness_quadrat_long, by = "ID") |>
      # dplyr::mutate("Shannon.Evenness" = Shannon.Diversity / log(Richness)) |>
      dplyr::mutate("Shannon.Evenness" = (Shannon.Diversity / max(Shannon.Diversity)), .keep = "unused")
    
    # Simpson's evenness
    simpsonEvenness <- inverseSimpsonDiversity |>
      dplyr::left_join(speciesRichness_quadrat_long, by = "ID") |>
      dplyr::mutate("Simpson.Evenness" = (InverseSimpson.Diversity / Richness), .keep = "unused")
    
    # Rényi diversities and Hill Numbers
    # vegan::renyi(surveyDataWide)
    
    # Diversity Metrics Table
    diversityIndicesTable <- speciesRichness_quadrat_long |>
      dplyr::left_join(shannonDiversity, by = "ID") |>
      dplyr::left_join(simpsonDiversity, by = "ID") |>
      dplyr::left_join(inverseSimpsonDiversity, by = "ID") |>
      dplyr::left_join(shannonsEvenness, by = "ID") |>
      dplyr::left_join(simpsonEvenness, by = "ID") |>
      dplyr::left_join(surveyData_conc, by = "ID") |>
      dplyr::select(Year, Group, Quadrat, Richness, Shannon.Diversity, Simpson.Diversity, InverseSimpson.Diversity, Shannon.Evenness, Simpson.Evenness)
    

# Update summaryTable -----------------------------------------------------
    output$diversitySummaryTable <- reactable::renderReactable({
      
      diversitySummaryTable <- reactable::reactable(data = summaryTable,
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
                                                    )
                                                    )
      
      return(diversitySummaryTable)
      
    })
    
    diversitySummaryTable_rval(summaryTable)
    

# Update diversityIndicesTable --------------------------------------------
    output$diversityIndicesTable <- reactable::renderReactable({
      
      diversityIndicesTable <- reactable::reactable(data = diversityIndicesTable,
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
                                                    ))
      
      return(diversityIndicesTable)
      
    })
    
    diversityIndicesTable_rval(diversityIndicesTable)
    

# Update speciesRichnessSiteTable -----------------------------------------
    output$speciesRichnessSiteTable <- reactable::renderReactable({
      
      speciesRichnessSiteTable <- reactable::reactable(data = speciesRichness_site_wide,
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
                                                       )
                                                       )
      
      return(speciesRichnessSiteTable)
      
    })
    
    speciesRichnessSiteTable_rval(speciesRichness_site_wide)
    
    
    

# Update speciesRichnessGroupTable ----------------------------------------
    output$speciesRichnessGroupTable <- reactable::renderReactable({
      
      speciesRichnessGroupTable <- reactable::reactable(data = speciesRichness_group_wide,
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
                                                          Group = reactable::colDef(
                                                            filterable = TRUE,
                                                            filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                         return rows.filter(function(row) {
                                                                                         return row.values[columnId] == filterValue
                                                                                         })
                                                                                         }")
                                                          )
                                                        )
      )
      
      return(speciesRichnessGroupTable)
      
    })
    
    speciesRichnessGroupTable_rval(speciesRichness_group_wide)
    
    

# Update speciesRichnessQuadratTable --------------------------------------
    output$speciesRichnessQuadratTable <- reactable::renderReactable({
      
      speciesRichnessQuadratTable <- reactable::reactable(data = speciesRichness_quadrat_wide,
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
      
      return(speciesRichnessQuadratTable)
      
    })
    
    speciesRichnessQuadratTable_rval(speciesRichness_quadrat_wide)
      
    

# Compile All Data --------------------------------------------------------
    diversityDataAll <- list("diversitySummary" = diversitySummaryTable_rval(),
                             "diversityIndices" = diversityIndicesTable_rval(),
                             "speciesRichnessSite" = speciesRichnessSiteTable_rval(),
                             "speciesRichnessGroup" = speciesRichnessGroupTable_rval(),
                             "speciesRichnessQuadrat" = speciesRichnessQuadratTable_rval())

    diversityDataAll_rval(diversityDataAll)
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE, 
              label = "calcDivMetrics")
  


# Ensure Tables Are Not Suspended Are Hidden ------------------------------
  outputOptions(output, "speciesRichnessSiteTable", suspendWhenHidden = FALSE)
  outputOptions(output, "speciesRichnessGroupTable", suspendWhenHidden = FALSE)
  outputOptions(output, "speciesRichnessQuadratTable", suspendWhenHidden = FALSE)
  
  

# Return Data -------------------------------------------------------------
  return(diversityDataAll_rval)
  
}
