diversityAnalysis <- function(input, output, session, setupData, surveyData, sidebar_options) {
  
  ns <- session$ns
  
  dataListFilter <- function(tableId, style = "width: 100%; height: 28px;") {
    function(values, name) {
      dataListId <- sprintf("%s-%s-list", tableId, name)
      tagList(
        tags$input(
          type = "text",
          list = dataListId,
          oninput = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
          "aria-label" = sprintf("Filter %s", name),
          style = style
        ),
        tags$datalist(
          id = dataListId,
          lapply(unique(values), function(value) tags$option(value = value))
        )
      )
    }
  }
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  aggTaxaOpts <- reactiveVal()
  resultsViewDiversity <- reactiveVal()
  hillq <- reactiveVal()
  divMetrics <- reactiveVal()
  divMeasures <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    aggTaxaOpts(sidebar_options()$aggTaxaOpts)
    resultsViewDiversity(sidebar_options()$resultsViewDiversity)
    hillq(sidebar_options()$hillq)
    divMetrics(sidebar_options()$divMetrics)
    divMeasures(sidebar_options()$divMeasures)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

# Retrieve setup data -----------------------------------------------------
  regional_availability <- reactiveVal()
  higher_taxa <- reactiveVal()
  phylo_taxa_lookup <- reactiveVal()
  phylo_tree <- reactiveVal()
  
  observe({
    
    regional_availability(setupData()$regional_availability)
    higher_taxa(setupData()$higher_taxa)
    phylo_taxa_lookup(setupData()$phylo_taxa_lookup)
    phylo_tree(setupData()$phylo_tree)
    
  }) |>
    shiny::bindEvent(setupData(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)

# Initialise reactive objects ---------------------------------------------
  diversityData_rval <- reactiveVal()
  diversityTableYear_rval <- reactiveVal()
  diversityTableGroup_rval <- reactiveVal()
  diversityTableQuadrat_rval <- reactiveVal()


# Show/Hide Results -------------------------------------------------------
  observe({
    
    # diversityTableYear
    if("year" %in% resultsViewDiversity()){
      shinyjs::show(id = "diversityTableYear_div")
    } else {
      shinyjs::hide(id = "diversityTableYear_div")
    }
    
    # diversityTableGroup
    if("group" %in% resultsViewDiversity()){
      shinyjs::show(id = "diversityTableGroup_div")
    } else {
      shinyjs::hide(id = "diversityTableGroup_div")
    }
    
    # diversityTableQuadrat
    if("quadrat" %in% resultsViewDiversity()){
      shinyjs::show(id = "diversityTableQuadrat_div")
    } else {
      shinyjs::hide(id = "diversityTableQuadrat_div")
    }
    
  }) |>
    bindEvent(resultsViewDiversity(),
              ignoreInit = FALSE,
              ignoreNULL = FALSE)

# Initialise Species Richness Quadrat Table -------------------------------
  diversityTableQuadrat_init <- data.frame("Year" = integer(),
                                           "Group" = character(),
                                           "Quadrat" = character(),
                                           "Metric" = character(),
                                           "Measure" = character(),
                                           "Hill.Number" = integer(),
                                           "Diversity" = double())
  
  diversityTableQuadrat_rval <- reactiveVal(diversityTableQuadrat_init)

  output$diversityTableQuadrat <- reactable::renderReactable({

    diversityTableQuadrat <- reactable::reactable(data = diversityTableQuadrat_init,
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

    return(diversityTableQuadrat)

  })
  

# Initialise Species Richness Group Table ---------------------------------
  diversityTableGroup_init <- data.frame("Year" = integer(),
                                         "Group" = character(),
                                         "Metric" = character(),
                                         "Measure" = character(),
                                         "Hill.Number" = integer(),
                                         "Diversity" = double())
  
  diversityTableGroup_rval <- reactiveVal(diversityTableGroup_init)
  
  output$diversityTableGroup <- reactable::renderReactable({
    
    diversityTableGroup <- reactable::reactable(data = diversityTableGroup_init,
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
    
    return(diversityTableGroup)
    
  })


# Initialise Species Richness Site Table ----------------------------------
  diversityTableYear_init <- data.frame("Year" = integer(),
                                        "Metric" = character(),
                                        "Measure" = character(),
                                        "Hill.Number" = integer(),
                                        "Diversity" = double())
  
  diversityTableYear_rval <- reactiveVal(diversityTableYear_init)
  
  output$diversityTableYear <- reactable::renderReactable({
    
    diversityTableYear <- reactable::reactable(data = diversityTableYear_init,
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
    
    return(diversityTableYear)
    
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
    shiny::req(length(hillq()) > 0)
    shiny::req(length(divMetrics()) > 0)
    shiny::req(length(divMeasures()) > 0)
    
    shiny::isolate({
      
      hillq <- hillq()
      divMetrics <- divMetrics()
      divMeasures <- divMeasures()
      
      if(isTRUE(regional_availability()$aggTaxa) & "diversity" %in% aggTaxaOpts()){
        
        surveyData_long <- surveyData()$surveyData_long_prop_agg
        
      } else {
        
        surveyData_long <- surveyData()$surveyData_long_prop
        
      }
      
      higher_taxa <- higher_taxa()
      phylo_taxa_lookup <- phylo_taxa_lookup()
      phylo_tree <- phylo_tree()
      
    })
    
    assign(x = "hillq", value = hillq, envir = .GlobalEnv)
    assign(x = "divMetrics", value = divMetrics, envir = .GlobalEnv)
    assign(x = "divMeasures", value = divMeasures, envir = .GlobalEnv)
    assign(x = "surveyData_long", value = surveyData_long, envir = .GlobalEnv)
    assign(x = "higher_taxa", value = higher_taxa, envir = .GlobalEnv)
    assign(x = "phylo_taxa_lookup", value = phylo_taxa_lookup, envir = .GlobalEnv)
    assign(x = "phylo_tree", value = phylo_tree, envir = .GlobalEnv)
    
    if(any(is.na(surveyData_long$Cover))){
      surveyData_long$Cover <- 1
    }
    
    plot_data_year <- surveyData_long |>
      tidyr::nest(data = c(Year, Group, Quadrat, Species, Cover), .by = c("Year")) |>
      dplyr::summarise(
        "data" = list(dplyr::bind_rows(data)),
        .by = c(Year)
      )
    
    plot_data_group <- surveyData_long |>
      tidyr::nest(data = c(Year, Group, Quadrat, Species, Cover), .by = c("Year", "Group")) |>
      dplyr::summarise(
        "data" = list(dplyr::bind_rows(data)),
        .by = c(Year, Group)
      )
    
    plot_data_quadrat <- surveyData_long |>
      tidyr::nest(data = c(Year, Group, Quadrat, Species, Cover), .by = c("Year", "Group", "Quadrat")) |>
      dplyr::summarise(
        "data" = list(dplyr::bind_rows(data)),
        .by = c(Year, Group, Quadrat)
      )
    
    plot_data_year_results <- plot_data_year |>
      dplyr::rowwise() |>
      dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                          higher_taxa = higher_taxa, 
                                                                          phylo_tree = phylo_tree, 
                                                                          phylo_taxa_lookup = phylo_taxa_lookup)),
                    .keep = "unused") |>
      dplyr::cross_join(tibble::tibble("q" = as.numeric(hillq))) |>
      dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_meta(rdiv_objects = rdiv_objects,
                                                                          measures = divMeasures,
                                                                          metrics = divMetrics,
                                                                          q = q))) |>
      dplyr::ungroup() |>
      dplyr::select(-rdiv_objects, -q) |>
      tidyr::unnest(results) |>
      dplyr::select(
        "Year" = "Year",
        "Metric" = "dat_id",
        "Measure" = "measure",
        "Hill.Number" = "q",
        "Diversity" = "diversity"
      ) |>
      dplyr::mutate_if(.predicate = is.character,
                       .funs = list(~stringr::str_to_sentence(.))) |>
      suppressMessages()
    
    plot_data_group_results <- plot_data_group |>
      dplyr::rowwise() |>
      dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                          higher_taxa = higher_taxa, 
                                                                          phylo_tree = phylo_tree, 
                                                                          phylo_taxa_lookup = phylo_taxa_lookup)),
                    .keep = "unused") |>
      dplyr::cross_join(tibble::tibble("q" = as.numeric(hillq))) |>
      dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_meta(rdiv_objects = rdiv_objects, 
                                                                          measures = divMeasures,
                                                                          metrics = divMetrics,
                                                                          q = q))) |>
      dplyr::ungroup() |>
      dplyr::select(-rdiv_objects, -q) |>
      tidyr::unnest(results) |>
      dplyr::select(
        "Year" = "Year",
        "Group" = "Group",
        "Metric" = "dat_id",
        "Measure" = "measure",
        "Hill.Number" = "q",
        "Diversity" = "diversity"
      ) |>
      dplyr::mutate_if(.predicate = is.character,
                       .funs = list(~stringr::str_to_sentence(.))) |>
      suppressMessages()
    
    plot_data_quadrat_results <- plot_data_quadrat |>
      dplyr::rowwise() |>
      dplyr::mutate("rdiv_objects" = list(RMAVIS::calc_rdiversity_objects(plot_data = data, 
                                                                          higher_taxa = higher_taxa, 
                                                                          phylo_tree = phylo_tree, 
                                                                          phylo_taxa_lookup = phylo_taxa_lookup)),
                    .keep = "unused") |>
      dplyr::cross_join(tibble::tibble("q" = as.numeric(hillq))) |>
      dplyr::mutate("results" = list(RMAVIS::calc_rdiversity_metrics_subcom(rdiv_objects = rdiv_objects,
                                                                            measures = divMeasures,
                                                                            metrics = divMetrics,
                                                                            q = q))) |>
      dplyr::ungroup() |>
      dplyr::select(-rdiv_objects, -q) |>
      tidyr::unnest(results) |>
      dplyr::select(
        "Year" = "Year",
        "Group" = "Group",
        "Quadrat" = "Quadrat",
        "Metric" = "dat_id",
        "Measure" = "measure",
        "Hill.Number" = "q",
        "Diversity" = "diversity"
      ) |>
      dplyr::mutate_if(.predicate = is.character,
                       .funs = list(~stringr::str_to_sentence(.))) |>
      suppressMessages()
    

# Update diversityTableYear -----------------------------------------
    output$diversityTableYear <- reactable::renderReactable({
      
      diversityTableYear <- reactable::reactable(data = plot_data_year_results,
                                                 height = 800,
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
                                                     filterable = TRUE,
                                                     format = reactable::colFormat(digits = 0),
                                                     filterInput = dataListFilter("diversityTableYear")
                                                   ),
                                                   Metric = reactable::colDef(
                                                     filterable = TRUE,
                                                     filterInput = dataListFilter("diversityTableYear")
                                                   ),
                                                   Measure = reactable::colDef(
                                                     filterable = TRUE,
                                                     filterInput = dataListFilter("diversityTableYear")
                                                   ),
                                                   Hill.Number = reactable::colDef(
                                                     filterable = TRUE,
                                                     format = reactable::colFormat(digits = 0),
                                                     filterInput = dataListFilter("diversityTableYear")
                                                   ),
                                                   Diversity = reactable::colDef(
                                                     filterable = FALSE,
                                                     filterInput = dataListFilter("diversityTableYear")
                                                   )
                                                 ),
                                                 elementId = "diversityTableYear"
                                                 )
      
      return(diversityTableYear)
      
    })
    
    diversityTableYear_rval(plot_data_year_results)
    
    
    

# Update diversityTableGroup ----------------------------------------
    output$diversityTableGroup <- reactable::renderReactable({
      
      diversityTableGroup <- reactable::reactable(data = plot_data_group_results,
                                                  height = 800,
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
                                                      filterable = TRUE,
                                                      format = reactable::colFormat(digits = 0),
                                                      filterInput = dataListFilter("diversityTableGroup")
                                                    ),
                                                    Group = reactable::colDef(
                                                      filterable = TRUE,
                                                      filterInput = dataListFilter("diversityTableGroup")
                                                    ),
                                                    Metric = reactable::colDef(
                                                      filterable = TRUE,
                                                      filterInput = dataListFilter("diversityTableGroup")
                                                    ),
                                                    Measure = reactable::colDef(
                                                      filterable = TRUE,
                                                      filterInput = dataListFilter("diversityTableGroup")
                                                    ),
                                                    Hill.Number = reactable::colDef(
                                                      filterable = TRUE,
                                                      format = reactable::colFormat(digits = 0),
                                                      filterInput = dataListFilter("diversityTableGroup")
                                                    ),
                                                    Diversity = reactable::colDef(
                                                      filterable = FALSE
                                                    )
                                                  ),
                                                  elementId = "diversityTableGroup"
      )
      
      return(diversityTableGroup)
      
    })
    
    diversityTableGroup_rval(plot_data_group_results)
    
    

# Update diversityTableQuadrat --------------------------------------
    output$diversityTableQuadrat <- reactable::renderReactable({
      
      diversityTableQuadrat <- reactable::reactable(data = plot_data_quadrat_results,
                                                    height = 800,
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
                                                        filterable = TRUE,
                                                        format = reactable::colFormat(digits = 0),
                                                        filterInput = dataListFilter("diversityTableQuadrat")
                                                      ),
                                                      Group = reactable::colDef(
                                                        filterable = TRUE,
                                                        filterInput = dataListFilter("diversityTableQuadrat")
                                                      ),
                                                      Quadrat = reactable::colDef(
                                                        filterable = TRUE,
                                                        filterInput = dataListFilter("diversityTableQuadrat")
                                                      ),
                                                      Metric = reactable::colDef(
                                                        filterable = TRUE,
                                                        filterInput = dataListFilter("diversityTableQuadrat")
                                                      ),
                                                      Measure = reactable::colDef(
                                                        filterable = TRUE,
                                                        filterInput = dataListFilter("diversityTableQuadrat")
                                                      ),
                                                      Hill.Number = reactable::colDef(
                                                        filterable = TRUE,
                                                        format = reactable::colFormat(digits = 0),
                                                        filterInput = dataListFilter("diversityTableQuadrat")
                                                      ),
                                                      Diversity = reactable::colDef(
                                                        filterable = FALSE
                                                      )
                                                    ),
                                                    elementId = "diversityTableQuadrat"
                                                    )
      
      return(diversityTableQuadrat)
      
    })
    
    diversityTableQuadrat_rval(plot_data_quadrat_results)
    

# Compile All Data --------------------------------------------------------
    diversityData <- list("diversityYear" = diversityTableYear_rval(),
                          "diversityGroup" = diversityTableGroup_rval(),
                          "diversityQuadrat" = diversityTableQuadrat_rval())

    diversityData_rval(diversityData)
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE, 
              label = "calcDivMetrics")
  


# Ensure Tables Are Not Suspended Are Hidden ------------------------------
  outputOptions(output, "diversityTableYear", suspendWhenHidden = FALSE)
  outputOptions(output, "diversityTableGroup", suspendWhenHidden = FALSE)
  outputOptions(output, "diversityTableQuadrat", suspendWhenHidden = FALSE)
  
  

# Return Data -------------------------------------------------------------
  return(diversityData_rval)
  
}
