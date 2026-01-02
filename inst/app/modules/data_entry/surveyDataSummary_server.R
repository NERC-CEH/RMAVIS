surveyDataSummary <- function(input, output, session, setupData, surveyData) {
  
  ns <- session$ns
  
  # Retrieve Setup Data -----------------------------------------------------
  floristic_tables <- reactiveVal()
  ft_taxon_name_col <- reactiveVal()
  EIVs_available <- reactiveVal()
  phylo_taxa_lookup <- reactiveVal()
  
  observe({
    
    floristic_tables(setupData()$floristic_tables)
    ft_taxon_name_col(setupData()$ft_taxon_name_col)
    EIVs_available(setupData()$regional_availability$avgEIVs)
    phylo_taxa_lookup(setupData()$phylo_taxa_lookup)
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE)
  
  # Create Survey Data Structure Table data ------------------------------
  surveyDataStructure_rval <- reactiveVal()
  speciesDataAvailability_summary_rval <- reactiveVal()
  
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
    shiny::req(nrow(surveyData()$surveyData_long) > 0)
    
    shiny::isolate({
      surveyData_long <- surveyData()$surveyData_long
      floristic_tables <- floristic_tables()
      ft_taxon_name_col <- ft_taxon_name_col()
      phylo_taxa_lookup <- phylo_taxa_lookup()
    })
    
    speciesDataAvailability <- surveyData_long |>
      dplyr::select("Species") |>
      dplyr::distinct() |>
      dplyr::mutate(
        "Veg.Class" = 
          dplyr::case_when(
            Species %in% unique(floristic_tables[[ft_taxon_name_col]]) ~ "Yes",
            TRUE ~ as.character("No")
          )
      ) |>
      dplyr::mutate(
        "Phylo.Tree" = 
          dplyr::case_when(
            Species %in% (dplyr::filter(phylo_taxa_lookup, phylo == TRUE) |> dplyr::pull(taxon_name)) ~ "Yes",
            TRUE ~ as.character("No")
          )
      )
    
    if(isTRUE(EIVs_available())){
      
      hill_ellenberg_w_names <- RMAVIS::hill_ellenberg |>
        dplyr::left_join(UKVegTB::taxa_lookup, by = "TVK") |>
        dplyr::select("Species" = "recommended_taxon_name", `F`, L, N, R, S)
      
      speciesDataAvailability <- speciesDataAvailability |>
        dplyr::mutate(
          "EIV" = 
            dplyr::case_when(
              Species %in% unique(dplyr::filter(hill_ellenberg_w_names, !is.na(`F`)) |> dplyr::pull(Species)) ~ "Yes",
              TRUE ~ as.character("No")
            )
        )
      
    }
    
    speciesDataAvailability_rval(speciesDataAvailability)
    
    speciesDataAvailability_summary <- speciesDataAvailability |>
      tidyr::pivot_longer(cols = -Species,
                          names_to = "metric",
                          values_to = "value") |>
      dplyr::group_by(metric) |>
      dplyr::count(value) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(id_cols = metric,
                         names_from = value,
                         values_from = n,
                         values_fill = 0) |>
      dplyr::mutate("Percentage" = (Yes / (Yes + No)) * 100) |>
      tidyr::pivot_longer(cols = -metric) |>
      tidyr::pivot_wider(id_cols = name,
                         names_from = metric,
                         values_from = value) |>
      dplyr::select(" " = "name", dplyr::any_of(c("Veg.Class", "Phylo.Tree", "EIV")))
    
    
    speciesDataAvailability_summary_rval(speciesDataAvailability_summary)
    
  }) |>
    bindEvent(surveyData(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
# Initialise speciesDataAvailability Table ------------------------------
  speciesDataAvailabilityTable_init <- data.frame("Species" = integer(0),
                                                  "Veg.Class" = character(0),
                                                  "Phylo.Tree" = character(0),
                                                  "EIV" = character(0)
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
  
# Initialise speciesDataAvailabilitySummary Table ------------------------------
  speciesDataAvailability_summary_init <- data.frame("Species" = integer(0),
                                                     "Veg.Class" = character(0),
                                                     "Phylo.Tree" = character(0),
                                                     "EIV" = character(0)
  )
  
  speciesDataAvailability_summary_rval <- reactiveVal(speciesDataAvailability_summary_init)
  
  output$speciesDataAvailabilitySummaryTable <- reactable::renderReactable({
    
    speciesDataAvailabilitySummaryTable <- reactable::reactable(data = speciesDataAvailability_summary_init,
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
    
    return(speciesDataAvailabilitySummaryTable)
    
  })
  
  
  # Initialise quadratsPerYear Table ----------------------------------------
  quadratsPerYearTable_init <- data.frame("Year" = integer(),
                                          "n" = numeric(),
                                          "Czekanowski.Similarities.Calculable?" = character()
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
  quadratsPerYearGroupTable_init <- data.frame("Year" = integer(0),
                                               "Group" = character(0),
                                               "n" = numeric(0),
                                               "Czekanowski.Similarities.Calculable?" = character(0)
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
        "Czekanowski.Similarities.Calculable?" = 
          dplyr::case_when(
            n < 2 ~ "No",
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
        "Czekanowski.Similarities.Calculable?" = 
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
                                                           filterable = TRUE,
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
    
  }) |>
    bindEvent(speciesDataAvailability_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  # Update speciesDataAvailabilitySummary Table ----------------------------------
  observe({
    
    req(speciesDataAvailability_summary_rval())
    
    speciesDataAvailability_summary <- speciesDataAvailability_summary_rval()
    
    output$speciesDataAvailabilitySummaryTable <- reactable::renderReactable({
      
      speciesDataAvailabilitySummaryTable <- reactable::reactable(data = speciesDataAvailability_summary,
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
      
      return(speciesDataAvailabilitySummaryTable)
      
    })
    
  }) |>
    bindEvent(speciesDataAvailability_summary_rval(),
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
