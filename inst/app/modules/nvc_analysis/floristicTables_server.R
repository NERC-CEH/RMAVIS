floristicTables <- function(input, output, session, setupData, surveyData, surveyDataSummary, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  
  removeLowFreqTaxa <- reactiveVal()
  floristicTablesView <- reactiveVal()
  floristicTablesSetView <- reactiveVal()
  composedFloristicTable <- reactiveVal()
  nvcFloristicTable <- reactiveVal()
  matchSpecies <- reactiveVal()
  runAnalysis <- reactiveVal()

  observe({

    removeLowFreqTaxa(sidebar_options()$removeLowFreqTaxa)
    floristicTablesView(sidebar_options()$floristicTablesView)
    floristicTablesSetView(sidebar_options()$floristicTablesSetView)
    composedFloristicTable(sidebar_options()$composedFloristicTable)
    nvcFloristicTable(sidebar_options()$nvcFloristicTable)
    matchSpecies(sidebar_options()$matchSpecies)
    runAnalysis(sidebar_options()$runAnalysis)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
# Retrieve Setup Data -----------------------------------------------------
  floristic_tables <- reactiveVal()
  community_attributes <- reactiveVal()
  
  observe({
    
    shiny::isolate({
      setupData <- setupData()
    })
    
    floristic_tables(setupData$floristic_tables)
    community_attributes(setupData$community_attributes)
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = FALSE)
  

# Create object containing all composed tables ----------------------------
  floristicTables_composed_all_rval <- reactiveVal()
  floristicTables_composed_all_wide_rval <- reactiveVal()
  floristicTables <- reactiveVal()
  
  observe({
    
    shiny::req(surveyData())

    shiny::isolate({
      surveyData <- surveyData()
      removeLowFreqTaxa <- removeLowFreqTaxa()
    })
    
    surveyData_long <- surveyData$surveyData_long
    
    floristicTables_composed_all <- data.frame("ID" = character(),
                                               "Species" = character(),
                                               "Constancy" = factor())
    
    if(removeLowFreqTaxa == TRUE){
      removeLowFreqTaxa_value <- 0.05
    } else if(removeLowFreqTaxa == FALSE){
      removeLowFreqTaxa_value <- NULL
    }
    
    ## Create composed floristic tables across all groups ----------------------
    floristicTables_composed_year_group <- RMAVIS::composeSyntopicTables(surveyData = surveyData_long, 
                                                                         group_cols = c("Year", "Group"), 
                                                                         species_col_name = "Species", 
                                                                         plot_col_name = "Quadrat",
                                                                         numeral_constancy = TRUE,
                                                                         remove_low_freq_taxa = removeLowFreqTaxa_value)
    
    floristicTables_composed_year <- RMAVIS::composeSyntopicTables(surveyData = surveyData_long, 
                                                                   group_cols = c("Year"), 
                                                                   species_col_name = "Species", 
                                                                   plot_col_name = "Quadrat",
                                                                   numeral_constancy = TRUE,
                                                                   remove_low_freq_taxa = removeLowFreqTaxa_value)
    
    floristicTables_composed_all <- rbind(floristicTables_composed_year, floristicTables_composed_year_group)
    
    floristicTables_composed_all_rval(floristicTables_composed_all)

    ## Create wide composed floristic tables -----------------------------------
    floristicTables_composed_all_wide <- floristicTables_composed_all |>
      dplyr::mutate("Year" = stringr::str_extract(string = ID, pattern = "\\d{4}")) |>
      dplyr::mutate("Group" = stringr::str_extract(string = ID, pattern = "(?<=\\s-\\s).*$"))
    
    floristicTables_composed_all_wide_year <- floristicTables_composed_all_wide |>
      dplyr::filter(is.na(Group)) |>
      tidyr::pivot_wider(id_cols = c(Species), names_from = Year, values_from = Constancy) |>
      dplyr::mutate("Group" = "All", .before = Species)
    
    floristicTables_composed_all_wide_yearGroup <- floristicTables_composed_all_wide |>
      dplyr::filter(!is.na(Group)) |>
      dplyr::arrange(Group) |>
      tidyr::pivot_wider(id_cols = c(Group, Species), names_from = Year, values_from = Constancy)
    
    floristicTables_composed_all_wide_all <- rbind(floristicTables_composed_all_wide_year,
                                                   floristicTables_composed_all_wide_yearGroup)
    
    
    floristicTables_composed_all_wide_rval(floristicTables_composed_all_wide_all)
    

    ## Compose Object to Return From Module ------------------------------------
    floristicTables(list("floristicTables_composed_all" = floristicTables_composed_all_rval(),
                         "floristicTables_composed_all_wide" = floristicTables_composed_all_wide_rval()))

  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)

  # Create Composed Floristic Table -----------------------------------------
  
  ## Initialise Composed Floristic Tables ------------------------------------
  floristicTables_composed_init <- data.frame("Species" = character(),
                                              "Constancy" = character())
  
  output$floristicTables_composed <- reactable::renderReactable({
    
    floristicTables_composed <-  reactable::reactable(data = floristicTables_composed_init,
                                                      filterable = FALSE,
                                                      pagination = FALSE, 
                                                      highlight = TRUE,
                                                      bordered = TRUE,
                                                      sortable = FALSE, 
                                                      wrap = FALSE,
                                                      resizable = TRUE,
                                                      class = "my-tbl",
                                                      # style = list(fontSize = "1rem"),
                                                      rowClass = "my-row",
                                                      defaultColDef = reactable::colDef(
                                                        headerClass = "my-header",
                                                        class = "my-col",
                                                        align = "center" # Needed as alignment is not passing through to header
                                                      )
    )
    
    return(floristicTables_composed)
    
  })

  ## Update Composed Floristic Tables ----------------------------------------
  observe({
    
    shiny::req(floristicTables_composed_all_rval())
    shiny::req(!is.null(composedFloristicTable()))
    shiny::req(!is.null(nvcFloristicTable()))
    shiny::req(nvcFloristicTable() != "")
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      composedFloristicTable <- composedFloristicTable()
      nvcFloristicTable <- nvcFloristicTable()
      floristic_tables <- floristic_tables()
      
    })
    
    floristicTables_composed_selected <- floristicTables_composed_all |>
      dplyr::filter(ID == composedFloristicTable) |>
      dplyr::select(-ID)
    
    floristicTables_nvc <- floristic_tables |>
      dplyr::filter(nvc_code == nvcFloristicTable) |>
      dplyr::select("Species" = "nvc_taxon_name", "Constancy" = "constancy") |>
      dplyr::mutate(
        "Constancy" = 
          dplyr::case_when(
            Constancy == 1 ~ "I",
            Constancy == 2 ~ "II",
            Constancy == 3 ~ "III",
            Constancy == 4 ~ "IV",
            Constancy == 5 ~ "V",
            TRUE ~ NA
          )
      ) |>
      dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
      dplyr::arrange(Constancy, Species)
    
    floristicTables_composed_compToNVC <- floristicTables_nvc |>
      dplyr::select(-Constancy) |>
      dplyr::left_join(floristicTables_composed_selected, by = "Species") |>
      dplyr::mutate(
        "Species" = 
          dplyr::case_when(
            is.na(Constancy) ~ "",
            TRUE ~ as.character(Species)
          )
      )
    
    if(matchSpecies() == "No"){
      
      floristicTables_composed_view <- floristicTables_composed_selected
      
    } else if(matchSpecies() == "compToNVC"){
      
      floristicTables_composed_view <- floristicTables_composed_compToNVC
      
    } else if(matchSpecies() == "NVCToComp"){
      
      floristicTables_composed_view <- floristicTables_composed_selected
      
    }
    
    output$floristicTables_composed <- reactable::renderReactable({

      floristicTables_composed <- reactable::reactable(data = floristicTables_composed_view, 
                                                       filterable = FALSE,
                                                       pagination = FALSE, 
                                                       highlight = TRUE,
                                                       bordered = TRUE,
                                                       sortable = FALSE, 
                                                       wrap = FALSE,
                                                       resizable = TRUE,
                                                       class = "my-tbl",
                                                       # style = list(fontSize = "1rem"),
                                                       rowClass = "my-row",
                                                       defaultColDef = reactable::colDef(
                                                         headerClass = "my-header",
                                                         class = "my-col",
                                                         align = "center" # Needed as alignment is not passing through to header
                                                       )
                                                       )

      return(floristicTables_composed)
      
      

    })
    
  }) |>
    bindEvent(floristicTables_composed_all_rval(), 
              matchSpecies(),
              nvcFloristicTable(),
              composedFloristicTable(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_composed", suspendWhenHidden = FALSE)
  
## Create Composed Floristic Table Title -----------------------------------
  composedFloristicTableTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                        "Composed Floristic Table ",
                                                        "</font>") 
  )
  
  observe({
    
    shiny::req(surveyDataSummary())
    shiny::req(composedFloristicTable())
    
    quadratsPerID <- surveyDataSummary()$surveyDataStructure$quadratsPerID
    
    composedFloristicTable_n <- quadratsPerID |>
      dplyr::filter(ID == composedFloristicTable()) |>
      dplyr::pull(n)
    
    composedFloristicTableTitle <- paste("<font size=4.75>",
                                         composedFloristicTable(),
                                         " (n = ",
                                         composedFloristicTable_n,
                                         ")",
                                         "</font>",
                                         sep = "") 
    
    composedFloristicTableTitle_rval(composedFloristicTableTitle)
    
  }) |>
    bindEvent(surveyDataSummary(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$composedFloristicTableTitle <- renderText({ 
    
    composedFloristicTableTitle <- composedFloristicTableTitle_rval()
    
    paste(composedFloristicTableTitle) 
    
  })
  
  # Create All Wide Composed Year Floristic Table ------------------------------
  
  ## Initialise All Wide Composed Year Floristic Table -------------------------
  floristicTablesWide_composed_init <- data.frame("Species" = character(),
                                                  "Constancy" = character())

  output$floristicTablesWide_composed <- reactable::renderReactable({

    floristicTablesWide_composed <-  reactable::reactable(data = floristicTablesWide_composed_init,
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
                                                          defaultColDef = reactable::colDef(
                                                            headerClass = "my-header",
                                                            class = "my-col",
                                                            align = "center" # Needed as alignment is not passing through to header
                                                          ),
                                                          columns = list(
                                                            Species = reactable::colDef(
                                                              minWidth = 275,
                                                              sticky = "left",
                                                              style = list(borderRight = "1px solid #eee"),
                                                              headerStyle = list(borderRight = "1px solid #eee")
                                                            )
                                                          )
       )

    return(floristicTablesWide_composed)

  })
  
  ## Update All Wide Composed Year Floristic Tables ---------------------------
  observe({
    
    shiny::req(floristicTables_composed_all_wide_rval())
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all_wide <- floristicTables_composed_all_wide_rval()
      
      floristicTables_composed_all_wide_selected <- floristicTables_composed_all_wide |>
        dplyr::filter(Group == floristicTablesSetView()) |>
        dplyr::select(-Group)
      
    }) # close isolate
    
    output$floristicTablesWide_composed <- reactable::renderReactable({

      floristicTablesWide_composed <- reactable::reactable(data = floristicTables_composed_all_wide_selected,
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
                                                           defaultColDef = reactable::colDef(
                                                             headerClass = "my-header",
                                                             class = "my-col",
                                                             align = "center" # Needed as alignment is not passing through to header
                                                           ),
                                                           columns = list(
                                                             Species = reactable::colDef(
                                                               minWidth = 275,
                                                               sticky = "left",
                                                               style = list(borderRight = "1px solid #eee"),
                                                               headerStyle = list(borderRight = "1px solid #eee")
                                                             )
                                                           )
          )

      return(floristicTablesWide_composed)

    })
    
  }) |>
    bindEvent(floristicTables_composed_all_wide_rval(),
              floristicTablesSetView(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTablesWide_composed", suspendWhenHidden = FALSE)
  
  ## Create All Wide Composed Year Floristic Table Title -----------------
  floristicTablesWide_composedTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                        "Composed Floristic Tables ",
                                                        "</font>") 
                                                        )
  
  observe({
    
    shiny::req(surveyDataSummary())
    shiny::req(floristicTables_composed_all_wide_rval())
    
    floristicTablesWide_composedTitle <- paste("<font size=4.75>",
                                               floristicTablesSetView(),
                                               "</font>",
                                               sep = "")

    floristicTablesWide_composedTitle_rval(floristicTablesWide_composedTitle)
    
  }) |>
    bindEvent(surveyDataSummary(), 
              floristicTablesSetView(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$floristicTablesWide_composedTitle <- renderText({ 
    
    floristicTablesWide_composedTitle <- floristicTablesWide_composedTitle_rval()
    
    paste(floristicTablesWide_composedTitle) 
    
  })

  # NVC Floristic Tables ----------------------------------------------------

  ## Intialise NVC Floristic Table -------------------------------------------
  floristicTables_nvc_init <- data.frame("Species" = character(),
                                         "Constancy" = character())
  
  output$floristicTables_nvc <- reactable::renderReactable({
    
    floristicTables_nvc <- reactable::reactable(data = floristicTables_nvc_init,
                                                filterable = FALSE,
                                                pagination = FALSE, 
                                                highlight = TRUE,
                                                bordered = TRUE,
                                                sortable = FALSE, 
                                                wrap = FALSE,
                                                resizable = TRUE,
                                                class = "my-tbl",
                                                # style = list(fontSize = "1rem"),
                                                rowClass = "my-row",
                                                defaultColDef = reactable::colDef(
                                                  headerClass = "my-header",
                                                  class = "my-col",
                                                  align = "center" # Needed as alignment is not passing through to header
                                                )
                                                )
    
    return(floristicTables_nvc)
    
  })
  

  ## Update NVC Floristic Table ----------------------------------------------
  observe({
    
    shiny::req(!is.null(composedFloristicTable()))
    shiny::req(!is.null(nvcFloristicTable()))
    shiny::req(nvcFloristicTable() != "")
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      composedFloristicTable <- composedFloristicTable()
      nvcFloristicTable <- nvcFloristicTable()
      floristic_tables <- floristic_tables()
      
    })
      
    floristicTables_composed_selected <- floristicTables_composed_all |>
      dplyr::filter(ID == composedFloristicTable) |>
      dplyr::select(-ID)
    
    floristicTables_nvc <- floristic_tables |>
      dplyr::filter(nvc_code == nvcFloristicTable) |>
      dplyr::select("Species" = "nvc_taxon_name", "Constancy" = "constancy") |>
      dplyr::mutate(
        "Constancy" = 
          dplyr::case_when(
            Constancy == 1 ~ "I",
            Constancy == 2 ~ "II",
            Constancy == 3 ~ "III",
            Constancy == 4 ~ "IV",
            Constancy == 5 ~ "V",
            TRUE ~ NA
          )
      ) |>
      dplyr::mutate("Constancy" = factor(Constancy, levels = c("V", "IV", "III", "II", "I"))) |>
      dplyr::arrange(Constancy, Species)

    floristicTables_nvc_NVCToComp <- floristicTables_composed_selected |>
      dplyr::select(-Constancy) |>
      dplyr::left_join(floristicTables_nvc, by = "Species") |>
      dplyr::mutate(
        "Species" =
          dplyr::case_when(
            is.na(Constancy) ~ "",
            TRUE ~ as.character(Species)
          )
      )
    
    if(matchSpecies() == "No"){
      
      floristicTables_nvc_view <- floristicTables_nvc
      
    } else if(matchSpecies() == "compToNVC"){
      
      floristicTables_nvc_view <- floristicTables_nvc
      
    } else if(matchSpecies() == "NVCToComp"){
      
      floristicTables_nvc_view <- floristicTables_nvc_NVCToComp
      
    }
    
    output$floristicTables_nvc <- reactable::renderReactable({
      
      floristicTables_nvc <- reactable::reactable(data = floristicTables_nvc_view, 
                                                  filterable = FALSE,
                                                  pagination = FALSE, 
                                                  highlight = TRUE,
                                                  bordered = TRUE,
                                                  sortable = FALSE, 
                                                  wrap = FALSE,
                                                  resizable = TRUE,
                                                  class = "my-tbl",
                                                  # style = list(fontSize = "1rem"),
                                                  rowClass = "my-row",
                                                  defaultColDef = reactable::colDef(
                                                    headerClass = "my-header",
                                                    class = "my-col",
                                                    align = "center" # Needed as alignment is not passing through to header
                                                  )
                                                  )
      
      return(floristicTables_nvc)
      
    })
    
  }) |>
    bindEvent(floristicTables_composed_all_rval(),
              nvcFloristicTable(), 
              matchSpecies(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_nvc", suspendWhenHidden = FALSE)
  
  
  ## Create NVC Floristic Table Title -----------------------------------
  nvcFloristicTableTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                   "NVC Floristic Table ",
                                                   "</font>") 
  )
  
  observe({
    
    shiny::req(surveyDataSummary())
    shiny::req(nvcFloristicTable())
    shiny::req(composedFloristicTable())
    
    shiny::isolate({
      community_attributes <- community_attributes()
    })
    
    nvcFloristicTable_n <- community_attributes |>
      dplyr::filter(nvc_code == nvcFloristicTable()) |>
      dplyr::pull(num_samples)
    
    nvcFloristicTableTitle <- paste("<font size=4.75>",
                                    nvcFloristicTable(),
                                    " (n = ",
                                    nvcFloristicTable_n,
                                    ")",
                                    "</font>",
                                    sep = "") 
    
    nvcFloristicTableTitle_rval(nvcFloristicTableTitle)
    
  }) |>
    bindEvent(surveyDataSummary(), 
              nvcFloristicTable(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$nvcFloristicTableTitle <- renderText({ 
    
    nvcFloristicTableTitle <- nvcFloristicTableTitle_rval()
    
    paste(nvcFloristicTableTitle) 
    
  })
  

  # Show/Hide Selected Tables -----------------------------------------------
  observe({
    
    shinyjs::show(id = "singleComposedVsNVC_div")
    shinyjs::show(id = "multipleComposed_div")
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    if(floristicTablesView() == "singleComposedVsNVC") {
      
      shinyjs::show(id = "singleComposedVsNVC_div")
      shinyjs::hide(id = "multipleComposed_div")
      
    } else if(floristicTablesView() == "multipleComposed") {
      
      shinyjs::hide(id = "singleComposedVsNVC_div")
      shinyjs::show(id = "multipleComposed_div")
      
    }
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreInit = FALSE)
  
  observe({
    
    shinyjs::show(id = "singleComposedVsNVC_div")
    
  }) |>
    bindEvent(floristicTablesView(), 
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  

  # Return Data -------------------------------------------------------------
  return(floristicTables)
  
}
