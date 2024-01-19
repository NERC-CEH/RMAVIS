floristicTables <- function(input, output, session, surveyTable, surveyTableSummary, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  
  composedFloristicTable <- reactiveVal()
  nvcFloristicTable <- reactiveVal()
  matchSpecies <- reactiveVal()
  runAnalysis <- reactiveVal()

  observe({

    composedFloristicTable(sidebar_options()$composedFloristicTable)
    nvcFloristicTable(sidebar_options()$nvcFloristicTable)
    matchSpecies(sidebar_options()$matchSpecies)
    runAnalysis(sidebar_options()$runAnalysis)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

# Initialise Composed Floristic Tables ------------------------------------
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
  

# Create object containing all composed tables ----------------------------
  floristicTables_composed_all_rval <- reactiveVal()
  
  observe({
    
    shiny::req(surveyTable())

    surveyTable <- surveyTable()
    
    floristicTables_composed_all <- data.frame("ID" = character(),
                                               "Species" = character(),
                                               "Constancy" = factor())
    
# Create composed floristic tables across all groups ----------------------
    
    floristicTables_composed_year_group <- composeSyntopicTables(surveyTable = surveyTable, 
                                                                 group_cols = c("Year", "Group"), 
                                                                 species_col_name = "Species", 
                                                                 plot_col_name = "Quadrat")
    
    floristicTables_composed_year <- composeSyntopicTables(surveyTable = surveyTable, 
                                                           group_cols = c("Year"), 
                                                           species_col_name = "Species", 
                                                           plot_col_name = "Quadrat")
    
    floristicTables_composed_all <- rbind(floristicTables_composed_year, floristicTables_composed_year_group)
    
    floristicTables_composed_all_rval(floristicTables_composed_all)
    
    # assign(x = "floristicTables_composed_all", value = floristicTables_composed_all, envir = .GlobalEnv)

  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, ignoreNULL = TRUE)

# Create Composed Floristic Table -----------------------------------------
  observe({
    
    shiny::req(floristicTables_composed_all_rval())
    shiny::req(!is.null(composedFloristicTable()))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      
      floristicTables_composed_selected <- floristicTables_composed_all |>
        dplyr::filter(ID == composedFloristicTable()) |>
        dplyr::select(-ID)
      
      floristicTables_nvc <- nvc_floristic_tables |>
        dplyr::filter(NVC.Code == nvcFloristicTable()) |>
        dplyr::select(-NVC.Code) |>
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

    })
    
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
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "floristicTables_composed", suspendWhenHidden = FALSE)
  
## Create Composed Floristic Table Title -----------------------------------
  composedFloristicTableTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                        "Composed Floristic Table ",
                                                        "</font>") 
  )
  
  observe({
    
    shiny::req(surveyTableSummary())
    shiny::req(composedFloristicTable())
    
    quadratsPerID <- surveyTableSummary()$surveyTableStructure$quadratsPerID
    
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
    bindEvent(surveyTableSummary(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$composedFloristicTableTitle <- renderText({ 
    
    composedFloristicTableTitle <- composedFloristicTableTitle_rval()
    
    paste(composedFloristicTableTitle) 
    
  })
  

# NVC Floristic Tables ----------------------------------------------------


# Intialise NVC Floristic Table -------------------------------------------
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
  
  observe({
    
    # shiny::req(input$floristicTables_nvc)
    # shiny::req(input$floristicTables_composed)
    shiny::req(!is.null(composedFloristicTable()))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      floristicTables_composed_all <- floristicTables_composed_all_rval()
      
      floristicTables_composed_selected <- floristicTables_composed_all |>
        dplyr::filter(ID == composedFloristicTable()) |>
        dplyr::select(-ID)
      
      floristicTables_nvc <- nvc_floristic_tables |>
        dplyr::filter(NVC.Code == nvcFloristicTable()) |>
        dplyr::select(-NVC.Code) |>
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
      
    })
    
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
  
  
  ## Create Composed Floristic Table Title -----------------------------------
  nvcFloristicTableTitle_rval <- reactiveVal(paste("<font size=4.75>",
                                                   "NVC Floristic Table ",
                                                   "</font>") 
  )
  
  observe({
    
    shiny::req(surveyTableSummary())
    shiny::req(nvcFloristicTable())
    shiny::req(composedFloristicTable())
    
    nvcFloristicTableTitle <- paste("<font size=4.75>",
                                    nvcFloristicTable(),
                                    "</font>") 
    
    nvcFloristicTableTitle_rval(nvcFloristicTableTitle)
    
  }) |>
    bindEvent(surveyTableSummary(), 
              nvcFloristicTable(), 
              composedFloristicTable(),
              ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$nvcFloristicTableTitle <- renderText({ 
    
    nvcFloristicTableTitle <- nvcFloristicTableTitle_rval()
    
    paste(nvcFloristicTableTitle) 
    
  })
  

# Return Data -------------------------------------------------------------
  return(floristicTables_composed_all_rval)
  
}
