nvcAssignment <- function(input, output, session, setupData, surveyTable, surveyTableSummary, floristicTables, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve Setup Data -----------------------------------------------------
  nvc_pquads_final <- reactiveVal()
  nvc_floristic_tables_numeric <- reactiveVal()
  
  observe({
    
    setupData <- setupData()
    
    nvc_pquads_final(setupData$nvc_pquads_final)
    nvc_floristic_tables_numeric(setupData$nvc_floristic_tables_numeric)
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE)
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()
  resultsViewNVCAssign <- reactiveVal()

  observe({

    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)
    resultsViewNVCAssign(sidebar_options()$resultsViewNVCAssign)

  }) |>
    bindEvent(sidebar_options(), 
              ignoreInit = TRUE)
  
# Show/Hide Results -------------------------------------------------------
  observe({
    
    shinyjs::show(id = "nvcAssignmentQuadratTable_Jaccard_div")
    shinyjs::show(id = "nvcAssignmentSiteTable_Czekanowski_div")
    shinyjs::show(id = "nvcAssignmentGroupTable_Czekanowski_div")

    
  }) |>
    bindEvent(resultsViewNVCAssign(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    if("nvcAssignQuadratJaccard" %in% resultsViewNVCAssign()){
      shinyjs::show(id = "nvcAssignmentQuadratTable_div")
    } else {
      shinyjs::hide(id = "nvcAssignmentQuadratTable_div")
    }
    
    if("nvcAssignSiteCzekanowski" %in% resultsViewNVCAssign()){
      shinyjs::show(id = "nvcAssignmentSiteTable_Czekanowski_div")
    } else {
      shinyjs::hide(id = "nvcAssignmentSiteTable_Czekanowski_div")
    }
    
    if("nvcAssignGroupCzekanowski" %in% resultsViewNVCAssign()){
      shinyjs::show(id = "nvcAssignmentGroupTable_Czekanowski_div")
    } else {
      shinyjs::hide(id = "nvcAssignmentGroupTable_Czekanowski_div")
    }
    
  }) |>
    bindEvent(resultsViewNVCAssign(),
              ignoreInit = FALSE,
              ignoreNULL = FALSE)
  
  observe({
    
    shinyjs::show(id = "nvcAssignmentSiteTable_div")
    
  }) |>
    bindEvent(resultsViewNVCAssign(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  

# Calculate ALL nvcAssignment results -------------------------------------
  nvcAssignmentQuadrat_rval <- reactiveVal()
  nvcAssignmentSite_Czekanowski_rval <- reactiveVal()
  nvcAssignmentGroup_Czekanowski_rval <- reactiveVal()
  
  observe({
    
    shiny::req(floristicTables())
    shiny::req(surveyTableSummary())
    
    # req(isFALSE(runAnalysis() == 0))
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating NVC Community Similarity"
    )
    
    shiny::isolate({
      
      surveyTable <- surveyTable()
      surveyTableSummary <- surveyTableSummary()
      
      # Retrieve the site and group ID's for which there are less than the threshold 
      threshold <- 5
      site_group_ids_remove <- surveyTableSummary$surveyTableStructure$quadratsPerID |>
        dplyr::filter(n < threshold) |>
        dplyr::pull(ID)
      
      # Add an ID column to the survey data table
      surveyTable_prepped <- surveyTable |>
        tidyr::unite(col = "ID", c("Year", "Group", "Quadrat"), sep = " - ", remove = FALSE) |>
        dplyr::rename("species" = "Species")
      
      # Create a concordance to join back on to the results of nva_average_sim
      surveyTable_IDs <- surveyTable_prepped |>
        dplyr::select(ID, Year, Group, Quadrat) |>
        dplyr::distinct()
      
      # Select the pseudo-quadrats to use in the NVC assignment process
      pquads_to_use <- nvc_pquads_final()
      
      if(!is.null(habitatRestriction())){
        
        codes_regex <- c()
        
        for(code in habitatRestriction()){
          
          regex <- paste0("^", code, "\\d{1,}.+(?![a-z*][P])")
          
          codes_regex <- c(codes_regex, regex)
          
          codes_regex <- stringr::str_c(codes_regex, collapse = "|")
          
        }
        
        pquads_to_use <- nvc_pquads_final() |>
          dplyr::filter(stringr::str_detect(string = Pid3, pattern = codes_regex))
        
      }

      # Calculate NVC Similarity by Quadrat
      nvcAssignmentQuadrat <- assignNVC::nvc_average_sim(samp_df = surveyTable_prepped,
                                                         comp_df = pquads_to_use,
                                                         spp_col = "species",
                                                         samp_id = "ID",
                                                         comp_id = "Pid3") |>
        dplyr::select("ID" = FOCAL_ID,
                      "Mean.Similarity" = MEAN_SIM, 
                      "Standard.Deviation" = SD,
                      "NVC.Code" = NVC) |>
        dplyr::group_by(ID) |>
        dplyr::arrange(ID, dplyr::desc(Mean.Similarity)) |>
        dplyr::ungroup() |>
        dplyr::left_join(surveyTable_IDs, by = "ID")
      
      nvcAssignmentQuadrat_prepped <- nvcAssignmentQuadrat |>
        dplyr::select(Year, Group, Quadrat, NVC.Code, Mean.Similarity, Standard.Deviation)|>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::slice(1:10) |>
        dplyr::ungroup() |>
        dplyr::arrange(Year, Group, Quadrat, dplyr::desc(Mean.Similarity))
        
      nvcAssignmentQuadrat_rval(nvcAssignmentQuadrat_prepped)
      
    })
    
    # Prepare composed floristicTables
    floristicTables <- floristicTables()
    
    floristicTables_prepped <- floristicTables  |>
      dplyr::mutate(
        "Constancy" = 
          dplyr::case_when(
            Constancy == "I" ~ 0.2,
            Constancy == "II" ~ 0.4,
            Constancy == "III" ~ 0.6,
            Constancy == "IV" ~ 0.8,
            Constancy == "V" ~ 1.0,
            TRUE ~ as.numeric(0)
          )
      ) |> 
      dplyr::filter(!(ID %in% site_group_ids_remove))
    
    # Prepare nvc_floristic_tables_numeric
    if(!is.null(habitatRestriction())){
      
      nvc_floristic_tables_numeric_prepped <- nvc_floristic_tables_numeric() |>
        dplyr::filter(stringr::str_detect(string = NVC.Code, pattern = codes_regex))
      
    } else {
      
      nvc_floristic_tables_numeric_prepped <- nvc_floristic_tables_numeric()
      
    }
    
    # Calculate NVC Similarity by Site using the Czekanowski index
    nvcAssignmentSiteGroup_Czekanowski <- similarityCzekanowski(samp_df = floristicTables_prepped,
                                                                comp_df = nvc_floristic_tables_numeric_prepped,
                                                                samp_species_col = "Species",
                                                                comp_species_col = "Species",
                                                                samp_group_name = "ID",
                                                                comp_group_name = "NVC.Code",
                                                                samp_weight_name = "Constancy",
                                                                comp_weight_name = "Constancy")
    
    nvcAssignmentSite_Czekanowski <- nvcAssignmentSiteGroup_Czekanowski |>
      dplyr::filter(stringr::str_detect(string = ID, pattern = "^\\b[0-9_]+\\b$")) |>
      dplyr::mutate("Year" = ID) |>
      dplyr::select(Year, NVC.Code, Similarity)|>
      dplyr::arrange(Year, dplyr::desc(Similarity))
    
    nvcAssignmentGroup_Czekanowski <- nvcAssignmentSiteGroup_Czekanowski |>
      dplyr::filter(stringr::str_detect(string = ID, pattern = "^\\b[0-9_]+\\b$", negate = TRUE)) |>
      dplyr::mutate("Year" = stringr::str_extract(string = ID, pattern = "\\d{4}")) |>
      dplyr::mutate("Group" = stringr::str_extract(string = ID, pattern = "(?<=\\s-\\s).*$")) |>
      dplyr::select(Year, Group, NVC.Code, Similarity) |>
      dplyr::arrange(Year, Group, dplyr::desc(Similarity))
    
    nvcAssignmentSite_Czekanowski_rval(nvcAssignmentSite_Czekanowski)
    nvcAssignmentGroup_Czekanowski_rval(nvcAssignmentGroup_Czekanowski)
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              # nvc_pquads_final(),
              # nvc_floristic_tables_numeric(),
              ignoreInit = TRUE)


# Intialise NVC Assignment Site Table -------------------------------------
  nvcAssignmentSiteTable_init <- data.frame("Year" = integer(),
                                            "Mean.Similarity" = numeric(),
                                            "Standard.Deviation" = numeric(),
                                            "NVC.Code" = character()
  )
  
  output$nvcAssignmentSiteTable <- reactable::renderReactable({
    
    nvcAssignmentSiteTable <- reactable::reactable(data = nvcAssignmentSiteTable_init,
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
    
    return(nvcAssignmentSiteTable)
    
  })
  

# Initialise NVC Assignment Quadrat Table ---------------------------------
  nvcAssignmentQuadratTable_init <- data.frame("Year" = integer(),
                                               "Group" = character(),
                                               "Quadrat" = character(),
                                               "Mean.Similarity" = numeric(),
                                               "Standard.Deviation" = numeric(),
                                               "NVC.Code" = character()
  )
  
  nvcAssignmentQuadratTable_rval <- reactiveVal(nvcAssignmentQuadratTable_init)
  
  output$nvcAssignmentQuadratTable <- reactable::renderReactable({
    
    nvcAssignmentQuadratTable <- reactable::reactable(data = nvcAssignmentQuadratTable_init,
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
    
    return(nvcAssignmentQuadratTable)
    
  })
  

# Update NVC Assignment Quadrat Table -------------------------------------
  observe({
    
    req(nvcAssignmentQuadrat_rval())
    
    nvcAssignmentQuadrat <- nvcAssignmentQuadrat_rval() |>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::slice(1:nTopResults()) |>
      dplyr::ungroup()
    
    output$nvcAssignmentQuadratTable <- reactable::renderReactable({
      
      nvcAssignmentQuadratTable <- reactable::reactable(data = nvcAssignmentQuadrat, 
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
      
      return(nvcAssignmentQuadratTable)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentQuadrat_rval(),
              nTopResults(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentQuadratTable", suspendWhenHidden = FALSE)
  
  # Intialise NVC Assignment Site Czekanowski Table -----------------------
  nvcAssignmentSiteTable_Czekanowski_init <- data.frame("Year" = integer(),
                                                        "Similarity" = numeric(),
                                                        "NVC.Code" = character()
  )
  
  output$nvcAssignmentSiteTable_Czekanowski <- reactable::renderReactable({
    
    nvcAssignmentSiteTable_Czekanowski <- reactable::reactable(data = nvcAssignmentSiteTable_Czekanowski_init,
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
    
    return(nvcAssignmentSiteTable_Czekanowski)
    
  })
  
  
  
  # Update NVC Assignment Site Czekanowski Table --------------------------
  observe({
    
    
    shiny::req(nvcAssignmentSite_Czekanowski_rval())
    
    nvcAssignmentSite_Czekanowski <- nvcAssignmentSite_Czekanowski_rval() |>
      dplyr::group_by(Year) |>
      dplyr::slice(1:nTopResults()) |>
      dplyr::ungroup()
    
    output$nvcAssignmentSiteTable_Czekanowski <- reactable::renderReactable({
      
      nvcAssignmentSiteTable_Czekanowski <- reactable::reactable(data = nvcAssignmentSite_Czekanowski, 
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
                                                                 )
      )
      
      return(nvcAssignmentSiteTable_Czekanowski)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentSite_Czekanowski_rval(), 
              nTopResults(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentSiteTable_Czekanowski", suspendWhenHidden = FALSE)
  
  
  # Initialise NVC Assignment Group Czekanowski Table ---------------------
  nvcAssignmentGroupTable_Czekanowski_init <- data.frame("Year" = integer(),
                                                         "Group" = character(),
                                                         "Similarity" = numeric(),
                                                         "NVC.Code" = character()
                                                         )
  
  nvcAssignmentGroupTable_Czekanowski_rval <- reactiveVal(nvcAssignmentGroupTable_Czekanowski_init)
  
  output$nvcAssignmentGroupTable_Czekanowski <- reactable::renderReactable({
    
    nvcAssignmentGroupTable_Czekanowski <- reactable::reactable(data = nvcAssignmentGroupTable_Czekanowski_init,
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
    
    return(nvcAssignmentGroupTable_Czekanowski)
    
  })
  
  
  # Update NVC Assignment Group CzekanowskiTable ------------------------
  observe({
    
    req(nvcAssignmentGroup_Czekanowski_rval())
    
    nvcAssignmentGroup_Czekanowski <- nvcAssignmentGroup_Czekanowski_rval() |>
      dplyr::group_by(Year, Group) |>
      dplyr::slice(1:nTopResults()) |>
      dplyr::ungroup()
    
    output$nvcAssignmentGroupTable_Czekanowski <- reactable::renderReactable({
      
      nvcAssignmentGroupTable_Czekanowski <- reactable::reactable(data = nvcAssignmentGroup_Czekanowski, 
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
                                                                  )
                  )
      
      return(nvcAssignmentGroupTable_Czekanowski)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentGroup_Czekanowski_rval(),
              nTopResults(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentGroupTable_Czekanowski", suspendWhenHidden = FALSE)
  

# Compose All NVC Assignment Results --------------------------------------
  nvcAssignmentAll_rval <- reactiveVal()
  
  observe({
    
    shiny::req(nvcAssignmentQuadrat_rval())
    
    # Select the top-N fitted commmunities
    nvcAssignmentQuadrat <- nvcAssignmentQuadrat_rval() |>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::slice(1:nTopResults()) |>
      dplyr::ungroup()
    
    nvcAssignmentSite_Czekanowski <- nvcAssignmentSite_Czekanowski_rval() |>
      dplyr::group_by(Year) |>
      dplyr::slice(1:nTopResults()) |>
      dplyr::ungroup()
    
    nvcAssignmentGroup_Czekanowski <- nvcAssignmentGroup_Czekanowski_rval() |>
      dplyr::group_by(Year, Group) |>
      dplyr::slice(1:nTopResults()) |>
      dplyr::ungroup()
    
    # Get all NVC communities and sub-communities from nvc assignment results
    NVC_communities_all <- nvcAssignmentSite_Czekanowski |>
      dplyr::pull(NVC.Code)
    
    # Get all NVC communities from community and sub-community codes
    NVC_communities_fromSubCom <- stringr::str_replace(string = NVC_communities_all, 
                                                       pattern = "(\\d)[^0-9]+$", 
                                                       replace = "\\1") |>
      unique()
    
    # Create data frame containing top-fitted NVC subcommunities and communities
    NVC_communities_final <- unique(c(NVC_communities_all, NVC_communities_fromSubCom))
    
    nvcAssignmentAll_list <- list("nvcAssignmentQuadrat" = nvcAssignmentQuadrat,
                                  "nvcAssignmentSite_Czekanowski" = nvcAssignmentSite_Czekanowski,
                                  "nvcAssignmentGroup_Czekanowski" = nvcAssignmentGroup_Czekanowski,
                                  "topNVCCommunities" = NVC_communities_final)
    
    nvcAssignmentAll_rval(nvcAssignmentAll_list)
    
  }) |>
    bindEvent(nTopResults(),
              nvcAssignmentSite_Czekanowski_rval(),
              nvcAssignmentGroup_Czekanowski_rval(),
              nvcAssignmentQuadrat_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)


# Return NVC Assignment Data ----------------------------------------------
  return(nvcAssignmentAll_rval)
  
}
