nvcAssignment <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()
  # nvcAssignMethods <- reactiveVal()
  resultsViewNVCAssign <- reactiveVal()

  observe({

    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)
    # nvcAssignMethods(sidebar_options()$nvcAssignMethods)
    resultsViewNVCAssign(sidebar_options()$resultsViewNVCAssign)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  # Show/Hide Results -------------------------------------------------------
  observe({
    
    shinyjs::show(id = "nvcAssignmentSiteTable_div")
    shinyjs::show(id = "nvcAssignmentGroupTable_div")
    shinyjs::show(id = "nvcAssignmentQuadratTable_div")
    
  }) |>
    bindEvent(resultsViewNVCAssign(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    if("nvcAssignSitePseudo" %in% resultsViewNVCAssign()){
      shinyjs::show(id = "nvcAssignmentSiteTable_div")
    } else {
      shinyjs::hide(id = "nvcAssignmentSiteTable_div")
    }
    
    if("nvcAssignGroupPseudo" %in% resultsViewNVCAssign()){
      shinyjs::show(id = "nvcAssignmentGroupTable_div")
    } else {
      shinyjs::hide(id = "nvcAssignmentGroupTable_div")
    }
    
    if("nvcAssignQuadratPseudo" %in% resultsViewNVCAssign()){
      shinyjs::show(id = "nvcAssignmentQuadratTable_div")
    } else {
      shinyjs::hide(id = "nvcAssignmentQuadratTable_div")
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
  
# Calculate nvcAssignment results by site ----------------------------------------
  nvcAssignmentQuadrat_rval <- reactiveVal()
  nvcAssignmentGroup_rval <- reactiveVal()
  nvcAssignmentSite_rval <- reactiveVal()
  
  observe({
    
    # req(isFALSE(runAnalysis() == 0))
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating NVC Community Similarity"
    )
    
    shiny::isolate({
      
      surveyTable <- surveyTable()
      
      # assign(x = "surveyTable", value = surveyTable, envir = .GlobalEnv)
      
      surveyTable_prepped <- surveyTable |>
        tidyr::unite(col = "ID", c("Year", "Group", "Quadrat"), sep = " - ", remove = FALSE) |>
        dplyr::rename("species" = "Species")
      
      # Create a concordance to join back on to the results of nva_average_sim
      surveyTable_IDs <- surveyTable_prepped |>
        dplyr::select(ID, Year, Group, Quadrat) |>
        dplyr::distinct()
      
      pquads_to_use <- nvc_pquads_final
      
      codes_regex <- c()

      for(code in habitatRestriction()){

        regex <- paste0("^", code, "\\d{1,}.+(?![a-z*][P])") #

        codes_regex <- c(codes_regex, regex)

        codes_regex <- stringr::str_c(codes_regex, collapse = "|")

      }
      
      
      if(!is.null(habitatRestriction())){
        
        pquads_to_use <- nvc_pquads_final |>
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
                      # "Standard.Deviation" = SD,
                      "NVC.Code" = NVC) |>
        dplyr::group_by(ID) |>
        dplyr::arrange(ID, dplyr::desc(Mean.Similarity)) |>
        dplyr::ungroup() |>
        dplyr::left_join(surveyTable_IDs, by = "ID")
      
      # assign(x = "nvcAssignmentQuadrat", value = nvcAssignmentQuadrat, envir = .GlobalEnv)
      
      nvcAssignmentQuadrat_prepped <- nvcAssignmentQuadrat |>
        # tidyr::unite(col = "ID", c("Year", "Group", "Quadrat"), sep = " - ", remove = TRUE) |>
        dplyr::select(Year, Group, Quadrat, NVC.Code, Mean.Similarity)|>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::slice(1:as.numeric(nTopResults())) |>
        dplyr::ungroup() |>
        dplyr::arrange(Year, Group, Quadrat, dplyr::desc(Mean.Similarity))
        
      nvcAssignmentQuadrat_rval(nvcAssignmentQuadrat_prepped)
      

      # Calculate NVC Similarity by Group
      nvcAssignmentGroup <- nvcAssignmentQuadrat |>
        dplyr::select(-ID) |>
        dplyr::group_by(Year, Group, NVC.Code) |>
        dplyr::summarise("Mean.Similarity" = mean(Mean.Similarity), .groups = "drop") |>
        dplyr::group_by(Year, Group) |>
        dplyr::arrange(Year, Group, dplyr::desc(Mean.Similarity)) |> #
        dplyr::slice(1:as.numeric(nTopResults())) |>
        dplyr::ungroup()
      
      nvcAssignmentGroup_prepped <- nvcAssignmentGroup |>
        # tidyr::unite(col = "ID", c("Year", "Group"), sep = " - ", remove = TRUE) |>
        dplyr::select(Year, Group, NVC.Code, Mean.Similarity) |>
        dplyr::arrange(Year, Group, dplyr::desc(Mean.Similarity))
      
      nvcAssignmentGroup_rval(nvcAssignmentGroup_prepped)


      # Calculate NVC Similarity by Site
      nvcAssignmentSite <- nvcAssignmentQuadrat |> #nvcAssignmentGroup |>
        dplyr::group_by(Year, NVC.Code) |>
        dplyr::summarise("Mean.Similarity" = mean(Mean.Similarity), .groups = "drop") |>
        dplyr::group_by(Year) |>
        dplyr::arrange(Year, dplyr::desc(Mean.Similarity)) |>
        dplyr::slice(1:as.numeric(nTopResults())) |>
        dplyr::ungroup()
      
      nvcAssignmentSite_prepped <- nvcAssignmentSite |>
        dplyr::select(Year, NVC.Code, Mean.Similarity) |>
        dplyr::arrange(Year, dplyr::desc(Mean.Similarity))
      
      nvcAssignmentSite_rval(nvcAssignmentSite_prepped)
      
    })
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE)


# Intialise NVC Assignment Site Table -------------------------------------
  nvcAssignmentSiteTable_init <- data.frame("Year" = integer(),
                                            "Mean.Similarity" = numeric(),
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
  
  

# Update NVC Assignment Site Table ----------------------------------------
  observe({
    
    # req(input$nvcAssignmentSiteTable)
    
    nvcAssignmentSite <- nvcAssignmentSite_rval()
    
    output$nvcAssignmentSiteTable <- reactable::renderReactable({
      
      nvcAssignmentSiteTable <- reactable::reactable(data = nvcAssignmentSite, 
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
                                                         filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                       return rows.filter(function(row) {
                                                                                       return row.values[columnId] == filterValue
                                                                                       })
                                                                                       }")
                                                       )
                                                     )
                                                     )
      
      return(nvcAssignmentSiteTable)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentSite_rval(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentSiteTable", suspendWhenHidden = FALSE)

# Initialise NVC Assignment Group Table -----------------------------------
  nvcAssignmentGroupTable_init <- data.frame("Year" = integer(),
                                             "Group" = character(),
                                             "Mean.Similarity" = numeric(),
                                             "NVC.Code" = character()
  )
  
  nvcAssignmentGroupTable_rval <- reactiveVal(nvcAssignmentGroupTable_init)
  
  output$nvcAssignmentGroupTable <- reactable::renderReactable({
    
    nvcAssignmentGroupTable <- reactable::reactable(data = nvcAssignmentGroupTable_init,
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
    
    return(nvcAssignmentGroupTable)
    
  })
  

# Update NVC Assignment Group Table ---------------------------------------
  observe({
    
    # req(input$nvcAssignmentGroupTable)
    
    nvcAssignmentGroup <- nvcAssignmentGroup_rval()
    
    output$nvcAssignmentGroupTable <- reactable::renderReactable({
      
      nvcAssignmentGroupTable <- reactable::reactable(data = nvcAssignmentGroup, 
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
      
      return(nvcAssignmentGroupTable)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentGroup_rval(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentGroupTable", suspendWhenHidden = FALSE)
  

# Initialise NVC Assignment Quadrat Table ---------------------------------
  nvcAssignmentQuadratTable_init <- data.frame("Year" = integer(),
                                               "Group" = character(),
                                               "Quadrat" = character(),
                                               "Mean.Similarity" = numeric(),
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
                                                        headerClass = "my-header",
                                                        class = "my-col",
                                                        align = "center" # Needed as alignment is not passing through to header
                                                      ))
    
    return(nvcAssignmentQuadratTable)
    
  })
  

# Update NVC Assignment Quadrat Table -------------------------------------
  observe({
    
    # req(input$nvcAssignmentQuadratTable)
    
    nvcAssignmentQuadrat <- nvcAssignmentQuadrat_rval()
    
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
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentQuadratTable", suspendWhenHidden = FALSE)
  
  


# Return NVC Assignment Data ----------------------------------------------
  return(nvcAssignmentSite_rval)
  
}
