nvcAssignment <- function(input, output, session, setupData, surveyData, surveyDataSummary, floristicTables, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  assignQuadrats <- reactiveVal(FALSE)
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()
  resultsViewNVCAssign <- reactiveVal()

  observe({

    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    assignQuadrats(sidebar_options()$assignQuadrats)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)
    resultsViewNVCAssign(sidebar_options()$resultsViewNVCAssign)

  }) |>
    bindEvent(sidebar_options(), 
              ignoreInit = TRUE)
  

# Create floristic tables list --------------------------------------------
  samp_ft_rval <- reactiveVal()
  comp_ft_rval <- reactiveVal()
  
  observe({
    
    shiny::req(!is.null(floristicTables()))
    
    shiny::isolate({
      setupData <- setupData()
      floristicTables <- floristicTables()
    })
    
    samp_ft_rval(floristicTables)
    comp_ft_rval(setupData$floristic_tables)
    
  }) |>
    bindEvent(floristicTables(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  
  
# Show/Hide Results -------------------------------------------------------
  observe({
    
    shinyjs::show(id = "nvcAssignmentPlot_Jaccard_div")
    shinyjs::show(id = "nvcAssignmentSiteTable_Czekanowski_div")
    shinyjs::show(id = "nvcAssignmentGroupTable_Czekanowski_div")

    
  }) |>
    bindEvent(resultsViewNVCAssign(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    if("nvcAssignPlotJaccard" %in% resultsViewNVCAssign()){
      shinyjs::show(id = "nvcAssignmentPlot_Jaccard_div")
    } else {
      shinyjs::hide(id = "nvcAssignmentPlot_Jaccard_div")
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
    bindEvent(assignQuadrats(),
              resultsViewNVCAssign(),
              ignoreInit = FALSE,
              ignoreNULL = FALSE)
  
  observe({
    
    shinyjs::show(id = "nvcAssignmentSiteTable_div")
    
  }) |>
    bindEvent(resultsViewNVCAssign(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  

# Calculate Quadrat Jaccard Similarities ----------------------------------
  nvcAssignmentPlot_Jaccard_rval <- reactiveVal()
  
  observe({
    
    shiny::req(surveyData())
    
    if(assignQuadrats() == TRUE){
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "#3F9280",
        text = "Calculating Similarities - Quadrats"
      )
      
    shiny::isolate({
      
      surveyData <- surveyData()
      setupData <- setupData()
      habitatRestriction <- habitatRestriction()
      
    })
    
    pquads_to_use <- setupData$pquads
    
    surveyData_long <- surveyData$surveyData_long
      
    # Add an ID column to the survey data table
    surveyData_prepped <- surveyData_long |>
      tidyr::unite(col = "ID", c("Year", "Group", "Quadrat"), sep = " - ", remove = FALSE) |>
      dplyr::rename("species" = "Species") 
    
    # Create a concordance to join back on to the results of nva_average_sim
    surveyData_IDs <- surveyData_prepped |>
      dplyr::select(ID, Year, Group, Quadrat) |>
      dplyr::distinct()
    
    if(!is.null(habitatRestriction)){
      
      pquads_to_use <- RMAVIS::subset_nvcData(nvc_data = pquads_to_use, habitatRestriction = habitatRestriction, col_name = "nvc_code")
      
    }
    
    # Calculate NVC Similarity by Quadrat
    nvcAssignmentPlot_Jaccard <- RMAVIS::similarityJaccard(samp_df = surveyData_prepped,
                                                           comp_df = pquads_to_use,
                                                           samp_species_col = "species",
                                                           comp_species_col = "nvc_taxon_name",
                                                           samp_group_name = "ID",
                                                           comp_group_name = "psq_id",
                                                           comp_groupID_name = "nvc_code",
                                                           remove_zero_matches = TRUE,
                                                           average_comp = TRUE) |>
      dplyr::select("ID" = ID,
                    "Mean.Similarity" = Similarity,
                    "NVC.Code" = nvc_code)|>
      dplyr::group_by(ID) |>
      dplyr::arrange(ID, dplyr::desc(Mean.Similarity)) |>
      dplyr::ungroup() |>
      dplyr::left_join(surveyData_IDs, by = "ID")
    
    nvcAssignmentPlot_Jaccard_prepped <- nvcAssignmentPlot_Jaccard |>
      dplyr::select(Year, Group, Quadrat, NVC.Code, Mean.Similarity)|>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::slice(1:10) |>
      dplyr::ungroup() |>
      dplyr::arrange(Year, Group, Quadrat, dplyr::desc(Mean.Similarity))
    
    nvcAssignmentPlot_Jaccard_rval(nvcAssignmentPlot_Jaccard_prepped)
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()
      
    } else if(assignQuadrats() == FALSE){
      
      nvcAssignmentPlot_Jaccard_rval(NULL)
      
    }
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = FALSE)

# Calculate Group and Year Czekanowski Similarities -----------------------
  nvcAssignmentSite_Czekanowski_rval <- reactiveVal()
  nvcAssignmentGroup_Czekanowski_rval <- reactiveVal()
  
  observe({
    
    shiny::req(!is.null(samp_ft_rval()))
    shiny::req(!is.null(comp_ft_rval()))
    shiny::req(surveyDataSummary())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating Similarities - Groups"
    )
    
    shiny::isolate({
      
      surveyDataSummary <- surveyDataSummary()
      habitatRestriction <- habitatRestriction()
      samp_ft <- samp_ft_rval()
      comp_ft <- comp_ft_rval()
      
    })
    
    # Retrieve the site and group ID's for which there are less than the threshold 
    threshold <- 2
    
    site_group_ids_remove <- surveyDataSummary$surveyDataStructure$quadratsPerID |>
      dplyr::filter(n < threshold) |>
      dplyr::pull(ID)
    
    # Prepare composed/sample floristic tables 
    samp_ft_all <- samp_ft$floristicTables_composed_all
    
    samp_ft_prepped <- samp_ft_all  |>
      dplyr::mutate(
        "Constancy" = 
          dplyr::case_when(
            Constancy == "I" ~ 1,
            Constancy == "II" ~ 2,
            Constancy == "III" ~ 3,
            Constancy == "IV" ~ 4,
            Constancy == "V" ~ 5,
            TRUE ~ as.numeric(0)
          )
      ) |> 
      dplyr::filter(!(ID %in% site_group_ids_remove))
    
    # Only use Czekanowski index to calculate site and group similarities if there are floristic tables composed from more than threshold quadrats.
    if(nrow(samp_ft_prepped) > 0){
      
      # Prepare comparison floristic tables
      if(!is.null(habitatRestriction)){
        
        comp_ft_prepped <- RMAVIS::subset_nvcData(nvc_data = comp_ft, habitatRestriction = habitatRestriction, col_name = "nvc_code")
        
      } else {
        
        comp_ft_prepped <- comp_ft
        
      }
      
      # Calculate NVC Similarity by Site using the Czekanowski index
      nvcAssignmentSiteGroup_Czekanowski <- RMAVIS::similarityCzekanowski(samp_df = samp_ft_prepped,
                                                                          comp_df = comp_ft_prepped,
                                                                          samp_species_col = "Species",
                                                                          comp_species_col = "nvc_taxon_name",
                                                                          samp_group_name = "ID",
                                                                          comp_group_name = "nvc_code",
                                                                          samp_weight_name = "Constancy",
                                                                          comp_weight_name = "constancy",
                                                                          downweight_threshold = 1, 
                                                                          downweight_value = 0.1)
      
      nvcAssignmentSite_Czekanowski <- nvcAssignmentSiteGroup_Czekanowski |>
        dplyr::filter(stringr::str_detect(string = ID, pattern = "^\\b[0-9_]+\\b$")) |>
        dplyr::mutate("Year" = ID) |>
        dplyr::select(Year, "NVC.Code" = "nvc_code", Similarity)|>
        dplyr::arrange(Year, dplyr::desc(Similarity))
      
      nvcAssignmentGroup_Czekanowski <- nvcAssignmentSiteGroup_Czekanowski |>
        dplyr::filter(stringr::str_detect(string = ID, pattern = "^\\b[0-9_]+\\b$", negate = TRUE)) |>
        dplyr::mutate("Year" = stringr::str_extract(string = ID, pattern = "\\d{4}")) |>
        dplyr::mutate("Group" = stringr::str_extract(string = ID, pattern = "(?<=\\s-\\s).*$")) |>
        dplyr::select(Year, Group, "NVC.Code" = "nvc_code", Similarity) |>
        dplyr::arrange(Year, Group, dplyr::desc(Similarity))
      
      nvcAssignmentSite_Czekanowski_rval(nvcAssignmentSite_Czekanowski)
      nvcAssignmentGroup_Czekanowski_rval(nvcAssignmentGroup_Czekanowski)
      
    } else {
      
      nvcAssignmentSite_Czekanowski_rval(NULL)
      nvcAssignmentGroup_Czekanowski_rval(NULL)
      
    }
    
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(comp_ft_rval(),
              samp_ft_rval(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  

# Initialise NVC Assignment Quadrat Table ---------------------------------
  nvcAssignmentPlot_JaccardTable_init <- data.frame("Year" = integer(),
                                                    "Group" = character(),
                                                    "Quadrat" = character(),
                                                    "Mean.Similarity" = numeric(),
                                                    "NVC.Code" = character()
                                                    )
  
  nvcAssignmentPlot_JaccardTable_rval <- reactiveVal(nvcAssignmentPlot_JaccardTable_init)
  
  output$nvcAssignmentPlot_JaccardTable <- reactable::renderReactable({
    
    nvcAssignmentPlot_JaccardTable <- reactable::reactable(data = nvcAssignmentPlot_JaccardTable_init,
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
    
    return(nvcAssignmentPlot_JaccardTable)
    
  })
  

# Update NVC Assignment Quadrat Table -------------------------------------
  observe({
    
    if(!is.null(nvcAssignmentPlot_Jaccard_rval())){
      
      nvcAssignmentPlot_Jaccard <- nvcAssignmentPlot_Jaccard_rval() |>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::slice(1:nTopResults()) |>
        dplyr::ungroup()
      
    } else if(is.null(nvcAssignmentPlot_Jaccard_rval())){
      
      nvcAssignmentPlot_Jaccard <- nvcAssignmentPlot_JaccardTable_init
      
    }
    
    output$nvcAssignmentPlot_JaccardTable <- reactable::renderReactable({
      
      nvcAssignmentPlot_JaccardTable <- reactable::reactable(data = nvcAssignmentPlot_Jaccard, 
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
      
      return(nvcAssignmentPlot_JaccardTable)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentPlot_Jaccard_rval(),
              nTopResults(),
              ignoreInit = TRUE, 
              ignoreNULL = FALSE)
  
  
  outputOptions(output, "nvcAssignmentPlot_JaccardTable", suspendWhenHidden = FALSE)
  
  # Initialise NVC Assignment Site Czekanowski Table -----------------------
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
    
    if(!is.null(nvcAssignmentSite_Czekanowski_rval())){
      
      nvcAssignmentSite_Czekanowski <- nvcAssignmentSite_Czekanowski_rval() |>
        dplyr::group_by(Year) |>
        dplyr::slice(1:nTopResults()) |>
        dplyr::ungroup()
      
    } else if(is.null(nvcAssignmentSite_Czekanowski_rval())){
      
      nvcAssignmentSite_Czekanowski <- nvcAssignmentSiteTable_Czekanowski_init
      
    }
    
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
    
    if(!is.null(nvcAssignmentGroup_Czekanowski_rval())){
      
      nvcAssignmentGroup_Czekanowski <- nvcAssignmentGroup_Czekanowski_rval() |>
        dplyr::group_by(Year, Group) |>
        dplyr::slice(1:nTopResults()) |>
        dplyr::ungroup()
      
    } else if(is.null(nvcAssignmentGroup_Czekanowski_rval())){
      
      nvcAssignmentGroup_Czekanowski <- nvcAssignmentGroupTable_Czekanowski_init
      
    }
    
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
              ignoreNULL = FALSE)
  
  
  outputOptions(output, "nvcAssignmentGroupTable_Czekanowski", suspendWhenHidden = FALSE)
  

# Compose All NVC Assignment Results --------------------------------------
  nvcAssignment_rval <- reactiveVal()
  
  observe({
    
    shiny::req(isTRUE(!is.null(nvcAssignmentPlot_Jaccard_rval()) | !is.null(nvcAssignmentSite_Czekanowski_rval())))
    
    if(!is.null(nvcAssignmentPlot_Jaccard_rval())){
      
      nvcAssignmentPlot_Jaccard <- nvcAssignmentPlot_Jaccard_rval() |>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::slice(1:nTopResults()) |>
        dplyr::ungroup()
      
    } else {
      
      nvcAssignmentPlot_Jaccard <- NULL
      
    }
    
    if(!is.null(nvcAssignmentSite_Czekanowski_rval())){
      
      nvcAssignmentSite_Czekanowski <- nvcAssignmentSite_Czekanowski_rval() |>
        dplyr::group_by(Year) |>
        dplyr::slice(1:nTopResults()) |>
        dplyr::ungroup()
      
      # Get all NVC communities and sub-communities from nvc assignment results
      NVC_communities_all <- nvcAssignmentSite_Czekanowski |>
        dplyr::pull(NVC.Code)
      
    } else {
      
      nvcAssignmentSite_Czekanowski <- NULL
      
      # Get all NVC communities and sub-communities from nvc assignment results
      NVC_communities_all <- nvcAssignmentPlot_Jaccard |>
        dplyr::pull(NVC.Code)
      
    }
    
    if(!is.null(nvcAssignmentGroup_Czekanowski_rval())){
      
      nvcAssignmentGroup_Czekanowski <- nvcAssignmentGroup_Czekanowski_rval() |>
        dplyr::group_by(Year, Group) |>
        dplyr::slice(1:nTopResults()) |>
        dplyr::ungroup()
      
    } else {
      
      nvcAssignmentGroup_Czekanowski <- NULL
        
    }
    
    # Get all NVC communities from community and sub-community codes
    NVC_communities_fromSubCom <- stringr::str_replace(string = NVC_communities_all, 
                                                       pattern = "(\\d)[^0-9]+$", 
                                                       replace = "\\1") |>
      unique()
    
    # Create data frame containing top-fitted NVC subcommunities and communities
    topNVCCommunities <- unique(c(NVC_communities_all, NVC_communities_fromSubCom))
    
    nvcAssignment_list <- list("nvcAssignmentPlot_Jaccard" = nvcAssignmentPlot_Jaccard,
                               "nvcAssignmentSite_Czekanowski" = nvcAssignmentSite_Czekanowski,
                               "nvcAssignmentGroup_Czekanowski" = nvcAssignmentGroup_Czekanowski,
                               "topNVCCommunities" = topNVCCommunities)
    
    nvcAssignment_rval(nvcAssignment_list)
    
  }) |>
    bindEvent(nTopResults(),
              nvcAssignmentSite_Czekanowski_rval(),
              nvcAssignmentGroup_Czekanowski_rval(),
              nvcAssignmentPlot_Jaccard_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)


# Return NVC Assignment Data ----------------------------------------------
  return(nvcAssignment_rval)
  
}
