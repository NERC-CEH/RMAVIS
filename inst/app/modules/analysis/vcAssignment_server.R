vcAssignment <- function(input, output, session, setupData, surveyData, surveyDataSummary, floristicTables, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  aggTaxaOpts <- reactiveVal()
  coverMethod <- reactiveVal()
  assignQuadrats <- reactiveVal(FALSE)
  habitatRestriction <- reactiveVal()
  resultsViewVCAssign <- reactiveVal()

  observe({

    runAnalysis(sidebar_options()$runAnalysis)
    aggTaxaOpts(sidebar_options()$aggTaxaOpts)
    coverMethod(sidebar_options()$coverMethod)
    assignQuadrats(sidebar_options()$assignQuadrats)
    habitatRestriction(sidebar_options()$habitatRestriction)
    resultsViewVCAssign(sidebar_options()$resultsViewVCAssign)

  }) |>
    bindEvent(sidebar_options(), 
              ignoreInit = TRUE)

# Retrieve setup data -----------------------------------------------------
  regional_availability <- reactiveVal()
  ft_taxon_name_col <- reactiveVal()
  psq_taxon_name_col <- reactiveVal()
  unit_name_col <- reactiveVal()
  hab_rest_pref <- reactiveVal()
  
  observe({
    regional_availability(setupData()$regional_availability)
    ft_taxon_name_col(setupData()$ft_taxon_name_col)
    psq_taxon_name_col(setupData()$psq_taxon_name_col)
    unit_name_col(setupData()$unit_name_col)
    hab_rest_pref(setupData()$hab_rest_pref)
    
  }) |>
    shiny::bindEvent(setupData(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)

# Create floristic tables list --------------------------------------------
  samp_ft_rval <- reactiveVal()
  trigger <- reactiveVal(0)

  observe({

    shiny::req(!is.null(floristicTables()))

    shiny::isolate({
      floristicTables <- floristicTables()
    })
    
    trigger(trigger() + 1)

    samp_ft_rval(floristicTables)

  }) |>
    bindEvent(runAnalysis(),
              floristicTables(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  
  
# Show/Hide Results -------------------------------------------------------
  observe({
    
    shinyjs::show(id = "vcAssignmentPlot_Jaccard_div")
    shinyjs::show(id = "vcAssignmentSiteTable_Czekanowski_div")
    shinyjs::show(id = "vcAssignmentGroupTable_Czekanowski_div")

    
  }) |>
    bindEvent(resultsViewVCAssign(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  
  observe({
    
    if("vcAssignPlotJaccard" %in% resultsViewVCAssign()){
      shinyjs::show(id = "vcAssignmentPlot_Jaccard_div")
    } else {
      shinyjs::hide(id = "vcAssignmentPlot_Jaccard_div")
    }
    
    if("vcAssignSiteCzekanowski" %in% resultsViewVCAssign()){
      shinyjs::show(id = "vcAssignmentSiteTable_Czekanowski_div")
    } else {
      shinyjs::hide(id = "vcAssignmentSiteTable_Czekanowski_div")
    }
    
    if("vcAssignGroupCzekanowski" %in% resultsViewVCAssign()){
      shinyjs::show(id = "vcAssignmentGroupTable_Czekanowski_div")
    } else {
      shinyjs::hide(id = "vcAssignmentGroupTable_Czekanowski_div")
    }
    
  }) |>
    bindEvent(assignQuadrats(),
              resultsViewVCAssign(),
              ignoreInit = FALSE,
              ignoreNULL = FALSE)
  
  observe({
    
    shinyjs::show(id = "vcAssignmentSiteTable_div")
    
  }) |>
    bindEvent(resultsViewVCAssign(),
              ignoreNULL = FALSE,
              ignoreInit = FALSE,
              once = TRUE)
  

# Calculate Quadrat Jaccard Similarities ----------------------------------
  vcAssignmentPlot_Jaccard_rval <- reactiveVal()
  
  observe({
    
    shiny::req(surveyData())
    shiny::req(setupData())
    shiny::req(aggTaxaOpts())
    
    if(assignQuadrats() == TRUE){
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "#3F9280",
        text = "Calculating Similarities - Quadrats"
      )
      
    shiny::isolate({
      
      pquads_to_use <- setupData()$pquads
      habitatRestriction <- habitatRestriction()
      hab_rest_pref <- hab_rest_pref()
      unit_name_col <- unit_name_col()
      
      if(isTRUE(regional_availability()$aggTaxa) & "vc_assign" %in% aggTaxaOpts()){
        
        surveyData_long <- surveyData()$surveyData_long_prop_agg
        
      } else {
        
        surveyData_long <- surveyData()$surveyData_long_prop
        
      }
      
    })
    
    # Add an ID column to the survey data table
    surveyData_prepped <- surveyData_long |>
      tidyr::unite(col = "ID", c("Year", "Group", "Quadrat"), sep = " - ", remove = FALSE) |>
      dplyr::rename("species" = "Species") 
    
    # Create a concordance to join back on to the results
    surveyData_IDs <- surveyData_prepped |>
      dplyr::select(ID, Year, Group, Quadrat) |>
      dplyr::distinct()
    
    if(!is.null(habitatRestriction)){
      
      pquads_to_use <- RMAVIS::subset_vcData(vc_data = pquads_to_use, 
                                             habitatRestriction = habitatRestriction, 
                                             col_name = unit_name_col, 
                                             habitatRestrictionPrefixes = as.list(hab_rest_pref))
      
    }
    
    # Calculate VC Similarity by Quadrat
    vcAssignmentPlot_Jaccard <- RMAVIS::similarityJaccard(samp_df = surveyData_prepped,
                                                          comp_df = pquads_to_use,
                                                          samp_species_col = "species",
                                                          comp_species_col = psq_taxon_name_col(),
                                                          samp_group_name = "ID",
                                                          comp_group_name = "psq_id",
                                                          comp_groupID_name = unit_name_col(),
                                                          remove_zero_matches = TRUE,
                                                          average_comp = TRUE) |>
      dplyr::select("ID" = ID,
                    "Mean.Similarity" = Similarity,
                    "VC.Code" = unit_name_col())|>
      dplyr::left_join(surveyData_IDs, by = "ID")
    
    vcAssignmentPlot_Jaccard_prepped <- vcAssignmentPlot_Jaccard |>
      dplyr::select(Year, Group, Quadrat, VC.Code, Mean.Similarity)|>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::mutate("Rank" = rank(-Mean.Similarity), .before = "Mean.Similarity") |>
      dplyr::ungroup() |>
      dplyr::arrange(Year, Group, Quadrat, Rank)
    
    vcAssignmentPlot_Jaccard_rval(vcAssignmentPlot_Jaccard_prepped)
    
    # Stop busy spinner
    shinybusy::remove_modal_spinner()
      
    } else if(assignQuadrats() == FALSE){
      
      vcAssignmentPlot_Jaccard_rval(NULL)
      
    }
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = FALSE)

# Calculate Group and Year Czekanowski Similarities -----------------------
  vcAssignmentSite_Czekanowski_rval <- reactiveVal()
  vcAssignmentGroup_Czekanowski_rval <- reactiveVal()
  
  observe({
    
    shiny::req(!is.null(samp_ft_rval()))
    shiny::req(!is.null(setupData()$floristic_tables))
    shiny::req(surveyDataSummary())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating Similarities - Groups"
    )
    
    shiny::isolate({
      
      surveyDataSummary <- surveyDataSummary()
      habitatRestriction <- habitatRestriction()
      hab_rest_pref <- hab_rest_pref()
      samp_ft <- samp_ft_rval()
      comp_ft <- setupData()$floristic_tables
      
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
        
        comp_ft_prepped <- RMAVIS::subset_vcData(vc_data = comp_ft, habitatRestriction = habitatRestriction, col_name = unit_name_col(), habitatRestrictionPrefixes = as.list(hab_rest_pref))
        
      } else {
        
        comp_ft_prepped <- comp_ft
        
      }
      
      # Calculate VC Similarity by Site using the Czekanowski index
      vcAssignmentSiteGroup_Czekanowski <- RMAVIS::similarityCzekanowski(samp_df = samp_ft_prepped,
                                                                         comp_df = comp_ft_prepped,
                                                                         samp_species_col = "Species",
                                                                         comp_species_col = ft_taxon_name_col(),
                                                                         samp_group_name = "ID",
                                                                         comp_group_name = unit_name_col(),
                                                                         samp_weight_name = "Constancy",
                                                                         comp_weight_name = "constancy",
                                                                         downweight_threshold = 1, 
                                                                         downweight_value = 0.1)
      
      vcAssignmentSite_Czekanowski <- vcAssignmentSiteGroup_Czekanowski |>
        dplyr::filter(stringr::str_detect(string = ID, pattern = "^\\b[0-9_]+\\b$")) |>
        dplyr::mutate("Year" = ID) |>
        dplyr::select(Year, "VC.Code" = unit_name_col(), Similarity) |>
        dplyr::group_by(Year) |>
        dplyr::mutate("Rank" = rank(-Similarity), .before = "Similarity") |>
        dplyr::ungroup() |>
        dplyr::arrange(Year, Rank)
      
      vcAssignmentGroup_Czekanowski <- vcAssignmentSiteGroup_Czekanowski |>
        dplyr::filter(stringr::str_detect(string = ID, pattern = "^\\b[0-9_]+\\b$", negate = TRUE)) |>
        dplyr::mutate("Year" = stringr::str_extract(string = ID, pattern = "\\d{4}")) |>
        dplyr::mutate("Group" = stringr::str_extract(string = ID, pattern = "(?<=\\s-\\s).*$")) |>
        dplyr::select(Year, Group, "VC.Code" = unit_name_col(), Similarity) |>
        dplyr::group_by(Year, Group) |>
        dplyr::mutate("Rank" = rank(-Similarity), .before = "Similarity") |>
        dplyr::ungroup() |>
        dplyr::arrange(Year, Group, Rank)
      
      vcAssignmentSite_Czekanowski_rval(vcAssignmentSite_Czekanowski)
      vcAssignmentGroup_Czekanowski_rval(vcAssignmentGroup_Czekanowski)
      
    } else {
      
      vcAssignmentSite_Czekanowski_rval(NULL)
      vcAssignmentGroup_Czekanowski_rval(NULL)
      
    }
    
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(trigger(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  

# Initialise VC Assignment Quadrat Table ---------------------------------
  vcAssignmentPlot_JaccardTable_init <- data.frame("Year" = integer(),
                                                   "Group" = character(),
                                                   "Quadrat" = character(),
                                                   "Rank" = integer(),
                                                   "Mean.Similarity" = numeric(),
                                                   "VC.Code" = character()
                                                   )
  
  vcAssignmentPlot_JaccardTable_rval <- reactiveVal(vcAssignmentPlot_JaccardTable_init)
  
  output$vcAssignmentPlot_JaccardTable <- reactable::renderReactable({
    
    vcAssignmentPlot_JaccardTable <- reactable::reactable(data = vcAssignmentPlot_JaccardTable_init,
                                                          filterable = FALSE,
                                                          pagination = TRUE, 
                                                          defaultPageSize = 30,
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
    
    return(vcAssignmentPlot_JaccardTable)
    
  })
  

# Update VC Assignment Quadrat Table -------------------------------------
  observe({
    
    if(!is.null(vcAssignmentPlot_Jaccard_rval())){
      
      vcAssignmentPlot_Jaccard <- vcAssignmentPlot_Jaccard_rval()
      
    } else if(is.null(vcAssignmentPlot_Jaccard_rval())){
      
      vcAssignmentPlot_Jaccard <- vcAssignmentPlot_JaccardTable_init
      
    }
    
    output$vcAssignmentPlot_JaccardTable <- reactable::renderReactable({
      
      vcAssignmentPlot_JaccardTable <- reactable::reactable(data = vcAssignmentPlot_Jaccard, 
                                                            filterable = FALSE,
                                                            pagination = TRUE,
                                                            defaultPageSize = 30,
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
                                                             VC.Code = reactable::colDef(
                                                               filterable = TRUE
                                                             ),
                                                             Rank = reactable::colDef(
                                                               format = reactable::colFormat(digits = 0),
                                                               filterable = TRUE,
                                                               filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                            return rows.filter(function(row) {
                                                                                            return row.values[columnId] <= filterValue
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
      
      return(vcAssignmentPlot_JaccardTable)
      
    })
    
  }) |>
    bindEvent(vcAssignmentPlot_Jaccard_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = FALSE)
  
  
  outputOptions(output, "vcAssignmentPlot_JaccardTable", suspendWhenHidden = FALSE)
  
  # Initialise VC Assignment Site Czekanowski Table -----------------------
  vcAssignmentSiteTable_Czekanowski_init <- data.frame("Year" = integer(),
                                                       "Rank" = integer(),
                                                       "Similarity" = numeric(),
                                                       "VC.Code" = character()
                                                       )
  
  output$vcAssignmentSiteTable_Czekanowski <- reactable::renderReactable({
    
    vcAssignmentSiteTable_Czekanowski <- reactable::reactable(data = vcAssignmentSiteTable_Czekanowski_init,
                                                              filterable = FALSE,
                                                              pagination = TRUE, 
                                                              defaultPageSize = 30,
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
        return(vcAssignmentSiteTable_Czekanowski)
    
  })
  
  # Update VC Assignment Site Czekanowski Table --------------------------
  observe({
    
    if(!is.null(vcAssignmentSite_Czekanowski_rval())){
      
      vcAssignmentSite_Czekanowski <- vcAssignmentSite_Czekanowski_rval()
      
    } else if(is.null(vcAssignmentSite_Czekanowski_rval())){
      
      vcAssignmentSite_Czekanowski <- vcAssignmentSiteTable_Czekanowski_init
      
    }
    
    output$vcAssignmentSiteTable_Czekanowski <- reactable::renderReactable({
      
      vcAssignmentSiteTable_Czekanowski <- reactable::reactable(data = vcAssignmentSite_Czekanowski, 
                                                                filterable = FALSE,
                                                                pagination = TRUE, 
                                                                defaultPageSize = 30,
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
                                                                 VC.Code = reactable::colDef(
                                                                   filterable = TRUE
                                                                 ),
                                                                 Rank = reactable::colDef(
                                                                   format = reactable::colFormat(digits = 0),
                                                                   filterable = TRUE,
                                                                   filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                                return rows.filter(function(row) {
                                                                                                return row.values[columnId] <= filterValue
                                                                                                })
                                                                                                }")
                                                                 )
                                                                )
      )
      
      return(vcAssignmentSiteTable_Czekanowski)
      
    })
    
  }) |>
    bindEvent(vcAssignmentSite_Czekanowski_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "vcAssignmentSiteTable_Czekanowski", suspendWhenHidden = FALSE)
  
  
  # Initialise VC Assignment Group Czekanowski Table ---------------------
  vcAssignmentGroupTable_Czekanowski_init <- data.frame("Year" = integer(),
                                                        "Group" = character(),
                                                        "Rank" = integer(),
                                                        "Similarity" = numeric(),
                                                        "VC.Code" = character()
                                                        )
  
  vcAssignmentGroupTable_Czekanowski_rval <- reactiveVal(vcAssignmentGroupTable_Czekanowski_init)
  
  output$vcAssignmentGroupTable_Czekanowski <- reactable::renderReactable({
    
    vcAssignmentGroupTable_Czekanowski <- reactable::reactable(data = vcAssignmentGroupTable_Czekanowski_init,
                                                               filterable = FALSE,
                                                               pagination = TRUE, 
                                                               defaultPageSize = 30,
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
    
    return(vcAssignmentGroupTable_Czekanowski)
    
  })
  
  # Update VC Assignment Group CzekanowskiTable ------------------------
  observe({
    
    if(!is.null(vcAssignmentGroup_Czekanowski_rval())){
      
      vcAssignmentGroup_Czekanowski <- vcAssignmentGroup_Czekanowski_rval()
      
    } else if(is.null(vcAssignmentGroup_Czekanowski_rval())){
      
      vcAssignmentGroup_Czekanowski <- vcAssignmentGroupTable_Czekanowski_init
      
    }
    
    output$vcAssignmentGroupTable_Czekanowski <- reactable::renderReactable({
      
      vcAssignmentGroupTable_Czekanowski <- reactable::reactable(data = vcAssignmentGroup_Czekanowski, 
                                                                 filterable = FALSE,
                                                                 pagination = TRUE, 
                                                                 defaultPageSize = 30,
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
                                                                   VC.Code = reactable::colDef(
                                                                     filterable = TRUE
                                                                   ),
                                                                   Rank = reactable::colDef(
                                                                     format = reactable::colFormat(digits = 0),
                                                                     filterable = TRUE,
                                                                     filterMethod = reactable::JS("function(rows, columnId, filterValue) {
                                                                                                  return rows.filter(function(row) {
                                                                                                  return row.values[columnId] <= filterValue
                                                                                                  })
                                                                                                  }")
                                                                   )
                                                                 )
                  )
      
      return(vcAssignmentGroupTable_Czekanowski)
      
    })
    
  }) |>
    bindEvent(vcAssignmentGroup_Czekanowski_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = FALSE)
  
  
  outputOptions(output, "vcAssignmentGroupTable_Czekanowski", suspendWhenHidden = FALSE)
  

# Compose All VC Assignment Results --------------------------------------
  vcAssignment_rval <- reactiveVal()
  
  observe({
    
    shiny::req(isTRUE(!is.null(vcAssignmentPlot_Jaccard_rval()) | !is.null(vcAssignmentSite_Czekanowski_rval())))
    
    # Site/Year similarities
    if(!is.null(vcAssignmentSite_Czekanowski_rval())){
      
      vcAssignmentSite_Czekanowski <- vcAssignmentSite_Czekanowski_rval()
      
      vc_communities_site <- vcAssignmentSite_Czekanowski |>
        dplyr::filter(Rank <= 1) |>
        dplyr::distinct(VC.Code) |>
        dplyr::pull(VC.Code)
      
    } else {
      
      vcAssignmentSite_Czekanowski <- NULL
      
      vc_communities_site <- NULL
      
    }
    
    # Group similarities
    if(!is.null(vcAssignmentGroup_Czekanowski_rval())){
      
      vcAssignmentGroup_Czekanowski <- vcAssignmentGroup_Czekanowski_rval()
      
      vc_communities_group <- vcAssignmentGroup_Czekanowski |>
        dplyr::filter(Rank <= 1) |>
        dplyr::distinct(VC.Code) |>
        dplyr::pull(VC.Code)
      
    } else {
      
      vcAssignmentGroup_Czekanowski <- NULL
      
      vc_communities_group <- NULL
        
    }
    
    # Quadrat similarities
    if(!is.null(vcAssignmentPlot_Jaccard_rval())){
      
      vcAssignmentPlot_Jaccard <- vcAssignmentPlot_Jaccard_rval()
      
      vc_communities_quadrat <- vcAssignmentPlot_Jaccard |>
        dplyr::filter(Rank <= 1) |>
        dplyr::distinct(VC.Code) |>
        dplyr::pull(VC.Code)
      
    } else {
      
      vcAssignmentPlot_Jaccard <- NULL
      
      vc_communities_quadrat <- NULL
      
    }
    
    topVCSubCommsAndComms <- unique(purrr::list_c(list(vc_communities_site, vc_communities_group, vc_communities_quadrat)))
    
    topVCComms <- stringr::str_replace(string = topVCSubCommsAndComms, 
                                       pattern = "(\\d)[^0-9]+$", 
                                       replace = "\\1") |>
      unique()
    
    vcAssignment_list <- list("vcAssignmentPlot_Jaccard" = vcAssignmentPlot_Jaccard,
                              "vcAssignmentSite_Czekanowski" = vcAssignmentSite_Czekanowski,
                              "vcAssignmentGroup_Czekanowski" = vcAssignmentGroup_Czekanowski,
                              "topVCComms" = topVCComms,
                              "topVCSubCommsAndComms" = topVCSubCommsAndComms)
    
    vcAssignment_rval(vcAssignment_list)
    
  }) |>
    bindEvent(vcAssignmentSite_Czekanowski_rval(),
              vcAssignmentGroup_Czekanowski_rval(),
              vcAssignmentPlot_Jaccard_rval(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)


# Return VC Assignment Data ----------------------------------------------
  return(vcAssignment_rval)
  
}
