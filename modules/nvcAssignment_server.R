nvcAssignment <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()
  # nvcAssignMethods <- reactiveVal()

  observe({

    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)
    # nvcAssignMethods(sidebar_options()$nvcAssignMethods)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
# Calculate nvcAssignment results by site ----------------------------------------
  nvcAssignmentQuadrat_rval <- reactiveVal()
  nvcAssignmentGroup_rval <- reactiveVal()
  nvcAssignmentSite_rval <- reactiveVal()
  
  observe({
    
    req(isFALSE(runAnalysis() == 0))
    
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
      
      if(!is.null(habitatRestriction())){
        pquads_to_use <- nvc_pquads_final |>
          dplyr::filter(stringr::str_detect(NVC, (stringr::str_c(habitatRestriction(), collapse = "|"))))
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
        dplyr::slice(1:as.numeric(nTopResults())) |>
        dplyr::arrange(ID, dplyr::desc(Mean.Similarity)) |>
        dplyr::ungroup() |>
        dplyr::left_join(surveyTable_IDs, by = "ID")
      
      assign(x = "nvcAssignmentQuadrat", value = nvcAssignmentQuadrat, envir = .GlobalEnv)
      
      nvcAssignmentQuadrat_prepped <- nvcAssignmentQuadrat |>
        tidyr::unite(col = "ID", c("Year", "Group", "Quadrat"), sep = " - ", remove = TRUE) |>
        dplyr::select(ID, NVC.Code, Mean.Similarity) |>
        dplyr::arrange(ID, dplyr::desc(Mean.Similarity))
        
      nvcAssignmentQuadrat_rval(nvcAssignmentQuadrat_prepped)
      

      # Calculate NVC Similarity by Group
      nvcAssignmentGroup <- nvcAssignmentQuadrat |>
        dplyr::select(-ID) |>
        dplyr::group_by(Year, Group, NVC.Code) |>
        dplyr::summarise("Mean.Similarity" = mean(Mean.Similarity), .groups = "drop") |>
        dplyr::group_by(Year, Group) |>
        dplyr::slice(1:as.numeric(nTopResults())) |>
        dplyr::ungroup()
      
      nvcAssignmentGroup_prepped <- nvcAssignmentGroup |>
        tidyr::unite(col = "ID", c("Year", "Group"), sep = " - ", remove = TRUE) |>
        dplyr::select(ID, NVC.Code, Mean.Similarity) |>
        dplyr::arrange(ID, dplyr::desc(Mean.Similarity))
      
      nvcAssignmentGroup_rval(nvcAssignmentGroup_prepped)


      # Calculate NVC Similarity by Site
      nvcAssignmentSite <- nvcAssignmentQuadrat |> #nvcAssignmentGroup |>
        dplyr::group_by(Year, NVC.Code) |>
        dplyr::summarise("Mean.Similarity" = mean(Mean.Similarity), .groups = "drop") |>
        dplyr::group_by(Year) |>
        dplyr::slice(1:as.numeric(nTopResults())) |>
        dplyr::ungroup()
      
      nvcAssignmentSite_prepped <- nvcAssignmentSite |>
        dplyr::mutate("ID" = Year, .keep = "unused") |>
        dplyr::select(ID, NVC.Code, Mean.Similarity) |>
        dplyr::arrange(ID, dplyr::desc(Mean.Similarity))
      
      nvcAssignmentSite_rval(nvcAssignmentSite_prepped)
            
      
    })
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE)
  
  

# NVC Assignment Site Table -----------------------------------------------
  nvcAssignmentSiteTable_init <- data.frame("ID" = character(),
                                            "Mean.Similarity" = numeric(),
                                            "NVC.Code" = character()
  )
  
  nvcAssignmentSiteTable_rval <- reactiveVal(nvcAssignmentSiteTable_init)
  
  output$nvcAssignmentSiteTable <- rhandsontable::renderRHandsontable({
    
    nvcAssignmentSiteTable <- rhandsontable::rhandsontable(data = nvcAssignmentSiteTable_init,
                                                           rowHeaders = NULL,
                                                           width = "100%"#,
                                                           # overflow = "visible",
                                                           # stretchH = "all"
                                                           ) |>
      rhandsontable::hot_col(col = colnames(nvcAssignmentSiteTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(nvcAssignmentSiteTable)
    
  })
  
  observe({
    
    req(input$nvcAssignmentSiteTable)
    
    nvcAssignmentSite <- nvcAssignmentSite_rval()
    
    output$nvcAssignmentSiteTable <- rhandsontable::renderRHandsontable({
      
      nvcAssignmentSiteTable <- rhandsontable::rhandsontable(data = nvcAssignmentSite,
                                                             rowHeaders = NULL#,
                                                             # overflow = "visible",
                                                             # stretchH = "all"
                                                             ) |>
        rhandsontable::hot_col(col = colnames(nvcAssignmentSite), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(nvcAssignmentSiteTable)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentSite_rval(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentSiteTable", suspendWhenHidden = FALSE)
  

# NVC Assignment Group Table ----------------------------------------------
  nvcAssignmentGroupTable_init <- data.frame("ID" = character(),
                                              "Mean.Similarity" = numeric(),
                                              "NVC.Code" = character()
                                              )
  
  nvcAssignmentGroupTable_rval <- reactiveVal(nvcAssignmentGroupTable_init)
  
  output$nvcAssignmentGroupTable <- rhandsontable::renderRHandsontable({
    
    nvcAssignmentGroupTable <- rhandsontable::rhandsontable(data = nvcAssignmentGroupTable_init,
                                                            rowHeaders = NULL,
                                                            width = "100%"#,
                                                            # overflow = "visible",
                                                            # stretchH = "all"
                                                            ) |>
      rhandsontable::hot_col(col = colnames(nvcAssignmentGroupTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(nvcAssignmentGroupTable)
    
  })
  
  observe({
    
    req(input$nvcAssignmentGroupTable)
    
    nvcAssignmentGroup <- nvcAssignmentGroup_rval()
    
    output$nvcAssignmentGroupTable <- rhandsontable::renderRHandsontable({
      
      nvcAssignmentGroupTable <- rhandsontable::rhandsontable(data = nvcAssignmentGroup,
                                                              rowHeaders = NULL#,
                                                              # overflow = "visible",
                                                              # stretchH = "all"
                                                              ) |>
        rhandsontable::hot_col(col = colnames(nvcAssignmentGroup), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(nvcAssignmentGroupTable)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentGroup_rval(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentGroupTable", suspendWhenHidden = FALSE)
  

  # NVC Assignment Quadrat Table ----------------------------------------------
  nvcAssignmentQuadratTable_init <- data.frame("ID" = character(),
                                               "Mean.Similarity" = numeric(),
                                               "NVC.Code" = character()
  )
  
  nvcAssignmentQuadratTable_rval <- reactiveVal(nvcAssignmentQuadratTable_init)
  
  output$nvcAssignmentQuadratTable <- rhandsontable::renderRHandsontable({
    
    nvcAssignmentQuadratTable <- rhandsontable::rhandsontable(data = nvcAssignmentQuadratTable_init,
                                                              rowHeaders = NULL,
                                                              width = "100%"#,
                                                              # overflow = "visible",
                                                              # stretchH = "all"
                                                              ) |>
      rhandsontable::hot_col(col = colnames(nvcAssignmentQuadratTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(nvcAssignmentQuadratTable)
    
  })
  
  observe({
    
    req(input$nvcAssignmentQuadratTable)
    
    nvcAssignmentQuadrat <- nvcAssignmentQuadrat_rval()
    
    output$nvcAssignmentQuadratTable <- rhandsontable::renderRHandsontable({
      
      nvcAssignmentQuadratTable <- rhandsontable::rhandsontable(data = nvcAssignmentQuadrat,
                                                                rowHeaders = NULL#,
                                                                # overflow = "visible",
                                                                # stretchH = "all"
                                                                ) |>
        rhandsontable::hot_col(col = colnames(nvcAssignmentQuadrat), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(nvcAssignmentQuadratTable)
      
    })
    
  }) |>
    bindEvent(nvcAssignmentQuadrat_rval(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAssignmentQuadratTable", suspendWhenHidden = FALSE)
  
  

# Return nvcAssignmentSiteTable data (nvcAssignmentSite_rval) -------------
  return(nvcAssignmentSite_rval)
  
}
