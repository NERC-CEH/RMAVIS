nvcAverageSim <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()
  nvcAssignMethods <- reactiveVal()

  observe({

    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)
    nvcAssignMethods(sidebar_options()$nvcAssignMethods)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
# Calculate nvcAverageSim results by site ----------------------------------------
  nvcAverageSimSite_rval <- reactiveVal()
  
  observe({
    
    print(nvcAssignMethods())
    
    req(isFALSE(runAnalysis() == 0))
    # req("pseudoQuadratSite" %in% nvcAssignMethods())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating NVC Community Similarity"
    )
    
    shiny::isolate({
      
      surveyTable <- surveyTable()
      
      surveyTable_prepped <- surveyTable |>
        dplyr::select(Year, Species) |>
        dplyr::mutate("ID" = Year, .before = "Year", .keep = "unused") |>
        dplyr::rename("species" = "Species")
      
      pquads_to_use <- nvc_pquads_final
      
      if(!is.null(habitatRestriction())){
        pquads_to_use <- nvc_pquads_final |>
          dplyr::filter(stringr::str_detect(NVC, (stringr::str_c(habitatRestriction(), collapse = "|"))))
      }
      
      nvcAverageSimSite <- assignNVC::nvc_average_sim(samp_df = surveyTable_prepped,
                                                      comp_df = pquads_to_use, # 
                                                      spp_col = "species",
                                                      samp_id = "ID",
                                                      comp_id = "Pid3") |>
        dplyr::select("ID" = FOCAL_ID,
                      "Mean.Similarity" = MEAN_SIM, 
                      "Standard.Deviation" = SD,
                      "NVC.Code" = NVC) |>
        dplyr::group_by(ID) |>
        dplyr::arrange(dplyr::desc(Mean.Similarity)) |>
        dplyr::slice(1:as.numeric(nTopResults())) |>
        dplyr::arrange(ID, dplyr::desc(Mean.Similarity)) |>
        dplyr::ungroup()
      
      nvcAverageSimSite_rval(nvcAverageSimSite)
      
    })
    
    print(nvcAverageSimSite)
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              # nTopResults(), 
              ignoreInit = TRUE)
  
  nvcAverageSimSiteTable_init <- data.frame("ID" = character(),
                                            "Mean.Similarity" = numeric(),
                                            "Standard.Deviation" = numeric(),
                                            "NVC.Code" = character()
  )
  
  nvcAverageSimSiteTable_rval <- reactiveVal(nvcAverageSimSiteTable_init)
  
  output$nvcAverageSimSiteTable <- rhandsontable::renderRHandsontable({
    
    nvcAverageSimSiteTable <- rhandsontable::rhandsontable(data = nvcAverageSimSiteTable_init,
                                                           rowHeaders = NULL,
                                                           width = "100%"#,
                                                           # overflow = "visible",
                                                           # stretchH = "all"
                                                           ) |>
      rhandsontable::hot_col(col = colnames(nvcAverageSimSiteTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(nvcAverageSimSiteTable)
    
  })
  
  observe({
    
    req(input$nvcAverageSimSiteTable)
    
    nvcAverageSimSite <- nvcAverageSimSite_rval()
    
    output$nvcAverageSimSiteTable <- rhandsontable::renderRHandsontable({
      
      nvcAverageSimSiteTable <- rhandsontable::rhandsontable(data = nvcAverageSimSite,
                                                             rowHeaders = NULL#,
                                                             # overflow = "visible",
                                                             # stretchH = "all"
                                                             ) |>
        rhandsontable::hot_col(col = colnames(nvcAverageSimSite), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(nvcAverageSimSiteTable)
      
    })
    
  }) |>
    bindEvent(nvcAverageSimSite_rval(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAverageSimSiteTable", suspendWhenHidden = FALSE)
  
  
# Calculate nvcAverageSim results by group ----------------------------------------
  nvcAverageSim <- reactiveVal()
  
  observe({
    
    req(isFALSE(runAnalysis() == 0))
    # req("pseudoQuadratGroup" %in% nvcAssignMethods())
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating NVC Community Similarity"
    )
    
    shiny::isolate({
      
      surveyTable <- surveyTable()
      
      # surveyTable_prepped <- surveyTable |>
      #   dplyr::select(Year, Group, Quadrat, Species, Cover) |>
      #   tidyr::unite(col = "ID", c("Year", "Group", "Quadrat"), sep = " - ", remove = FALSE) |>
      #   dplyr::rename("species" = "Species")
      
      surveyTable_prepped <- surveyTable |>
        dplyr::select(Year, Group, Quadrat, Species, Cover) |>
        tidyr::unite(col = "ID", c("Year", "Group"), sep = " - ", remove = FALSE) |>
        dplyr::select(-Quadrat) |>
        dplyr::rename("species" = "Species")
      
      pquads_to_use <- nvc_pquads_final
      
      if(!is.null(habitatRestriction())){
        pquads_to_use <- nvc_pquads_final |>
          dplyr::filter(stringr::str_detect(NVC, (stringr::str_c(habitatRestriction(), collapse = "|"))))
      }
      
      fitted_nvc <- assignNVC::nvc_average_sim(samp_df = surveyTable_prepped,
                                               comp_df = pquads_to_use, # 
                                               spp_col = "species",
                                               samp_id = "ID",
                                               comp_id = "Pid3") |>
        dplyr::select("ID" = FOCAL_ID,
                      "Mean.Similarity" = MEAN_SIM, 
                      "Standard.Deviation" = SD,
                      "NVC.Code" = NVC) |>
        dplyr::group_by(ID) |>
        dplyr::arrange(dplyr::desc(Mean.Similarity)) |>
        dplyr::slice(1:as.numeric(nTopResults())) |>
        dplyr::arrange(ID, dplyr::desc(Mean.Similarity)) |>
        dplyr::ungroup()
      
      nvcAverageSim(fitted_nvc)
      
    })
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              # nTopResults(), 
              ignoreInit = TRUE)
  
  nvcAverageSimTable_init <- data.frame("ID" = character(),
                                        "Mean.Similarity" = numeric(),
                                        "Standard.Deviation" = numeric(),
                                        "NVC.Code" = character()
                                        )
  
  nvcAverageSimTable_rval <- reactiveVal(nvcAverageSimTable_init)
  
  output$nvcAverageSimTable <- rhandsontable::renderRHandsontable({
    
    nvcAverageSimTable <- rhandsontable::rhandsontable(data = nvcAverageSimTable_init,
                                                       rowHeaders = NULL,
                                                       width = "100%"#,
                                                       # overflow = "visible",
                                                       # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(nvcAverageSimTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(nvcAverageSimTable)
    
  })
  
  observe({
    
    req(input$nvcAverageSimTable)
    
    shiny::isolate({
      
      nvcAverageSim <- nvcAverageSim()

    })
    
    output$nvcAverageSimTable <- rhandsontable::renderRHandsontable({
      
      nvcAverageSimTable <- rhandsontable::rhandsontable(data = nvcAverageSim,
                                                         rowHeaders = NULL#,
                                                         # overflow = "visible",
                                                         # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(nvcAverageSim), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nvcAssignment_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(nvcAverageSimTable)
      
    })
    
  }) |>
    bindEvent(nvcAverageSim(), ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  outputOptions(output, "nvcAverageSimTable", suspendWhenHidden = FALSE)
  
  
  return(nvcAverageSim)
  
}