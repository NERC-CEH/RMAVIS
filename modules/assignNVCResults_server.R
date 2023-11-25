assignNVCResults <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  dataEntryFormat <- reactiveVal()
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()

  observe({

    dataEntryFormat(sidebar_options()$dataEntryFormat)
    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  # Calculate assignNVC results ---------------------------------------------
  assignNVCResults <- reactiveVal()
  
  observe({
    
    surveyTable <- surveyTable()
    
    surveyTable_prepped <- surveyTable |>
      dplyr::select(Sample, Species) |>
      dplyr::rename("ID" = "Sample",
                    "species" = "Species")
    
    pquads_to_use <- nvc_pquads_tidied
    
    if(!is.null(habitatRestriction())){
      pquads_to_use <- nvc_pquads_tidied |>
        dplyr::filter(stringr::str_detect(NVC, (stringr::str_c(habitatRestriction(), collapse = "|"))))
    }
    
    fitted_nvc <- assignNVC::assign_nvc(samp_df = surveyTable_prepped,
                                        comp_df = pquads_to_use,
                                        spp_col = "species",
                                        samp_id = "ID",
                                        comp_id = "Pid3",
                                        top_n = as.numeric(nTopResults())) |>
      dplyr::rename("Sample" = "FOCAL_ID", 
                    "Pseudo.Quadrat" = "COMP_ID", 
                    "Jaccard.Similarity" = "JAC_SIM", 
                    "NVC.Code" = "NVC") |>
      dplyr::arrange(Sample, dplyr::desc(Jaccard.Similarity))
    
    assignNVCResults(fitted_nvc)
    
  }) |>
    bindEvent(runAnalysis(), 
              habitatRestriction(), 
              nTopResults(), 
              ignoreInit = TRUE)
  
  resultsTable_init <- tibble::tribble(
    ~Sample, ~Pseudo.Quadrat, ~Jaccard.Similarity, ~NVC.Code,
    "", "", "", ""
  )
  
  resultsTable_rval <- reactiveVal(resultsTable_init)
  
  output$resultsTable <- rhandsontable::renderRHandsontable({

    resultsTable <- rhandsontable::rhandsontable(data = resultsTable_init,
                                                 rowHeaders = NULL,
                                                 width = "100%"#,
                                                # overflow = "visible",
                                                # stretchH = "all"
                                                ) |>
      rhandsontable::hot_col(col = colnames(resultsTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"resultsAndHabCor_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")

    return(resultsTable)

  })
  
  observe({

    req(input$resultsTable)
    
    assignNVCResults <- assignNVCResults()
    
    # isolate({
    #   
    # })

    output$resultsTable <- rhandsontable::renderRHandsontable({

      resultsTable <- rhandsontable::rhandsontable(data = assignNVCResults,
                                                   rowHeaders = NULL#,
                                                   # overflow = "visible",
                                                   # stretchH = "all"
                                                   ) |>
        rhandsontable::hot_col(col = colnames(assignNVCResults), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"resultsAndHabCor_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")

      return(resultsTable)

    })

  }) |>
    bindEvent(assignNVCResults(), ignoreInit = TRUE, ignoreNULL = TRUE)


  outputOptions(output, "resultsTable", suspendWhenHidden = FALSE)
  
  
  return(assignNVCResults)
  
}
