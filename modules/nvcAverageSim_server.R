nvcAverageSim <- function(input, output, session, surveyTable, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  dataEntryFormat <- reactiveVal()
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()
  groupSample <- reactiveVal(TRUE) # TRUE
  
  observe({
    
    dataEntryFormat(sidebar_options()$dataEntryFormat)
    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)
    groupSample(sidebar_options()$groupSample)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  # Calculate nvcAverageSim results ----------------------------------------
  nvcAverageSim <- reactiveVal()
  
  observe({
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Calculating NVC Community Similarity"
    )
    
    surveyTable <- surveyTable()
    
    if(groupSample() == TRUE){
      
      surveyTable_prepped <- surveyTable |>
        dplyr::select(Sample, Species) |>
        dplyr::rename("ID" = "Sample",
                      "species" = "Species") |>
        dplyr::mutate(ID = "All")
      
    } else if(groupSample() == FALSE){
      
      surveyTable_prepped <- surveyTable |>
        dplyr::select(Sample, Species) |>
        dplyr::rename("ID" = "Sample",
                      "species" = "Species")
      
    }
    
    pquads_to_use <- nvc_pquads_tidied
    
    if(!is.null(habitatRestriction())){
      pquads_to_use <- nvc_pquads_tidied |>
        dplyr::filter(stringr::str_detect(NVC, (stringr::str_c(habitatRestriction(), collapse = "|"))))
    }
    
    fitted_nvc <- assignNVC::nvc_average_sim(samp_df = surveyTable_prepped,
                                             comp_df = pquads_to_use,
                                             spp_col = "species",
                                             samp_id = "ID",
                                             comp_id = "Pid3") |>
      dplyr::select("Sample" = FOCAL_ID,
                    "Mean.Similarity" = MEAN_SIM, 
                    "Standard.Deviation" = SD,
                    "NVC.Code" = NVC) |>
      dplyr::group_by(Sample) |>
      dplyr::arrange(dplyr::desc(Mean.Similarity)) |>
      dplyr::slice(1:as.numeric(nTopResults())) |>
      dplyr::arrange(Sample, dplyr::desc(Mean.Similarity)) |>
      dplyr::ungroup()
    
    nvcAverageSim(fitted_nvc)
    
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(), 
              habitatRestriction(), 
              nTopResults(), 
              groupSample(),
              ignoreInit = TRUE)
  
  nvcAverageSimTable_init <- tibble::tribble(
    ~Sample, ~Mean.Similarity, ~Standard.Deviation, ~NVC.Code,
    "", "", "", ""
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
          $('a[data-value=\"results_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(nvcAverageSimTable)
    
  })
  
  observe({
    
    req(input$nvcAverageSimTable)
    
    nvcAverageSim <- nvcAverageSim()
    
    # isolate({
    #   
    # })
    
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
          $('a[data-value=\"results_panel\"').on('click', function(){
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