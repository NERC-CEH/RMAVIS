speciesFreq <- function(input, output, session, surveyTable, surveyTableWide, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  runAnalysis <- reactiveVal()
  
  observe({
    
    runAnalysis(sidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  

# Intialise Species Frequency Table ---------------------------------------
  speciesFrequencyTable_init <- data.frame("Year" = integer(),
                                           "Species" = character())
  
  speciesFrequencyTable_rval <- reactiveVal(speciesFrequencyTable_init)
  
  output$speciesFrequencyTable <- rhandsontable::renderRHandsontable({
    
    speciesFrequencyTable <- rhandsontable::rhandsontable(data = speciesFrequencyTable_init,
                                                          rowHeaders = NULL,
                                                          width = "100%"#,
                                                          # overflow = "visible",
                                                          # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(speciesFrequencyTable_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_cols(columnSorting = TRUE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"speciesFreq_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    return(speciesFrequencyTable)
    
  })
  
  

# Compile Frequency Table -------------------------------------------------
  observe({
    
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      color = "#3F9280",
      text = "Compiling Frequency Table"
    )
    
    shiny::req(surveyTable())
    shiny::req(surveyTableWide())
    
    surveyTable <- surveyTable()
    surveyTableWide <- surveyTableWide()
    
    isolate({
      
      # I need to find a better way to do this with tidy select
      max_year <- max(surveyTable$Year) |>
        as.character()
      min_year <- min(surveyTable$Year) |>
        as.character()
      
      speciesFrequency <- surveyTable |>
        dplyr::group_by(Year, Species) |>
        dplyr::summarise(Frequency = dplyr::n()) |>
        tidyr::pivot_wider(id_cols = Species,
                           names_from = Year,
                           values_from = Frequency) |>
        dplyr::mutate(
          "Difference" = 
            dplyr::case_when(
              is.na(get(min_year)) ~ as.numeric(get(max_year)),
              is.na(get(max_year)) ~ as.numeric(get(min_year)) * -1,
              !is.na(get(min_year)) & !is.na(get(max_year)) ~ as.numeric(get(max_year)) - as.numeric(get(min_year)),
              TRUE ~ 0
            )
        ) |>
        dplyr::mutate(
          "Change" = 
            dplyr::case_when(
              is.na(get(min_year)) & !is.na(get(max_year)) ~ "Gain",
              is.na(get(max_year)) & !is.na(get(min_year))~ "Loss",
              Difference > 0 ~ "Net Increase",
              Difference < 0 ~ "Net Decrease",
              Difference == 0 ~ "No Net Difference",
              TRUE ~ "Gain then Loss"
            )
        )
      
      text_renderer <- "
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        # This is the column which you want to check for coloring
        var col_value = instance.getData()[row][4]
        if (col_value == 'Gain') {
          td.style.background = 'lightgreen';
        } else if (col_value == 'Net Increase') {
          td.style.background = 'lightgreen';
        } else if (col_value == 'Loss') {
          td.style.background = 'lightred';
        } else if (col_value == 'Net Decrease') {
          td.style.background = 'lightred';
        }
      }"
      
    })
      
      
      output$speciesFrequencyTable <- rhandsontable::renderRHandsontable({
        
        speciesFrequencyTable <- rhandsontable::rhandsontable(data = speciesFrequency,
                                                              rowHeaders = NULL#,
                                                              # width = "100%"#,
                                                              # overflow = "visible",
                                                              # stretchH = "all"
        ) |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
          rhandsontable::hot_col(col = colnames(speciesFrequency), halign = "htCenter", readOnly = TRUE) |>
          # rhandsontable::hot_col(col = "Change", renderer = text_renderer) |>
          rhandsontable::hot_cols(columnSorting = TRUE) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
          htmlwidgets::onRender("
          function(el, x) {
            var hot = this.hot
            $('a[data-value=\"speciesFreq_panel\"').on('click', function(){
              setTimeout(function() {hot.render();}, 0);
            })
          }")
        
        return(speciesFrequencyTable)
        
      })
      
    speciesFrequencyTable_rval <- reactiveVal(rhandsontable::hot_to_r(input$speciesFrequencyTable))
      
    shinybusy::remove_modal_spinner()
    
  }) |>
    bindEvent(runAnalysis(),
              ignoreInit = TRUE, 
              ignoreNULL = TRUE)
  
  
  outputOptions(output, "speciesFrequencyTable", suspendWhenHidden = FALSE)
  
  return(speciesFrequencyTable_rval)
  
}