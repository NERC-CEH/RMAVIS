surveyTable <- function(input, output, session, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  
  exampleData <- reactiveVal()
  dataEntryFormat <- reactiveVal()
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()

  observe({
    
    exampleData(sidebar_options()$exampleData)
    dataEntryFormat(sidebar_options()$dataEntryFormat)
    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

# Initial survey table data -----------------------------------------------

  surveyTable_init <- data.frame("Sample" = character(),
                                 "Species" = numeric(),
                                 "Cover" = character())
  
# Survey Data Entry Table -------------------------------------------------
  
  surveyTable_rval <- reactiveVal(surveyTable_init)
  
  output$surveyTable <- rhandsontable::renderRHandsontable({
    
    surveyTable <- rhandsontable::rhandsontable(data = surveyTable_init,
                                                rowHeaders = NULL,
                                                width = "100%"#,
                                                # overflow = "visible"
                                                # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(surveyTable_init), halign = "htCenter") |>
      rhandsontable::hot_col(
        col = "Sample",
        readOnly = FALSE,
        type = "text"
      ) |>
      rhandsontable::hot_col(
        col = "Species",
        readOnly = FALSE,
        type = "dropdown",
        source = speciesNames, # [1:50]
        strict = TRUE,
        default = as.character(NA_character_)
      ) |>
      rhandsontable::hot_col(
        col = "Cover",
        readOnly = FALSE,
        type = "numeric",
        strict = FALSE
      ) |>
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"surveyData_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(surveyTable)
    
  })
  
  observe({
    
    shiny::isolate({
      
      surveyTable <- example_data_df |>
        dplyr::filter(Site == exampleData()) |>
        dplyr::select(-Site)
      
      # if (exampleData() == "none"){
      #   
      #   surveyTable <- surveyTable_init
      #     
      # } else {
      #   
      #   surveyTable <- example_data_df |>
      #     dplyr::filter(Site == exampleData()) |>
      #     dplyr::select(-Site)
      #   
      # } 
      
      # print(surveyTable)
      
    })

    output$surveyTable <- rhandsontable::renderRHandsontable({

      surveyTable <- rhandsontable::rhandsontable(data = surveyTable,
                                                  rowHeaders = NULL,
                                                  width = "100%"#,
                                                  # overflow = "visible",
                                                  # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(surveyTable), halign = "htCenter") |>
        rhandsontable::hot_col(
          col = "Sample",
          readOnly = FALSE,
          type = "text"
        ) |>
        rhandsontable::hot_col(
          col = "Species",
          readOnly = FALSE,
          type = "dropdown",
          source = speciesNames, # [1:50]
          strict = TRUE,
          default = as.character(NA_character_)
        ) |>
        rhandsontable::hot_col(
          col = "Cover",
          readOnly = FALSE,
          type = "numeric",
          strict = FALSE
        ) |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"surveyData_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")

      return(surveyTable)

    })
      
    # surveyTable_rval(rhandsontable::hot_to_r(input$surveyTable))
      
  }) |>
    bindEvent(exampleData(), 
              ignoreInit = TRUE)
  
  
  observe({

    surveyTable_rval(rhandsontable::hot_to_r(input$surveyTable))

  }) |>
    bindEvent(input$surveyTable, ignoreInit = TRUE)
    
  
  outputOptions(output, "surveyTable", suspendWhenHidden = FALSE)
  
  
  return(surveyTable_rval)

}
