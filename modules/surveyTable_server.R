surveyTable <- function(input, output, session, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  
  # resetTable <- reactiveVal()
  inputMethod <- reactiveVal()
  exampleData <- reactiveVal()
  dataEntryFormat <- reactiveVal()
  runAnalysis <- reactiveVal()
  coverMethod <- reactiveVal()
  habitatRestriction <- reactiveVal()
  nTopResults <- reactiveVal()

  observe({
    
    # resetTable(sidebar_options()$resetTable)
    inputMethod(sidebar_options()$inputMethod)
    exampleData(sidebar_options()$exampleData)
    dataEntryFormat(sidebar_options()$dataEntryFormat)
    runAnalysis(sidebar_options()$runAnalysis)
    coverMethod(sidebar_options()$coverMethod)
    habitatRestriction(sidebar_options()$habitatRestriction)
    nTopResults(sidebar_options()$nTopResults)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)

# Initial survey table data -----------------------------------------------

  surveyTable_init <- data.frame("Year" = as.character(rep(2024, 20)),
                                 "Site" = as.character(rep("Placeholder", 20)),
                                 "Quadrat" = as.character(rep("1", 20)),
                                 "Quadrat.Group" = as.character(rep("A", 20)),
                                 "Species" = as.character(rep("", 20)),
                                 "Cover" = as.numeric(rep(0, 20)))
  
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
        col = "Year",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "Site",
        readOnly = FALSE,
        type = "text"
      ) |>
      rhandsontable::hot_col(
        col = "Quadrat.Group",
        readOnly = FALSE,
        type = "text"
      ) |>
      rhandsontable::hot_col(
        col = "Quadrat",
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
      
      if(inputMethod() == "manual"){
        
        surveyTable <- rhandsontable::hot_to_r(input$surveyTable)
        
      } else if(inputMethod() == "example"){
        
        surveyTable <- example_data_df |>
          dplyr::filter(Site == exampleData()) |>
          dplyr::mutate(Year = "2023") |>
          dplyr::select(Year, Site, Quadrat, Quadrat.Group, Species, Cover)
        
      } else if(inputMethod() == "upload"){
        
        surveyTable <- rhandsontable::hot_to_r(input$surveyTable)
        
      }
      
      # if(resetTable() == TRUE){
      #   
      #   surveyTable = surveyTable_init
      #   
      # }
      
      if (inputMethod() == "uploadData") {
        
        # From uploaded csv
        uploaded_data_raw <- input$uploadData
        
        if (is.null(uploaded_data_raw)) {
          return(NULL)
        }
        
        print(uploaded_data_raw)
        
      }
      
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
          col = "Year",
          readOnly = FALSE,
          type = "numeric"
        ) |>
        rhandsontable::hot_col(
          col = "Site",
          readOnly = FALSE,
          type = "text"
        ) |>
        rhandsontable::hot_col(
          col = "Quadrat.Group",
          readOnly = FALSE,
          type = "text"
        ) |>
        rhandsontable::hot_col(
          col = "Quadrat",
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
    bindEvent(inputMethod(),
              exampleData(),
              # resetTable(),
              ignoreInit = TRUE)
  
  
  observe({

    surveyTable_rval(rhandsontable::hot_to_r(input$surveyTable))

  }) |>
    bindEvent(input$surveyTable, ignoreInit = TRUE)
    
  
  outputOptions(output, "surveyTable", suspendWhenHidden = FALSE)
  
  
  return(surveyTable_rval)

}
