surveyTable <- function(input, output, session, uploadDataTable, surveyTableValidator, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  
  # resetTable <- reactiveVal()
  inputMethod <- reactiveVal()
  exampleData <- reactiveVal()
  runAnalysis <- reactiveVal()
  # coverMethod <- reactiveVal()

  observe({
    
    # resetTable(sidebar_options()$resetTable)
    inputMethod(sidebar_options()$inputMethod)
    exampleData(sidebar_options()$exampleData)
    runAnalysis(sidebar_options()$runAnalysis)
    # coverMethod(sidebar_options()$coverMethod)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  

# Retrieve Survey Table Correction Button ---------------------------------
  correctSpecies <- reactiveVal()
  
  observe({
    
    correctSpecies(surveyTableValidator()$correctSpecies)
    
  }) |>
    bindEvent(surveyTableValidator(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)


# Initial survey table data -----------------------------------------------

  surveyTable_init <- data.frame("Year" = as.integer(rep(as.numeric(format(Sys.Date(), "%Y")), 20)),
                                 "Group" = as.character(rep("A", 20)),
                                 "Quadrat" = as.character(rep("1", 20)),
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
        col = "Group",
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
        source = speciesNames,
        strict = FALSE,
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
      # rhandsontable::hot_validate_character(cols = "Species", choices = speciesNames) |>
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
        
        surveyTable <- example_data_all |>
          dplyr::filter(Site == exampleData()) |>
          dplyr::select(-Site) |>
          dplyr::arrange(Year, Group, Quadrat)
        
        # surveyTable <- surveyTable |>
        #   # tidyr::unite(col = "ID", c(Year, Site, Group, Quadrat), sep = " - ", remove = TRUE) |>
        #   dplyr::select(Quadrat, Species, Cover) |>
        #   dplyr::filter(!is.na(Cover)) |>
        #   tidyr::pivot_wider(id_cols = Quadrat,
        #                      names_from = Species, 
        #                      values_from = Cover) |>
        #   # dplyr::select(-ID) |>
        #   tibble::column_to_rownames(var = "Quadrat")
        # 
        # write.csv(x = surveyTable, file = "./data/bundled_data/example_data_long.csv", row.names = FALSE)
        # write.csv(x = surveyTable, file = "./data/bundled_data/example_data_wide.csv", row.names = TRUE)
        
      } else if(inputMethod() == "upload"){
        
        surveyTable <- rhandsontable::hot_to_r(input$surveyTable)
        
        # print(uploadDataTable())
        
        if(!is.null(uploadDataTable())){
          
          surveyTable <- uploadDataTable()
          
        }
        
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
          col = "Group",
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
          source = speciesNames,
          strict = FALSE,
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
        # rhandsontable::hot_validate_character(cols = "Species", choices = speciesNames) |>
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
    # 
    # print(head(surveyTable_rval()))
      
  }) |>
    bindEvent(inputMethod(),
              exampleData(),
              uploadDataTable(),
              ignoreInit = TRUE)
  
  

# Validate Survey Data Table ----------------------------------------------
  surveyTable_corrected_rval <- reactiveVal()
  
  observe({

    req(surveyTableValidator())
    req(input$surveyTable)

    surveyTable <- rhandsontable::hot_to_r(input$surveyTable)

    isolate({

      surveyTableValidator <- surveyTableValidator()

      assign(x = "surveyTableValidator", value = surveyTableValidator, envir = .GlobalEnv)
      assign(x = "surveyTable", value = surveyTable, envir = .GlobalEnv)

      if(!is.null(surveyTableValidator()$speciesCorrectionTable)){

        speciesCorrectionTable <- surveyTableValidator$speciesCorrectionTable |>
          dplyr::rename("Species" = Species.Submitted)

        surveyTable_corrected <- surveyTable |>
          dplyr::left_join(speciesCorrectionTable, by = "Species") |>
          dplyr::mutate(
            "Species" = dplyr::case_when(
              is.na(Species.Corrected) ~ Species,
              TRUE ~ as.character(Species.Corrected)
            )
          ) |>
          dplyr::select(-Species.Corrected, -Species.Ignore)
        
        surveyTable_corrected_rval(surveyTable_corrected)

      }

    })

  }) |>
    bindEvent(input$surveyTable,
              surveyTableValidator(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  observe({
    
    surveyTable_corrected <- surveyTable_corrected_rval()
    
    # print(head(surveyTable_corrected))
    
    output$surveyTable <- rhandsontable::renderRHandsontable({
      
      surveyTable <- rhandsontable::rhandsontable(data = surveyTable_corrected,
                                                  rowHeaders = NULL,
                                                  width = "100%"#,
                                                  # overflow = "visible",
                                                  # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(surveyTable_corrected), halign = "htCenter") |>
        rhandsontable::hot_col(
          col = "Year",
          readOnly = FALSE,
          type = "numeric"
        ) |>
        rhandsontable::hot_col(
          col = "Group",
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
          source = speciesNames,
          strict = FALSE,
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
        # rhandsontable::hot_validate_character(cols = "Species", choices = speciesNames) |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"surveyData_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(surveyTable)
      
    })
    
  }) |>
    bindEvent(correctSpecies(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

  

# Save Survey Table to Reactive Val ---------------------------------------
  observe({

    # req(nrow(surveyTable_rval()) > 0)

    # print(surveyTable_rval())

    surveyTable_rval(rhandsontable::hot_to_r(input$surveyTable))
    
    # print(surveyTable_rval())

  }) |>
    bindEvent(input$surveyTable,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
    
  
  outputOptions(output, "surveyTable", suspendWhenHidden = FALSE)
  
  
  return(surveyTable_rval)

}
