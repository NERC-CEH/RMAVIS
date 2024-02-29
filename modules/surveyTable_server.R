surveyTable <- function(input, output, session, uploadDataTable, setupData, surveyTableValidator, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve Setup Data -----------------------------------------------------
  exampleData <- reactiveVal()
  
  observe({
    
    setupData <- setupData()
    
    exampleData(setupData$example_data)
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE)
  
# Retrieve sidebar options ------------------------------------------------
  
  inputMethod <- reactiveVal()
  selectedExampleData <- reactiveVal()
  runAnalysis <- reactiveVal()

  observe({
    
    inputMethod(sidebar_options()$inputMethod)
    selectedExampleData(sidebar_options()$selectedExampleData)
    runAnalysis(sidebar_options()$runAnalysis)

  }) |>
    bindEvent(sidebar_options(), 
              ignoreInit = TRUE)
  

# Retrieve Survey Table Correction Button ---------------------------------
  adjustSpecies <- reactiveVal()
  reallocateGroups <- reactiveVal()
  combineDuplicates <- reactiveVal()
  speciesAdjustmentTable <- reactiveVal()
  reallocateGroupsTable <- reactiveVal()
  combineDuplicates <- reactiveVal()
  
  observe({
    
    adjustSpecies(surveyTableValidator()$adjustSpecies)
    reallocateGroups(surveyTableValidator()$reallocateGroups)
    combineDuplicates(surveyTableValidator()$combineDuplicates)
    speciesAdjustmentTable(surveyTableValidator()$speciesAdjustmentTable)
    reallocateGroupsTable(surveyTableValidator()$reallocateGroupsTable)
    combineDuplicates(surveyTableValidator()$combineDuplicates)
    
  }) |>
    bindEvent(surveyTableValidator(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)


# Initial survey table data -----------------------------------------------

  surveyTable_init <- data.frame("Year" = as.integer(rep(as.numeric(format(Sys.Date(), "%Y")), 20)),
                                 "Group" = as.character(rep("A", 20)),
                                 "Quadrat" = as.character(rep("1", 20)),
                                 "Species" = as.character(rep("", 20)),
                                 "Cover" = as.numeric(rep(NA, 20)))
  
# Survey Data Entry Table -------------------------------------------------
  
  surveyTable_rval <- reactiveVal(surveyTable_init)
  
  output$surveyTable <- rhandsontable::renderRHandsontable({
    
    surveyTable <- rhandsontable::rhandsontable(data = surveyTable_init,
                                                height = 600,
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
        
        surveyTable <- example_data_all |> # exampleData()
          dplyr::filter(Site == selectedExampleData()) |>
          dplyr::select(-Site) |>
          dplyr::arrange(Year, Group, Quadrat)
        
      } else if(inputMethod() == "upload"){
        
        surveyTable <- rhandsontable::hot_to_r(input$surveyTable)
        
        if(!is.null(uploadDataTable())){
          
          surveyTable <- uploadDataTable()
          
        }
        
      }
      
    })

    output$surveyTable <- rhandsontable::renderRHandsontable({

      surveyTable <- rhandsontable::rhandsontable(data = surveyTable,
                                                  height = 600,
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
              selectedExampleData(),
              # exampleData(),
              uploadDataTable(),
              ignoreInit = TRUE)
  
  



# Survey Table Validation Actions -----------------------------------------
  surveyTable_corrected_rval <- reactiveVal()
  
## Adjust Species Names ---------------------------------------------------
  observe({

    req(speciesAdjustmentTable())
    req(input$surveyTable)
    
    # print(speciesAdjustmentTable())

    isolate({
      
      surveyTable <- rhandsontable::hot_to_r(input$surveyTable)

      if(!is.null(speciesAdjustmentTable())){

        speciesAdjustmentTable <- speciesAdjustmentTable() |>
          dplyr::rename("Species" = Species.Submitted) |>
          dplyr::select(-Species.Ignore)

        surveyTable_corrected <- surveyTable |>
          tibble::as_tibble() |>
          dplyr::left_join(speciesAdjustmentTable, by = "Species") |>
          dplyr::mutate(
            "Species" = dplyr::case_when(
              is.na(Species.Adjusted) ~ Species,
              TRUE ~ as.character(Species.Adjusted)
            )
          ) |>
          dplyr::filter(Species.Remove != TRUE | is.na(Species.Remove)) |>
          dplyr::select(-Species.Adjusted, -Species.Remove)
        
        surveyTable_corrected_rval(surveyTable_corrected)

      }

    })

  }) |>
    bindEvent(adjustSpecies(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

## Re-allocate Groups -----------------------------------------------------
  observe({
    
    req(reallocateGroupsTable())
    req(input$surveyTable)
    
    isolate({
      
      surveyTable <- rhandsontable::hot_to_r(input$surveyTable)
      
      if(!is.null(reallocateGroupsTable())){
        
        reallocateGroupsTable <- reallocateGroupsTable()
        
        surveyTable_corrected <- surveyTable |>
          tibble::as_tibble() |>
          dplyr::select(-Group) |>
          dplyr::left_join(reallocateGroupsTable, by = "Quadrat") |>
          dplyr::select(Year, Group, Quadrat, Species, Cover)
        
        surveyTable_corrected_rval(surveyTable_corrected)
        
      }
      
    })
    
  }) |>
    bindEvent(reallocateGroups(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

## Combine Duplicates -----------------------------------------------------
  observe({
    
    # req(surveyTable_corrected_rval())
    req(input$surveyTable)
    
    isolate({
      
      surveyTable <- rhandsontable::hot_to_r(input$surveyTable)
      surveyTable_corrected <- surveyTable_corrected_rval()

      # print(surveyTable)

      surveyTable_noDuplicates <- surveyTable |>
        dplyr::group_by(Year, Group, Quadrat, Species) |>
        dplyr::summarise("Cover" = sum(Cover)) |>
        dplyr::ungroup()
      
      

      surveyTable_corrected_rval(surveyTable_noDuplicates)

    })
    
  }) |>
    bindEvent(combineDuplicates(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  observe({
    
    shiny::req(surveyTable_corrected_rval())
    
    surveyTable_corrected <- surveyTable_corrected_rval()
    
    # print(head(surveyTable_corrected))
    
    output$surveyTable <- rhandsontable::renderRHandsontable({
      
      surveyTable <- rhandsontable::rhandsontable(data = surveyTable_corrected,
                                                  height = 600,
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
    bindEvent(surveyTable_corrected_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

  

# Save Survey Table to Reactive Val ---------------------------------------
  observe({

    # req(nrow(surveyTable_rval()) > 0)

    surveyTable_rval(rhandsontable::hot_to_r(input$surveyTable))

  }) |>
    bindEvent(input$surveyTable,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
    
  
  outputOptions(output, "surveyTable", suspendWhenHidden = FALSE)
  
  
  return(surveyTable_rval)

}
