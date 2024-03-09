surveyData <- function(input, output, session, uploadDataTable, setupData, surveyDataValidator, sidebar_options) {
  
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
  surveyDataValidation <- reactiveVal()
  okToProceed <- reactiveVal()
  
  observe({
    
    adjustSpecies(surveyDataValidator()$adjustSpecies)
    reallocateGroups(surveyDataValidator()$reallocateGroups)
    combineDuplicates(surveyDataValidator()$combineDuplicates)
    speciesAdjustmentTable(surveyDataValidator()$speciesAdjustmentTable)
    reallocateGroupsTable(surveyDataValidator()$reallocateGroupsTable)
    combineDuplicates(surveyDataValidator()$combineDuplicates)
    surveyDataValidation(surveyDataValidator()$surveyDataValidation)
    okToProceed(surveyDataValidator()$okToProceed)
    
  }) |>
    bindEvent(surveyDataValidator(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)


# Initial survey table data -----------------------------------------------
  surveyData_long_init <- data.frame("Year" = as.integer(rep(as.numeric(format(Sys.Date(), "%Y")), 20)),
                                     "Group" = as.character(rep("A", 20)),
                                     "Quadrat" = as.character(rep("1", 20)),
                                     "Species" = as.character(rep("", 20)),
                                     "Cover" = as.numeric(rep(NA, 20)))
  
# Survey Data Entry Table -------------------------------------------------
  output$surveyData <- rhandsontable::renderRHandsontable({
    
    surveyData <- rhandsontable::rhandsontable(data = surveyData_long_init,
                                                height = 600,
                                                rowHeaders = NULL,
                                                width = "100%"#,
                                                # overflow = "visible"
                                                # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(surveyData_long_init), halign = "htCenter") |>
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
    
    return(surveyData)
    
  })
  
  observe({
    
    shiny::isolate({
      
      if(inputMethod() == "manual"){
        
        surveyData <- rhandsontable::hot_to_r(input$surveyData)
        
      } else if(inputMethod() == "example"){
        
        surveyData <- example_data_all |> # exampleData()
          dplyr::filter(Site == selectedExampleData()) |>
          dplyr::select(-Site) |>
          dplyr::arrange(Year, Group, Quadrat)
        
      } else if(inputMethod() == "upload"){
        
        surveyData <- rhandsontable::hot_to_r(input$surveyData)
        
        if(!is.null(uploadDataTable())){
          
          surveyData <- uploadDataTable()
          
        }
        
      }
      
    })

    output$surveyData <- rhandsontable::renderRHandsontable({

      surveyData <- rhandsontable::rhandsontable(data = surveyData,
                                                  height = 600,
                                                  rowHeaders = NULL,
                                                  width = "100%"#,
                                                  # overflow = "visible",
                                                  # stretchH = "all"
                                                  ) |>
        rhandsontable::hot_col(col = colnames(surveyData), halign = "htCenter") |>
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
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"surveyData_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")

      return(surveyData)

    })
      
  }) |>
    bindEvent(inputMethod(),
              selectedExampleData(),
              uploadDataTable(),
              ignoreInit = TRUE)
  
  



# Survey Table Validation Actions -----------------------------------------
  surveyData_corrected_rval <- reactiveVal()
  
## Adjust Species Names ---------------------------------------------------
  observe({

    req(speciesAdjustmentTable())
    req(input$surveyData)

    isolate({
      
      surveyData <- rhandsontable::hot_to_r(input$surveyData)
      speciesAdjustmentTable <- speciesAdjustmentTable()

      if(!is.null(speciesAdjustmentTable)){

        speciesAdjustmentTable <- speciesAdjustmentTable |>
          dplyr::rename("Species" = Species.Submitted) |>
          dplyr::select(-Species.Ignore)

        surveyData_corrected <- surveyData |>
          tibble::as_tibble() |>
          dplyr::left_join(speciesAdjustmentTable, by = "Species") |>
          dplyr::mutate(
            "Species" = dplyr::case_when(
              is.na(Species.Adjusted) ~ Species,
              TRUE ~ as.character(Species.Adjusted)
            )
          ) |>
          dplyr::filter(Species.Remove != "Yes" | is.na(Species.Remove)) |>
          dplyr::select(-Species.Adjusted, -Species.Remove)
        
        surveyData_corrected_rval(surveyData_corrected)

      }

    })

  }) |>
    bindEvent(adjustSpecies(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

## Re-allocate Groups -----------------------------------------------------
  observe({
    
    req(reallocateGroupsTable())
    req(input$surveyData)
    
    isolate({
      
      surveyData <- rhandsontable::hot_to_r(input$surveyData)
      
      if(!is.null(reallocateGroupsTable())){
        
        reallocateGroupsTable <- reallocateGroupsTable()
        
        surveyData_corrected <- surveyData |>
          tibble::as_tibble() |>
          dplyr::select(-Group) |>
          dplyr::left_join(reallocateGroupsTable, by = "Quadrat") |>
          dplyr::select(Year, Group, Quadrat, Species, Cover)
        
        surveyData_corrected_rval(surveyData_corrected)
        
      }
      
    })
    
  }) |>
    bindEvent(reallocateGroups(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

## Combine Duplicates -----------------------------------------------------
  observe({
    
    req(input$surveyData)
    
    isolate({
      
      surveyData <- rhandsontable::hot_to_r(input$surveyData)
      surveyData_corrected <- surveyData_corrected_rval()

      surveyData_noDuplicates <- surveyData |>
        dplyr::group_by(Year, Group, Quadrat, Species) |>
        dplyr::summarise("Cover" = sum(Cover)) |>
        dplyr::ungroup()

      surveyData_corrected_rval(surveyData_noDuplicates)

    })
    
  }) |>
    bindEvent(combineDuplicates(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  observe({
    
    shiny::req(surveyData_corrected_rval())
    
    surveyData_corrected <- surveyData_corrected_rval()
    
    output$surveyData <- rhandsontable::renderRHandsontable({
      
      surveyData <- rhandsontable::rhandsontable(data = surveyData_corrected,
                                                  height = 600,
                                                  rowHeaders = NULL,
                                                  width = "100%"#,
                                                  # overflow = "visible",
                                                  # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(surveyData_corrected), halign = "htCenter") |>
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
      
      return(surveyData)
      
    })
    
  }) |>
    bindEvent(surveyData_corrected_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

  

# Save Survey Table to Reactive Val ---------------------------------------
  # surveyData_rval <- reactiveVal(surveyDataR6)
  surveyData_rval <- reactiveVal(list(
    "surveyData_long" = NULL,
    "surveyData_wide" = NULL,
    "surveyData_mat" = NULL
  ))
  
  observe({
    
    surveyData_long <- rhandsontable::hot_to_r(input$surveyData)
    surveyData <- surveyData_rval()
    surveyData$surveyData_long <- surveyData_long
    surveyData_rval(surveyData)

  }) |>
    bindEvent(input$surveyData,
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  

# Save Survey Data Wide and Mat -------------------------------------------
  observe({

    # Require the following conditions to be true to create surveyData_wide and
    # surveyData_mat.
    # This is to prevent list columns, empty column names, and incomplete row
    # names (mat) or incomplete ID columns (wide) when pivoting wide.
    shiny::req(isTRUE(surveyDataValidation()$yearComplete))
    shiny::req(isTRUE(surveyDataValidation()$groupComplete))
    shiny::req(isTRUE(surveyDataValidation()$quadratComplete))
    shiny::req(isTRUE(surveyDataValidation()$speciesComplete))
    shiny::req(isTRUE(surveyDataValidation()$quadratIDUnique))
    shiny::req(isTRUE(surveyDataValidation()$groupIDUnique))

    # Retrieve long survey table
    surveyData <- surveyData_rval()
    surveyData_long <- surveyData$surveyData_long

    # Check whether there are any cover values supplied
    # noCoverValues <- isFALSE(surveyDataValidation()$coverSupplied)
    noCoverValues <- isTRUE(surveyData_long$Cover |> unique() |> is.na())

    if(noCoverValues == TRUE){

      surveyData_wide <- surveyData_long |>
        dplyr::mutate("Cover" = 1) |>
        tidyr::pivot_wider(names_from = Species,
                           values_from = Cover) |>
        dplyr::mutate_all(~replace(., is.na(.), 0))

      surveyData_mat <- surveyData_long |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE) |>
        dplyr::mutate("Cover" = 1) |>
        tidyr::pivot_wider(names_from = Species,
                           values_from = Cover) |>
        tibble::column_to_rownames(var = "ID") |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix()

    } else if(noCoverValues == FALSE){

      surveyData_wide <- surveyData_long |>
        tidyr::pivot_wider(names_from = Species,
                           values_from = Cover) |>
        dplyr::mutate_all(~replace(., is.na(.), 0))

      surveyData_mat <- surveyData_long |>
        tidyr::unite(col = "ID", c(Year, Group, Quadrat), sep = " - ", remove = TRUE) |>
        tidyr::pivot_wider(names_from = Species,
                           values_from = Cover) |>
        tibble::column_to_rownames(var = "ID") |>
        dplyr::mutate_all(~replace(., is.na(.), 0)) |>
        as.matrix()

    }
    
    surveyData$surveyData_wide <- surveyData_wide
    surveyData$surveyData_mat <- surveyData_mat
    surveyData_rval(surveyData)

  }) |>
    bindEvent(surveyData_rval(),
              input$surveyData,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
    
  # Ensure table 
  outputOptions(output, "surveyData", suspendWhenHidden = FALSE)
  
  
  return(surveyData_rval)

}
