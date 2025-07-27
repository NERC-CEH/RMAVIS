surveyData <- function(input, output, session, uploadDataTable, setupData, surveyDataValidator, deSidebar_options) {
  
  ns <- session$ns
  
# Retrieve Setup Data -----------------------------------------------------
  exampleData <- reactiveVal()
  speciesNames <- reactiveVal()
  
  observe({
    
    setupData <- setupData()
    
    exampleData(setupData$example_data)
    speciesNames(setupData$species_names)
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE)
  
# Retrieve sidebar options ------------------------------------------------
  clearTable <- reactiveVal()
  inputMethod <- reactiveVal()
  selectedExampleData <- reactiveVal()
  coverScale <- reactiveVal()
  runAnalysis <- reactiveVal()

  observe({
    
    clearTable(deSidebar_options()$clearTable)
    inputMethod(deSidebar_options()$inputMethod)
    selectedExampleData(deSidebar_options()$selectedExampleData)
    coverScale(deSidebar_options()$coverScale)
    runAnalysis(deSidebar_options()$runAnalysis)
    
  }) |>
    bindEvent(deSidebar_options(), 
              ignoreInit = FALSE)
  

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
                                     "Species" = as.character(rep(NA, 20)),
                                     "Cover" = as.numeric(rep(NA, 20)))
  
# Survey Data Entry Table -------------------------------------------------
  output$surveyData <- rhandsontable::renderRHandsontable({
    
    surveyData <- rhandsontable::rhandsontable(data = surveyData_long_init,
                                               height = 800,
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
        source = speciesNames(),
        strict = FALSE,
        default = as.character(NA_character_)
      ) |>
      rhandsontable::hot_col(
        col = "Cover",
        readOnly = FALSE,
        type = "numeric",
        strict = FALSE,
        format = "0.000"
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
  
  

# Reset Survey Data Table -------------------------------------------------
  observe({
    
    coverScale <- coverScale()
    
    if(coverScale == "none"){
      
      cover_type <- "numeric"
      cover_format <- NULL
      cover_source <- NULL
      surveyData_reset <- surveyData_long_init
      
    } else if(coverScale == "percentage"){
      
      cover_type <- "numeric"
      cover_format <- "0.00" #0%
      cover_source <- NULL
      surveyData_reset <- surveyData_long_init
      
    } else if(coverScale == "proportional"){
      
      cover_type <- "numeric"
      cover_format <- "0.000"
      cover_source <- NULL
      surveyData_reset <- surveyData_long_init
      
    } else if(coverScale == "domin"){
      
      cover_type <- "dropdown"
      cover_format <- NULL
      cover_source <- RMAVIS:::domin_options
      surveyData_reset <- surveyData_long_init |>
        dplyr::mutate(Cover = as.factor("+"))
      
    } else if(coverScale == "braunBlanquet"){
      
      cover_type <- "dropdown"
      cover_format <- NULL
      cover_source <- RMAVIS:::braunBlanquet_options
      surveyData_reset <- surveyData_long_init |>
        dplyr::mutate(Cover = as.factor("+"))
      
    }
    
    output$surveyData <- rhandsontable::renderRHandsontable({
      
      surveyData <- rhandsontable::rhandsontable(data = surveyData_reset,
                                                 height = 800,
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
          source = speciesNames(),
          strict = FALSE,
          default = as.character(NA_character_)
        ) |>
        rhandsontable::hot_col(
          col = "Cover",
          readOnly = FALSE,
          type = cover_type,
          strict = FALSE,
          format = cover_format,
          source = cover_source
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
    bindEvent(clearTable(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

# Update Survey Data Table ------------------------------------------------
  observe({
    
    inputMethod <- inputMethod()
    selectedExampleData <- selectedExampleData()
    
    shiny::isolate({
      
      coverScale <- coverScale()
      
      if(inputMethod == "manual"){
        
        surveyData <- rhandsontable::hot_to_r(input$surveyData)
        
        readOnly_value <- FALSE
        
      } else if(inputMethod == "example"){
        
        surveyData <- rhandsontable::hot_to_r(input$surveyData)
        
        readOnly_value <- TRUE
        
        if(selectedExampleData != "none"){
          
          surveyData <- RMAVIS::example_data |>
            magrittr::extract2(selectedExampleData) |>
            dplyr::select(-Site) |>
            dplyr::arrange(Year, Group, Quadrat)
          
        }
        
      } else if(inputMethod == "upload"){
        
        surveyData <- rhandsontable::hot_to_r(input$surveyData)
        
        readOnly_value <- FALSE
        
        if(!is.null(uploadDataTable())){
          
          surveyData <- uploadDataTable()
          
        }
        
      }
      
      if(coverScale == "none"){

        cover_type <- "numeric"
        cover_format <- NULL
        cover_source <- NULL

      } else if(coverScale == "percentage"){

        cover_type <- "numeric"
        cover_format <- "0.00" #0%
        cover_source <- NULL

      } else if(coverScale == "proportional"){

        cover_type <- "numeric"
        cover_format <- "0.000"
        cover_source <- NULL

      } else if(coverScale == "domin"){

        cover_type <- "dropdown"
        cover_format <- NULL
        cover_source <- RMAVIS:::domin_options

      } else if(coverScale == "braunBlanquet"){

        cover_type <- "dropdown"
        cover_format <- NULL
        cover_source <- RMAVIS:::braunBlanquet_options

      }
      
    }) # close isolate

    output$surveyData <- rhandsontable::renderRHandsontable({

      surveyData <- rhandsontable::rhandsontable(data = surveyData,
                                                 height = 800,
                                                 rowHeaders = NULL,
                                                 width = "100%"#,
                                                 # overflow = "visible",
                                                 # stretchH = "all"
                                                 ) |>
        rhandsontable::hot_col(col = colnames(surveyData), halign = "htCenter") |>
        rhandsontable::hot_col(
          col = "Year",
          readOnly = readOnly_value,
          type = "numeric"
        ) |>
        rhandsontable::hot_col(
          col = "Group",
          readOnly = readOnly_value,
          type = "text"
        ) |>
        rhandsontable::hot_col(
          col = "Quadrat",
          readOnly = readOnly_value,
          type = "text"
        ) |>
        rhandsontable::hot_col(
          col = "Species",
          readOnly = readOnly_value,
          type = "dropdown",
          source = speciesNames(),
          strict = FALSE,
          default = as.character(NA_character_)
        ) |>
        rhandsontable::hot_col(
          col = "Cover",
          readOnly = readOnly_value,
          type = cover_type,
          strict = FALSE,
          format = cover_format,
          source = cover_source
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
              coverScale(),
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
      
      surveyData <- rhandsontable::hot_to_r(input$surveyData) |>
        dplyr::mutate("Quadrat" = as.character(Quadrat))
      
      if(!is.null(reallocateGroupsTable())){
        
        reallocateGroupsTable <- reallocateGroupsTable() |>
          dplyr::mutate("Quadrat" = as.character(Quadrat))
        
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
  

# Adjust/Correct Species Names --------------------------------------------
  observe({
    
    shiny::req(surveyData_corrected_rval())
    
    surveyData_corrected <- surveyData_corrected_rval()
    coverScale <- coverScale()
    
    if(coverScale == "none"){
      
      cover_type <- "numeric"
      cover_format <- NULL
      cover_source <- NULL
      
    } else if(coverScale == "percentage"){
      
      cover_type <- "numeric"
      cover_format <- "0.00" #0%
      cover_source <- NULL
      
    } else if(coverScale == "proportional"){
      
      cover_type <- "numeric"
      cover_format <- "0.000"
      cover_source <- NULL
      
    } else if(coverScale == "domin"){
      
      cover_type <- "dropdown"
      cover_format <- NULL
      cover_source <- RMAVIS:::domin_options
      
    } else if(coverScale == "braunBlanquet"){
      
      cover_type <- "dropdown"
      cover_format <- NULL
      cover_source <- RMAVIS:::braunBlanquet_options
      
    }
    
    output$surveyData <- rhandsontable::renderRHandsontable({
      
      surveyData <- rhandsontable::rhandsontable(data = surveyData_corrected,
                                                 height = 800,
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
          source = speciesNames(),
          strict = FALSE,
          default = as.character(NA_character_)
        ) |>
        rhandsontable::hot_col(
          col = "Cover",
          readOnly = FALSE,
          type = cover_type,
          strict = FALSE,
          format = cover_format,
          source = cover_source
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
    bindEvent(surveyData_corrected_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

# Save Survey Table to Reactive Val ---------------------------------------
  # surveyData_rval <- reactiveVal(surveyDataR6)
  surveyData_rval <- reactiveVal(list(
    "surveyData_original" = NULL,
    "surveyData_long" = NULL,
    "surveyData_wide" = NULL,
    "surveyData_mat" = NULL
  ))
  
  observe({
    
    coverScale <- coverScale()
    surveyData <- surveyData_rval()
    
    surveyData$surveyData_original <- rhandsontable::hot_to_r(input$surveyData)
    
    surveyData_long <- surveyData$surveyData_original
    
    # if(coverScale == "none"){
    #   
    #   surveyData_long <- surveyData$surveyData_original
    #   
    # } else if(coverScale == "percentage"){
    #   
    #   surveyData_long <- surveyData$surveyData_original |>
    #     dplyr::mutate("Cover" = Cover / 100)
    #   
    # } else if(coverScale == "proportional"){
    #   
    #   surveyData_long <- surveyData$surveyData_original
    #   
    # } else if(coverScale == "domin"){
    #   
    #   surveyData_long <- surveyData$surveyData_original |>
    #     dplyr::mutate("Cover" = as.character(Cover)) |>
    #     dplyr::left_join(RMAVIS:::dominConvert, by = c("Cover")) |>
    #     dplyr::select(-Cover) |>
    #     dplyr::rename("Cover" = "Value")
    #   
    # } else if(coverScale == "braunBlanquet"){
    #   
    #   surveyData_long <- rhandsontable::hot_to_r(input$surveyData) |>
    #     dplyr::mutate("Cover" = as.character(Cover)) |>
    #     dplyr::left_join(RMAVIS:::braunBlanquetConvert, by = c("Cover")) |>
    #     dplyr::select(-Cover) |>
    #     dplyr::rename("Cover" = "Value")
    #   
    # }
    
    # Ensure Group and Quadrat columns are of class character
    surveyData_long <- surveyData_long |>
      dplyr::mutate(Group = as.character(Group),
                    Quadrat = as.character(Quadrat))
    
    # Store surveyData_long
    surveyData$surveyData_long <- surveyData_long
    surveyData_rval(surveyData)

  }) |>
    bindEvent(input$surveyData,
              ignoreInit = TRUE,
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
    
    # Check whether any and all cover values are supplied
    coverSupplied <- surveyDataValidation()$coverSupplied
    
    # isolate({
      
      # Retrieve long survey table
      surveyData <- surveyData_rval()
      surveyData_long <- surveyData$surveyData_long
      
      # I currently need this if statement as the surveyDataValidation()$speciesComplete statement isn't being triggered correctly.
      if(all(!is.na(surveyData_long$Species))){
        
        if(coverSupplied == FALSE){
          
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
          
        } else if(coverSupplied == TRUE){
          
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
          
        } else {
          
          surveyData_wide <- NULL
          surveyData_mat <- NULL
        }
        
        surveyData$surveyData_wide <- surveyData_wide
        surveyData$surveyData_mat <- surveyData_mat
        surveyData_rval(surveyData)
        
        
      }
      
    # })

  }) |>
    bindEvent(surveyDataValidation(),
              surveyData_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
    
  # Ensure table is created whilst hidden.
  outputOptions(output, "surveyData", suspendWhenHidden = FALSE)
  
  return(surveyData_rval)

}
