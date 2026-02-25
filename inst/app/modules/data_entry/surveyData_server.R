surveyData <- function(input, output, session, uploadDataTable, setupData, surveyDataValidator, deSidebar_options) {
  
  ns <- session$ns
  
# Retrieve Setup Data -----------------------------------------------------
  region <- reactiveVal()
  regional_availability <- reactiveVal()
  exampleData <- reactiveVal()
  speciesNames <- reactiveVal()
  aggLookup <- reactiveVal()
  taxa_lookup <- reactiveVal()
  sd_taxon_name_col <- reactiveVal()
  
  observe({
    
    shiny::isolate({
      region(setupData()$region)
      regional_availability(setupData()$regional_availability)
      exampleData(setupData()$example_data)
      speciesNames(setupData()$species_names)
      aggLookup(setupData()$agg_lookup)
      taxa_lookup(setupData()$taxa_lookup)
      sd_taxon_name_col(setupData()$sd_taxon_name_col)
    })
    
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
  aggTaxa <- reactiveVal()
  matchAccepted <- reactiveVal()
  
  speciesAdjustmentTable <- reactiveVal()
  reallocateGroupsTable <- reactiveVal()
  surveyDataValidation <- reactiveVal()
  
  okToProceed <- reactiveVal()
  
  observe({
    
    adjustSpecies(surveyDataValidator()$adjustSpecies)
    reallocateGroups(surveyDataValidator()$reallocateGroups)
    combineDuplicates(surveyDataValidator()$combineDuplicates)
    aggTaxa(surveyDataValidator()$aggTaxa)
    matchAccepted(surveyDataValidator()$matchAccepted)
    
    speciesAdjustmentTable(surveyDataValidator()$speciesAdjustmentTable)
    reallocateGroupsTable(surveyDataValidator()$reallocateGroupsTable)
    surveyDataValidation(surveyDataValidator()$surveyDataValidation)
    
    okToProceed(surveyDataValidator()$okToProceed)
    
  }) |>
    bindEvent(surveyDataValidator(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
 

# Establish constants -----------------------------------------------------
  surveyData_gbnvc_init <- data.frame("Year" = as.integer(rep(as.numeric(format(Sys.Date(), "%Y")), 20)),
                                      "Group" = as.character(rep("A", 20)),
                                      "Quadrat" = as.character(rep("1", 20)),
                                      "Species" = as.character(rep(NA_character_, 20)),
                                      "Cover" = as.numeric(rep(NA_real_, 20)))
  
  surveyData_mnnpc_init <- data.frame("Year" = as.integer(rep(as.numeric(format(Sys.Date(), "%Y")), 20)),
                                      "Group" = as.character(rep("A", 20)),
                                      "Releve.Number" = as.character(rep("1", 20)),
                                      "Phys.Code" = as.character(rep("G", 20)),
                                      "Min.Ht" = as.integer(rep(1, 20)),
                                      "Max.Ht" = as.integer(rep(1, 20)),
                                      "Taxon" = as.character(rep(NA_character_, 20)),
                                      "Cover" = as.numeric(rep(NA_real_, 20)))


# Establish module-specific functions -------------------------------------
  format_columns_gbnvc <- function(.data){
    
    .data  |>
      rhandsontable::hot_col(
        col = "Year",
        readOnly = FALSE,
        type = "numeric",
        format = "0"
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
      )
    
  }
  
  format_columns_mnnpc <- function(.data){
    
    .data  |>
      rhandsontable::hot_col(
        col = "Year",
        readOnly = FALSE,
        type = "numeric",
        format = "0"
      ) |>
      rhandsontable::hot_col(
        col = "Group",
        readOnly = FALSE,
        type = "text"
      ) |>
      rhandsontable::hot_col(
        col = "Releve.Number",
        readOnly = FALSE,
        type = "text"
      ) |>
      rhandsontable::hot_col(
        col = "Phys.Code",
        readOnly = FALSE,
        type = "dropdown",
        source = unique(MNNPC::mnnpc_strata$physcode),
        strict = FALSE,
        default = as.character(NA_character_)
      ) |>
      rhandsontable::hot_col(
        col = "Min.Ht",
        readOnly = FALSE,
        type = "dropdown",
        source = MNNPC::mnnpc_ht_conv$ht,
        strict = FALSE,
        default = as.character(NA_character_)
      ) |>
      rhandsontable::hot_col(
        col = "Max.Ht",
        readOnly = FALSE,
        type = "dropdown",
        source = MNNPC::mnnpc_ht_conv$ht,
        strict = FALSE,
        default = as.character(NA_character_)
      ) |>
      rhandsontable::hot_col(
        col = "Taxon",
        readOnly = FALSE,
        type = "dropdown",
        source = speciesNames(),
        strict = FALSE,
        default = as.character(NA_character_)
      )
    
  }
  
# Establish initial table ------------------------------------------------
  surveyData_init <- reactiveVal()
  
  observe({
    
    if(region() == "gbnvc"){
      
      surveyData_init(surveyData_gbnvc_init)
      
    } else if(region() == "mnnpc"){
      
      surveyData_init(surveyData_mnnpc_init)
      
    }
    
    
  }) |>
    bindEvent(region(),
              ignoreInit = FALSE,
              ignoreNULL = FALSE)
  
# Initialise Survey Data Entry Table --------------------------------------
  output$surveyData <- rhandsontable::renderRHandsontable({
    
    surveyData <- rhandsontable::rhandsontable(data = surveyData_init(),
                                               height = 800,
                                               rowHeaders = NULL,
                                               width = "100%"#,
                                               # overflow = "visible"
                                               # stretchH = "all"
                                               ) |>
      rhandsontable::hot_col(col = colnames(surveyData_init()), halign = "htCenter") |>
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
      function(el, x) {
        var hot = this.hot
        $('a[data-value=\"surveyData_panel\"').on('click', function(){
          setTimeout(function() {hot.render();}, 0);
        })
      }")
    
    if(region() == "mnnpc"){
      
      surveyData <- surveyData |>
        format_columns_mnnpc()
      
    } else if(region() == "gbnvc"){
      
      surveyData <- surveyData |>
        format_columns_gbnvc()
      
    }
    
    surveyData <- surveyData |>
      rhandsontable::hot_col(
        col = "Cover",
        readOnly = FALSE,
        type = cover_type(),
        source = cover_source(),
        strict = cover_strict(),
        format = cover_format()
      )
    
    return(surveyData)
    
  })
  

# Update cover options ----------------------------------------------------
  cover_type <- reactiveVal()
  cover_format <- reactiveVal()
  cover_source <- reactiveVal()
  cover_strict <- reactiveVal()
  
  observe({
    
    if(coverScale() == "none"){
      
      cover_type("numeric")
      cover_format(NULL)
      cover_source(NULL)
      cover_strict(NULL)
      
    } else if(coverScale() == "percentage"){
      
      cover_type("numeric")
      cover_format("0.00")
      cover_source(NULL)
      cover_strict(NULL)
      
    } else if(coverScale() == "proportional"){
      
      cover_type("numeric")
      cover_format("0.000")
      cover_source(NULL)
      cover_strict(NULL)
      
    } else if(coverScale() == "domin"){
      
      cover_type("dropdown")
      cover_format(NULL)
      cover_source(RMAVIS:::domin_options)
      cover_strict(TRUE)
      
    } else if(coverScale() == "braunBlanquet"){
      
      cover_type("dropdown")
      cover_format(NULL)
      cover_source(RMAVIS:::braunBlanquet_options)
      cover_strict(TRUE)
      
    }
    
  }) |>
    bindEvent(coverScale(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  

# Initialise surveyData_corrected -----------------------------------------
surveyData_corrected_rval <- reactiveVal()

# Clear Survey Data Table -------------------------------------------------
  observe({
    
    shiny::isolate({
      surveyData_init <- surveyData_init()
      cover_type <- cover_type()
      cover_format <- cover_format()
      cover_source <- cover_source()
      cover_strict <- cover_strict()
      region <- region()
    })
    
    output$surveyData <- rhandsontable::renderRHandsontable({
      
      surveyData <- rhandsontable::rhandsontable(data = surveyData_init,
                                                 height = 800,
                                                 rowHeaders = NULL,
                                                 width = "100%"#,
                                                 # overflow = "visible"
                                                 # stretchH = "all"
      ) |>
        rhandsontable::hot_col(col = colnames(surveyData_init), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"surveyData_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      if(region == "mnnpc"){
        
        surveyData <- surveyData |>
          format_columns_mnnpc()
        
      } else if(region == "gbnvc"){
        
        surveyData <- surveyData |>
          format_columns_gbnvc()
        
      }
      
      surveyData <- surveyData |>
        rhandsontable::hot_col(
          col = "Cover",
          readOnly = FALSE,
          type = cover_type,
          source = cover_source,
          strict = cover_strict,
          format = cover_format
        )
      
      # Reset surveyData_corrected_rval
      surveyData_corrected_rval(surveyData_init)
      
      return(surveyData)
      
    })
    
  }) |>
    bindEvent(clearTable(),
              region(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

# Reset surveyDataCorrected if input method changes -----------------------
  observe({
    
    shiny::isolate({
      surveyData_init <- surveyData_init()
    })
    
    surveyData_corrected_rval(surveyData_init)
    
  }) |>
    bindEvent(input$inputMethod,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

# Retrieve Survey Data Table ----------------------------------------------
  surveyDataTableData <- reactiveVal()
  
  observe({
    
    surveyDataTableData(rhandsontable::hot_to_r(input$surveyData))
    
  }) |> 
    bindEvent(input$surveyData,
              ignoreInit = FALSE,
              ignoreNULL = TRUE)

# Update Survey Data Table ------------------------------------------------
  observe({
    
    shiny::req(!is.null(surveyDataTableData()))
    
    shiny::isolate({
      
      surveyDataTableData <- surveyDataTableData()
      inputMethod <- inputMethod()
      selectedExampleData <- selectedExampleData()
      exampleData <- exampleData()
      cover_type <- cover_type()
      cover_format <- cover_format()
      cover_source <- cover_source()
      cover_strict <- cover_strict()
      region <- region()
      uploadDataTable <- uploadDataTable()
      
    }) # close isolate
    
    if(inputMethod == "manual"){
      
      surveyData <- surveyDataTableData
      
      readOnly_value <- FALSE
      
    } else if(inputMethod == "example"){
      
      surveyData <- surveyDataTableData
      
      readOnly_value <- TRUE
      
      if(selectedExampleData != "none"){
        
        surveyData <- exampleData |>
          magrittr::extract2(selectedExampleData) |>
          dplyr::select(-dplyr::any_of("Site"))
        
      }
      
    } else if(inputMethod == "upload"){
      
      surveyData <- surveyDataTableData
      
      readOnly_value <- FALSE
      
      if(!is.null(uploadDataTable)){
        
        surveyData <- uploadDataTable
        
      }
      
    }

    output$surveyData <- rhandsontable::renderRHandsontable({

      surveyData <- rhandsontable::rhandsontable(data = surveyData,
                                                 height = 800,
                                                 rowHeaders = NULL,
                                                 width = "100%"#,
                                                 # overflow = "visible",
                                                 # stretchH = "all"
                                                 ) |>
        rhandsontable::hot_col(col = colnames(surveyData), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"surveyData_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      if(region == "mnnpc"){

        surveyData <- surveyData |>
          format_columns_mnnpc()

      } else if(region == "gbnvc"){

        surveyData <- surveyData |>
          format_columns_gbnvc()

      }
      
      surveyData <- surveyData |>
        rhandsontable::hot_col(
          col = "Cover",
          readOnly = FALSE,
          type = cover_type,
          source = cover_source,
          strict = cover_strict,
          format = cover_format
        )
        

      return(surveyData)

    })
      
  }) |>
    bindEvent(inputMethod(),
              coverScale(),
              selectedExampleData(),
              uploadDataTable(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)

# Survey Table Validation Actions -----------------------------------------
  
## Adjust Species Names ---------------------------------------------------
  observe({

    req(speciesAdjustmentTable())
    req(surveyDataTableData())

    isolate({
      
      surveyData <- surveyDataTableData()
      speciesAdjustmentTable <- speciesAdjustmentTable()
      sd_taxon_name_col <- sd_taxon_name_col()
      
    })
    
    if(!is.null(speciesAdjustmentTable)){
      
      species_adjust <- speciesAdjustmentTable |>
        dplyr::rename("{sd_taxon_name_col}" := "Species.Submitted") |>
        dplyr::select(-Species.Ignore)
      
      surveyData_corrected <- surveyData |>
        tibble::as_tibble() |>
        dplyr::left_join(species_adjust, by = dplyr::join_by(!!!sd_taxon_name_col)) |>
        dplyr::mutate(
          "{sd_taxon_name_col}" := dplyr::case_when(
            is.na(Species.Adjusted) ~ .data[[sd_taxon_name_col]],
            TRUE ~ as.character(Species.Adjusted)
          )
        ) |>
        dplyr::filter(Species.Remove != "Yes" | is.na(Species.Remove)) |>
        dplyr::select(-Species.Adjusted, -Species.Remove)
      
      surveyData_corrected_rval(surveyData_corrected)
      
    }

  }) |>
    bindEvent(adjustSpecies(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

## Re-allocate Groups -----------------------------------------------------
  observe({
    
    req(reallocateGroupsTable())
    req(surveyDataTableData())
    
    shiny::isolate({
      
      surveyData <- surveyDataTableData() |>
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
    
    shiny::req(surveyDataTableData())
    
    shiny::isolate({
      
      surveyData <- surveyDataTableData()
      region <- region()
      
    })
    
    if(region == "gbnvc"){
      
      surveyData_noDuplicates <- surveyData |>
        dplyr::group_by(Year, Group, Quadrat, Species) |>
        dplyr::summarise("Cover" = sum(Cover)) |>
        dplyr::ungroup()
      
      surveyData_corrected_rval(surveyData_noDuplicates)
      
    }
    
  }) |>
    bindEvent(combineDuplicates(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

## Match to accepted taxa -------------------------------------------------
  observe({

    shiny::req(surveyDataTableData())

    isolate({
      surveyDataTableData <- surveyDataTableData()
      sd_taxon_name_col <- sd_taxon_name_col()
      taxa_lookup <- taxa_lookup()
    })
    
    surveyData_matchedAccepted <- surveyDataTableData |>
      dplyr::mutate("{sd_taxon_name_col}" := stringr::str_remove_all(string = .data[[sd_taxon_name_col]],
                                                                     pattern = "\\ss.s.$|\\ss.l.$|\\ss.a.$")) |>
      dplyr::mutate("{sd_taxon_name_col}" := .data[[sd_taxon_name_col]] |> dplyr::recode(!!!tibble::deframe(taxa_lookup[, 1:2])))
    
    surveyData_corrected_rval(surveyData_matchedAccepted)

  }) |>
    bindEvent(matchAccepted(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

  
## Adjust/Correct Species Names -------------------------------------------
  observe({
    
    shiny::req(surveyData_corrected_rval())
    
    shiny::isolate({
      surveyData_corrected <- surveyData_corrected_rval()
      cover_type <- cover_type()
      cover_format <- cover_format()
      cover_source <- cover_source()
      cover_strict <- cover_strict()
    })
    
    output$surveyData <- rhandsontable::renderRHandsontable({
      
      surveyData <- rhandsontable::rhandsontable(data = surveyData_corrected,
                                                 height = 800,
                                                 rowHeaders = NULL,
                                                 width = "100%"#,
                                                 # overflow = "visible",
                                                 # stretchH = "all"
                                                 ) |>
        rhandsontable::hot_col(col = colnames(surveyData_corrected), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"surveyData_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      if(region() == "mnnpc"){
        
        surveyData <- surveyData |>
          format_columns_mnnpc()
        
      } else if(region() == "gbnvc"){
        
        surveyData <- surveyData |>
          format_columns_gbnvc()
        
      }
      
      surveyData <- surveyData |>
        rhandsontable::hot_col(
          col = "Cover",
          readOnly = FALSE,
          type = cover_type,
          source = cover_source,
          strict = cover_strict,
          format = cover_format
        )
      
      return(surveyData)
      
    })
    
  }) |>
    bindEvent(surveyData_corrected_rval(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

# Save Survey Table to Reactive Val ---------------------------------------
  surveyData_rval <- reactiveVal(list(
    "surveyData_original" = NULL,
    "surveyData_long" = NULL,
    "surveyData_long_prop" = NULL
  ))
  
  observe({
    
    shiny::isolate({
      region <- region()
      regional_availability <- regional_availability()
      coverScale <- coverScale()
      surveyData <- surveyData_rval()
      surveyData_init <- surveyData_init()
      surveyDataTableData <- surveyDataTableData()
    })
    
    shiny::req(!is.null(surveyDataTableData))
    shiny::req(!isTRUE(all.equal(surveyDataTableData, surveyData_init)))
    
    # Save original data
    surveyData$surveyData_original <- surveyDataTableData
    
    # Process the data if the region is MNNPC
    if(region == "mnnpc"){
      
      print(coverScale)
      
      # assign(x = "surveyDataTableData", value = surveyDataTableData, envir = .GlobalEnv)
      # assign(x = "coverScale", value = coverScale, envir = .GlobalEnv)
      
      surveyData_long <- surveyDataTableData |>
        dplyr::distinct() |>
        dplyr::rename("year" = "Year",
                      "group" = "Group",
                      "relnumb" = "Releve.Number",
                      "physcode" = "Phys.Code",
                      "minht" = "Min.Ht",
                      "maxht" = "Max.Ht",
                      "taxon" = "Taxon",
                      "scov" = "Cover") |>
        MNNPC::process_dnr_releves(strip_suffixes = FALSE,
                                   match_to_accepted = FALSE,
                                   aggregate_into_assigned = TRUE,
                                   aggregate_into_analysis_groups = FALSE,
                                   cover_scale = coverScale) |>
        suppressWarnings() |>
        suppressMessages()
      
    } else {
      
      surveyData_long <- surveyDataTableData
      
    }
    
    # Ensure Group and Quadrat or Releve.Number columns are of class character
    surveyData_long <- surveyData_long |>
      dplyr::mutate(dplyr::across(dplyr::any_of(c("Group", "Quadrat")), as.character))
    
    # Create proportional data
    if(coverScale == "none"){

      surveyData_long_prop <- surveyData_long

    } else if(coverScale == "percentage" | region == "mnnpc"){

      surveyData_long_prop <- surveyData_long |>
        dplyr::mutate("Cover" = as.numeric(Cover) / 100)

    } else if(coverScale == "proportional"| region != "mnnpc"){

      surveyData_long_prop <- surveyData_long

    } else if(coverScale == "domin"| region != "mnnpc"){

      surveyData_long_prop <- surveyData_long |>
        dplyr::mutate("Cover" = as.character(Cover)) |>
        dplyr::left_join(RMAVIS:::dominConvert, by = c("Cover")) |>
        dplyr::select(-Cover) |>
        dplyr::rename("Cover" = "Value")

    } else if(coverScale == "braunBlanquet"| region != "mnnpc"){

      surveyData_long_prop <- surveyData_long |>
        dplyr::mutate("Cover" = as.character(Cover)) |>
        dplyr::left_join(RMAVIS:::braunBlanquetConvert, by = c("Cover")) |>
        dplyr::select(-Cover) |>
        dplyr::rename("Cover" = "Value")

    }
    
    # Ensure prop data cover values sum to 1
    surveyData_long_prop <- surveyData_long_prop |>
      dplyr::group_by(Year, Group, Quadrat) |>
      dplyr::mutate("Cover" = signif(Cover * (1 / sum(Cover, na.rm = TRUE)), digits = 2)) |>
      dplyr::ungroup()
    
    # Aggregate data
    if(isTRUE(regional_availability$aggTaxa)){
      
      surveyData_long_prop_agg <- surveyDataTableData |>
        dplyr::distinct() |>
        dplyr::mutate(dplyr::across(dplyr::any_of(c("Group", "Quadrat", "Releve.Number")), as.character)) |>
        dplyr::rename("year" = "Year",
                      "group" = "Group",
                      "relnumb" = "Releve.Number",
                      "physcode" = "Phys.Code",
                      "minht" = "Min.Ht",
                      "maxht" = "Max.Ht",
                      "taxon" = "Taxon",
                      "scov" = "Cover") |>
        MNNPC::process_dnr_releves(strip_suffixes = TRUE,
                                   match_to_accepted = TRUE,
                                   aggregate_into_assigned = FALSE,
                                   aggregate_into_analysis_groups = TRUE,
                                   cover_scale = coverScale) |>
        suppressMessages() |>
        suppressWarnings()|>
        dplyr::mutate("Cover" = as.numeric(Cover) / 100)|>
        dplyr::group_by(Year, Group, Quadrat) |>
        dplyr::mutate("Cover" = signif(Cover * (1 / sum(Cover, na.rm = TRUE)), digits = 2)) |>
        dplyr::ungroup()
      
    } else {
      
      surveyData_long_prop_agg <- NULL
      
    }
    
    # Store surveyData_long
    surveyData$surveyData_long <- surveyData_long
    surveyData$surveyData_long_prop <- surveyData_long_prop
    surveyData$surveyData_long_prop_agg <- surveyData_long_prop_agg
    surveyData_rval(surveyData)

  }) |>
    bindEvent(surveyDataTableData(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

    
# Ensure table is created whilst hidden.
  outputOptions(output, "surveyData", suspendWhenHidden = FALSE)
  
  return(surveyData_rval)

}
