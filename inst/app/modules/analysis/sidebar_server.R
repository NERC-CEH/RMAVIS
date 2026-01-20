sidebar <- function(input, output, session, 
                    region,
                    deSidebar_options,
                    setupData,
                    surveyData, surveyDataValidator, surveyDataSummary,
                    floristicTables, vcAssignment, habCor, speciesFreq,
                    avgEIVs, diversityAnalysis, 
                    mvaNationalRefResults) {
  
  ns <- session$ns
  
# Retrieve setup data -----------------------------------------------------
  unit_name_col <- reactiveVal()
  show_eivs <- reactiveVal()
  show_habCor <- reactiveVal()
  
  observe({
    
    unit_name_col(setupData()$unit_name_col)
    show_eivs(setupData()$regional_availability$avgEIVs)
    show_habCor(setupData()$regional_availability$habCor)
    
  }) |>
    shiny::bindEvent(setupData(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)
  
# Compose list of inputs to return from module ----------------------------
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "runAnalysis" = input$runAnalysis,
      "aggTaxaOpts" = input$aggTaxaOpts,
      "selectVCtypes" = input$selectVCtypes,
      "assignQuadrats" = input$assignQuadrats,
      "removeLowFreqTaxa" = input$removeLowFreqTaxa,
      "habitatRestriction" = input$habitatRestriction,
      "resultsViewVCAssign" = input$resultsViewVCAssign,
      "habCorClass" = input$habCorClass,
      "floristicTablesView" = input$floristicTablesView,
      "floristicTablesSetView" = input$floristicTablesSetView,
      "composedFloristicTable" = input$composedFloristicTable,
      "vcFloristicTable" = input$vcFloristicTable,
      "matchSpecies" = input$matchSpecies,
      "restrictVCFlorTablesOpts" = input$restrictVCFlorTablesOpts,
      "resultsViewEIVs"  = input$resultsViewEIVs,
      "resultsViewDiversity"  = input$resultsViewDiversity,
      "selectedReferenceSpaces" = input$selectedReferenceSpaces,
      "groupSurveyPlots" = input$groupSurveyPlots,
      "selectSurveyMethod" = input$selectSurveyMethod,
      "selectSurveyYears" = input$selectSurveyYears,
      "selectSurveyQuadrats" = input$selectSurveyQuadrats,
      "selectSurveyGroups" = input$selectSurveyGroups,
      "dcaAxisSelection" = input$dcaAxisSelection,
      "dcaVars" = input$dcaVars,
      "ccaVars" = input$ccaVars,
      "reportAuthorName" = input$reportAuthorName,
      "reportProjectName" = input$reportProjectName,
      "reportOptions" = input$reportOptions
    )
    
    sidebar_options(sidebar_options_list)
    
  }) |>
    bindEvent(input$runAnalysis,
              input$aggTaxaOpts,
              input$selectVCtypes,
              input$assignQuadrats,
              input$removeLowFreqTaxa,
              input$habitatRestriction, 
              input$resultsViewVCAssign,
              input$habCorClass, 
              input$floristicTablesView,
              input$floristicTablesSetView,
              input$composedFloristicTable,
              input$vcFloristicTable, 
              input$matchSpecies,
              input$restrictVCFlorTablesOpts,
              input$resultsViewEIVs,
              input$resultsViewDiversity,
              input$selectedReferenceSpaces,
              input$groupSurveyPlots,
              input$selectSurveyMethod,
              input$selectSurveyYears,
              input$selectSurveyQuadrats,
              input$selectSurveyGroups,
              input$dcaAxisSelection,
              input$dcaVars,
              input$ccaVars,
              input$reportAuthorName,
              input$reportProjectName,
              input$reportOptions,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

# Update remove low frequency taxa based on region ------------------------
  observe({
    
    if(region() == "gbnvc"){
      shinyWidgets::updateSwitchInput(session = session,
                                      inputId = "removeLowFreqTaxa",
                                      value = TRUE)
    } else if(region() == "mnnpc"){
      shinyWidgets::updateSwitchInput(session = session,
                                      inputId = "removeLowFreqTaxa",
                                      value = TRUE)
    }
    
  }) |>
    bindEvent(region(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)

# Show/Hide and Update aggregate taxa options -----------------------------
  observe({
    
    if(region() == "gbnvc"){
      
      shinyjs::hide(id = "aggTaxa_div")
      
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "aggTaxaOpts",
                                      choices = c("VC Assignment" = "vc_assign",
                                                  "Floristic Tables" = "floristic_tables",
                                                  "Frequency" = "frequency",
                                                  "EIVs" = "eivs",
                                                  "Diversity" = "diversity",
                                                  "MVA" = "mva"),
                                      selected = c("vc_assign", "floristic_tables", "mva"))
      
    } else if(region() == "mnnpc"){
      
      shinyjs::show(id = "aggTaxa_div")
      
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "aggTaxaOpts",
                                      choices = c("VC Assignment" = "vc_assign",
                                                  "Floristic Tables" = "floristic_tables",
                                                  "Frequency" = "frequency",
                                                  "Diversity" = "diversity",
                                                  "MVA" = "mva"),
                                      selected = c("vc_assign", "floristic_tables", "mva"))
      
    }
    
  }) |>
    bindEvent(region(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  

# Update select VC types --------------------------------------------------
  observe({
    
    if(region() == "gbnvc"){
      
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "selectVCtypes",
                                      choices = RMAVIS:::nvcType_options,
                                      selected = "Original",
                                      choicesOpt = list(
                                        style = rep_len("font-size: 100%; line-height: 1.6;", length(RMAVIS:::nvcType_options))
                                      ))
      
    } else if(region() == "mnnpc"){
      
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "selectVCtypes",
                                      choices = MNNPC::mnnpc_ecs_sections,
                                      selected = "statewide",
                                      choicesOpt = list(
                                        style = rep_len("font-size: 75%; line-height: 1.6;", length(MNNPC::mnnpc_ecs_sections))
                                      ))
    }
    
  }) |>
    bindEvent(region(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  
  

# Update habitat restriction prefixes -------------------------------------
  observe({
    
    if(region() == "gbnvc"){
      
      shiny::updateSelectizeInput(session = session,
                                  inputId = "habitatRestriction",
                                  choices = RMAVIS:::habitatRestrictionPrefixes,
                                  selected = NULL)
      
    } else if(region() == "mnnpc"){
      
      shiny::updateSelectizeInput(session = session,
                                  inputId = "habitatRestriction",
                                  choices = c(MNNPC::mnnpc_vc_systems_named, MNNPC::mnnpc_vc_systems_flreg_named),
                                  selected = NULL)
    }
    
  }) |>
    bindEvent(region(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)

  

# Update Options Based On Example Data ------------------------------------
  observe({
    
    de_inputMethod <- deSidebar_options()$inputMethod
    de_selectedExampleData <- deSidebar_options()$selectedExampleData
    
    if(de_inputMethod == "example" & "Original" %in% input$selectVCtypes & region() == "gbnvc"){
      
      if(de_selectedExampleData == "Parsonage Down"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "CG"
        )
        
      } else if(de_selectedExampleData == "Whitwell Common"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "M"
        )
        
      } else if(de_selectedExampleData == "Leith Hill Place Wood"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "W"
        )
        
      } else if(de_selectedExampleData == "Newborough Warren"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "SD"
        )
        
      }
      
    } else {
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "habitatRestriction",
        selected = character(0)
      )
      
    }
    
    
    if(de_inputMethod == "example"){
        
        shiny::updateTextInput(
          session = session,
          inputId = "reportProjectName",
          value = de_selectedExampleData
        )
      
    }
    
    
  }) |>
    bindEvent(deSidebar_options(),
              input$selectVCtypes,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
# Enable/disable selected action buttons ----------------------------------
  observe({
    
    shiny::req(!is.null(surveyData()$surveyData_long))
    
    shiny::isolate({
      okToProceed <- surveyDataValidator()$surveyDataValidation$okToProceed
      surveyData_long <- surveyData()$surveyData_long
    })

    if(okToProceed == TRUE & nrow(surveyData_long) > 0 & length(input$selectVCtypes) > 0 & !is.null(input$selectVCtypes)){

      shinyjs::enable(id = "runAnalysis")
      shinyjs::enable(id = "generateReport")

    } else if(okToProceed == FALSE | length(input$selectVCtypes) == 0 | is.null(input$selectVCtypes)){

      shinyjs::disable(id = "runAnalysis")
      shinyjs::disable(id = "generateReport")

    }

  }) |>
    bindEvent(surveyDataValidator(),
              input$selectVCtypes,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

# Show/Hide Floristic Table Options ---------------------------------------
  observe({
    
    if(input$floristicTablesView == "singleComposedVsVC") {
      
      shinyjs::hide(id = "floristicTablesSetViewOpts_div")
      shinyjs::show(id = "restrictVCFlorTablesOpts_div")
      shinyjs::show(id = "vcFloristicTable_div")
      shinyjs::show(id = "composedFloristicTable_div")
      shinyjs::show(id = "matchSpecies_div")
      
    } else if(input$floristicTablesView == "multipleComposed") {
      
      shinyjs::show(id = "floristicTablesSetViewOpts_div")
      shinyjs::hide(id = "restrictVCFlorTablesOpts_div")
      shinyjs::hide(id = "vcFloristicTable_div")
      shinyjs::hide(id = "composedFloristicTable_div")
      shinyjs::hide(id = "matchSpecies_div")
      
    }
    
  }) |>
    bindEvent(input$floristicTablesView,
              ignoreInit = FALSE)
  

# Show/Hide accordion panels ----------------------------------------------
  observe({

    if(isTRUE(show_eivs())) {
      shinyjs::show(id = "eivs_accordion_panel_div")
    } else {
      shinyjs::hide(id = "eivs_accordion_panel_div")
    }

    if(isTRUE(show_habCor())) {
      shinyjs::show(id = "habCor_accordion_panel_div")
    } else {
      shinyjs::hide(id = "habCor_accordion_panel_div")
    }

  }) |>
    bindEvent(show_habCor(),
              show_eivs(),
              ignoreInit = FALSE)
  

# Update assignQuadrats Switch --------------------------------------------
  observe({
    
    req(surveyDataSummary())
    
    surveyDataSummary <- surveyDataSummary()
    
    # If there are any years with less than n (the threshold) quadrats, enable
    # assignQuadrats as the group similarities will no be calculated.
    threshold <- 5
    years_quadrats_less_threshold <- surveyDataSummary$surveyDataStructure$quadratsPerYear |>
      dplyr::filter(quadratsPerYear < threshold) |>
      nrow()
    fix_assignQuadrats_enabled <- isTRUE(years_quadrats_less_threshold > 0)
    
    if(fix_assignQuadrats_enabled == TRUE){
      
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "assignQuadrats",
        value = TRUE,
        disabled = TRUE
      )
      
      # shinyjs::disable(id = "resultsViewVCAssign")
      
    } else if(fix_assignQuadrats_enabled == FALSE){
      
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "assignQuadrats",
        value = NULL,
        disabled = FALSE
      )
      
      # shinyjs::enable(id = "resultsViewVCAssign")
      
    }
    
  }) |>
    bindEvent(surveyDataSummary(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

# Reactively update vcFloristicTable options -----------------------------
  observe({
    
    shiny::req(vcAssignment())
    shiny::req(setupData())
    
    vcAssignment <- vcAssignment()
    setupData <- setupData()
    
    topVCCommunities <- vcAssignment$topVCCommunities
    vc_code_values <- unique(setupData$floristic_tables[[unit_name_col()]])
    
    if(input$restrictVCFlorTablesOpts == TRUE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "vcFloristicTable",
        choices = topVCCommunities,
        selected = topVCCommunities[1]
      )
      
    } else if(input$restrictVCFlorTablesOpts == FALSE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "vcFloristicTable",
        choices = vc_code_values,
        selected = topVCCommunities[1]
      )
      
    }
    
  }) |>
    bindEvent(input$restrictVCFlorTablesOpts,
              vcAssignment(),
              setupData(),
              ignoreInit = TRUE)
  

  # Reactively update floristicTablesSetView options ------------------------
  observe({
    
    shiny::isolate({
      
      floristicTables <- floristicTables()
      
    })
    
    if(nrow(floristicTables$floristicTables_composed_all_wide) != 0){
      
      uniq_IDs <- floristicTables$floristicTables_composed_all_wide |>
        dplyr::pull(Group) |>
        unique()
      
      names(uniq_IDs) <- uniq_IDs
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "floristicTablesSetView",
        choices = uniq_IDs,
        selected = character(0),
        server = FALSE
      )
      
    }
    
  }) |>
    bindEvent(vcAssignment(),
              # floristicTables(),
              ignoreInit = TRUE)
  
  
  # Reactively update composedFloristicTable options ------------------------
  observe({
    
    if(nrow(floristicTables()$floristicTables_composed_all) != 0){
      
      uniq_IDs <- floristicTables()$floristicTables_composed_all |>
        dplyr::pull(ID) |>
        unique()
      
      names(uniq_IDs) <- uniq_IDs
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "composedFloristicTable",
        choices = uniq_IDs,
        selected = uniq_IDs[1],
        server = FALSE
      )
      
    }
    
  }) |>
    bindEvent(floristicTables(),
              ignoreInit = TRUE)
  
  
# Reactively update DCA survey quadrat selection method -------------------
  observe({
    
    if(input$selectSurveyMethod == "all") {
      
      shinyjs::hide(id = "selectSurveyYears_div")
      shinyjs::hide(id = "selectSurveyQuadrats_div")
      shinyjs::hide(id = "selectSurveyGroups_div")
      
    } else if(input$selectSurveyMethod == "selectYears") {
      
      shinyjs::show(id = "selectSurveyYears_div")
      shinyjs::hide(id = "selectSurveyGroups_div")
      shinyjs::hide(id = "selectSurveyQuadrats_div")
      
    } else if(input$selectSurveyMethod == "selectGroups") {
      
      shinyjs::hide(id = "selectSurveyYears_div")
      shinyjs::show(id = "selectSurveyGroups_div")
      shinyjs::hide(id = "selectSurveyQuadrats_div")
      
    } else if(input$selectSurveyMethod == "selectQuadrats") {
      
      shinyjs::hide(id = "selectSurveyYears_div")
      shinyjs::hide(id = "selectSurveyGroups_div")
      shinyjs::show(id = "selectSurveyQuadrats_div")
      
    }
    
    # This goes second as it takes priority
    if(input$groupSurveyPlots == "no"){
      
      shinyjs::show(id = "selectSurveyMethod_div")
      
    } else if(input$groupSurveyPlots == "group" || input$groupSurveyPlots == "year") {
      
      shinyjs::hide(id = "selectSurveyMethod_div")
      shinyjs::hide(id = "selectSurveyYears_div")
      shinyjs::hide(id = "selectSurveyQuadrats_div")
      shinyjs::hide(id = "selectSurveyGroups_div")
      
    }
    
  }) |>
    bindEvent(input$groupSurveyPlots,
              input$selectSurveyMethod, 
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  
  # Reactively update global reference DCA space selection ------------------
  observe({
    
    shiny::isolate({
      vcAssignment <- vcAssignment()
      setupData <- setupData()
      unit_name_col <- unit_name_col()
    })
    
    topVCCommunities <- vcAssignment$topVCCommunities
    
    selectedReferenceSpaces_options <- unique(setupData$floristic_tables[[unit_name_col]])
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selectedReferenceSpaces",
      choices = selectedReferenceSpaces_options,
      selected = topVCCommunities
    )
    
  }) |>
    bindEvent(vcAssignment(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  # Reactively update DCA survey quadrat selection options ------------------
  observe({
    
    surveyData <- surveyData()
    surveyData_long <- surveyData$surveyData_long
    
    if(is.null(mvaNationalRefResults()) == FALSE){
      
      uniq_years <- surveyData_long |>
        dplyr::pull(Year) |>
        unique()
      
      uniq_quadrats <- surveyData_long |>
        dplyr::pull(Quadrat) |>
        unique()
      
      uniq_groups <- surveyData_long |>
        dplyr::pull(Group) |>
        unique()
      
      if(input$selectSurveyMethod == "all" || input$groupSurveyPlots != "no"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyYears",
          choices = uniq_years,
          selected = uniq_years,
          server = FALSE
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyGroups",
          choices = uniq_groups,
          selected = uniq_groups,
          server = FALSE
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyQuadrats",
          choices = uniq_quadrats,
          selected = uniq_quadrats,
          server = FALSE
        )
        
        
      } else if(input$selectSurveyMethod == "selectYears"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyYears",
          choices = uniq_years,
          selected = uniq_years[1],
          server = FALSE
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyGroups",
          choices = NULL,
          selected = NULL,
          server = FALSE
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyQuadrats",
          choices = NULL,
          selected = NULL,
          server = FALSE
        )
        
        
      } else if(input$selectSurveyMethod == "selectGroups"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyYears",
          choices = NULL,
          selected = NULL,
          server = FALSE
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyGroups",
          choices = uniq_groups,
          selected = uniq_groups[1],
          server = FALSE
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyQuadrats",
          choices = NULL,
          selected = NULL,
          server = FALSE
        )
        
        
      } else if(input$selectSurveyMethod == "selectQuadrats"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyYears",
          choices = NULL,
          selected = NULL,
          server = FALSE
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyGroups",
          choices = NULL,
          selected = NULL,
          server = FALSE
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "selectSurveyQuadrats",
          choices = uniq_quadrats,
          selected = uniq_quadrats[1],
          server = FALSE
        )
      }
    }
    
  }) |>
    bindEvent(input$selectSurveyMethod,
              input$groupSurveyPlots,
              ignoreInit = TRUE)

# Update dcaVars checkboxGroupInput and ccaVars_div ----------------------
  observe({
    
    updated_dcaVars_options <- RMAVIS:::dcaVars_options
    
    if(isFALSE(show_eivs())){
      updated_dcaVars_options <- updated_dcaVars_options[names(updated_dcaVars_options) != "Hill-Ellenberg"]
    }
    
    shiny::updateCheckboxGroupInput(session = session,
                                    inputId = "dcaVars",
                                    choices = updated_dcaVars_options,
                                    selected = c("referenceSpace", "surveyQuadrats"))
    
    if(isTRUE(show_eivs())){
      
      shinyjs::show(id = "ccaVars_div")
      
    } else if(isFALSE(show_eivs())){
      
      shinyjs::hide(id = "ccaVars_div")
      
    }
    
    
  }) |> 
    shiny::bindEvent(show_eivs(),
                     ignoreInit = TRUE,
                     ignoreNULL = FALSE)

# Update report options ---------------------------------------------------
  observe({
    
    updated_reportOptions_options <- RMAVIS:::reportOptions_options
    
    if(isFALSE(show_eivs())){
      updated_reportOptions_options <- updated_reportOptions_options[names(updated_reportOptions_options) != "EIVs (incl. Mean Hill-Ellenberg)"]
    }
    
    if(isFALSE(show_habCor())){
      updated_reportOptions_options <- updated_reportOptions_options[names(updated_reportOptions_options) != "Habitat Correspondence"]
    }
    
    shinyWidgets::updatePickerInput(inputId = "reportOptions",
                                    choices = updated_reportOptions_options,
                                    selected = c("vcAssignmentResultsSite_Czekanowski", 
                                                 "composedFloristicTablesSite", 
                                                 "speciesFrequencyTable",
                                                 "diversity_year"))
    
    
  }) |> 
    shiny::bindEvent(show_eivs(),
                     show_habCor(),
                     ignoreInit = TRUE,
                     ignoreNULL = FALSE)

# Download RMAVIS Results -----------------------------------------------
  output$downloadRMAVISResults <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.Results.",
             "v1-2-0.",
             format(Sys.time(), "%y-%m-%d.%H-%M-%S"),
             ".xlsx",
             sep="")
      
    },
    
    content = function(file) {
      
      floristicTables <- floristicTables()
      vcAssignment <- vcAssignment()
      habCor <- habCor()
      speciesFreq <- speciesFreq()
      avgEIVs <- avgEIVs()
      diversityAnalysis <- diversityAnalysis()
      
      sheets <- list(
        "Floristic Tables - Long" = floristicTables$floristicTables_composed_all,
        "Floristic Tables - Wide" = floristicTables$floristicTables_composed_all_wide,
        "VC Assignment - Quadrat" = vcAssignment$vcAssignmentPlot_Jaccard,
        "Frequency Table" = speciesFreq,
        "Diversity, Year" = diversityAnalysis$diversityYear,
        "Diversity, Group" = diversityAnalysis$diversityGroup,
        "Diversity, Quadrat" = diversityAnalysis$diversityQuadrat
      )
      
      if(isTRUE(show_habCor)){
        sheets[["Habitat Correspondences"]] <- habCor
      }
      
      if(isTRUE(show_eivs)){
        sheets[["EIVs, Weighted, Site"]] <- avgEIVs$weightedMeanHEValuesSite
        sheets[["EIVs, Unweighted, Site"]] <- avgEIVs$unweightedMeanHEValuesSite
        sheets[["EIVs, Weighted, Group"]] <- avgEIVs$weightedMeanHEValuesGroup
        sheets[["EIVs, Unweighted, Group"]] <- avgEIVs$unweightedMeanHEValuesGroup
        sheets[["EIVs, Weighted, Quadrat"]] <- avgEIVs$weightedMeanHEValuesQuadrat
        sheets[["EIVs, Unweighted, Quadrat"]] <- avgEIVs$unweightedMeanHEValuesQuadrat
      }
      
      if(!is.null(vcAssignment$vcAssignmentGroup_Czekanowski)){
        sheets[["VC Assignment - Group"]] <- vcAssignment$vcAssignmentGroup_Czekanowski
      }
      
      if(!is.null(vcAssignment$vcAssignmentSite_Czekanowski)){
        sheets[["VC Assignment - Site"]] <- vcAssignment$vcAssignmentSite_Czekanowski
      }
      
      sheets <- sheets[sort(names(sheets), decreasing = TRUE)]
      
      writexl::write_xlsx(x = sheets, path = file)
      
    }
  )
  
# Return sidebar options --------------------------------------------------
  return(sidebar_options)
  
}



