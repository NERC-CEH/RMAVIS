sidebar <- function(input, output, session, 
                    deSidebar_options,
                    setupData,
                    surveyData, surveyDataValidator, surveyDataSummary,
                    floristicTables, nvcAssignment, habCor, speciesFreq,
                    avgEIVs, diversityAnalysis, 
                    mvaLocalRefRestrictedResults) {
  
  ns <- session$ns
  
# Compose list of inputs to return from module ----------------------------
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "runAnalysis" = input$runAnalysis,
      "selectNVCtypes" = input$selectNVCtypes,
      "assignQuadrats" = input$assignQuadrats,
      "removeLowFreqTaxa" = input$removeLowFreqTaxa,
      "habitatRestriction" = input$habitatRestriction,
      "nTopResults" = input$nTopResults,
      "resultsViewNVCAssign" = input$resultsViewNVCAssign,
      "habCorClass" = input$habCorClass,
      "floristicTablesView" = input$floristicTablesView,
      "floristicTablesSetView" = input$floristicTablesSetView,
      "composedFloristicTable" = input$composedFloristicTable,
      "nvcFloristicTable" = input$nvcFloristicTable,
      "matchSpecies" = input$matchSpecies,
      "restrictNVCFlorTablesOpts" = input$restrictNVCFlorTablesOpts,
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
              input$selectNVCtypes,
              input$assignQuadrats,
              input$removeLowFreqTaxa,
              input$habitatRestriction, 
              input$nTopResults,
              input$resultsViewNVCAssign,
              input$habCorClass, 
              input$floristicTablesView,
              input$floristicTablesSetView,
              input$composedFloristicTable,
              input$nvcFloristicTable, 
              input$matchSpecies,
              input$restrictNVCFlorTablesOpts,
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
              ignoreInit = FALSE)


# Update Options Based On Example Data ------------------------------------
  observe({
    
    deSidebar_options <- deSidebar_options()
    
    de_inputMethod <- deSidebar_options$inputMethod
    de_selectedExampleData <- deSidebar_options$selectedExampleData
    
    if(de_inputMethod == "example" & "Original" %in% input$selectNVCtypes){
      
      if(de_selectedExampleData == "Parsonage Down"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "CG"
        )
        
        shiny::updateTextInput(
          session = session,
          inputId = "reportProjectName",
          value = "Parsonage Down"
        )
        
      } else if(de_selectedExampleData == "Whitwell Common"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "M"
        )
        
        shiny::updateTextInput(
          session = session,
          inputId = "reportProjectName",
          value = "Whitwell Common"
        )
        
      } else if(de_selectedExampleData == "Leith Hill Place Wood"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "W"
        )
        
        shiny::updateTextInput(
          session = session,
          inputId = "reportProjectName",
          value = "Leith Hill Place Wood"
        )
        
      } else if(de_selectedExampleData == "Newborough Warren"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "SD"
        )
        
        shiny::updateTextInput(
          session = session,
          inputId = "reportProjectName",
          value = "Newborough Warren"
        )
        
      }
      
    } else if(de_inputMethod == "example" & !("Original" %in% input$selectNVCtypes)){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "habitatRestriction",
        selected = character(0)
      )
      
    } else if(de_inputMethod != "example" & !("Original" %in% input$selectNVCtypes)){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "habitatRestriction",
        selected = character(0)
      )
      
    }
    
  }) |>
    bindEvent(deSidebar_options(),
              input$selectNVCtypes,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
# Disable selected action buttons if okToProceed == FALSE ---------------
  observe({

    surveyDataValidator <- surveyDataValidator()

    okToProceed <- surveyDataValidator$surveyDataValidation$okToProceed

    if(okToProceed == TRUE & nrow(surveyData()$surveyData_long) > 0 & length(input$selectNVCtypes) > 0){

      shinyjs::enable(id = "runAnalysis")
      shinyjs::enable(id = "generateReport")

    } else if(okToProceed == FALSE | length(input$selectNVCtypes) == 0 | is.null(input$selectNVCtypes)){

      shinyjs::disable(id = "runAnalysis")
      shinyjs::disable(id = "generateReport")

    }

  }) |>
    bindEvent(surveyDataValidator(),
              input$selectNVCtypes,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  

# Show/Hide Floristic Table Options ---------------------------------------
  observe({
    
    if(input$floristicTablesView == "singleComposedVsNVC") {
      
      shinyjs::hide(id = "floristicTablesSetViewOpts_div")
      shinyjs::show(id = "restrictNVCFlorTablesOpts_div")
      shinyjs::show(id = "nvcFloristicTable_div")
      shinyjs::show(id = "composedFloristicTable_div")
      shinyjs::show(id = "matchSpecies_div")
      
    } else if(input$floristicTablesView == "multipleComposed") {
      
      shinyjs::show(id = "floristicTablesSetViewOpts_div")
      shinyjs::hide(id = "restrictNVCFlorTablesOpts_div")
      shinyjs::hide(id = "nvcFloristicTable_div")
      shinyjs::hide(id = "composedFloristicTable_div")
      shinyjs::hide(id = "matchSpecies_div")
      
    }
    
  }) |>
    bindEvent(input$floristicTablesView,
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
      
      # shinyjs::disable(id = "resultsViewNVCAssign")
      
    } else if(fix_assignQuadrats_enabled == FALSE){
      
      shinyWidgets::updateSwitchInput(
        session = session,
        inputId = "assignQuadrats",
        value = NULL,
        disabled = FALSE
      )
      
      # shinyjs::enable(id = "resultsViewNVCAssign")
      
    }
    
  }) |>
    bindEvent(surveyDataSummary(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

# Reactively update nvcFloristicTable options -----------------------------
  observe({
    
    shiny::req(nvcAssignment())
    shiny::req(setupData())
    
    nvcAssignment <- nvcAssignment()
    setupData <- setupData()
    
    topNVCCommunities <- nvcAssignment$topNVCCommunities
    nvc_code_values <- unique(setupData$floristic_tables[["nvc_code"]])
    
    if(input$restrictNVCFlorTablesOpts == TRUE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "nvcFloristicTable",
        choices = topNVCCommunities,
        selected = topNVCCommunities[1]
      )
      
    } else if(input$restrictNVCFlorTablesOpts == FALSE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "nvcFloristicTable",
        choices = nvc_code_values,
        selected = topNVCCommunities[1]
      )
      
    }
    
  }) |>
    bindEvent(input$restrictNVCFlorTablesOpts,
              nvcAssignment(),
              setupData(),
              ignoreInit = TRUE)
  

  # Reactively update floristicTablesSetView options ------------------------
  observe({
    
    if(nrow(floristicTables()$floristicTables_composed_all_wide) != 0){
      
      uniq_IDs <- floristicTables()$floristicTables_composed_all_wide |>
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
    bindEvent(floristicTables(),
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
              ignoreInit = FALSE)
  
  # Reactively update global reference DCA space selection ------------------
  observe({
    
    shiny::isolate({
      nvcAssignment <- nvcAssignment()
      setupData <- setupData()
    })
    
    topNVCCommunities <- nvcAssignment$topNVCCommunities
    selectedReferenceSpaces_options <- unique(setupData$floristic_tables[["nvc_code"]])
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selectedReferenceSpaces",
      choices = selectedReferenceSpaces_options,
      selected = topNVCCommunities
    )
    
  }) |>
    bindEvent(nvcAssignment(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  # Reactively update DCA survey quadrat selection options ------------------
  observe({
    
    surveyData <- surveyData()
    surveyData_long <- surveyData$surveyData_long
    
    if(is.null(mvaLocalRefRestrictedResults()) == FALSE){
      
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

# Download RMAVIS Results -----------------------------------------------
  output$downloadRMAVISResults <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.Results.",
             "v1-1-3.",
             format(Sys.time(), "%y-%m-%d.%H-%M-%S"),
             ".xlsx",
             sep="")
      
    },
    
    content = function(file) {
      
      floristicTables <- floristicTables()
      nvcAssignment <- nvcAssignment()
      habCor <- habCor()
      speciesFreq <- speciesFreq()
      avgEIVs <- avgEIVs()
      diversityAnalysis <- diversityAnalysis()
      
      sheets <- list(
        "Floristic Tables - Long" = floristicTables$floristicTables_composed_all,
        "Floristic Tables - Wide" = floristicTables$floristicTables_composed_all_wide,
        "NVC Assignment - Quadrat" = nvcAssignment$nvcAssignmentPlot_Jaccard,
        "Habitat Correspondences" = habCor,
        "Frequency Table" = speciesFreq,
        "EIVs, Weighted, Site" = avgEIVs$weightedMeanHEValuesSite,
        "EIVs, Unweighted, Site" = avgEIVs$unweightedMeanHEValuesSite,
        "EIVs, Weighted, Group" = avgEIVs$weightedMeanHEValuesGroup,
        "EIVs, Unweighted, Group" = avgEIVs$unweightedMeanHEValuesGroup,
        "EIVs, Weighted, Quadrat" = avgEIVs$weightedMeanHEValuesQuadrat,
        "EIVs, Unweighted, Quadrat" = avgEIVs$unweightedMeanHEValuesQuadrat,
        "Diversity, Summary" = diversityAnalysis$diversitySummary,
        "Diversity, Quadrat Indices" = diversityAnalysis$diversityIndices,
        "Diversity, Richness, Site" = diversityAnalysis$speciesRichnessSite,
        "Diversity, Richness, Group" = diversityAnalysis$speciesRichnessGroup,
        "Diversity, Richness, Quadrat" = diversityAnalysis$speciesRichnessQuadrat
      )
      
      if(!is.null(nvcAssignment$nvcAssignmentGroup_Czekanowski)){
        sheets[["NVC Assignment - Group"]] <- nvcAssignment$nvcAssignmentGroup_Czekanowski
      }
      
      if(!is.null(nvcAssignment$nvcAssignmentSite_Czekanowski)){
        sheets[["NVC Assignment - Site"]] <- nvcAssignment$nvcAssignmentSite_Czekanowski
      }
      
      writexl::write_xlsx(x = sheets, path = file)
      
    }
  )
  
# Return sidebar options --------------------------------------------------
  return(sidebar_options)
  
}



