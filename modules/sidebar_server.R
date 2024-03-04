sidebar <- function(input, output, session, surveyData, surveyDataValidator, nvcAssignment, floristicTables, mvaLocalRefRestrictedResults) {
  
  ns <- session$ns
  

# Compose list of inputs to return from module ----------------------------
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "inputMethod" = input$inputMethod,
      "includeBryophytes" = input$includeBryophytes,
      # "resetTable" = input$resetTable,
      "selectedExampleData" = input$selectedExampleData,
      "runAnalysis" = input$runAnalysis,
      # "coverMethod" = input$coverMethod,
      "habitatRestriction" = input$habitatRestriction,
      "nTopResults" = input$nTopResults,
      "resultsViewNVCAssign" = input$resultsViewNVCAssign,
      "habCorClass" = input$habCorClass,
      "composedFloristicTable" = input$composedFloristicTable,
      "nvcFloristicTable" = input$nvcFloristicTable,
      "matchSpecies" = input$matchSpecies,
      "restrictNVCFlorTablesOpts" = input$restrictNVCFlorTablesOpts,
      "resultsViewEIVs"  = input$resultsViewEIVs,
      "resultsViewDiversity"  = input$resultsViewDiversity,
      "nationalReferenceSpaces" = input$nationalReferenceSpaces,
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
    bindEvent(input$inputMethod,
              input$includeBryophytes,
              # input$resetTable, 
              input$selectedExampleData, 
              input$runAnalysis, 
              # input$coverMethod, 
              input$habitatRestriction, 
              input$nTopResults,
              input$resultsViewNVCAssign,
              input$habCorClass, 
              input$composedFloristicTable,
              input$nvcFloristicTable, 
              input$matchSpecies,
              input$restrictNVCFlorTablesOpts,
              input$resultsViewEIVs,
              input$resultsViewDiversity,
              input$nationalReferenceSpaces,
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
              ignoreInit = TRUE)

# Show/Hide inputMethod-related inputs ------------------------------------
  observe({
    
    if (input$inputMethod == "manual") {
      
      shinyjs::hide(id = "exampleData_div")
      shinyjs::hide(id = "uploadData_div")
      
    } else if (input$inputMethod == "example") {
      
      shinyjs::show(id = "exampleData_div")
      shinyjs::hide(id = "uploadData_div")
      
    } else if (input$inputMethod == "upload") {
      
      shinyjs::hide(id = "exampleData_div")
      shinyjs::show(id = "uploadData_div")
      
    }
    
  }) |>
    bindEvent(input$inputMethod, ignoreInit = FALSE)


# Update habitat restriction if example is used ---------------------------
  
  observe({
    
    if(input$inputMethod == "example"){
      
      if(input$selectedExampleData == "Parsonage Down"){
        
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
        
      } else if(input$selectedExampleData == "Whitwell Common"){
        
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
        
      } else if(input$selectedExampleData == "Leith Hill Place Wood"){
        
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
        
      } else if(input$selectedExampleData == "Newborough Warren"){
        
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
      
    }
    
  }) |>
    bindEvent(input$inputMethod,
              input$selectedExampleData,
              ignoreInit = TRUE)


# Validate Survey Table Data Modal Popup ----------------------------------
  
  observe({
    
    shiny::showModal(
      
      session = session,
      
      shiny::modalDialog(
        
        title = "Validate Survey Table Data",
        id = "validatesurveyDataDataModal",
        footer = shiny::tagList(
          shiny::modalButton("Close")
        ),
        size = "xl",
        easyClose = FALSE,
        fade = TRUE,
        
        surveyDataValidatorUI(id = "surveyDataValidator_id_1")
        
      )
    )
    
  }) |>
    bindEvent(input$validatesurveyData,
              ignoreInit = TRUE)


# Disable selected action buttons if okToProceed == FALSE ---------------
  observe({

    surveyDataValidator <- surveyDataValidator()

    okToProceed <- surveyDataValidator$surveyDataValidation$okToProceed

    if(okToProceed == TRUE & nrow(surveyData()$surveyData_long) > 0){

      shinyjs::enable(id = "runAnalysis")
      shinyjs::enable(id = "generateReport")

    } else if(okToProceed == FALSE){

      shinyjs::disable(id = "runAnalysis")
      shinyjs::disable(id = "generateReport")

    }

  }) |>
    bindEvent(surveyDataValidator(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
# Upload Data Modal Popup -------------------------------------------------
  
  observe({
    
    shiny::showModal(
      
      session = session,
      
      shiny::modalDialog(
        
        title = "Upload Data",
        id = "uploadDataModal",
        footer = shiny::tagList(
          shiny::modalButton("Close")
        ),
        size = "xl",
        easyClose = FALSE,
        fade = TRUE,
        
        uploadDataUI(id = "uploadData_id_1"),
        
      )
    )
    
  }) |>
    bindEvent(input$uploadData,
              ignoreInit = TRUE)
  
  
  # Reactively update nvcFloristicTable options -----------------------------
  observe({
    
    shiny::req(nvcAssignment())
    
    nvcAssignment <- nvcAssignment()
    
    topNVCCommunities <- nvcAssignment$topNVCCommunities
    
    if(input$restrictNVCFlorTablesOpts == TRUE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "nvcFloristicTable",
        choices = topNVCCommunities,
        selected = topNVCCommunities[1],
        server = TRUE
      )
      
    } else if(input$restrictNVCFlorTablesOpts == FALSE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "nvcFloristicTable",
        choices = nvc_community_codes,
        selected = topNVCCommunities[1],
        server = TRUE
      )
      
    }
    
  }) |>
    bindEvent(input$restrictNVCFlorTablesOpts,
              nvcAssignment(),
              ignoreInit = TRUE)
  
  
  # Reactively update composedFloristicTable options ------------------------
  observe({
    
    if(nrow(floristicTables()) != 0){
      
      uniq_IDs <- floristicTables() |>
        dplyr::pull(ID) |>
        unique()
      
      names(uniq_IDs) <- uniq_IDs
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "composedFloristicTable",
        choices = uniq_IDs,
        selected = NULL,
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
    
    nvcAssignment <- nvcAssignment()
    
    topNVCCommunities <- nvcAssignment$topNVCCommunities
    
    # shinyWidgets::updatePickerInput(
    shiny::updateSelectizeInput(
      session = session,
      inputId = "nationalReferenceSpaces",
      selected = topNVCCommunities
    )
    
  }) |>
    bindEvent(nvcAssignment(),
              ignoreInit = TRUE)
  
  # Reactively update DCA survey quadrat selection options ------------------
  observe({
    
    if(is.null(mvaLocalRefRestrictedResults()) == FALSE){
      
      uniq_years <- surveyData() |>
        dplyr::pull(Year) |>
        unique()
      
      uniq_quadrats <- surveyData() |>
        dplyr::pull(Quadrat) |>
        unique()
      
      uniq_groups <- surveyData() |>
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
  

# Download Survey Data ----------------------------------------------------
  output$downloadSurveyData <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.SurveyData.",
             gsub(x = gsub(x = Sys.time(),
                           pattern = "\\s",
                           replacement = "."),
                  pattern = ":",
                  replacement = "-"),
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      surveyData <- surveyData()
      
      write.csv(x = surveyData, file, row.names = FALSE, fileEncoding = "UTF-8")
      
    }
  )

# Download Accepted Species -----------------------------------------------
  output$downloadSpeciesData <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.AcceptedSpecies.",
             gsub(x = gsub(x = Sys.time(),
                           pattern = "\\s",
                           replacement = "."),
                  pattern = ":",
                  replacement = "-"),
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      write.csv(x = acceptedSpecies, file, row.names = FALSE, fileEncoding = "UTF-8")
      
    }
  )


# Return sidebar options --------------------------------------------------
  return(sidebar_options)
  
}



