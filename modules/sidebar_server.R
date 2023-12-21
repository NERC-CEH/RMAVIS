sidebar <- function(input, output, session, surveyTable, surveyTableValidator, nvcAssignment, floristicTables, mvaLocalRefRestrictedResults) {
  
  ns <- session$ns
  

# Compose list of inputs to return from module ----------------------------
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "inputMethod" = input$inputMethod,
      # "resetTable" = input$resetTable,
      "exampleData" = input$exampleData,
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
    
    # print(input$reportOptions)
    
  }) |>
    bindEvent(input$inputMethod, 
              # input$resetTable, 
              input$exampleData, 
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
      
      if(input$exampleData == "Parsonage Down"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "CG"
        )
        
      } else if(input$exampleData == "Whitwell Common"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "M"
        )
        
      } else if(input$exampleData == "Leith Hill Place Wood"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = "W"
        )
        
      } else if(input$exampleData == "Sand Dune"){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "habitatRestriction",
          selected = NULL
        )
        
      }
      
    }
    
  }) |>
    bindEvent(input$inputMethod,
              input$exampleData,
              ignoreInit = TRUE)


# Validate Survey Table Data Modal Popup ----------------------------------
  
  observe({
    
    shiny::showModal(
      
      session = session,
      
      shiny::modalDialog(
        
        title = "Validate Survey Table Data",
        id = "validateSurveyTableDataModal",
        footer = shiny::tagList(
          shiny::modalButton("Close")
        ),
        size = "xl",
        easyClose = FALSE,
        fade = TRUE,
        
        surveyTableValidatorUI(id = "surveyTableValidator_id_1")
        
      )
    )
    
  }) |>
    bindEvent(input$validateSurveyTable,
              ignoreInit = TRUE)


# Disable Run Analysis ActionButton if okToProceed == FALSE ---------------
  # observe({
  # 
  #   surveyTableValidator <- surveyTableValidator()
  #   
  #   # print(surveyTableValidator)
  # 
  #   okToProceed <- surveyTableValidator$surveyTableValidation$okToProceed
  # 
  #   if(okToProceed == TRUE){
  # 
  #     shinyjs::enable(id = "runAnalysis")
  # 
  #   } else if(okToProceed == FALSE){
  # 
  #     shinyjs::disable(id = "runAnalysis")
  # 
  #   }
  # 
  # }) |>
  #   bindEvent(surveyTableValidator(),
  #             ignoreInit = TRUE,
  #             ignoreNULL = TRUE)
  
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
    
    # Get all NVC communities and sub-communities from nvc assignment results
    NVC_communities_all <- nvcAssignment()$nvcAssignmentSite |>
      dplyr::pull(NVC.Code)
    
    # Get all NVC communities from community and sub-community codes
    NVC_communities_fromSubCom <- stringr::str_replace(string = NVC_communities_all, 
                                                       pattern = "(\\d)[^0-9]+$", 
                                                       replace = "\\1") |>
      unique()
    
    NVC_communities_final <- unique(c(NVC_communities_all, NVC_communities_fromSubCom))
    
    if(input$restrictNVCFlorTablesOpts == TRUE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "nvcFloristicTable",
        choices = NVC_communities_final,
        selected = NVC_communities_final[1],
        server = TRUE
      )
      
    } else if(input$restrictNVCFlorTablesOpts == FALSE){
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "nvcFloristicTable",
        choices = nvc_community_codes,
        selected = NVC_communities_final[1],
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
    
    if (input$selectSurveyMethod == "all") {
      
      shinyjs::hide(id = "selectSurveyYears_div")
      shinyjs::hide(id = "selectSurveyQuadrats_div")
      shinyjs::hide(id = "selectSurveyGroups_div")
      
    } else if (input$selectSurveyMethod == "selectYears") {
      
      shinyjs::show(id = "selectSurveyYears_div")
      shinyjs::hide(id = "selectSurveyGroups_div")
      shinyjs::hide(id = "selectSurveyQuadrats_div")
      
    } else if (input$selectSurveyMethod == "selectGroups") {
      
      shinyjs::hide(id = "selectSurveyYears_div")
      shinyjs::show(id = "selectSurveyGroups_div")
      shinyjs::hide(id = "selectSurveyQuadrats_div")
      
    } else if (input$selectSurveyMethod == "selectQuadrats") {
      
      shinyjs::hide(id = "selectSurveyYears_div")
      shinyjs::hide(id = "selectSurveyGroups_div")
      shinyjs::show(id = "selectSurveyQuadrats_div")
      
    }
    
  }) |>
    bindEvent(input$selectSurveyMethod, ignoreInit = FALSE)
  
  
  # Reactively update global reference DCA space selection ------------------
  
  observe({
    
    # Get all NVC communities and sub-communities from nvc assignment results
    NVC_communities_all <- nvcAssignment()$nvcAssignmentSite |> # nvcAssignment()
      dplyr::pull(NVC.Code)
    
    # Get all NVC communities from community and sub-community codes
    NVC_communities_fromSubCom <- stringr::str_replace(string = NVC_communities_all, 
                                                       pattern = "(\\d)[^0-9]+$", 
                                                       replace = "\\1") |>
      unique()
    
    NVC_communities_final <- unique(c(NVC_communities_all, NVC_communities_fromSubCom))
    
    # shinyWidgets::updatePickerInput(
    shiny::updateSelectizeInput(
      session = session,
      inputId = "nationalReferenceSpaces",
      selected = NVC_communities_final
    )
    
  }) |>
    bindEvent(nvcAssignment(),
              ignoreInit = TRUE)
  
  # Reactively update DCA survey quadrat selection options ------------------
  observe({
    
    # print(mvaLocalRefRestrictedResults())
    
    if(is.null(mvaLocalRefRestrictedResults()) == FALSE){
      
      uniq_years <- surveyTable() |>
        dplyr::pull(Year) |>
        unique()
      
      uniq_quadrats <- surveyTable() |>
        dplyr::pull(Quadrat) |>
        unique()
      
      uniq_groups <- surveyTable() |>
        dplyr::pull(Group) |>
        unique()
      
      if(input$selectSurveyMethod == "all"){
        
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
              ignoreInit = TRUE)
  
  
  # Return sidebar options --------------------------------------------------
  return(sidebar_options)
  
}



