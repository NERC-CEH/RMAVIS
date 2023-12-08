sidebar <- function(input, output, session, surveyTable, nvcAverageSim, floristicTables, dcaSubsetNVCResults) {
  
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
      "habCorClass" = input$habCorClass,
      "composedFloristicTable" = input$composedFloristicTable,
      "nvcFloristicTable" = input$nvcFloristicTable,
      "crossTabulate" = input$crossTabulate,
      "restrictNVCFlorTablesOpts" = input$restrictNVCFlorTablesOpts,
      # "generateReport" = input$generateReport
      "selectSurveyMethod" = input$selectSurveyMethod,
      "selectSurveyYears" = input$selectSurveyYears,
      "selectSurveyQuadrats" = input$selectSurveyQuadrats,
      "selectSurveyGroups" = input$selectSurveyGroups,
      "dcaVars" = input$dcaVars,
      "ccaVars" = input$ccaVars
    )
    
    sidebar_options(sidebar_options_list)
    
  }) |>
    bindEvent(input$inputMethod, 
              # input$resetTable, 
              input$exampleData, 
              input$runAnalysis, 
              # input$coverMethod, 
              input$habitatRestriction, input$nTopResults,
              input$habCorClass, input$composedFloristicTable,
              input$nvcFloristicTable, input$crossTabulate,
              input$restrictNVCFlorTablesOpts,
              # input$generateReport,
              input$selectSurveyMethod,
              input$selectSurveyYears,
              input$selectSurveyQuadrats,
              input$selectSurveyGroups,
              input$dcaVars,
              input$ccaVars,
              ignoreInit = TRUE)

# Show/Hide inputMethod-related inputs ------------------------------------
  observe({
    
    if (input$inputMethod == "manual") {
      
      shinyjs::hide(id = "exampleData")
      shinyjs::hide(id = "uploadData")
      
      shinyjs::hideElement(id = "exampleDataInfo")
      shinyjs::hideElement(id = "uploadDataInfo")
      
    } else if (input$inputMethod == "example") {
      
      shinyjs::show(id = "exampleData")
      shinyjs::hide(id = "uploadData")
      
      shinyjs::showElement(id = "exampleDataInfo")
      shinyjs::hideElement(id = "uploadDataInfo")
      
    } else if (input$inputMethod == "upload") {
      
      shinyjs::hide(id = "exampleData")
      shinyjs::show(id = "uploadData")
      
      shinyjs::hideElement(id = "exampleDataInfo")
      shinyjs::showElement(id = "uploadDataInfo")
      
    }
    
  }) |>
    bindEvent(input$inputMethod, ignoreInit = FALSE)


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
    
    # Get all NVC communities and sub-communities from nvc assignment results
    NVC_communities_all <- nvcAverageSim() |> # nvcAverageSim()
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
              nvcAverageSim(),
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
      
      shinyjs::hideElement(id = "selectSurveyYears")
      shinyjs::hideElement(id = "selectSurveyQuadrats")
      shinyjs::hideElement(id = "selectSurveyGroups")
      
    } else if (input$selectSurveyMethod == "selectYears") {
      
      shinyjs::showElement(id = "selectSurveyYears")
      shinyjs::hideElement(id = "selectSurveyGroups")
      shinyjs::hideElement(id = "selectSurveyQuadrats")
      
    } else if (input$selectSurveyMethod == "selectGroups") {
      
      shinyjs::hideElement(id = "selectSurveyYears")
      shinyjs::showElement(id = "selectSurveyGroups")
      shinyjs::hideElement(id = "selectSurveyQuadrats")
      
    } else if (input$selectSurveyMethod == "selectQuadrats") {
      
      shinyjs::hideElement(id = "selectSurveyYears")
      shinyjs::hideElement(id = "selectSurveyGroups")
      shinyjs::showElement(id = "selectSurveyQuadrats")
      
    }
    
  }) |>
    bindEvent(input$selectSurveyMethod, ignoreInit = FALSE)
  
  
# Reactively update DCA survey quadrat selection options ------------------
  observe({
    
    # print(dcaSubsetNVCResults())

    if(is.null(dcaSubsetNVCResults()) == FALSE){

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



