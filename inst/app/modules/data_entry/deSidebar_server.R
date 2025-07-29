deSidebar <- function(input, output, session, 
                     surveyData, surveyDataValidator, surveyDataSummary) {
  
  ns <- session$ns
  

# Compose list of inputs to return from module ----------------------------
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "inputMethod" = input$inputMethod,
      "clearTable" = input$clearTable,
      "selectedExampleData" = input$selectedExampleData,
      "coverScale" = input$coverScale
    )
    
    sidebar_options(sidebar_options_list)
    
  }) |>
    bindEvent(input$inputMethod,
              input$clearTable,
              input$selectedExampleData, 
              input$coverScale,
              ignoreInit = FALSE)

# Show/Hide inputMethod-related inputs ------------------------------------
  observe({
    
    if(input$inputMethod == "manual") {
      
      shinyjs::hide(id = "exampleData_div")
      shinyjs::hide(id = "uploadData_div")
      shinyjs::enable(id = "coverScale")
      
    } else if(input$inputMethod == "example") {
      
      shinyjs::show(id = "exampleData_div")
      shinyjs::hide(id = "uploadData_div")
      shinyjs::disable(id = "coverScale")
      
    } else if(input$inputMethod == "upload") {
      
      shinyjs::hide(id = "exampleData_div")
      shinyjs::show(id = "uploadData_div")
      shinyjs::enable(id = "coverScale")
      
    }
    
  }) |>
    bindEvent(input$inputMethod, ignoreInit = FALSE)



  # Update Options Based On Example Data ------------------------------------
  observe({

    if(input$inputMethod == "example"){

      if(input$selectedExampleData == "Parsonage Down"){

        shiny::updateSelectizeInput(
          session = session,
          inputId = "coverScale",
          selected = "domin"
        )

      } else if(input$selectedExampleData == "Whitwell Common"){

        shiny::updateSelectizeInput(
          session = session,
          inputId = "coverScale",
          selected = "none"
        )

      } else if(input$selectedExampleData == "Leith Hill Place Wood"){

        shiny::updateSelectizeInput(
          session = session,
          inputId = "coverScale",
          selected = "none"
        )

      } else if(input$selectedExampleData == "Newborough Warren"){

        shiny::updateSelectizeInput(
          session = session,
          inputId = "coverScale",
          selected = "percentage"
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
        footer = shiny::modalButton("Close"),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE,
        
        surveyDataValidatorUI(id = "surveyDataValidator_id_1")
        
      )
    )
    
  }) |>
    bindEvent(input$validatesurveyData,
              ignoreInit = TRUE)
  
# Upload Data Modal Popup -------------------------------------------------
  observe({
    
    shiny::showModal(
      
      session = session,
      
      shiny::modalDialog(
        
        title = "Upload Data",
        id = "uploadDataModal",
        footer = shiny::modalButton("Close"),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE,
        
        uploadDataUI(id = "uploadData_id_1"),
        
      )
    )
    
  }) |>
    bindEvent(input$uploadData,
              ignoreInit = TRUE)
  
  
  # Download Survey Data ----------------------------------------------------
  output$downloadSurveyData <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.SurveyData.",
             "v1-1-3.",
             format(Sys.time(), "%y-%m-%d.%H-%M-%S"),
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      surveyData <- surveyData()
      surveyData_long <- surveyData$surveyData_long
      
      write.csv(x = surveyData_long, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      
    }
  )
  
  # Download Taxonomic Backbone ---------------------------------------------
  output$downloadAcceptedTaxa <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.AcceptedTaxa.",
             "v1-1-3",
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      write.csv(x = RMAVIS::accepted_taxa, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      
    }
  )

  # Download Taxonomic Backbone ---------------------------------------------
  output$downloadTaxonomicBackbone <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.TaxonomicBackbone.",
             "v1-1-3",
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      write.csv(x = UKVegTB::taxonomic_backbone, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      
    }
  )

  # Download Taxon Lookup ---------------------------------------------------
  output$downloadTaxonLookup <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.TaxonLookup.",
             "v1-1-3",
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      write.csv(x = UKVegTB::taxa_lookup, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      
    }
  )

  
# Return sidebar options --------------------------------------------------
  return(sidebar_options)
  
}



