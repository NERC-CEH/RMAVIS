deSidebar <- function(input, output, session, setupData,
                      surveyData, surveyDataValidator, surveyDataSummary) {
  
  ns <- session$ns

# Compose list of inputs to return from module ----------------------------
  sidebar_options <- reactiveVal()
  
  observe({
    
    sidebar_options_list <- list(
      "inputMethod" = input$inputMethod,
      "clearTable" = input$clearTable,
      "selectedExampleData" = input$selectedExampleData,
      "coverScale" = input$coverScale,
      "uploadData" = input$uploadData
    )
    
    sidebar_options(sidebar_options_list)
    
  }) |>
    bindEvent(input$inputMethod,
              input$clearTable,
              input$selectedExampleData, 
              input$coverScale,
              input$uploadData,
              ignoreInit = FALSE)
  

# Retrieve setup data -----------------------------------------------------
  example_data_options <- reactiveVal()
  accepted_taxa <- reactiveVal()
  taxa_lookup <- reactiveVal()
  taxonomic_backbone <- reactiveVal()
  region <- reactiveVal()
  
  observe({
    
    region(setupData()$region)
    
    if(region() == "gbnvc"){
      
      taxa_lookup(UKVegTB::taxa_lookup)
      taxonomic_backbone(UKVegTB::taxonomic_backbone)
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "coverScale",
        selected = "percentage"
      )
      
    } else if(region() == "mnnpc"){
      
      taxa_lookup(MNNPC::mnnpc_taxa_lookup)
      taxonomic_backbone(MNNPC::mnnpc_taxonomic_backbone)
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "coverScale",
        selected = "braunBlanquet"
      )
      
    }
    
    example_data_options(setupData()$example_data_options)
    
    accepted_taxa(setupData()$accepted_species)
    
  }) |>
    bindEvent(setupData(),
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
  
# Update Example Data Options --------------------------------------------
  observe({
        
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selectedExampleData",
      choices = example_data_options(),
      selected = "none"
    )
    
  }) |>
    bindEvent(example_data_options(),
              ignoreInit = FALSE)


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
        
      } else if(input$selectedExampleData %in% c("St. Croix State Forest", "Earthworm-Invaded Forests")){
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "coverScale",
          selected = "braunBlanquet"
        )
        
      }

    }

  }) |>
    bindEvent(input$inputMethod,
              input$selectedExampleData,
              ignoreInit = FALSE)


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
  
# Download Survey Data ----------------------------------------------------
  output$downloadSurveyData <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.SurveyData.",
             "v1-2-0.",
             format(Sys.time(), "%y-%m-%d.%H-%M-%S"),
             ".xlsx",
             sep="")
      
    },
    
    content = function(file) {
      
      surveyData <- surveyData()
      
      sheets <- list(
        "original" = surveyData$surveyData_original,
        "formatted" = surveyData$surveyData_long_prop
      )
      
      if(!is.null(surveyData()$surveyData_long_prop_agg)){
        sheets[["aggregated"]] <- surveyData()$surveyData_long_prop_agg
      }
      
      writexl::write_xlsx(x = sheets, path = file)
      
    }
  )
  
  # Download Taxonomic Backbone ---------------------------------------------
  output$downloadAcceptedTaxa <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.AcceptedTaxa.",
             stringr::str_to_upper(region()),
             ".v1-2-0",
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      write.csv(x = accepted_taxa(), file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      
    }
  )

  # Download Taxonomic Backbone ---------------------------------------------
  output$downloadTaxonomicBackbone <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.TaxonomicBackbone.",
             stringr::str_to_upper(region()),
             ".v1-2-0",
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      write.csv(x = taxonomic_backbone(), file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      
    }
  )

  # Download Taxon Lookup ---------------------------------------------------
  output$downloadTaxonLookup <- downloadHandler(
    
    filename = function() {
      
      paste0("RMAVIS.TaxonLookup.",
             stringr::str_to_upper(region()),
             ".v1-2-0",
             ".csv",
             sep="")
      
    },
    
    content = function(file) {
      
      write.csv(x = taxa_lookup(), file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      
    }
  )

  
# Return sidebar options --------------------------------------------------
  return(sidebar_options)
  
}



