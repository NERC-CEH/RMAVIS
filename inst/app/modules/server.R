server <- function(input, output, session) {

  # Region ------------------------------------------------------------------
  region <- shiny::callModule(module = regionSelect,
                              id = "regionSelect_id_1")
  
  # Update page navbar ------------------------------------------------------
  observe({

    if(region() == "mnnpc"){

      shinyjs::show(id = "mnnpc_logo_div")

    } else {

      shinyjs::hide(id = "mnnpc_logo_div")
    }

  }) |>
    shiny::bindEvent(region(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)
  

  # Additional Info ---------------------------------------------------------
  shiny::callModule(module = additionalInfo,
                    id = "additional_info_id_1",
                    region = region)
  
  # Setup Data --------------------------------------------------------------
  setupData <- shiny::callModule(module = setupData,
                                 id = "setupData_id_1",
                                 region = region,
                                 deSidebar_options = deSidebar_options,
                                 sidebar_options = analysis$sidebar_options)
  
  # VC Information ----------------------------------------------------------
  shiny::callModule(module = vcInformation,
                    id = "vcInfo_id_1",
                    region = region,
                    setupData = setupData)
  
  # Data Entry --------------------------------------------------------------
  deSidebar_options <- shiny::callModule(module = deSidebar,
                                         id = "deSidebar_id_1",
                                         setupData = setupData,
                                         surveyData = surveyData,
                                         surveyDataValidator = surveyDataValidator,
                                         surveyDataSummary = surveyDataSummary)

  uploadDataTable <- shiny::callModule(module = uploadData,
                                       id = "uploadData_id_1",
                                       des_opts = deSidebar_options,
                                       setupData = setupData)
  
  surveyData <- shiny::callModule(module = surveyData,
                                  id = "surveyData_id_1",
                                  setupData = setupData,
                                  uploadDataTable = uploadDataTable,
                                  surveyDataValidator = surveyDataValidator,
                                  deSidebar_options = deSidebar_options)

  surveyDataValidator <- shiny::callModule(module = surveyDataValidator,
                                           id = "surveyDataValidator_id_1",
                                           setupData = setupData,
                                           surveyData = surveyData,
                                           deSidebar_options = deSidebar_options)

  surveyDataSummary <- shiny::callModule(module = surveyDataSummary,
                                         id = "surveyDataSummary_id_1",
                                         setupData = setupData,
                                         surveyData = surveyData)
  
  taxonomicBackbone <- shiny::callModule(module = taxonomicBackbone,
                                         id = "taxonomicBackbone_id_1",
                                         region = region)
  
  taxaLookup <- shiny::callModule(module = taxaLookup,
                                  id = "taxaLookup_id_1",
                                  region = region)

  # Analysis ----------------------------------------------------------------
  analysis <- shiny::callModule(module = analysis,
                                id = "analysis_id_1",
                                region = region,
                                setupData = setupData,
                                deSidebar_options = deSidebar_options,
                                surveyData = surveyData,
                                surveyDataValidator = surveyDataValidator,
                                surveyDataSummary = surveyDataSummary)

  # Save Module Outputs -----------------------------------------------------
  # Save module outputs to global environment, uncomment for development only!
  # observe({
  # 
  #   assign(x = "surveyData", value = surveyData(), envir = .GlobalEnv)
  #   assign(x = "surveyDataValidator", value = surveyDataValidator(), envir = .GlobalEnv)
  #   assign(x = "surveyDataSummary", surveyDataSummary(), envir = .GlobalEnv)
  #   assign(x = "analysis", value = analysis(), envir = .GlobalEnv)
  # 
  # })

}
