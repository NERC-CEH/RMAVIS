# Server
server <- function(input, output, session) {
  

  # Region ------------------------------------------------------------------
  region <- shiny::callModule(module = regionSelect,
                              id = "regionSelect_id_1")
  
  # Setup Data --------------------------------------------------------------
  setupData <- shiny::callModule(module = setupData,
                                 id = "setupData_id_1",
                                 region = region,
                                 deSidebar_options = deSidebar_options,
                                 sidebar_options = sidebar_options)
  
  # VC Information ----------------------------------------------------------
  vcCommNamesLookup <- shiny::callModule(module = vcCommNamesLookup,
                                         region = region,
                                         id = "vcCommNamesLookup_id_1")
  
  shiny::callModule(module = vcTaxonNamesLookup,
                    region = region,
                    id = "vcTaxonNamesLookup_id_1")
  
  vcFlorTabs <- shiny::callModule(module = vcFlorTabs,
                                  region = region,
                                  id = "vcFlorTabs_id_1")
  
  vcCommAttr <- shiny::callModule(module = vcCommAttr,
                                  region = region,
                                  id = "vcCommAttr_id_1")
  
  shiny::callModule(module = vcInfoSidebar,
                    id = "vcInfoSidebar_id_1",
                    region = region,
                    vcCommNamesLookup = vcCommNamesLookup, 
                    vcFlorTabs = vcFlorTabs, 
                    vcCommAttr = vcCommAttr
                    )
  
  # Data Entry --------------------------------------------------------------
  deSidebar_options <- shiny::callModule(module = deSidebar,
                                         id = "deSidebar_id_1",
                                         setupData = setupData,
                                         surveyData = surveyData,
                                         surveyDataValidator = surveyDataValidator,
                                         surveyDataSummary = surveyDataSummary)

  uploadDataTable <- shiny::callModule(module = uploadData,
                                       id = "uploadData_id_1")

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
  
  shiny::callModule(module = rmavisTaxonNamesLookup,
                    id = "rmavisTaxonNamesLookup_id_1",
                    region = region)


  # Core --------------------------------------------------------------------
  sidebar_options <- shiny::callModule(module = sidebar,
                                       id = "sidebar_id_1",
                                       region = region,
                                       setupData = setupData,
                                       deSidebar_options = deSidebar_options,
                                       surveyData = surveyData,
                                       surveyDataValidator = surveyDataValidator,
                                       surveyDataSummary = surveyDataSummary,
                                       floristicTables = floristicTables,
                                       vcAssignment = vcAssignment,
                                       habCor = habCor,
                                       speciesFreq = speciesFreq,
                                       avgEIVs = avgEIVs,
                                       diversityAnalysis = diversityAnalysis,
                                       mvaLocalRefRestrictedResults = mvaLocalRefRestrictedResults)
    
  floristicTables <- shiny::callModule(module = floristicTables,
                                       id = "floristicTables_id_1",
                                       region = region,
                                       setupData = setupData,
                                       surveyData = surveyData,
                                       avgEIVs = avgEIVs,
                                       deSidebar_options = deSidebar_options,
                                       sidebar_options = sidebar_options)
  
  vcAssignment <- shiny::callModule(module = vcAssignment,
                                    id = "vcAssignment_id_1",
                                    setupData = setupData,
                                    surveyData = surveyData,
                                    surveyDataSummary = surveyDataSummary,
                                    floristicTables = floristicTables,
                                    sidebar_options = sidebar_options)
  
  # habCor <- shiny::callModule(module = habCor,
  #                             id = "habCor_id_1",
  #                             vcAssignment = vcAssignment,
  #                             sidebar_options = sidebar_options)
  
  speciesFreq <- shiny::callModule(module = speciesFreq,
                                   id = "speciesFreq_id_1",
                                   surveyData = surveyData,
                                   sidebar_options = sidebar_options)
  
  # if(isTRUE(TRUE)){
  #   
  #   avgEIVs <- shiny::callModule(module = calcAvgEIVs,
  #                                id = "calcAvgEIVs_id_1",
  #                                surveyData = surveyData,
  #                                sidebar_options = sidebar_options)
  #   
  # }
  
  # avgEIVs <- shiny::callModule(module = calcAvgEIVs,
  #                              id = "calcAvgEIVs_id_1",
  #                              surveyData = surveyData,
  #                              sidebar_options = sidebar_options)
  
  diversityAnalysis <- shiny::callModule(module = diversityAnalysis,
                                         id = "diversityAnalysis_id_1",
                                         surveyData = surveyData,
                                         sidebar_options = sidebar_options)
  
  # mvaNationalRefResults <- shiny::callModule(module = mvaNationalRef,
  #                                            id = "mvaNationalRef_id_1",
  #                                            setupData = setupData,
  #                                            surveyData = surveyData,
  #                                            vcAssignment = vcAssignment,
  #                                            avgEIVs = avgEIVs,
  #                                            sidebar_options = sidebar_options)
  
  # mvaLocalRefRestrictedResults <- shiny::callModule(module = mvaLocalRefRestricted,
  #                                                   id = "mvaLocalRefRestricted_id_1",
  #                                                   setupData = setupData,
  #                                                   surveyData = surveyData,
  #                                                   vcAssignment = vcAssignment,
  #                                                   sidebar_options = sidebar_options)
  
  # mvaLocalRefUnrestrictedResults <- shiny::callModule(module = mvaLocalRefUnrestricted,
  #                                                     id = "mvaLocalRefUnrestricted_id_1",
  #                                                     setupData = setupData,
  #                                                     surveyData = surveyData,
  #                                                     vcAssignment = vcAssignment,
  #                                                     avgEIVs = avgEIVs,
  #                                                     sidebar_options = sidebar_options)
  
  
  # shiny::callModule(module = report,
  #                   id = "sidebar_id_1",
  #                   sidebar_options = sidebar_options,
  #                   surveyData = surveyData,
  #                   surveyDataValidator = surveyDataValidator,
  #                   surveyDataSummary = surveyDataSummary,
  #                   vcAssignment = vcAssignment,
  #                   habCor = habCor,
  #                   floristicTables = floristicTables,
  #                   speciesFreq = speciesFreq,
  #                   avgEIVs = avgEIVs,
  #                   diversityAnalysis = diversityAnalysis,
  #                   mvaNationalRefResults = mvaNationalRefResults,
  #                   mvaLocalRefRestrictedResults = mvaLocalRefRestrictedResults,
  #                   mvaLocalRefUnrestrictedResults = mvaLocalRefUnrestrictedResults
  #                   )

  # Save Module Outputs -----------------------------------------------------
  # Save module outputs to global environment, uncomment for development only!
  # observe({
  # 
  #   # assign(x = "sidebar_options", value = sidebar_options(), envir = .GlobalEnv)
  #   # assign(x = "surveyData", value = surveyData(), envir = .GlobalEnv)
  #   # assign(x = "surveyDataValidator", value = surveyDataValidator(), envir = .GlobalEnv)
  #   # assign(x = "surveyDataSummary", surveyDataSummary(), envir = .GlobalEnv)
  #   # assign(x = "floristicTables", value = floristicTables(), envir = .GlobalEnv)
  #   # assign(x = "vcAssignment", value = vcAssignment(), envir = .GlobalEnv)
  #   # assign(x = "habCor", value = habCor(), envir = .GlobalEnv)
  #   # assign(x = "speciesFreq", value = speciesFreq(), envir = .GlobalEnv)
  #   # assign(x = "avgEIVs", value = avgEIVs(), envir = .GlobalEnv)
  #   # assign(x = "diversityAnalysis", value = diversityAnalysis(), envir = .GlobalEnv)
  #   # assign(x = "mvaNationalRefResults", value = mvaNationalRefResults(), envir = .GlobalEnv)
  #   # assign(x = "mvaLocalRefRestrictedResults", value = mvaLocalRefRestrictedResults(), envir = .GlobalEnv)
  #   # assign(x = "mvaLocalRefUnrestrictedResults", value = mvaLocalRefUnrestrictedResults(), envir = .GlobalEnv)
  # 
  # })

}
