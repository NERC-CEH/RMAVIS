# Server
server <- function(input, output, session) {

  sidebar_options <- shiny::callModule(module = sidebar,
                                       id = "sidebar_id_1",
                                       surveyData = surveyData,
                                       surveyDataValidator = surveyDataValidator,
                                       surveyDataSummary = surveyDataSummary,
                                       floristicTables = floristicTables,
                                       nvcAssignment = nvcAssignment,
                                       habCor = habCor,
                                       speciesFreq = speciesFreq,
                                       avgEIVs = avgEIVs,
                                       diversityAnalysis = diversityAnalysis,
                                       mvaLocalRefRestrictedResults = mvaLocalRefRestrictedResults)
  
  nvcInfo <- shiny::callModule(module = nvcInfo,
                               id = "nvcInfo_id_1")
  
  setupData <- shiny::callModule(module = setupData,
                                 id = "setupData_id_1",
                                 sidebar_options = sidebar_options)
  
  uploadDataTable <- shiny::callModule(module = uploadData,
                                       id = "uploadData_id_1")
  
  surveyData <- shiny::callModule(module = surveyData,
                                  id = "surveyData_id_1",
                                  setupData = setupData,
                                  uploadDataTable = uploadDataTable,
                                  surveyDataValidator = surveyDataValidator,
                                  sidebar_options = sidebar_options)
  
  surveyDataValidator <- shiny::callModule(module = surveyDataValidator,
                                           id = "surveyDataValidator_id_1",
                                           setupData = setupData,
                                           surveyData = surveyData,
                                           sidebar_options = sidebar_options)
  
  surveyDataSummary <- shiny::callModule(module = surveyDataSummary,
                                         id = "surveyDataSummary_id_1",
                                         surveyData = surveyData)
  
  floristicTables <- shiny::callModule(module = floristicTables,
                                       id = "floristicTables_id_1",
                                       surveyData = surveyData,
                                       surveyDataSummary = surveyDataSummary,
                                       sidebar_options = sidebar_options)
  
  nvcAssignment <- shiny::callModule(module = nvcAssignment,
                                     id = "nvcAssignment_id_1",
                                     setupData = setupData,
                                     surveyData = surveyData,
                                     surveyDataSummary = surveyDataSummary,
                                     floristicTables = floristicTables,
                                     sidebar_options = sidebar_options)

  habCor <- shiny::callModule(module = habCor,
                              id = "habCor_id_1",
                              nvcAssignment = nvcAssignment,
                              sidebar_options = sidebar_options)
  
  speciesFreq <- shiny::callModule(module = speciesFreq,
                                   id = "speciesFreq_id_1",
                                   surveyData = surveyData,
                                   sidebar_options = sidebar_options)
  
  avgEIVs <- shiny::callModule(module = calcAvgEIVs,
                               id = "calcAvgEIVs_id_1",
                               surveyData = surveyData,
                               sidebar_options = sidebar_options)
  
  diversityAnalysis <- shiny::callModule(module = diversityAnalysis,
                                         id = "diversityAnalysis_id_1",
                                         surveyData = surveyData,
                                         sidebar_options = sidebar_options)
  
  mvaNationalRefResults <- shiny::callModule(module = mvaNationalRef,
                                             id = "mvaNationalRef_id_1",
                                             setupData = setupData,
                                             surveyData = surveyData,
                                             nvcAssignment = nvcAssignment,
                                             sidebar_options = sidebar_options)

  mvaLocalRefRestrictedResults <- shiny::callModule(module = mvaLocalRefRestricted,
                                                    id = "mvaLocalRefRestricted_id_1",
                                                    setupData = setupData,
                                                    surveyData = surveyData,
                                                    nvcAssignment = nvcAssignment,
                                                    sidebar_options = sidebar_options)

  mvaLocalRefUnrestrictedResults <- shiny::callModule(module = mvaLocalRefUnrestricted,
                                                      id = "mvaLocalRefUnrestricted_id_1",
                                                      setupData = setupData,
                                                      surveyData = surveyData,
                                                      nvcAssignment = nvcAssignment,
                                                      avgEIVs = avgEIVs,
                                                      sidebar_options = sidebar_options)
  
  shiny::callModule(module = report,
                    id = "sidebar_id_1",
                    sidebar_options = sidebar_options,
                    surveyData = surveyData,
                    surveyDataValidator = surveyDataValidator,
                    surveyDataSummary = surveyDataSummary,
                    nvcAssignment = nvcAssignment,
                    habCor = habCor,
                    floristicTables = floristicTables,
                    speciesFreq = speciesFreq,
                    avgEIVs = avgEIVs,
                    diversityAnalysis = diversityAnalysis,
                    mvaNationalRefResults = mvaNationalRefResults,
                    mvaLocalRefRestrictedResults = mvaLocalRefRestrictedResults,
                    mvaLocalRefUnrestrictedResults = mvaLocalRefUnrestrictedResults)
  
  
  # Save module outputs to global environment, uncomment for development only!
  # observe({
  # 
  #   # assign(x = "sidebar_options", value = sidebar_options(), envir = .GlobalEnv)
  #   # assign(x = "surveyData", value = surveyData(), envir = .GlobalEnv)
  #   # assign(x = "surveyDataValidator", value = surveyDataValidator(), envir = .GlobalEnv)
  #   # assign(x = "surveyDataSummary", surveyDataSummary(), envir = .GlobalEnv)
  #   # assign(x = "floristicTables", value = floristicTables(), envir = .GlobalEnv)
  #   # assign(x = "nvcAssignment", value = nvcAssignment(), envir = .GlobalEnv)
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
