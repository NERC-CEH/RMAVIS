# Server
server <- function(input, output, session) {

  sidebar_options <- shiny::callModule(module = sidebar,
                                       id = "sidebar_id_1",
                                       surveyTable = surveyTable,
                                       nvcAssignment = nvcAssignment,
                                       floristicTables = floristicTables,
                                       mvaLocalRefRestrictedResults = mvaLocalRefRestrictedResults)
  
  shiny::callModule(module = nvcInfo,
                    id = "nvcInfo_id_1")
  
  uploadDataTable <- shiny::callModule(module = uploadData,
                                       id = "uploadData_id_1")
  
  surveyTable <- shiny::callModule(module = surveyTable,
                                   id = "surveyTable_id_1",
                                   uploadDataTable = uploadDataTable,
                                   sidebar_options = sidebar_options)
  
  surveyTableWide <- shiny::callModule(module = surveyTableWide,
                                       id = "surveyTableWide_id_1",
                                       surveyTable = surveyTable,
                                       sidebar_options = sidebar_options)
  
  nvcAssignment <- shiny::callModule(module = nvcAssignment,
                                     id = "nvcAssignment_id_1",
                                     surveyTable = surveyTable,
                                     sidebar_options = sidebar_options)

  habCor <- shiny::callModule(module = habCor,
                              id = "habCor_id_1",
                              nvcAssignment = nvcAssignment,
                              sidebar_options = sidebar_options)

  floristicTables <- shiny::callModule(module = floristicTables,
                                       id = "floristicTables_id_1",
                                       surveyTable = surveyTable,
                                       sidebar_options = sidebar_options)
  
  avgEIVs <- shiny::callModule(module = calcAvgEIVs,
                               id = "calcAvgEIVs_id_1",
                               surveyTable = surveyTable,
                               sidebar_options = sidebar_options)
  
  shiny::callModule(module = diversityAnalysis,
                    id = "diversityAnalysis_id_1",
                    surveyTable = surveyTable,
                    surveyTableWide = surveyTableWide,
                    sidebar_options = sidebar_options)
  
  mvaNationalRefResults <- shiny::callModule(module = mvaNationalRef,
                                             id = "mvaNationalRef_id_1",
                                             surveyTable = surveyTable,
                                             # surveyTableWide = surveyTableWide,
                                             nvcAssignment = nvcAssignment,
                                             sidebar_options = sidebar_options)

  mvaLocalRefRestrictedResults <- shiny::callModule(module = mvaLocalRefRestricted,
                                                    id = "mvaLocalRefRestricted_id_1",
                                                    surveyTable = surveyTable,
                                                    # surveyTableWide = surveyTableWide,
                                                    nvcAssignment = nvcAssignment,
                                                    sidebar_options = sidebar_options)

  # mvaLocalRefUnrestrictedResults <- shiny::callModule(module = mvaLocalRefUnrestricted,
  #                                                     id = "mvaLocalRefUnrestricted_id_1",
  #                                                     surveyTable = surveyTable,
  #                                                     # surveyTableWide = surveyTableWide,
  #                                                     nvcAssignment = nvcAssignment,
  #                                                     sidebar_options = sidebar_options)
  

# Report ------------------------------------------------------------------
  shiny::callModule(module = report,
                    id = "sidebar_id_1",
                    surveyTable = surveyTable,
                    nvcAssignment = nvcAssignment,
                    mvaLocalRefRestrictedResults = mvaLocalRefRestrictedResults,
                    mvaLocalRefUnrestrictedResults = mvaLocalRefUnrestrictedResults,
                    sidebar_options = sidebar_options,
                    floristicTables = floristicTables)
  
  
  # observe({
  #   
  #   assign(x = "sidebar_options", value = sidebar_options(), envir = .GlobalEnv)
  #   assign(x = "surveyTable", value = surveyTable(), envir = .GlobalEnv)
  #   assign(x = "surveyTableWide", value = surveyTableWide(), envir = .GlobalEnv)
  #   assign(x = "nvcAssignment", value = nvcAssignment(), envir = .GlobalEnv)
  #   assign(x = "habCor", value = habCor(), envir = .GlobalEnv)
  #   
  # })

}
