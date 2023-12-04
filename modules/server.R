# Server
server <- function(input, output, session) {

  sidebar_options <- callModule(module = sidebar,
                                id = "sidebar_id_1",
                                nvcAverageSim = nvcAverageSim)
  
  uploadDataTable <- callModule(module = uploadData,
                                id = "uploadData_id_1")
  
  surveyTable <- callModule(module = surveyTable,
                            id = "surveyTable_id_1",
                            uploadDataTable = uploadDataTable,
                            sidebar_options = sidebar_options)
  
  surveyTablePrepped <- callModule(module = surveyTablePrepped,
                                   id = "surveyTablePrepped_id_1",
                                   surveyTable = surveyTable,
                                   sidebar_options = sidebar_options)
  
  nvcAverageSim <- callModule(module = nvcAverageSim,
                              id = "nvcAverageSim_id_1",
                              surveyTable = surveyTable,
                              sidebar_options = sidebar_options)

  assignNVCResults <- callModule(module = assignNVCResults,
                                 id = "assignNVCResults_id_1",
                                 surveyTable = surveyTable,
                                 sidebar_options = sidebar_options)

  habCor <- callModule(module = habCor,
                       id = "habCor_id_1",
                       nvcAverageSim = nvcAverageSim,
                       sidebar_options = sidebar_options)

  callModule(module = floristicTables,
             id = "floristicTables_id_1",
             surveyTable = surveyTable,
             sidebar_options = sidebar_options)
  
  callModule(module = calcAvgEIVs,
             id = "calcAvgEIVs_id_1",
             surveyTablePrepped = surveyTablePrepped,
             sidebar_options = sidebar_options)
  
  callModule(module = diversityAnalysis,
             id = "diversityAnalysis_id_1",
             surveyTable = surveyTable,
             surveyTablePrepped = surveyTablePrepped,
             sidebar_options = sidebar_options)
  
  # callModule(module = heatmap,
  #            id = "heatmap_id_1",
  #            surveyTable = surveyTable,
  #            sidebar_options = sidebar_options)
  
  dcaFixedSpaceResults <- callModule(module = dcaFixedSpace,
                                     id = "dcaFixedSpace_id_1",
                                     surveyTable = surveyTable,
                                     # surveyTablePrepped = surveyTablePrepped,
                                     nvcAverageSim = nvcAverageSim,
                                     sidebar_options = sidebar_options)
  
  dcaAllQuadratsResults <- callModule(module = dcaAllQuadrats,
                                      id = "dcaAllQuadrats_id_1",
                                      surveyTable = surveyTable,
                                      # surveyTablePrepped = surveyTablePrepped,
                                      nvcAverageSim = nvcAverageSim,
                                      sidebar_options = sidebar_options)
  

# Report ------------------------------------------------------------------
  callModule(module = report,
             id = "sidebar_id_1",
             surveyTable = surveyTable,
             surveyTablePrepped = surveyTablePrepped,
             nvcAverageSim = nvcAverageSim,
             assignNVCResults = assignNVCResults,
             dcaFixedSpaceResults = dcaFixedSpaceResults,
             sidebar_options = sidebar_options)
  
  
  # observe({
  #   
  #   assign(x = "sidebar_options", value = sidebar_options(), envir = .GlobalEnv)
  #   assign(x = "surveyTable", value = surveyTable(), envir = .GlobalEnv)
  #   assign(x = "surveyTablePrepped", value = surveyTablePrepped(), envir = .GlobalEnv)
  #   assign(x = "nvcAverageSim", value = nvcAverageSim(), envir = .GlobalEnv)
  #   assign(x = "assignNVCResults", value = assignNVCResults(), envir = .GlobalEnv)
  #   assign(x = "habCor", value = habCor(), envir = .GlobalEnv)
  #   
  # })

}
