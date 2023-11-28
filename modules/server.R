# Server
server <- function(input, output, session) {

  sidebar_options <- callModule(module = sidebar,
                                id = "sidebar_id_1",
                                nvcAverageSim = nvcAverageSim)
  
  surveyTable <- callModule(module = surveyTable,
                            id = "surveyTable_id_1",
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

}
