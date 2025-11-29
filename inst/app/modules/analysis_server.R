analysis <- function(input, output, session,
                     region,
                     setupData, 
                     deSidebar_options,
                     surveyData,
                     surveyDataValidator,
                     surveyDataSummary
                     ){
  
  ns <- session$ns
  
# Reactively show/hide tabs -----------------------------------------------
  observe({
    
    show_eivs <- setupData()$regional_availability$avgEIVs
    show_habCor <- setupData()$regional_availability$habCor
    
    if(isTRUE(show_eivs)){
      shiny::showTab(inputId = "analysis_nct", target = "eivs_panel")
    } else {
      shiny::hideTab(inputId = "analysis_nct", target = "eivs_panel")
    }
    
    if(isTRUE(show_habCor)){
      shiny::showTab(inputId = "analysis_nct", target = "habCor_panel")
    } else {
      shiny::hideTab(inputId = "analysis_nct", target = "habCor_panel")
    }
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE)


# Run modules -------------------------------------------------------------
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
                                       mvaNationalRefResults = mvaNationalRefResults)
  
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

  habCor <- shiny::callModule(module = habCor,
                              id = "habCor_id_1",
                              setupData = setupData,
                              vcAssignment = vcAssignment,
                              sidebar_options = sidebar_options)

  speciesFreq <- shiny::callModule(module = speciesFreq,
                                   id = "speciesFreq_id_1",
                                   setupData = setupData,
                                   surveyData = surveyData,
                                   sidebar_options = sidebar_options)

  avgEIVs <- shiny::callModule(module = calcAvgEIVs,
                               id = "calcAvgEIVs_id_1",
                               setupData = setupData,
                               surveyData = surveyData,
                               sidebar_options = sidebar_options)

  diversityAnalysis <- shiny::callModule(module = diversityAnalysis,
                                         id = "diversityAnalysis_id_1",
                                         setupData = setupData,
                                         surveyData = surveyData,
                                         sidebar_options = sidebar_options)

  mvaNationalRefResults <- shiny::callModule(module = mvaNationalRef,
                                             id = "mvaNationalRef_id_1",
                                             setupData = setupData,
                                             surveyData = surveyData,
                                             vcAssignment = vcAssignment,
                                             avgEIVs = avgEIVs,
                                             sidebar_options = sidebar_options)

  mvaLocalRefRestrictedResults <- shiny::callModule(module = mvaLocalRefRestricted,
                                                    id = "mvaLocalRefRestricted_id_1",
                                                    setupData = setupData,
                                                    surveyData = surveyData,
                                                    vcAssignment = vcAssignment,
                                                    sidebar_options = sidebar_options)

  mvaLocalRefUnrestrictedResults <- shiny::callModule(module = mvaLocalRefUnrestricted,
                                                      id = "mvaLocalRefUnrestricted_id_1",
                                                      setupData = setupData,
                                                      surveyData = surveyData,
                                                      vcAssignment = vcAssignment,
                                                      avgEIVs = avgEIVs,
                                                      sidebar_options = sidebar_options)
  
  shiny::callModule(module = report,
                    id = "sidebar_id_1",
                    sidebar_options = sidebar_options,
                    surveyData = surveyData,
                    surveyDataValidator = surveyDataValidator,
                    surveyDataSummary = surveyDataSummary,
                    vcAssignment = vcAssignment,
                    habCor = habCor,
                    floristicTables = floristicTables,
                    speciesFreq = speciesFreq,
                    avgEIVs = avgEIVs,
                    diversityAnalysis = diversityAnalysis,
                    mvaNationalRefResults = mvaNationalRefResults,
                    mvaLocalRefRestrictedResults = mvaLocalRefRestrictedResults,
                    mvaLocalRefUnrestrictedResults = mvaLocalRefUnrestrictedResults
  )

  return(list("sidebar_options" = sidebar_options,
              "floristicTables" = floristicTables,
              "vcAssignment" = vcAssignment,
              "habCor" = habCor,
              "speciesFreq" = speciesFreq,
              "avgEIVs" = avgEIVs,
              "diversityAnalysis" = diversityAnalysis,
              "mvaNationalRefResults" = mvaNationalRefResults,
              "mvaLocalRefRestrictedResults" = mvaLocalRefRestrictedResults,
              "mvaLocalRefUnrestrictedResults" = mvaLocalRefUnrestrictedResults))
  
}
