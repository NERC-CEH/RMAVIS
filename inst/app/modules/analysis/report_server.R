report <- function(input, output, session,
                   setupData,
                   sidebar_options,
                   surveyData,
                   surveyDataValidator,
                   surveyDataSummary,
                   vcAssignment,
                   habCor,
                   floristicTables,
                   speciesFreq,
                   avgEIVs,
                   diversityAnalysis,
                   mvaNationalRefResults,
                   mvaLocalRefRestrictedResults,
                   mvaLocalRefUnrestrictedResults) {
  
  ns <- session$ns
  

# Retrieve setup data -----------------------------------------------------
  region <- reactiveVal()
  
  observe({
    
    region(setupData()$region)
    
  }) |>
    shiny::bindEvent(setupData(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)

  

# Render report -----------------------------------------------------------
  output$generateReport <- shiny::downloadHandler(
    
    filename = function() {
      paste0("RMAVIS.Report.",
             "v1-2-0.",
             format(Sys.time(), "%y-%m-%d.%H-%M-%S"),
             ".pdf",
             sep = "")
    },
    
    content = function(file) {
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "#3F9280",
        text = "Generating Report"
      )
      
      td <- tempdir()
      
      tempReport <- file.path(td, "Report.Rmd")
      tempPreamble <- file.path(td, "Report.preamble.tex")
      tempRefs <- file.path(td, "Report.refs.bib")
      tempLogo <- file.path(td, "logo.png")
      
      file.copy("./report/Report.Rmd", tempReport, overwrite = TRUE)
      file.copy("./report/Report.preamble.tex", tempPreamble, overwrite = TRUE)
      file.copy("./report/Report.refs.bib", tempRefs, overwrite = TRUE)
      
      if(region() == "gbnvc"){
        file.copy("./www/UKCEH_Logo_Master_Black.png", tempLogo, overwrite = TRUE)
      } else if(region() == "mnnpc"){
        file.copy("./www/DNR_Logo_RGB.png", tempLogo, overwrite = TRUE)
      }
      
      rmarkdown::render(tempReport,
                        clean = TRUE,
                        # output_dir,
                        output_file = file,
                        params = list(
                          setupData = setupData(),
                          sidebar_options = sidebar_options(),
                          reportAuthorName = sidebar_options()$reportAuthorName,
                          surveyData = surveyData(),
                          surveyDataValidator = surveyDataValidator(),
                          surveyDataSummary = surveyDataSummary(),
                          vcAssignment = vcAssignment(),
                          habCor = habCor(),
                          floristicTables = floristicTables(),
                          speciesFreq = speciesFreq(),
                          avgEIVs = avgEIVs(),
                          diversityAnalysis = diversityAnalysis(),
                          mvaNationalRefResults = mvaNationalRefResults(),
                          mvaLocalRefRestrictedResults = mvaLocalRefRestrictedResults(),
                          mvaLocalRefUnrestrictedResults = mvaLocalRefUnrestrictedResults()
                        ),
                        envir = new.env(parent = globalenv()))
      
      # Clean up temporary directories
      # rm(td)
      
      shinybusy::remove_modal_spinner()
    }
  )
  
}
