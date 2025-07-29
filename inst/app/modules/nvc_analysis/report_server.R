report <- function(input, output, session,
                   sidebar_options,
                   surveyData,
                   surveyDataValidator,
                   surveyDataSummary,
                   nvcAssignment,
                   habCor,
                   floristicTables,
                   speciesFreq,
                   avgEIVs,
                   diversityAnalysis,
                   mvaNationalRefResults,
                   mvaLocalRefRestrictedResults,
                   mvaLocalRefUnrestrictedResults) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  
  # observe({
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
 
  
  output$generateReport <- shiny::downloadHandler(
    
    filename = function() {
      paste0("RMAVIS.Report.",
             "v1-1-3.",
             format(Sys.time(), "%y-%m-%d.%H-%M-%S"),
             ".pdf",
             sep="")
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
      tempLogo <- file.path(td, "ukceh_logo_long_720x170_rgb.png")
      # tempConstants <- file.path(td, "create_constants.R")
      
      file.copy("./report/Report.Rmd", tempReport, overwrite = TRUE)
      file.copy("./report/Report.preamble.tex", tempPreamble, overwrite = TRUE)
      file.copy("./report/Report.refs.bib", tempRefs, overwrite = TRUE)
      file.copy("./www/ukceh_logo_long_720x170_rgb.png", tempLogo, overwrite = TRUE)
      # file.copy("./R/create_constants.R", tempConstants, overwrite = TRUE)
      
      
      rmarkdown::render(tempReport,
                        clean = TRUE,
                        # output_dir,
                        output_file = file,
                        params = list(
                          sidebar_options = sidebar_options(),
                          reportAuthorName = sidebar_options()$reportAuthorName,
                          surveyData = surveyData(),
                          surveyDataValidator = surveyDataValidator(),
                          surveyDataSummary = surveyDataSummary(),
                          nvcAssignment = nvcAssignment(),
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
