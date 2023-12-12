# To run app: shiny::runApp("app.R")

# shinyOptions(bslib = TRUE)

# Load data into memory ---------------------------------------------------
source("R/load_data.R", local = TRUE)

# Load constants into memory ----------------------------------------------
source("R/create_constants.R", local = TRUE)

# Render documentation ----------------------------------------------------
source("R/render_docs.R", local = TRUE)

# Source functions --------------------------------------------------------
source("R/functions.R", local = TRUE) # Do I need to switch this to global to allow knitr access?

# Source sub-modules ------------------------------------------------------
source("modules/documentation_ui.R", local = TRUE)

source("modules/releaseLog_ui.R", local = TRUE)

source("modules/nvcInfo_ui.R", local = TRUE)
source("modules/nvcInfo_server.R", local = TRUE)

source("modules/sidebar_ui.R", local = TRUE)
source("modules/sidebar_server.R", local = TRUE)

source("modules/uploadData_ui.R", local = TRUE)
source("modules/uploadData_server.R", local = TRUE)

source("modules/surveyTable_ui.R", local = TRUE)
source("modules/surveyTable_server.R", local = TRUE)

source("modules/surveyTableValidator_ui.R", local = TRUE)
source("modules/surveyTableValidator_server.R", local = TRUE)

source("modules/surveyTableWide_ui.R", local = TRUE)
source("modules/surveyTableWide_server.R", local = TRUE)

source("modules/nvcAssignment_ui.R", local = TRUE)
source("modules/nvcAssignment_server.R", local = TRUE)

source("modules/habCor_ui.R", local = TRUE)
source("modules/habCor_server.R", local = TRUE)

source("modules/floristicTables_ui.R", local = TRUE)
source("modules/floristicTables_server.R", local = TRUE)

source("modules/speciesFreq_ui.R", local = TRUE)
source("modules/speciesFreq_server.R", local = TRUE)

source("modules/calcAvgEIVs_ui.R", local = TRUE)
source("modules/calcAvgEIVs_server.R", local = TRUE)

source("modules/diversityAnalysis_ui.R", local = TRUE)
source("modules/diversityAnalysis_server.R", local = TRUE)

source("modules/mvaNationalRef_ui.R", local = TRUE)
source("modules/mvaNationalRef_server.R", local = TRUE)

source("modules/mvaLocalRefRestricted_ui.R", local = TRUE)
source("modules/mvaLocalRefRestricted_server.R", local = TRUE)

source("modules/mvaLocalRefUnrestricted_ui.R", local = TRUE)
source("modules/mvaLocalRefUnrestricted_server.R", local = TRUE)

# source("modules/report_ui.R", local = TRUE)
source("modules/report_server.R", local = TRUE)

# Source main UI and Server modules ---------------------------------------
source("modules/ui.R", local = TRUE)
source("modules/server.R", local = TRUE)

# Run the application -----------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
