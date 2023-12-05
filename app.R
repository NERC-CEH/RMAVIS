# To run app: shiny::runApp("app.R")

# shinyOptions(bslib = TRUE)

# Load constants into memory ----------------------------------------------
source("R/create_constants.R", local = TRUE)

# Load data into memory ---------------------------------------------------
source("R/load_data.R", local = TRUE)

# Render documentation ----------------------------------------------------
source("R/render_docs.R", local = TRUE)

# Source functions --------------------------------------------------------
source("R/functions.R", local = TRUE) # Do I need to switch this to global to allow knitr access?

# Source sub-modules ------------------------------------------------------
source("modules/documentation_ui.R", local = TRUE)

source("modules/sidebar_ui.R", local = TRUE)
source("modules/sidebar_server.R", local = TRUE)

source("modules/uploadData_ui.R", local = TRUE)
source("modules/uploadData_server.R", local = TRUE)

source("modules/surveyTable_ui.R", local = TRUE)
source("modules/surveyTable_server.R", local = TRUE)

source("modules/surveyTablePrepped_ui.R", local = TRUE)
source("modules/surveyTablePrepped_server.R", local = TRUE)

source("modules/assignNVCResults_ui.R", local = TRUE)
source("modules/assignNVCResults_server.R", local = TRUE)

source("modules/nvcAverageSim_ui.R", local = TRUE)
source("modules/nvcAverageSim_server.R", local = TRUE)

source("modules/habCor_ui.R", local = TRUE)
source("modules/habCor_server.R", local = TRUE)

source("modules/floristicTables_ui.R", local = TRUE)
source("modules/floristicTables_server.R", local = TRUE)

source("modules/calcAvgEIVs_ui.R", local = TRUE)
source("modules/calcAvgEIVs_server.R", local = TRUE)

source("modules/diversityAnalysis_ui.R", local = TRUE)
source("modules/diversityAnalysis_server.R", local = TRUE)

# source("modules/heatmap_ui.R", local = TRUE)
# source("modules/heatmap_server.R", local = TRUE)

source("modules/dcaFixedSpace_ui.R", local = TRUE)
source("modules/dcaFixedSpace_server.R", local = TRUE)

source("modules/dcaAllQuadrats_ui.R", local = TRUE)
source("modules/dcaAllQuadrats_server.R", local = TRUE)

# source("modules/report_ui.R", local = TRUE)
source("modules/report_server.R", local = TRUE)

# Source main UI and Server modules ---------------------------------------
source("modules/ui.R", local = TRUE)
source("modules/server.R", local = TRUE)

# Run the application -----------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
