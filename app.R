# Silence dplyr::summarise() messages -------------------------------------
options(dplyr.summarise.inform = FALSE)

# Load data into memory ---------------------------------------------------
source("R/load_data.R", local = TRUE)

# Load constants into memory ----------------------------------------------
source("R/create_constants.R", local = TRUE)

# Render documentation ----------------------------------------------------
source("R/render_docs.R", local = TRUE) # This can be disabled if one wishes to pre-render the documentation manually.

# Source functions --------------------------------------------------------
source("R/syntopicTable_functions.R", local = TRUE)
source("R/similarity_functions.R", local = TRUE)
source("R/reactable_functions.R", local = TRUE)

# Source sub-modules ------------------------------------------------------
source("modules/privacy_ui.R", local = TRUE)

source("modules/documentation_ui.R", local = TRUE)

source("modules/nvcInfo_ui.R", local = TRUE)
source("modules/nvcInfo_server.R", local = TRUE)

source("modules/sidebar_ui.R", local = TRUE)
source("modules/sidebar_server.R", local = TRUE)

source("modules/setupData_server.R", local = TRUE)

source("modules/uploadData_ui.R", local = TRUE)
source("modules/uploadData_server.R", local = TRUE)

source("modules/surveyTable_ui.R", local = TRUE)
source("modules/surveyTable_server.R", local = TRUE)

source("modules/surveyTableValidator_ui.R", local = TRUE)
source("modules/surveyTableValidator_server.R", local = TRUE)

source("modules/surveyTableSummary_ui.R", local = TRUE)
source("modules/surveyTableSummary_server.R", local = TRUE)

source("modules/surveyTableWide_ui.R", local = TRUE)
source("modules/surveyTableWide_server.R", local = TRUE)

# source("modules/selectedPquads_server.R", local = TRUE)

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

source("modules/report_server.R", local = TRUE)

# Source main UI and Server modules ---------------------------------------
source("modules/ui.R", local = TRUE)
source("modules/server.R", local = TRUE)

# Run the application -----------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
