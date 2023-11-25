# shinyOptions(bslib = TRUE)

# Load constants into memory
source("R/create_constants_load.R", local = TRUE)

# Render documentation
source("R/render_docs.R", local = TRUE)

# Source functions

# Source sub-modules
source("modules/documentation_ui.R", local = TRUE)

source("modules/sidebar_ui.R", local = TRUE)
source("modules/sidebar_server.R", local = TRUE)

source("modules/surveyTable_ui.R", local = TRUE)
source("modules/surveyTable_server.R", local = TRUE)

source("modules/assignNVCResults_ui.R", local = TRUE)
source("modules/assignNVCResults_server.R", local = TRUE)

source("modules/nvcAverageSim_ui.R", local = TRUE)
source("modules/nvcAverageSim_server.R", local = TRUE)

source("modules/habCor_ui.R", local = TRUE)
source("modules/habCor_server.R", local = TRUE)

source("modules/floristicTables_ui.R", local = TRUE)
source("modules/floristicTables_server.R", local = TRUE)

# Source main UI and Server modules
source("modules/ui.R", local = TRUE)
source("modules/server.R", local = TRUE)

# Run the application
shiny::shinyApp(ui = ui, server = server)
