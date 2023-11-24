# shinyOptions(bslib = TRUE)

# Source utility files
source("R/create_constants_load.R", local = TRUE)

# Render documentation
source("R/render_docs.R", local = TRUE)

# Source functions

# Source sub-modules
source("modules/documentation_ui.R", local = TRUE)
source("modules/sidebar_ui.R", local = TRUE)
source("modules/sidebar_server.R", local = TRUE)
source("modules/inputs_ui.R", local = TRUE)
source("modules/inputs_server.R", local = TRUE)
source("modules/results_ui.R", local = TRUE)
source("modules/results_server.R", local = TRUE)
source("modules/habCor_ui.R", local = TRUE)
source("modules/habCor_server.R", local = TRUE)

# Source main UI and Server modules
source("modules/ui.R", local = TRUE)
source("modules/server.R", local = TRUE)

# Run the application
shiny::shinyApp(ui = ui, server = server)
