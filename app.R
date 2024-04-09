# To run app --------------------------------------------------------------
# 1. Ensure the dependencies are installed by running `renv::activate()` and 
#    `renv::restore()`
# 2. Run the app with `shiny::runApp("app.R")`

# Silence dplyr::summarise() messages -------------------------------------
options(dplyr.summarise.inform = FALSE)

# Load the RMAVIS package -------------------------------------------------
pkgload::load_all(path = ".")

# Load required packages --------------------------------------------------
# Shiny-related
library(shiny)
library(rhandsontable)
library(reactable)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)

# General
library(rmarkdown)
library(bookdown)
library(tidyverse)
library(plotly)
library(kableExtra)
library(janitor)
library(writexl)

# Ecology
library(assignNVC)
library(vegan)

# Render documentation ----------------------------------------------------
rmarkdown::render(input = "docs/documentation.Rmd",  output_dir = "www")

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

source("modules/surveyData_ui.R", local = TRUE)
source("modules/surveyData_server.R", local = TRUE)

source("modules/surveyDataValidator_ui.R", local = TRUE)
source("modules/surveyDataValidator_server.R", local = TRUE)

source("modules/surveyDataSummary_ui.R", local = TRUE)
source("modules/surveyDataSummary_server.R", local = TRUE)

source("modules/floristicTables_ui.R", local = TRUE)
source("modules/floristicTables_server.R", local = TRUE)

source("modules/nvcAssignment_ui.R", local = TRUE)
source("modules/nvcAssignment_server.R", local = TRUE)

source("modules/habCor_ui.R", local = TRUE)
source("modules/habCor_server.R", local = TRUE)

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
