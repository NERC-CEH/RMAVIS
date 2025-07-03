# Silence dplyr::summarise() messages -------------------------------------
options(dplyr.summarise.inform = FALSE)

# Prevent auto-loading of all functions in /R -----------------------------
options(shiny.autoload.r = FALSE)

# Load the RMAVIS package -------------------------------------------------
# pkgload::load_all(path = ".")

# Check required packages -------------------------------------------------
# renv::dependencies()$Package |> unique() |> sort()
# The dependencies returned from the code above are listed below with the
# exception of: base, devtools, grid (part of base), grDevices (part of base), renv, 
# RMAVIS, rsconnect, testthat, tools, and usethis
# These are also the packages that should be recorded as Imports in the 
# DESCRIPTION file

# Load required packages --------------------------------------------------
suppressPackageStartupMessages({
  library(bookdown)
  library(bsicons)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(htmltools)
  library(htmlwidgets)
  library(kableExtra)
  library(knitr)
  library(magrittr)
  library(markdown)
  library(plotly)
  library(purrr)
  library(reactable)
  library(readr)
  library(rhandsontable)
  library(rmarkdown)
  library(shiny)
  library(shinybusy)
  library(shinyjs)
  library(shinyWidgets)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(vegan)
  library(writexl)
})

# Source sub-modules ------------------------------------------------------
source("./modules/home_ui.R", local = TRUE)

source("./modules/nvcInfo_ui.R", local = TRUE)
source("./modules/nvcInfo_server.R", local = TRUE)

source("./modules/privacy_ui.R", local = TRUE)

source("./modules/documentation_ui.R", local = TRUE)

source("./modules/news_ui.R", local = TRUE)

## Data entry -------------------------------------------------------------
source("./modules/data_entry/deSidebar_ui.R", local = TRUE)
source("./modules/data_entry/deSidebar_server.R", local = TRUE)

source("./modules/data_entry/setupData_server.R", local = TRUE)

source("./modules/data_entry/uploadData_ui.R", local = TRUE)
source("./modules/data_entry/uploadData_server.R", local = TRUE)

source("./modules/data_entry/surveyData_ui.R", local = TRUE)
source("./modules/data_entry/surveyData_server.R", local = TRUE)

source("./modules/data_entry/surveyDataValidator_ui.R", local = TRUE)
source("./modules/data_entry/surveyDataValidator_server.R", local = TRUE)

source("./modules/data_entry/surveyDataSummary_ui.R", local = TRUE)
source("./modules/data_entry/surveyDataSummary_server.R", local = TRUE)

## Core -------------------------------------------------------------------
source("./modules/core/sidebar_ui.R", local = TRUE)
source("./modules/core/sidebar_server.R", local = TRUE)

source("./modules/core/floristicTables_ui.R", local = TRUE)
source("./modules/core/floristicTables_server.R", local = TRUE)

source("./modules/core/nvcAssignment_ui.R", local = TRUE)
source("./modules/core/nvcAssignment_server.R", local = TRUE)

source("./modules/core/habCor_ui.R", local = TRUE)
source("./modules/core/habCor_server.R", local = TRUE)

source("./modules/core/speciesFreq_ui.R", local = TRUE)
source("./modules/core/speciesFreq_server.R", local = TRUE)

source("./modules/core/calcAvgEIVs_ui.R", local = TRUE)
source("./modules/core/calcAvgEIVs_server.R", local = TRUE)

source("./modules/core/diversityAnalysis_ui.R", local = TRUE)
source("./modules/core/diversityAnalysis_server.R", local = TRUE)

source("./modules/core/mvaNationalRef_ui.R", local = TRUE)
source("./modules/core/mvaNationalRef_server.R", local = TRUE)

source("./modules/core/mvaLocalRefRestricted_ui.R", local = TRUE)
source("./modules/core/mvaLocalRefRestricted_server.R", local = TRUE)

source("./modules/core/mvaLocalRefUnrestricted_ui.R", local = TRUE)
source("./modules/core/mvaLocalRefUnrestricted_server.R", local = TRUE)

source("./modules/core/report_server.R", local = TRUE)

# Source main UI and Server modules ---------------------------------------
source("./modules/ui.R", local = TRUE)
source("./modules/server.R", local = TRUE)

# Run the application -----------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
