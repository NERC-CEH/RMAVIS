# Silence dplyr::summarise() messages -------------------------------------
options(dplyr.summarise.inform = FALSE)

# Prevent auto-loading of all functions in /R -----------------------------
options(shiny.autoload.r = FALSE)

# Load the RMAVIS package -------------------------------------------------
# pkgload::load_all(path = ".")

# Check required packages -------------------------------------------------
# renv::dependencies()$Package |> unique() |> sort()
# The dependencies returned from the code above are listed below with the
# exception of: base, grid (part of base), grDevices (part of base), renv, 
# RMAVIS, rsconnect, testthat, tools, and usethis
# These are also the packages that should be recorded as Imports in the 
# DESCRIPTION file

# Load required packages --------------------------------------------------
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

# Render documentation ----------------------------------------------------
# rmarkdown::render(input = "./inst/app/docs/documentation.Rmd",  output_dir = "./inst/app/www")

# Source sub-modules ------------------------------------------------------
source("./modules/home_ui.R", local = TRUE)

source("./modules/privacy_ui.R", local = TRUE)

source("./modules/documentation_ui.R", local = TRUE)

source("./modules/news_ui.R", local = TRUE)

source("./modules/nvcInfo_ui.R", local = TRUE)
source("./modules/nvcInfo_server.R", local = TRUE)

source("./modules/sidebar_ui.R", local = TRUE)
source("./modules/sidebar_server.R", local = TRUE)

source("./modules/setupData_server.R", local = TRUE)

source("./modules/uploadData_ui.R", local = TRUE)
source("./modules/uploadData_server.R", local = TRUE)

source("./modules/surveyData_ui.R", local = TRUE)
source("./modules/surveyData_server.R", local = TRUE)

source("./modules/surveyDataValidator_ui.R", local = TRUE)
source("./modules/surveyDataValidator_server.R", local = TRUE)

source("./modules/surveyDataSummary_ui.R", local = TRUE)
source("./modules/surveyDataSummary_server.R", local = TRUE)

source("./modules/floristicTables_ui.R", local = TRUE)
source("./modules/floristicTables_server.R", local = TRUE)

source("./modules/nvcAssignment_ui.R", local = TRUE)
source("./modules/nvcAssignment_server.R", local = TRUE)

source("./modules/habCor_ui.R", local = TRUE)
source("./modules/habCor_server.R", local = TRUE)

source("./modules/speciesFreq_ui.R", local = TRUE)
source("./modules/speciesFreq_server.R", local = TRUE)

source("./modules/calcAvgEIVs_ui.R", local = TRUE)
source("./modules/calcAvgEIVs_server.R", local = TRUE)

source("./modules/diversityAnalysis_ui.R", local = TRUE)
source("./modules/diversityAnalysis_server.R", local = TRUE)

source("./modules/mvaNationalRef_ui.R", local = TRUE)
source("./modules/mvaNationalRef_server.R", local = TRUE)

source("./modules/mvaLocalRefRestricted_ui.R", local = TRUE)
source("./modules/mvaLocalRefRestricted_server.R", local = TRUE)

source("./modules/mvaLocalRefUnrestricted_ui.R", local = TRUE)
source("./modules/mvaLocalRefUnrestricted_server.R", local = TRUE)

source("./modules/report_server.R", local = TRUE)

# Source main UI and Server modules ---------------------------------------
source("./modules/ui.R", local = TRUE)
source("./modules/server.R", local = TRUE)

# Run the application -----------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
