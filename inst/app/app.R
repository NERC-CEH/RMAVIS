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
  library(UKVegTB)
  library(vegan)
  library(writexl)
})

# Source sub-modules ------------------------------------------------------
source("./modules/home_ui.R", local = TRUE)

source("./modules/privacy_ui.R", local = TRUE)

source("./modules/additional_info_ui.R", local = TRUE)

source("./modules/documentation_ui.R", local = TRUE)

source("./modules/news_ui.R", local = TRUE)

source("./modules/setupData_server.R", local = TRUE)

source("./modules/regionSelect_ui.R", local = TRUE)
source("./modules/regionSelect_server.R", local = TRUE)

## NVC information --------------------------------------------------------
source("./modules/vc_information/vcInfoSidebar_ui.R", local = TRUE)
source("./modules/vc_information/vcInfoSidebar_server.R", local = TRUE)

source("./modules/vc_information/vcCommNamesLookup_ui.R", local = TRUE)
source("./modules/vc_information/vcCommNamesLookup_server.R", local = TRUE)

source("./modules/vc_information/vcTaxonNamesLookup_ui.R", local = TRUE)
source("./modules/vc_information/vcTaxonNamesLookup_server.R", local = TRUE)

source("./modules/vc_information/vcFlorTabs_ui.R", local = TRUE)
source("./modules/vc_information/vcFlorTabs_server.R", local = TRUE)

source("./modules/vc_information/vcCommAttr_ui.R", local = TRUE)
source("./modules/vc_information/vcCommAttr_server.R", local = TRUE)

## Data entry -------------------------------------------------------------
source("./modules/data_entry/deSidebar_ui.R", local = TRUE)
source("./modules/data_entry/deSidebar_server.R", local = TRUE)

source("./modules/data_entry/uploadData_ui.R", local = TRUE)
source("./modules/data_entry/uploadData_server.R", local = TRUE)

source("./modules/data_entry/surveyData_ui.R", local = TRUE)
source("./modules/data_entry/surveyData_server.R", local = TRUE)

source("./modules/data_entry/surveyDataValidator_ui.R", local = TRUE)
source("./modules/data_entry/surveyDataValidator_server.R", local = TRUE)

source("./modules/data_entry/surveyDataSummary_ui.R", local = TRUE)
source("./modules/data_entry/surveyDataSummary_server.R", local = TRUE)

source("./modules/data_entry/rmavisTaxonNamesLookup_ui.R", local = TRUE)
source("./modules/data_entry/rmavisTaxonNamesLookup_server.R", local = TRUE)

## NVC --------------------------------------------------------------------
source("./modules/analysis/sidebar_ui.R", local = TRUE)
source("./modules/analysis/sidebar_server.R", local = TRUE)

source("./modules/analysis/floristicTables_ui.R", local = TRUE)
source("./modules/analysis/floristicTables_server.R", local = TRUE)

source("./modules/analysis/vcAssignment_ui.R", local = TRUE)
source("./modules/analysis/vcAssignment_server.R", local = TRUE)

source("./modules/analysis/habCor_ui.R", local = TRUE)
source("./modules/analysis/habCor_server.R", local = TRUE)

source("./modules/analysis/speciesFreq_ui.R", local = TRUE)
source("./modules/analysis/speciesFreq_server.R", local = TRUE)

source("./modules/analysis/calcAvgEIVs_ui.R", local = TRUE)
source("./modules/analysis/calcAvgEIVs_server.R", local = TRUE)

source("./modules/analysis/diversityAnalysis_ui.R", local = TRUE)
source("./modules/analysis/diversityAnalysis_server.R", local = TRUE)

source("./modules/analysis/mvaNationalRef_ui.R", local = TRUE)
source("./modules/analysis/mvaNationalRef_server.R", local = TRUE)

source("./modules/analysis/mvaLocalRefRestricted_ui.R", local = TRUE)
source("./modules/analysis/mvaLocalRefRestricted_server.R", local = TRUE)

source("./modules/analysis/mvaLocalRefUnrestricted_ui.R", local = TRUE)
source("./modules/analysis/mvaLocalRefUnrestricted_server.R", local = TRUE)

source("./modules/analysis/report_server.R", local = TRUE)

# Source main UI and Server modules ---------------------------------------
source("./modules/ui.R", local = TRUE)
source("./modules/server.R", local = TRUE)

# Run the application -----------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
