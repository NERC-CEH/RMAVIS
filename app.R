# App file for deployment -------------------------------------------------
dir <- system.file("app", package = "RMAVIS")
setwd(dir)
shiny::shinyAppDir(".")