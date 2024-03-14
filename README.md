
# RMAVIS

<!-- badges: start -->

[![Generic
badge](https://img.shields.io/badge/Version-0.99-green.svg)]()
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/722095560.svg)](https://zenodo.org/badge/latestdoi/722095560)
<!-- badges: end -->

The RMAVIS R Shiny web application is a tool to assist in the assignment
of vegetation plot sample data to UK National Vegetation Classification
units, with additional exploratory analyses.

## Running the app

You can run RMAVIS from [GitHub](https://github.com/ZekeMarshall/RMAVIS)
by cloning the repository, calling `renv::restore()`, and then calling
`shiny::runApp("app.R")`.

If `renv::restore()` fails run
`install.packages(unique(renv::dependencies()$Package), dependencies = TRUE)`.
Note that whilst the correct dependencies will be installed, the
versions may not match those in the renv.lock file.
