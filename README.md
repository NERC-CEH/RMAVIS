
# RMAVIS

<!-- badges: start -->

[![Generic
badge](https://img.shields.io/badge/Version-0.9995-green.svg)]()
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/722095560.svg)](https://zenodo.org/badge/latestdoi/722095560)
[![status](https://joss.theoj.org/papers/460c6f934a108fcf5a16d0f2ab77492e/status.svg)](https://joss.theoj.org/papers/460c6f934a108fcf5a16d0f2ab77492e)
[![License: CC BY
4.0](https://img.shields.io/badge/License-LGPL%203.0-lightgrey.svg)](https://opensource.org/license/lgpl-3-0)
<!-- badges: end -->

The RMAVIS R Shiny web application is a tool to assist in the assignment
of vegetation plot sample data to UK National Vegetation Classification
units, with additional exploratory analyses.

## Running the app

You can run RMAVIS from [GitHub](https://github.com/ZekeMarshall/RMAVIS)
by cloning the repository, calling `renv::restore()`, and then calling
`shiny::runApp("app.R")`.

If `renv::restore()` fails run
`install.packages(unique(renv::dependencies()$Package), dependencies = TRUE)`,
note that whilst the correct dependencies will be installed, the
versions may not match those in the renv.lock file, and so the correct
functioning of the app cannot be guaranteed.
