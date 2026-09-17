# Install all dependencies without using renv
# This can be used to setup a new profile, though the app will have to be inspected
# to ensure it is running correctly.
# NOTE: This will install the latest versions of the package dependencies, 
#       not the versions used in the most recent working app as recorded in renv
#       in previous app releases.
#       Therefore, the app is not guaranteed to run; neither are the results
#       guaranteed to be the same, all else being equal.

deps <- setdiff(unique(c(
  unique(renv::dependencies(path = "./inst/app/report")$Package),
  unique(renv::dependencies(path = "./inst/app/app.R")$Package),
  unique(renv::dependencies(path = "./inst/app/modules")$Package),
  unique(renv::dependencies(path = "./R")$Package)
  )), c("UKVegTB", "MNNPC", "GBNVC", "RMAVIS")) |>
  sort()

renv::install(deps)

remotes::install_github("NERC-CEH/UKVegTB@v0.1.9")
remotes::install_github("NERC-CEH/GBNVC@v0.1.0")
remotes::install_github("MN-DNR-MBS/MNNPC@v1.1.7")
