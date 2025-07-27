# Roughly adhere to this strategy: https://rstudio.github.io/cheatsheets/html/package-development.html

# If updates have only been made to inst/app files, you can test the app without 
# needing to reinstall the package.
# First, as RMAVIS::runApp masks shiny::runApp detach RMAVIS as follows unloadNamespace("RMAVIS")
# Second, use the RStudio run app button or run shiny::runApp("inst/app")

# 0) Copy README and NEWS to app directory
file.copy(from = "./NEWS.md", to = "./inst/app/NEWS.md", overwrite = TRUE)
file.copy(from = "./README.md", to = "./inst/app/README.md", overwrite = TRUE)

rmarkdown::render(input = "./inst/app/docs/documentation.Rmd",  output_dir = "./inst/app/www")

# 1) document functions and objects
devtools::document()

# 2) build package
devtools::build()

# 3) test package
# devtools::test()

# 4) check the package
# devtools::check(document = FALSE)

# 5) install package
devtools::install(pkg = ".")

# 6) restart R
.rs.restartR()

# 7) load package
library(RMAVIS)

# 8) Build package manual
devtools::build_manual(pkg = ".", path = ".")

# 9) Build pkgdown website
# devtools::build_site()

# 10) Manually deploy website
# pkgdown::deploy_to_branch()