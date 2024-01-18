setupData <- function(input, output, session, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  taxonomicBackboneMethod <- reactiveVal()
  bundledTaxonomicBackbone <- reactiveVal()
  wcvpCountries <- reactiveVal()
  
  observe({
    
    taxonomicBackboneMethod(sidebar_options()$taxonomicBackboneMethod)
    bundledTaxonomicBackbone(sidebar_options()$bundledTaxonomicBackbone)
    wcvpCountries(sidebar_options()$wcvpCountries)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = FALSE)
  

# Create Accepted Species Object ------------------------------------------
  observe({
    
  }) |>
    bindEvent(taxonomicBackboneMethod(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

# Create Setup Data List --------------------------------------------------
  setupData <- list()
  
# Return Setup Data -------------------------------------------------------
  return()
  
}