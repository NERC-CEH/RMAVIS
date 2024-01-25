setupData <- function(input, output, session, sidebar_options) {
  
  ns <- session$ns
  
# Establish reactive objects ----------------------------------------------
  uploadedTaxonomicBackbone <- reactiveVal()
  finalTaxonomicBackbone <- reactiveVal()

# Retrieve sidebar options ------------------------------------------------
  selectedTaxonomicBackboneMethod <- reactiveVal()
  selectedTaxonomicBackbone <- reactiveVal()
  wcvpCountries <- reactiveVal()
  
  observe({
    
    selectedTaxonomicBackboneMethod(sidebar_options()$taxonomicBackboneMethod)
    selectedTaxonomicBackbone(sidebar_options()$bundledTaxonomicBackbone)
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