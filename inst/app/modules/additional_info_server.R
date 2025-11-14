additionalInfo <- function(input, output, session,
                           region
                           ){
  
  ns <- session$ns
  
# Reactively show/hide info -----------------------------------------------
  observe({
    
    if(region() == "gbnvc"){
      shinyjs::show(id = "gbnvc_ai_div")
      shinyjs::hide(id = "mnnpc_ai_div")
    } else if(region() == "mnnpc") {
      shinyjs::show(id = "mnnpc_ai_div")
      shinyjs::hide(id = "gbnvc_ai_div")
    }
    
  }) |>
    bindEvent(region(),
              ignoreInit = FALSE)
  
}