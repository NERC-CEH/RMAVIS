privacyUI <- function(id) {
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Privacy notice"),
      
      shiny::div(
        
        shiny::br(),
        
        shiny::markdown(
          "
          pseudoMAVIS does not collect any data at present, however please
          refer to the UKCEH ['Privacy notice'](https://www.ceh.ac.uk/privacy-notice)
          web page for UKCEH privacy policy.
          
          "
          )
      )
    )
  )
  
}