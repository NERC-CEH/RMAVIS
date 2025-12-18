regionSelectUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    
    tags$head(
      tags$style(type = "text/css", "label{display: table-cell; text-align: center; vertical-align: middle;} .form-group{display: table-row;}")
    ),
    
    shiny::column(
      
      width = 12,
      
      shiny::div(
        
        shinyWidgets::slimSelectInput(inputId = ns("region"),
                                      label = paste0("🌍"),
                                      choices = RMAVIS:::region_options,
                                      selected = "mnnpc",
                                      search = FALSE,
                                      inline = TRUE,
                                      width = 250)
        
      )
    )
  )
  
}