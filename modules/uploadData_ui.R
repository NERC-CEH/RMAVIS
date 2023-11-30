uploadDataUI <- function(id) {
  
  ns <- NS(id)
  
  # Basic Inputs ------------------------------------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Survey Data Table"),
      
      shiny::fileInput(inputId = ns("uploadDataInput"),
                       label = "Browse",
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv"
                       )
      ),
      
      shiny::div(
        DT::DTOutput(outputId = ns("uploadDataTable"))
      )
      
      
    )
  )
  
}