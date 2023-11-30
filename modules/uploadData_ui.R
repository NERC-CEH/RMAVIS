uploadDataUI <- function(id) {
  
  ns <- NS(id)
  
  # Basic Inputs ------------------------------------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      bslib::layout_columns(
        
        col_widths = c(8, 4),
        
        shiny::fileInput(inputId = ns("uploadDataInput"),
                         label = "Browse",
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"
                         )
        )
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Uploaded Data"),
      
      shiny::markdown(
        "
        MAVIS expects data to be uploaded in .csv format with the following columns.
        
        - *Year:* Contains the year the quadrat was surveyed. Numeric.
        - *Site:* Contains the name of the site containing the quadrat. String.
        - *Quadrat.Group:* Contains the quadrat group. String.
        - *Quadrat:* Contains the quadrat ID. String.
        - *Species:* Contains the species, please download the list of accepted species names using the button above. String.
        - *Cover:* Contains the species-quadrat cover estimation, a value between 0 and 1. Numeric.
        
        Formatting issues are displayed below.
        
        "
      ),
      
      shiny::div(
        
        shiny::textOutput(outputId = ns("columnNames_correct_expression")),
        shiny::textOutput(outputId = ns("yearValues_numeric_expression")),
        shiny::textOutput(outputId = ns("speciesNames_correct_expression"))
        
      ),
      
      shiny::div(shiny::hr()),
      
      shiny::div(
        DT::DTOutput(outputId = ns("uploadDataTable"))
      )
      
      
    )
  )
  
}