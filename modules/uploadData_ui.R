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
        ),
        
        shiny::selectizeInput(inputId = ns("dataEntryFormat"),
                              label = "Data Entry Format",
                              choices = dataEntryFormat_options,
                              selected = "table",
                              multiple = FALSE)
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Uploaded Data"),
      
      shiny::markdown(
        "
        MAVIS expects data to be uploaded in .csv format in one of two formats: long or wide, see below.
        
        "
      ),
      
      bslib::nav_panel(
        
        "Main",
        
        bslib::navset_card_tab(
          
          bslib::nav_panel(
            
            full_screen = FALSE,
            
            bslib::card_header("Long Format"),
            
            
            shiny::markdown(
              "
              Long data must be structured as a six-column dataframe with the following columns.
              
              - *Year:* Contains the year the quadrat was surveyed. Numeric.
              - *Site:* Contains the name of the site containing the quadrat. String.
              - *Quadrat.Group:* Contains the quadrat group. String.
              - *Quadrat:* Contains the quadrat ID. String.
              - *Species:* Contains the species, please download the list of accepted species names using the button above. String.
              - *Cover:* Contains the species-quadrat cover estimates, with values between 0 and 1. Numeric.
              
              Formatting issues are displayed below.
              
              "
            )
            
          ),
          
          bslib::nav_panel(
            
            full_screen = FALSE,
            
            bslib::card_header("Wide Format"),
            
            
            shiny::markdown(
              "
              Wide data must be structured with rows as quadrat IDs and columns as species names, with values as species-quadrat cover estimates, with values between 0 and 1.
              
              Formatting issues are displayed below.
              
              "
              )
            )
          )
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        
        shiny::textOutput(outputId = ns("columnNames_correct_expression")),
        shiny::textOutput(outputId = ns("yearValues_numeric_expression")),
        shiny::textOutput(outputId = ns("speciesNames_correct_expression"))
        
      ),
      
      shiny::div(shiny::hr()),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("uploadDataTable"), height = "500px")
      )
      
      
    )
  )
  
}