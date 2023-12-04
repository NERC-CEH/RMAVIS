uploadDataUI <- function(id) {
  
  ns <- NS(id)
  
  # Basic Inputs ------------------------------------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      bslib::layout_columns(
        
        col_widths = c(6, 6, 4),
        
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
                              multiple = FALSE),
        
        shiny::actionButton(inputId = ns("confirmUpload"), 
                            label = "Confirm Upload")
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::markdown(
        "
        MAVIS expects data to be uploaded in .csv format in one of two formats: long or wide, see below.
        
        "
      ),
      
      shiny::div(
        
        id = ns("long_description"),
        
        shiny::markdown(
          "
          Long data must be structured as a five-column dataframe with the following columns.
          
          - *Year:* Contains the year the quadrat was surveyed. Numeric.
          - *Group:* Contains the quadrat group. String.
          - *Quadrat:* Contains the quadrat ID. String.
          - *Species:* Contains the species, please download the list of accepted species names using the button above. String.
          - *Cover:* Contains the species-quadrat cover estimates, with values between 0 and 1. Numeric.
          
          Formatting checks are displayed below.
          
          "
        )
        
      ),
      
      shiny::div(
        
        id = ns("wide_description"),
        
        shiny::markdown(
          "
          Wide data must be structured with rows as quadrat IDs and columns as species names, with values as species-quadrat cover estimates, with values between 0 and 1.
          
          Once uploaded wide data will be converted into long format, with placeholder Year and Group values which the user may edit.
          
          Formatting checks are displayed below.
          
          "
        )
        
      ),
      
      # shiny::div(shiny::br()),
      
      shiny::div(
        
        shiny::htmlOutput(outputId = ns("columnNames_correct_expression")),
        shiny::htmlOutput(outputId = ns("yearValues_numeric_expression")),
        shiny::htmlOutput(outputId = ns("speciesNames_correct_expression"))
        
      ),
      
      shiny::div(shiny::hr()),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("uploadDataTable"), height = "300px")
      )
      
      
    )
  )
  
}
