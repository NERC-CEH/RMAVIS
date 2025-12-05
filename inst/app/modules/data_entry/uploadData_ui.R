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
                              choices = RMAVIS:::dataEntryFormat_options,
                              selected = "long",
                              multiple = FALSE),
        
        shiny::actionButton(inputId = ns("confirmUpload"), 
                            label = "Confirm Upload",
                            disabled = TRUE)
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::markdown(
        "
        RMAVIS expects data to be uploaded as a .csv file in one of three formats: long, wide, or matrix.
        Use the 'Data Entry Format' option to view the format required by each option, and select the format to upload.
        
        "
      ),
      
      shiny::div(shiny::hr()),
      
      shinyjs::hidden(
        
        shiny::div(
          
          id = ns("long_description"),
          
          shiny::markdown(
          "
          Long data must be structured as a five-column table with the following columns.
          
          - *Year:* Contains the year the quadrat was surveyed. Integer.
          - *Group:* Contains the quadrat group. String.
          - *Quadrat:* Contains the quadrat ID. String.
          - *Species:* Contains the species. String.
          - *Cover:* Contains the species-quadrat cover estimates, with values between 0 and 1. Double.
          
          Example:
          "
          ),
          
          reactable::reactableOutput(outputId = ns("gbnvc_example_table_long"), height = "200px"),
          
          shiny::div(shiny::br()),
          
          shiny::markdown(
          "
          
          Formatting checks are displayed below.
          
          To test this functionality with pre-formatted data take the following steps:
          - Select the 'Example' option.
          - Select a example data set.
          - Go to the 'Download Options' section and download the Survey Data.
          
          "
          ),
          
        )
        
      ),
      
      shinyjs::hidden(
        
        shiny::div(
          
          id = ns("wide_description"),
          
          shiny::markdown(
            "
          Wide data must be structured as a table with three ID columns:
          
          - *Year:* Contains the year the quadrat was surveyed. Integer.
          - *Group:* Contains the quadrat group. String.
          - *Quadrat:* Contains the quadrat ID. String.
          
          with additional columns for each species and values as cover estimates (values between 0 and 1) or simply 1 to indicate the species presence.
          
          Once uploaded wide data will be converted into long format.
          
          Example:
          "
          ),
          
          reactable::reactableOutput(outputId = ns("gbnvc_example_table_wide")),
          
          shiny::div(shiny::br()),
          
          shiny::markdown(
          "
          Formatting checks are displayed below.
          
          "
          ),
          
        )
        
      ),
      
      shinyjs::hidden(
        
        shiny::div(
          
          id = ns("matrix_description"),
          
          shiny::markdown(
          "
          Matrix data must be structured with rows as quadrat IDs and columns as species names, with values as species-quadrat cover estimates, with values between 0 and 1, or simply 1 to indicate species presence.
          
          Once uploaded wide data will be converted into long format, with placeholder Year and Group values which the user may edit.
          
          Example:
          "
          ),
          
          reactable::reactableOutput(outputId = ns("gbnvc_example_table_matrix")),
          
          shiny::div(shiny::br()),
          
          shiny::markdown(
            "
          Formatting checks are displayed below.
          
          "
          ),
          
        )
        
      ),
      
      shinyjs::hidden(
        
        shiny::div(
          
          id = ns("mavis_description"),
          
          shiny::markdown(
          "
          Upload data saved from the Windows MAVIS application.
          
          Formatting checks are displayed below.
          
          "
          )
          
        )
        
      ),
      
      shinyjs::hidden(
        
        shiny::div(
          
          id = ns("mnnpc_description"),
          
          shiny::markdown(
          "
          Upload data in the format of the Minnesota Department of Natural Resources (DNR) relevé database.
          
          The data should be in long format and contain seven columns: year, group, relnumb, physcode, minht, maxht, taxon, and scov.
          
          - *year:* Contains the year the quadrat/relevé was surveyed. Integer.
          - *group:* Contains the quadrat/relevé group. String.
          - *relnumb:* Contains the quadrat/relevé ID. String.
          - *physcode:* Contains the ... . String.
          - *minht:* Contains the ... . String.
          - *maxht:* Contains the ... . String.
          - *taxon:* Contains the ... . String.
          - *scov:* Contains the... . String.
          
          Example:
          "
          ),
          
          reactable::reactableOutput(outputId = ns("mnnpc_example_table")),
          
          shiny::div(shiny::br()),
          
          shiny::markdown(
          "
          Formatting checks are displayed below.
          
          "
          ),
          
        )
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        
        shiny::fluidRow(
          
          shiny::htmlOutput(outputId = ns("columnNames_raw_correct_expression"), inline = TRUE),
          shiny::htmlOutput(outputId = ns("columnNames_prepped_correct_expression"), inline = TRUE)
          
        )
        
      ),
      
      shiny::div(shiny::hr()),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("uploadDataTable"), height = "300px")
      )
      
      
    )
  )
  
}
