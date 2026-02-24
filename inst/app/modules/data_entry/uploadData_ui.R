uploadDataUI <- function(id, choices, selected) {
  
  ns <- NS(id)
  
# Basic Inputs ------------------------------------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      bslib::layout_columns(
        
        col_widths = c(4, 6, 4),
        
        shiny::selectizeInput(inputId = ns("dataEntryFormat"),
                              label = "Data Entry Format",
                              choices = choices, #RMAVIS:::dataEntryFormat_options[1:4],
                              selected = selected, #"long",
                              multiple = FALSE),
        
        shiny::fileInput(inputId = ns("uploadDataInput"),
                         label = "Browse",
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"
                         )
        ),
        
        shiny::actionButton(inputId = ns("confirmUpload"), 
                            label = "Confirm Upload",
                            disabled = TRUE)
        
      ),
      
      shiny::div(shiny::br()),
      
      shiny::markdown(
        "
        RMAVIS expects data to be uploaded as a .csv file in one of five formats.
        Three formats are available for all vegetation classification systems: Long, Wide, or Matrix.
        Two vegetation classification-specific formats are also available: 1) MAVIS, which imports data using the outputs of the original MAVIS desktop program and is available for the GB-NVC only;
        and 2) MNNPC Relevés which imports data in the format of the Minnesota Department of Natural Resources (DNR) relevé database and is available for the MNNPC only.
        Use the 'Data Entry Format' option to view the format required by each option, and select the format to upload.
        
        After changing the data entry format option please re-upload the file.
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
          - *Cover:* Contains the species-quadrat cover estimates, either empty, or one of four systems: Percentage, Proportional, Domin, Braun-Blanquet.
          
          To test this functionality with pre-formatted data take the following steps:
          - Select the 'Example' option.
          - Select a example data set.
          - Go to the 'Download Options' section and download the Survey Data.
          
          Example:
          "
          ),
          
          reactable::reactableOutput(outputId = ns("gbnvc_example_table_long"), height = "200px"),
          
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
          
          id = ns("wide_description"),
          
          shiny::markdown(
            "
          Wide data must be structured as a table with three ID columns:
          
          - *Year:* Contains the year the quadrat was surveyed. Integer.
          - *Group:* Contains the quadrat group. String.
          - *Quadrat:* Contains the quadrat ID. String.
          
          with additional columns for each species and values as cover estimates, one of four systems: Percentage, Proportional, Domin, Braun-Blanquet.
          
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
          Matrix data must be structured with rows as quadrat IDs and columns as species names, with values as species-quadrat cover estimates, one of four systems: Percentage, Proportional, Domin, Braun-Blanquet (5 point).
          
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
          
          - *Year:* Contains the year the quadrat/relevé was surveyed. Integer.
          - *Group:* Contains the quadrat/relevé group. String.
          - *Releve.Number:* Contains the quadrat/relevé ID. String.
          - *Phys.Code:* Contains the life form of the Taxon, one of: B (Broadleaved evergreen), C (Climber), D (Broadleaved deciduous), E (Needleleaf evergreen), F (Floating-leaved), G (Graminoid), H (Forb), K (Stem succulent), S (Submerged), or X (Epiphyte). String.
          - *Min.Ht:* Contains the minimum height of the taxon, one of eight categories: 1 (0m-0.1m), 2 (0.1m-0.5m), 3 (0.5m-2m), 4 (2m-5m), 5 (5m-10m), 6 (10m-20m), 7 (20m-35m), and 8 (35m-50m). String.
          - *Max.Ht:* Contains the maximum height of the taxon, one of eight categories: 1 (0m-0.1m), 2 (0.1m-0.5m), 3 (0.5m-2m), 4 (2m-5m), 5 (5m-10m), 6 (10m-20m), 7 (20m-35m), and 8 (35m-50m). String.
          - *Taxon:* Contains the taxon names, see the taxon names present in the taxon_name column of `MNNPC::mnnpc_taxa_lookup`. String.
          - *Cover:* Contains the cover values, either empty, or one of four systems: Percentage, Proportional, Domin, Braun-Blanquet (5 point).
          
          Example:
          "
          ),
          
          reactable::reactableOutput(outputId = ns("mnnpc_example_table"), height = "200px"),
          
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
