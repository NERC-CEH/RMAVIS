surveyDataUI <- function(id) {
  ns <- NS(id)

# Basic Inputs ------------------------------------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      # shiny::h5("Data Input Table"),
      
      shiny::div(
        
        bslib::layout_columns(
          
          col_widths = c(2, 10),
          
          shiny::h5("Data Input Table"),
          
          bslib::popover(
            bsicons::bs_icon("info-circle"),
            title = "Data Input Table",
            id = ns("dataInputTableInfo"),
            shiny::markdown(
            "
            Please note that changing the region resets this table.
            "
            ),
            placement = "bottom"
          )
          
        ),
        
        style = "align-items: center !important" # Not working
        
      ),

      # shiny::div(shiny::br()),
      
      shiny::div(
        rhandsontable::rHandsontableOutput(outputId = ns("surveyData"))
      )
    )
  )
  
}
