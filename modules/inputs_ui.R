inputsUI <- function(id) {
  ns <- NS(id)

# Basic Inputs ------------------------------------------------------------
  # bslib::nav_panel(
  #   
  #   title = "Basic Inputs",
  #   value = "basic_inputs_panel",
    
    # shiny::h5("Survey Data Table Input")
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          rhandsontable::rHandsontableOutput(outputId = ns("surveyTable"))
          #, style = "margin-right: 5px !important;"
        )
      )
    )
  # )
}
