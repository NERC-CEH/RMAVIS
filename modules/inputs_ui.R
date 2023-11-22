inputsUI <- function(id) {
  ns <- NS(id)


  bslib::navset_card_pill(
# Basic Inputs ------------------------------------------------------------
    bslib::nav_panel(
      title = "Basic Inputs",
      value = "basic_inputs_panel",
      shiny::h5("Survey Data Table Input"),
      shiny::fluidRow(
        # bslib::layout_columns(
          shiny::selectizeInput(label = "Frequency Method", inputId = ns("freqMethod"), choices = freqMethod_options, selected = "directPercentage")
        # )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            rhandsontable::rHandsontableOutput(outputId = ns("surveyTable"))) # style = "margin-right: 5px !important;"
          )
        ),
      shiny::h5("Results"),
      shiny::h5("Habitat Correspondance")
      )
  )
}
