# Documentation -----------------------------------------------------------
documentationUI <- function(id) {
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::div(
        htmltools::tags$iframe(src = "documentation.html",
                               width = '100%',
                               height = "600px",
                               style = "border:none;")
        #, style = "margin-right: 5px !important;"
      )
    )
  )
}
