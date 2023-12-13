nvcAssignmentUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Site Similarities"),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("nvcAssignmentSiteTable"))
      ),
      
      shiny::div(shiny::br()),

      shiny::h5("Group Similarities"),

      shiny::div(
        reactable::reactableOutput(outputId = ns("nvcAssignmentGroupTable"))
      ),

      shiny::div(shiny::br()),

      shiny::h5("Quadrat Similarities"),

      shiny::div(
        reactable::reactableOutput(outputId = ns("nvcAssignmentQuadratTable"))
      )
    )
  )
}
