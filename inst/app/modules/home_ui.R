homeUI <- function(id) {
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      
      width = 12,
      
      shiny::div(
        
        shiny::br(),
        
        shiny::includeMarkdown("README.md"),

        bslib::layout_columns(
          bslib::card_image(file = "www/ukceh_logo_long_720x170_rgb.png", fill = FALSE, fillable = FALSE, width = "300px"),
          bslib::card_image(file = "www/UK_SCAPE_Logo_Positive-350px.png", fill = FALSE, fillable = FALSE, width = "300px"),
          col_widths = c(2, 2, 8),
          class = shiny::tags$style("vertical-align: centre !important")
        )
        
      )
    )
  )
  
}