dcaSubsetNVCUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,

      shiny::div(
        
        shiny::h5("Fixed Reference Space"),# |>
          
          # bslib::popover(
          #   bsicons::bs_icon("info-circle"),
          #   title = "Fixed Reference Space",
          #   paste0(""),
          #   placement = "bottom"
          # ),
        
        plotly::plotlyOutput(outputId = ns("dcaSubsetNVCPlot"), height = "600px")
        
      )

    )
  )
}
