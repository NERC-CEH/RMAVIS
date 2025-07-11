floristicTablesUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    
    shiny::div(
      
      id = ns("singleComposedVsNVC_div"),
      
      shiny::fluidRow(
        
        shiny::column(
          width = 6,
          
          # shiny::h5("Composed Floristic Table"),
          shiny::htmlOutput(outputId = ns("composedFloristicTableTitle")),
          
          shiny::div(
            reactable::reactableOutput(outputId = ns("floristicTables_composed"))
          )
          
        ),
        shiny::column(
          width = 6,
          
          # shiny::h5("NVC Floristic Table"),
          shiny::htmlOutput(outputId = ns("nvcFloristicTableTitle")),
          
          shiny::div(
            reactable::reactableOutput(outputId = ns("floristicTables_nvc"))
          )
          
        )
        
      )
      
    ),
    
    shiny::div(
      
      id = ns("multipleComposed_div"),
      
      shiny::column(
        width = 12,
        
        shiny::htmlOutput(outputId = ns("floristicTablesWide_composedTitle")),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("floristicTablesWide_composed"))
        )
        
      )
      
      
    )
    
  )
    
}