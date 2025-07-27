floristicTablesUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    
    shiny::div(
      
      id = ns("singleComposedVsNVC_div"),
      
      shiny::fluidRow(
        
        shiny::column(
          
          width = 6,
          
          shiny::fluidRow(
            
            shiny::column(
              width = 12,
              shiny::htmlOutput(outputId = ns("composedFloristicTableTitle"))
            ),
            
            shiny::column(
              width = 12,
              shiny::htmlOutput(outputId = ns("samp_eivs_text_output"))
            )
            
          ),
          
          shiny::div(
            reactable::reactableOutput(outputId = ns("floristicTables_composed"))
          )
          
        ),
        shiny::column(
          
          width = 6,
          
          shiny::fluidRow(
            
            shiny::column(
              width = 12,
              shiny::htmlOutput(outputId = ns("nvcFloristicTableTitle"))
            ),
            
            shiny::column(
              width = 12,
              shiny::htmlOutput(outputId = ns("comp_eivs_text_output"))
            )
            
          ),
          
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