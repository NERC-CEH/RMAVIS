nvcAssignmentUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      # shiny::div(
      #   id = ns("nvcAssignmentSiteTable_div"),
      #   
      #   shiny::h5("Site Similarities"),
      #   
      #   shiny::div(
      #     reactable::reactableOutput(outputId = ns("nvcAssignmentSiteTable"))
      #   ),
      #   
      #   shiny::div(shiny::br())
      #   
      # ),
      # 
      # shiny::div(
      #   id = ns("nvcAssignmentGroupTable_div"),
      #   
      #   shiny::h5("Group Similarities"),
      #   
      #   shiny::div(
      #     reactable::reactableOutput(outputId = ns("nvcAssignmentGroupTable"))
      #   ),
      #   
      #   shiny::div(shiny::br())
      #   
      # ),
      
      shiny::div(
        id = ns("nvcAssignmentPlot_Jaccard_div"),
        
        shiny::h5("Quadrat Similarities"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("nvcAssignmentPlot_JaccardTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        id = ns("nvcAssignmentSiteTable_Czekanowski_div"),
        
        shiny::h5("Site Similarities"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("nvcAssignmentSiteTable_Czekanowski"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        id = ns("nvcAssignmentGroupTable_Czekanowski_div"),
        
        shiny::h5("Group Similarities"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("nvcAssignmentGroupTable_Czekanowski"))
        ),
        
        shiny::div(shiny::br())
        
      )
    )
  )
}
