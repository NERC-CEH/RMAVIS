vcAssignmentUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      # shiny::div(
      #   id = ns("vcAssignmentSiteTable_div"),
      #   
      #   shiny::h5("Site Similarities"),
      #   
      #   shiny::div(
      #     reactable::reactableOutput(outputId = ns("vcAssignmentSiteTable"))
      #   ),
      #   
      #   shiny::div(shiny::br())
      #   
      # ),
      # 
      # shiny::div(
      #   id = ns("vcAssignmentGroupTable_div"),
      #   
      #   shiny::h5("Group Similarities"),
      #   
      #   shiny::div(
      #     reactable::reactableOutput(outputId = ns("vcAssignmentGroupTable"))
      #   ),
      #   
      #   shiny::div(shiny::br())
      #   
      # ),
      
      shiny::div(
        id = ns("vcAssignmentPlot_Jaccard_div"),
        
        shiny::h5("Quadrat Similarities"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("vcAssignmentPlot_JaccardTable"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        id = ns("vcAssignmentSiteTable_Czekanowski_div"),
        
        shiny::h5("Site Similarities"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("vcAssignmentSiteTable_Czekanowski"))
        ),
        
        shiny::div(shiny::br())
        
      ),
      
      shiny::div(
        id = ns("vcAssignmentGroupTable_Czekanowski_div"),
        
        shiny::h5("Group Similarities"),
        
        shiny::div(
          reactable::reactableOutput(outputId = ns("vcAssignmentGroupTable_Czekanowski"))
        ),
        
        shiny::div(shiny::br())
        
      )
    )
  )
}
