releaseLogUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h1("Release Log"),
      
      shiny::markdown(
       "
       ---
       
       ## v1.0
       #### 2024/01/XX
       
       - First public release.
       - Modifications:
       - Fixes:
       - New Features:
       
       ---
       
       ## v0.9
       
       #### 2023/12/23
       
       - First release for user testing.
       - Features:
       
       
       ---
       
       
       
       "
      )
    )
  )
}