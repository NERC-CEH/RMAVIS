sidebarUI <- function(id){
  
  ns <- NS(id)
  
  bslib::sidebar(
    shiny::h5("Model Options"),
    shiny::hr(),
    shiny::actionButton(inputId = ns("runAnalysis"),
                        label = "Run Analysis"),
    shiny::hr(),
    shiny::selectizeInput(inputId = ns("dataEntryFormat"), 
                          label = "Data Entry Format", 
                          choices = dataEntryFormat_options, 
                          selected = "table", 
                          multiple = FALSE),
    shiny::selectizeInput(inputId = ns("coverMethod"), 
                          label = "Cover Method", 
                          choices = coverMethod_options, 
                          selected = "directPercentage", 
                          multiple = FALSE),
    shiny::selectizeInput(inputId = ns("habitatRestriction"), 
                          label = "Restrict Habitat", 
                          choices = habitatRestriction_options, 
                          selected = NA, 
                          multiple = TRUE),
    shiny::selectizeInput(inputId = ns("nTopResults"), 
                          label = "Number of Top Results", 
                          choices = c(5:10), 
                          selected = 5, 
                          multiple = FALSE),
    shiny::selectizeInput(inputId = ns("habCorClass"), 
                          label = "Habitat Correspondance Classification", 
                          choices = all_habCor_classifications, 
                          selected = "UKHab - Level5", 
                          multiple = FALSE)
  )
  
}