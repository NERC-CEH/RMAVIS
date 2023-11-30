uploadData <- function(input, output, session) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  # observe({
  #   
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  
  observe({
    
    uploaded_data_raw <- read.csv(input$uploadDataInput$datapath)
    
    output$uploadDataTable <- DT::renderDT({
      
      uploadDataTable <- DT::datatable(
        data = uploaded_data_raw,
        editable = TRUE
      )
      
      return(uploadDataTable)
      
    })
    
  }) |>
    bindEvent(input$uploadDataInput,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

  
  
  # return()
  
}