uploadData <- function(input, output, session) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  # observe({
  #   
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  columnNames_correct <- reactiveVal()
  yearValues_numeric <- reactiveVal()
  speciesNames_correct <- reactiveVal()
  
  observe({
    
    uploaded_data_raw <- read.csv(input$uploadDataInput$datapath)
    
    output$uploadDataTable <- DT::renderDT({
      
      uploadDataTable <- DT::datatable(
        data = uploaded_data_raw,
        editable = FALSE
      )
      
      return(uploadDataTable)
      
    })
    
    # First check that the column names are correct
    
    columnNames_correct <- all(colnames(uploaded_data_raw) %in% c("Year", "Site", "Quadrat.Group", "Quadrat", "Species", "Cover"))
    columnNames_correct(columnNames_correct)
    
    output$columnNames_correct_expression <- shiny::renderText({
      
      paste0("Column Names Correct: ", as.character(columnNames_correct))
      
    })
    
    
    if(columnNames_correct == TRUE){
      
      yearValues_numeric <- all(is.numeric(uploaded_data_raw$Year))
      speciesNames_correct <- unique(uploaded_data_raw$Species) %in% speciesNames
      
      yearValues_numeric(yearValues_numeric)
      speciesNames_correct(speciesNames_correct)
      
      output$yearValues_numeric_expression <- shiny::renderText({
        
        paste0("Year Values Numeric: ", as.character(yearValues_numeric))
        
      })
      
      output$speciesNames_correct_expression <- shiny::renderText({
        
        paste0("Species Names Correct: ", as.character(speciesNames_correct))
        
      })
      
    }
    
    
  }) |>
    bindEvent(input$uploadDataInput,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

  
  
  # return()
  
}