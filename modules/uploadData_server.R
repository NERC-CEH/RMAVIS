uploadData <- function(input, output, session) {
  
  ns <- session$ns


# Intialiase data validation objects --------------------------------------
  columnNames_correct <- reactiveVal()
  yearValues_numeric <- reactiveVal()
  speciesNames_correct <- reactiveVal()
  

# Initialise table --------------------------------------------------------
  uploaded_data_init <- data.frame("Year" = character(),
                                   "Site" = character(),
                                   "Quadrat.Group" = character(),
                                   "Quadrat" = character(),
                                   "Species" = character(),
                                   "Cover" = numeric())
  
  output$uploadDataTable <- rhandsontable::renderRHandsontable({
    
    uploadDataTable <- rhandsontable::rhandsontable(data = uploaded_data_init,
                                                    rowHeaders = NULL,
                                                    width = "100%"
    ) |>
      rhandsontable::hot_col(col = colnames(uploaded_data_init), halign = "htCenter") |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
    
    return(uploadDataTable)
    
  })
  
  observe({
    
    # shiny::req(input$uploadDataTable)
    
    uploaded_data_raw <- read.csv(input$uploadDataInput$datapath)
    
    output$uploadDataTable <- rhandsontable::renderRHandsontable({
      
      uploadDataTable <- rhandsontable::rhandsontable(data = uploaded_data_raw,
                                                      rowHeaders = NULL,
                                                      width = "100%"
                                                      ) |>
        rhandsontable::hot_col(col = colnames(uploaded_data_raw), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
      
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

  # outputOptions(output, "uploadDataTable", suspendWhenHidden = FALSE)
  
  # return()
  
}