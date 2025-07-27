uploadData <- function(input, output, session) {
  
  ns <- session$ns


# Intialiase data validation objects --------------------------------------
  columnNames_correct <- reactiveVal()
  

# Show/Hide Long/Wide descriptions ----------------------------------------
  observe({
    
    if(input$dataEntryFormat == "long") {
      
      shinyjs::showElement(id = "long_description")
      shinyjs::hideElement(id = "wide_description")
      shinyjs::hideElement(id = "matrix_description")
      shinyjs::hideElement(id = "mavis_description")
      
    } else if(input$dataEntryFormat == "wide") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::showElement(id = "wide_description")
      shinyjs::hideElement(id = "matrix_description")
      shinyjs::hideElement(id = "mavis_description")
      
    } else if(input$dataEntryFormat == "matrix") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::hideElement(id = "wide_description")
      shinyjs::showElement(id = "matrix_description")
      shinyjs::hideElement(id = "mavis_description")
      
    } else if(input$dataEntryFormat == "mavis") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::hideElement(id = "wide_description")
      shinyjs::hideElement(id = "matrix_description")
      shinyjs::showElement(id = "mavis_description")
      
    }
    
  }) |>
    bindEvent(input$dataEntryFormat, 
              ignoreInit = FALSE)
  

# Initialise table --------------------------------------------------------
  
  uploadDataTable_rval <- reactiveVal()
  
  uploaded_data_init <- data.frame("Year" = integer(),
                                   "Group" = character(),
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
    
    if(input$dataEntryFormat == "long"){
      
      uploaded_data_raw <- read.csv(input$uploadDataInput$datapath)
      
    } else if(input$dataEntryFormat == "wide"){
      
      uploaded_data_raw <- read.csv(input$uploadDataInput$datapath, check.names = FALSE) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))|>
        tidyr::pivot_longer(cols = -c(Year, Group, Quadrat),
                            names_to = "Species",
                            values_to = "Cover",
                            values_transform = list(Cover = as.numeric)) |>
        dplyr::select(Year, Group, Quadrat, Species, Cover) |>
        dplyr::filter(!is.na(Cover))
      
    } else if(input$dataEntryFormat == "matrix"){
      
      uploaded_data_raw <- read.csv(input$uploadDataInput$datapath, check.names = FALSE, row.names = 1) |>
        tibble::rownames_to_column(var = "Quadrat")|>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))|>
        tidyr::pivot_longer(cols = -c(Quadrat),
                            names_to = "Species",
                            values_to = "Cover",
                            values_transform = list(Cover = as.numeric)) |> # as.numeric
        dplyr::mutate("Year" = as.integer(format(Sys.Date(), "%Y")),
                      "Group" = "A") |>
        dplyr::select(Year, Group, Quadrat, Species, Cover) |>
        dplyr::filter(!is.na(Cover))
      
    } else if(input$dataEntryFormat == "mavis"){
      
      uploaded_data_raw <- read_mavis_data(input$uploadDataInput$datapath)
      
    }
    
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
    columnNames_correct <- all(colnames(uploaded_data_raw) %in% c("Year", "Group", "Quadrat", "Species", "Cover"))
    columnNames_correct(columnNames_correct)
    
    output$columnNames_correct_expression <- shiny::renderText({
      
      paste0("Column Names Correct: ",
             ifelse(
               as.character(columnNames_correct) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(columnNames_correct), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(columnNames_correct), 
                     '</b></font>')
               )
             )
      
    })
    
  }) |>
    bindEvent(input$uploadDataInput,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  # Enable/Disable Confirm Upload -------------------------------------------
  observe({
    
    columnNames_correct <- columnNames_correct()
    
    if(columnNames_correct == TRUE){
      
      shinyjs::enable(id = "confirmUpload")
      
    } else if(columnNames_correct == FALSE){
      
      shinyjs::disable(id = "confirmUpload")
      
    }
    
  }) |>
    bindEvent(columnNames_correct(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  
  # Store Uploaded Data in Reactive Object ----------------------------------
  observe({
    
    uploadDataTable_rval(rhandsontable::hot_to_r(input$uploadDataTable))
    
  }) |>
    bindEvent(input$confirmUpload,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

  outputOptions(output, "uploadDataTable", suspendWhenHidden = FALSE)
  
  return(uploadDataTable_rval)
  
}
