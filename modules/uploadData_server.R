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
      
    } else if(input$dataEntryFormat == "wide") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::showElement(id = "wide_description")
      shinyjs::hideElement(id = "matrix_description")
      
    } else if(input$dataEntryFormat == "matrix") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::hideElement(id = "wide_description")
      shinyjs::showElement(id = "matrix_description")
      
    }
    
  }) |>
    bindEvent(input$dataEntryFormat, ignoreInit = FALSE)
  

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
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") #|>
      # rhandsontable::hot_validate_character(cols = "Species", choices = speciesNames, allowInvalid = FALSE)
    
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
                            values_transform = list(Cover = as.numeric)) |> # as.numeric
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
      
    }
    
    output$uploadDataTable <- rhandsontable::renderRHandsontable({
      
      uploadDataTable <- rhandsontable::rhandsontable(data = uploaded_data_raw,
                                                      rowHeaders = NULL,
                                                      width = "100%"
                                                      ) |>
        rhandsontable::hot_col(col = colnames(uploaded_data_raw), halign = "htCenter") |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") #|>
        # rhandsontable::hot_col(
        #   col = "Species",
        #   readOnly = FALSE,
        #   type = "dropdown",
        #   source = speciesNames,
        #   strict = FALSE
        # ) |>
        # rhandsontable::hot_validate_character(cols = "Species", choices = speciesNames, allowInvalid = FALSE) # speciesNames
      
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
              # input$dataEntryFormat,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  observe({
    
    uploadDataTable_rval(rhandsontable::hot_to_r(input$uploadDataTable))
    
    # shinyjs::click(id = "confirmUpload")
    
    # print(uploadDataTable_rval)
    
  }) |>
    bindEvent(input$confirmUpload,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)

  # outputOptions(output, "uploadDataTable", suspendWhenHidden = FALSE)
  
  return(uploadDataTable_rval)
  
}
