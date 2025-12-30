uploadData <- function(input, output, session, setupData) {
  
  ns <- session$ns

# Retieve setup data ------------------------------------------------------
  region <- reactiveVal()
  
  observe({
    
    region(setupData()$region)
    
  }) |>
    shiny::bindEvent(setupData(),
                     ignoreInit = FALSE,
                     ignoreNULL = TRUE)
  
  
# Example table data ------------------------------------------------------
  mnnpc_example_data <- MNNPC::mnnpc_example_releve
  
  gbnvc_example_data_long <- RMAVIS::example_data$`Parsonage Down` |> 
    dplyr::select(-Site) |>
    dplyr::filter(Quadrat == "3N.1")
  
  gbnvc_example_data_wide <- gbnvc_example_data_long |>
    tidyr::pivot_wider(id_cols = c(Year, Group, Quadrat),
                       names_from = Species,
                       values_from = Cover)
  
  gbnvc_example_data_matrix <- gbnvc_example_data_wide |>
    tidyr::unite(" ", Year, Group, Quadrat)

# Create example data tables ----------------------------------------------
  output$gbnvc_example_table_long <- reactable::renderReactable({
    
    gbnvc_example_table_long <- reactable::reactable(data = gbnvc_example_data_long,
                                                     filterable = FALSE,
                                                     pagination = FALSE, 
                                                     highlight = TRUE,
                                                     bordered = TRUE,
                                                     sortable = FALSE, 
                                                     wrap = FALSE,
                                                     resizable = FALSE,
                                                     style = list(fontSize = "1rem"),
                                                     class = "my-tbl",
                                                     rowClass = "my-row",
                                                     defaultColDef = reactable::colDef(
                                                       headerClass = "my-header",
                                                       class = "my-col",
                                                       align = "center" # Needed as alignment is not passing through to header
                                                     ),
                                                     columns = list(
                                                       Year = reactable::colDef(format = reactable::colFormat(digits = 0)),
                                                       Group = reactable::colDef(),
                                                       Quadrat = reactable::colDef(),
                                                       Species = reactable::colDef(),
                                                       Cover = reactable::colDef(format = reactable::colFormat(digits = 0))
                                                     ))
    
    return(gbnvc_example_table_long)
    
  })
  
  output$gbnvc_example_table_wide <- reactable::renderReactable({
    
    gbnvc_example_table_wide <- reactable::reactable(data = gbnvc_example_data_wide,
                                                     filterable = FALSE,
                                                     pagination = FALSE, 
                                                     highlight = TRUE,
                                                     bordered = TRUE,
                                                     sortable = FALSE, 
                                                     wrap = FALSE,
                                                     resizable = FALSE,
                                                     style = list(fontSize = "1rem"),
                                                     class = "my-tbl",
                                                     rowClass = "my-row",
                                                     defaultColDef = reactable::colDef(
                                                       headerClass = "my-header",
                                                       class = "my-col",
                                                       align = "center" # Needed as alignment is not passing through to header
                                                     ),
                                                     columns = list(
                                                       Year = reactable::colDef(format = reactable::colFormat(digits = 0)),
                                                       Group = reactable::colDef(),
                                                       Quadrat = reactable::colDef()
                                                     ))
    
    return(gbnvc_example_table_wide)
    
  })
  
  output$gbnvc_example_table_matrix <- reactable::renderReactable({
    
    gbnvc_example_table_matrix <- reactable::reactable(data = gbnvc_example_data_matrix,
                                                       filterable = FALSE,
                                                       pagination = FALSE, 
                                                       highlight = TRUE,
                                                       bordered = TRUE,
                                                       sortable = FALSE, 
                                                       wrap = FALSE,
                                                       resizable = FALSE,
                                                       style = list(fontSize = "1rem"),
                                                       class = "my-tbl",
                                                       rowClass = "my-row",
                                                       defaultColDef = reactable::colDef(
                                                         headerClass = "my-header",
                                                         class = "my-col",
                                                         align = "center" # Needed as alignment is not passing through to header
                                                       ),
                                                       columns = list(
                                                         ` ` = reactable::colDef(minWidth = 225)
                                                       ))
    
    return(gbnvc_example_table_matrix)
    
  })
  
  output$mnnpc_example_table <- reactable::renderReactable({
    
    mnnpc_example_table <- reactable::reactable(data = mnnpc_example_data,
                                                filterable = FALSE,
                                                pagination = FALSE, 
                                                highlight = TRUE,
                                                bordered = TRUE,
                                                sortable = FALSE, 
                                                wrap = FALSE,
                                                resizable = FALSE,
                                                style = list(fontSize = "1rem"),
                                                class = "my-tbl",
                                                rowClass = "my-row",
                                                defaultColDef = reactable::colDef(
                                                  format = reactable::colFormat(digits = 2),
                                                  headerClass = "my-header",
                                                  class = "my-col",
                                                  align = "center" # Needed as alignment is not passing through to header
                                                ),
                                                columns = list(
                                                  year = reactable::colDef(format = reactable::colFormat(digits = 0)),
                                                  group = reactable::colDef(),
                                                  relnumb = reactable::colDef(format = reactable::colFormat(digits = 0)),
                                                  physcode = reactable::colDef(),
                                                  minht = reactable::colDef(format = reactable::colFormat(digits = 0)),
                                                  maxht = reactable::colDef(format = reactable::colFormat(digits = 0)),
                                                  taxon = reactable::colDef(),
                                                  scov = reactable::colDef(format = reactable::colFormat(digits = 0))
                                                ))
    
    return(mnnpc_example_table)
    
  })
  

# Intialiase data validation objects --------------------------------------
  columnNames_raw_correct <- reactiveVal(FALSE)
  columnNames_prepped_correct <- reactiveVal(FALSE)
  

# Show/Hide Long/Wide descriptions ----------------------------------------
  observe({
    
    if(input$dataEntryFormat == "long") {
      
      shinyjs::showElement(id = "long_description")
      shinyjs::hideElement(id = "wide_description")
      shinyjs::hideElement(id = "matrix_description")
      shinyjs::hideElement(id = "mavis_description")
      shinyjs::hideElement(id = "mnnpc_description")
      
    } else if(input$dataEntryFormat == "wide") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::showElement(id = "wide_description")
      shinyjs::hideElement(id = "matrix_description")
      shinyjs::hideElement(id = "mavis_description")
      shinyjs::hideElement(id = "mnnpc_description")
      
    } else if(input$dataEntryFormat == "matrix") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::hideElement(id = "wide_description")
      shinyjs::showElement(id = "matrix_description")
      shinyjs::hideElement(id = "mavis_description")
      shinyjs::hideElement(id = "mnnpc_description")
      
    } else if(input$dataEntryFormat == "mavis") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::hideElement(id = "wide_description")
      shinyjs::hideElement(id = "matrix_description")
      shinyjs::showElement(id = "mavis_description")
      shinyjs::hideElement(id = "mnnpc_description")
      
    } else if(input$dataEntryFormat == "mnnpc_releves") {
      
      shinyjs::hideElement(id = "long_description")
      shinyjs::hideElement(id = "wide_description")
      shinyjs::hideElement(id = "matrix_description")
      shinyjs::hideElement(id = "mavis_description")
      shinyjs::showElement(id = "mnnpc_description")
      
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

# Read and wrangle uploaded data ------------------------------------------
  observe({
    
    shiny::isolate({
      
      region <- region()
      
    })
    
    # Based on the data entry method read uploaded data and check pre and post
    # preparation format.
    
    if(input$dataEntryFormat == "long"){
      
      uploaded_data_raw <- read.csv(input$uploadDataInput$datapath, check.names = FALSE)
      
      if(setequal(colnames(uploaded_data_raw), c("Year", "Group", "Quadrat", "Species", "Cover"))){
        
        uploaded_data_prepped <- uploaded_data_raw
        
        columnNames_raw_correct(TRUE)
        
      } else {
        
        uploaded_data_prepped <- NULL
        columnNames_raw_correct(FALSE)
        
      }
      
    } else if(input$dataEntryFormat == "wide"){
      
      uploaded_data_raw <- read.csv(input$uploadDataInput$datapath, check.names = FALSE)
      
      if(all(c("Year", "Group", "Quadrat") %in% colnames(uploaded_data_raw) & !(c("Species", "Cover") %in% colnames(uploaded_data_raw)))){
        
        uploaded_data_prepped <- uploaded_data_raw |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))|>
          tidyr::pivot_longer(cols = -c(Year, Group, Quadrat),
                              names_to = "Species",
                              values_to = "Cover",
                              values_transform = list(Cover = as.numeric)) |>
          dplyr::select(Year, Group, Quadrat, Species, Cover) |>
          dplyr::filter(!is.na(Cover))
        
        columnNames_raw_correct(TRUE)
        
      } else {
        
        uploaded_data_prepped <- NULL
        columnNames_raw_correct(FALSE)
        
      }
      
    } else if(input$dataEntryFormat == "matrix"){
      
      uploaded_data_raw <- read.csv(input$uploadDataInput$datapath, check.names = FALSE)
      
      if(!(any(c("Year", "Group", "Quadrat") %in% colnames(uploaded_data_raw)))){
        
        uploaded_data_prepped <- uploaded_data_raw |>
          dplyr::rename("Quadrat" = 1) |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
          tidyr::pivot_longer(cols = -c(Quadrat),
                              names_to = "Species",
                              values_to = "Cover",
                              values_transform = list(Cover = as.numeric)) |>
          dplyr::mutate("Year" = as.integer(format(Sys.Date(), "%Y")),
                        "Group" = "A") |>
          dplyr::select(Year, Group, Quadrat, Species, Cover) |>
          dplyr::filter(!is.na(Cover))
        
        columnNames_raw_correct(TRUE)
        
      } else {
        
        uploaded_data_prepped <- NULL
        columnNames_raw_correct(FALSE)
        
      }
      
    } else if(input$dataEntryFormat == "mavis"){
      
      if(region == "gbnvc"){
        
        uploaded_data_prepped <- read_mavis_data(input$uploadDataInput$datapath)
        
        columnNames_raw_correct(TRUE)
        
      } else {
        
        uploaded_data_prepped <- NULL
        columnNames_raw_correct(FALSE)
        
      }
      
    } else if(input$dataEntryFormat == "mnnpc_releves"){
      
      if(region == "mnnpc"){
        
        uploaded_data_raw <- read.csv(input$uploadDataInput$datapath, check.names = FALSE)
        
        if(setequal(colnames(uploaded_data_raw), c("year", "group", "relnumb", "physcode", "minht", "maxht", "taxon", "scov"))){
          
          uploaded_data_prepped <- uploaded_data_raw |>
            MNNPC::process_dnr_releves(process_malformed_data = TRUE,
                                       strip_suffixes = FALSE,
                                       match_to_accepted = FALSE,
                                       aggregate_into_analysis_groups = FALSE) |>
            suppressMessages() |>
            suppressWarnings()
          
          columnNames_raw_correct(TRUE)
          
        } else {
          
          uploaded_data_prepped <- NULL
          columnNames_raw_correct(FALSE)
          
        }
        
        uploaded_data_prepped <- NULL
        columnNames_raw_correct(FALSE)
        
      }
      
      uploaded_data_prepped <- NULL
      columnNames_raw_correct(FALSE)
      
    }
    
    if(!is.null(uploaded_data_prepped)){
      
      output$uploadDataTable <- rhandsontable::renderRHandsontable({
        
        uploadDataTable <- rhandsontable::rhandsontable(data = uploaded_data_prepped,
                                                        rowHeaders = NULL,
                                                        width = "100%"
        ) |>
          rhandsontable::hot_col(col = colnames(uploaded_data_prepped), halign = "htCenter") |>
          rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all")
        
        return(uploadDataTable)
        
      })
      
      # Check that the prepped column names are correct
      columnNames_prepped_correct <- all(colnames(uploaded_data_prepped) %in% c("Year", "Group", "Quadrat", "Species", "Cover"))
      columnNames_prepped_correct(columnNames_prepped_correct)
      
    } else {
      
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
      
      columnNames_prepped_correct(FALSE)
      
    }
    
    output$columnNames_raw_correct_expression <- shiny::renderText({
      
      paste0("Column Names Correct in Uploaded Data: ",
             ifelse(
               as.character(columnNames_raw_correct()) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(columnNames_raw_correct()), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(columnNames_raw_correct()), 
                     '</b></font>')
               )
             )
      
    })
    
    output$columnNames_prepped_correct_expression <- shiny::renderText({
      
      paste0("Column Names Correct in Prepared Data: ",
             ifelse(
               as.character(columnNames_prepped_correct()) == TRUE,
               paste('<font color="green"><b>', 
                     as.character(columnNames_prepped_correct()), 
                     '</b></font>'),
               paste('<font color="red"><b>', 
                     as.character(columnNames_prepped_correct()), 
                     '</b></font>')
             )
      )
      
    })
    
  }) |>
    bindEvent(input$uploadDataInput,
              # input$dataEntryFormat,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
  # Enable/Disable Confirm Upload -------------------------------------------
  observe({
    
    columnNames_prepped_correct <- columnNames_prepped_correct()
    
    if(columnNames_prepped_correct == TRUE){
      
      shinyjs::enable(id = "confirmUpload")
      
    } else if(columnNames_prepped_correct == FALSE){
      
      shinyjs::disable(id = "confirmUpload")
      
    }
    
  }) |>
    bindEvent(columnNames_prepped_correct(),
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
