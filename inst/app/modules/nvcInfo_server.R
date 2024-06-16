nvcInfo <- function(input, output, session, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  
  # observe({
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  # Initial survey table data -----------------------------------------------
  
  nvcInfoLookupTable_init <- RMAVIS::nvc_community_namesCodes |>
    dplyr::select(NVC.Code, NVC.Name)
  
  # Survey Data Entry Table -------------------------------------------------
  
  nvcInfoLookupTable_rval <- reactiveVal(nvcInfoLookupTable_init)
  
  output$nvcInfoLookupTable <- reactable::renderReactable({
    
    nvcInfoLookupTable <- reactable::reactable(data = nvcInfoLookupTable_init,
                                               filterable = FALSE,
                                               pagination = FALSE, 
                                               highlight = TRUE,
                                               bordered = TRUE,
                                               sortable = FALSE, 
                                               wrap = FALSE,
                                               resizable = TRUE,
                                               style = list(fontSize = "1rem"),
                                               class = "my-tbl",
                                               # style = list(fontSize = "1rem"),
                                               rowClass = "my-row",
                                               defaultColDef = reactable::colDef(
                                                 headerClass = "my-header",
                                                 class = "my-col",
                                                 align = "center" # Needed as alignment is not passing through to header
                                               ),
                                               columns = list(
                                                 NVC.Code = reactable::colDef(
                                                   filterable = TRUE,
                                                   maxWidth = 150
                                                   )
                                                 )
                                               )
    
    return(nvcInfoLookupTable)
    
  })
  
  
  outputOptions(output, "nvcInfoLookupTable", suspendWhenHidden = FALSE)
  
  
  # return(nvcInfoLookupTable_rval)
  
}