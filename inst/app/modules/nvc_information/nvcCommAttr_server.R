nvcCommAttr <- function(input, output, session, sidebar_options) {
  
  ns <- session$ns
  
  # Retrieve sidebar options ------------------------------------------------
  
  # observe({
  #   
  # }) |>
  #   bindEvent(sidebar_options(), ignoreInit = TRUE)
  
  # Initial survey table data -----------------------------------------------
  
  nvcNamesLookupLookupTable_init <- RMAVIS::nvc_community_namesCodes |>
    dplyr::select(NVC.Code, NVC.Name)
  
  # Survey Data Entry Table -------------------------------------------------
  
  nvcNamesLookupLookupTable_rval <- reactiveVal(nvcNamesLookupLookupTable_init)
  
  output$nvcNamesLookupLookupTable <- reactable::renderReactable({
    
    nvcNamesLookupLookupTable <- reactable::reactable(data = nvcNamesLookupLookupTable_init,
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
    
    return(nvcNamesLookupLookupTable)
    
  })
  
  
  outputOptions(output, "nvcNamesLookupLookupTable", suspendWhenHidden = FALSE)
  
  
  # return(nvcNamesLookupLookupTable_rval)
  
}
