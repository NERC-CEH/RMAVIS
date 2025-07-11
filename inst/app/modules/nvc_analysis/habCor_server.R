habCor <- function(input, output, session, nvcAssignment, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  habCorClass <- reactiveVal()

  observe({

    habCorClass(sidebar_options()$habCorClass)

  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  

# Create initial habitat correspondance table -----------------------------
  habCorData_init <- data.frame("NVC.Code" = character(),
                                "Relationship" = character(),
                                "Code" = character(),
                                "Label" = character()
                                )
  
  output$habCorTable <- reactable::renderReactable({
    
    habCorTable <- reactable::reactable(data = habCorData_init,
                                        filterable = FALSE,
                                        pagination = FALSE, 
                                        highlight = TRUE,
                                        bordered = TRUE,
                                        sortable = TRUE, 
                                        wrap = FALSE,
                                        resizable = TRUE,
                                        class = "my-tbl",
                                        # style = list(fontSize = "1rem"),
                                        rowClass = "my-row",
                                        defaultColDef = reactable::colDef(
                                          headerClass = "my-header",
                                          class = "my-col",
                                          align = "center" # Needed as alignment is not passing through to header
                                        ),
                                        columns = list(
                                          Label = reactable::colDef(minWidth = 700)
                                        )
                                        )
    
    return(habCorTable)
    
  })
  
  habCor_rval <- reactiveVal()
  
  observe({
    
    shiny::req(nvcAssignment())
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      nvcAssignment <- nvcAssignment()
      habCorClass <- habCorClass()
      
    })
      
    topNVCCommunities_df <- data.frame("nvc_code" = nvcAssignment$topNVCCommunities)
    
    habCor <- topNVCCommunities_df |>
      dplyr::left_join(RMAVIS::habitat_correspondences, relationship = "many-to-many", by = "nvc_code")
    
    habCorTable <- habCor |>
      dplyr::filter(classification == habCorClass) |>
      dplyr::select("NVC.Code" = "nvc_code", 
                    "Relationship" = "relationship_name", 
                    "Habitat" = "habitat") |>
      dplyr::distinct() |>
      dplyr::arrange(NVC.Code)
    
    habCor_rval(habCor)

    output$habCorTable <- rhandsontable::renderRHandsontable({
      
      habCorTable <- reactable::reactable(data = habCorTable,
                                          filterable = FALSE,
                                          pagination = FALSE, 
                                          highlight = TRUE,
                                          bordered = TRUE,
                                          sortable = TRUE, 
                                          wrap = FALSE,
                                          resizable = TRUE,
                                          class = "my-tbl",
                                          # style = list(fontSize = "1rem"),
                                          rowClass = "my-row",
                                          defaultColDef = reactable::colDef(
                                            headerClass = "my-header",
                                            class = "my-col",
                                            align = "center" # Needed as alignment is not passing through to header
                                          ),
                                          columns = list(
                                            NVC.Code = reactable::colDef(maxWidth = 150),
                                            Relationship = reactable::colDef(maxWidth = 300),
                                            Habitat = reactable::colDef(minWidth = 600)
                                          )
                                          )
      
      return(habCorTable)
      
    })
    
  }) |>
    bindEvent(nvcAssignment(),
              habCorClass(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE
              )
  
  
  outputOptions(output, "habCorTable", suspendWhenHidden = FALSE)
  
  return(habCor_rval)
  
}
