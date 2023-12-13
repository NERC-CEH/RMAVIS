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
  
  habCorTable_rval <- reactiveVal(habCorData_init)
  
  output$habCorTable <- reactable::renderReactable({
    
    habCorTable <- reactable::reactable(data = habCorData_init,
                                        class = "my-tbl",
                                        # style = list(fontSize = "1rem"),
                                        rowClass = "my-row",
                                        defaultColDef = reactable::colDef(
                                          headerClass = "my-header",
                                          class = "my-col",
                                          align = "center" # Needed as alignment is not passing through to header
                                        )
                                        )
    
    return(habCorTable)
    
  })
  
  observe({
    
    # shiny::req(input$habCorTable)
    shiny::req(nvcAssignment())
    
    # # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      # Get all NVC communities and sub-communities from nvc assignment results
      NVC_communities_all <- nvcAssignment() |> # nvcAssignment()
        dplyr::pull(NVC.Code)
      
      # Get all NVC communities from community and sub-community codes
      NVC_communities_fromSubCom <- stringr::str_replace(string = NVC_communities_all, 
                                                         pattern = "(\\d)[^0-9]+$", 
                                                         replace = "\\1") |>
        unique()

      NVC_communities_final <- data.frame(
        "NVC.Code" = unique(c(NVC_communities_all, NVC_communities_fromSubCom))
        )
      
      habCorTable <- NVC_communities_final |>
        dplyr::left_join(all_habCor_final, relationship = "many-to-many", by = dplyr::join_by(NVC.Code)) |>
        dplyr::filter(Classification == habCorClass()) |>
        dplyr::select(NVC.Code, Relationship, Code, Label) |>
        dplyr::distinct() |>
        dplyr::arrange(NVC.Code)
      
      # print(habCorTable)

    })

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
                                          )
                                          )
      
      return(habCorTable)
      
    })
    
    habCorTable_rval(habCorTable)
    
  }) |>
    bindEvent(nvcAssignment(),
              habCorClass(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE
              )
  
  
  outputOptions(output, "habCorTable", suspendWhenHidden = FALSE)
  
  return(habCorTable_rval)
  
}
