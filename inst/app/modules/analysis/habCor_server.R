habCor <- function(input, output, session, setupData, vcAssignment, sidebar_options) {
  
  ns <- session$ns
  
# Retrieve sidebar options ------------------------------------------------
  habCorClass <- reactiveVal()
  
  observe({

    habCorClass(sidebar_options()$habCorClass)
    
  }) |>
    bindEvent(sidebar_options(), ignoreInit = TRUE)
  

# Retrieve setup data -----------------------------------------------------
  habitat_correspondences <- reactiveVal()
  unit_name_col <- reactiveVal()
  run_module <- reactiveVal()
  
  observe({
    
    habitat_correspondences(setupData()$habitat_correspondences)
    unit_name_col(setupData()$unit_name_col)
    run_module(setupData()$regional_module_availability$avgEIVs)
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE,
              ignoreNULL = TRUE)
  

# Create initial habitat correspondance table -----------------------------
  habCorData_init <- data.frame("VC.Code" = character(),
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
    
    shiny::req(vcAssignment())
    shiny::req(habitat_correspondences())
    shiny::req(isTRUE(run_module()))
    
    # Retrieve the table, optionally modify the table without triggering recursion.
    shiny::isolate({
      
      vcAssignment <- vcAssignment()
      habCorClass <- habCorClass()
      habitat_correspondences <- habitat_correspondences()
      unit_name_col <- unit_name_col()
      
    })
      
    topVCCommunities_df <- tibble::tibble(!!unit_name_col := vcAssignment$topVCCommunities)
    
    habCor <- topVCCommunities_df |>
      dplyr::left_join(habitat_correspondences, relationship = "many-to-many", by = unit_name_col)
    
    habCorTable <- habCor |>
      dplyr::filter(classification == habCorClass) |>
      dplyr::select("VC.Code" = unit_name_col, 
                    "Relationship" = "relationship_name", 
                    "Habitat" = "habitat") |>
      dplyr::distinct() |>
      dplyr::arrange(VC.Code)
    
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
                                            VC.Code = reactable::colDef(maxWidth = 150),
                                            Relationship = reactable::colDef(maxWidth = 300),
                                            Habitat = reactable::colDef(minWidth = 600)
                                          )
                                          )
      
      return(habCorTable)
      
    })
    
  }) |>
    bindEvent(vcAssignment(),
              habCorClass(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE
              )
  
  
  outputOptions(output, "habCorTable", suspendWhenHidden = FALSE)
  
  return(habCor_rval)
  
}
