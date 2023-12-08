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
  
  output$habCorTable <- rhandsontable::renderRHandsontable({
    
    habCorTable <- rhandsontable::rhandsontable(data = habCorData_init,
                                                rowHeaders = NULL,
                                                width = "100%"#,
                                                # overflow = "visible",
                                                # stretchH = "all"
                                                ) |>
      rhandsontable::hot_col(col = colnames(habCorData_init), halign = "htCenter", readOnly = TRUE) |>
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"habCor_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(habCorTable)
    
  })
  
  observe({
    
    shiny::req(input$habCorTable)
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
      
      habCorTable <- rhandsontable::rhandsontable(data = habCorTable,
                                                  rowHeaders = NULL#,
                                                  # overflow = "visible",
                                                  # stretchH = "all"
                                                  ) |>
        rhandsontable::hot_col(col = colnames(habCorTable), halign = "htCenter", readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) |>
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
        htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"habCor_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
      
      return(habCorTable)
      
    })
    
    habCorTable_rval(rhandsontable::hot_to_r(input$habCorTable))
    
  }) |>
    bindEvent(nvcAssignment(),
              habCorClass(), 
              ignoreInit = TRUE, 
              ignoreNULL = TRUE
              )
  
  
  outputOptions(output, "habCorTable", suspendWhenHidden = FALSE)
  
  return(habCorTable_rval)
  
}
