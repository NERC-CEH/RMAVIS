nmDataInput <- function(input, output, session) {
  
  ns <- session$ns

# Initial survey table data -----------------------------------------------
  nmDataInput_init <- data.frame("Year" = as.integer(rep(as.numeric(format(Sys.Date(), "%Y")), 1)),
                                 "MinJanTemp" = as.double(0),
                                 "MaxJulyTemp" = as.double(0),
                                 "Moisture.F" = as.double(0),
                                 "Light.L" = as.double(0),
                                 "Nitrogen.N" = as.double(0),
                                 "Reaction.R" = as.double(0),
                                 "Salinity.S" = as.double(0),
                                 "Disturbance.D" = as.double(0))
  
# Survey Data Entry Table -------------------------------------------------
  output$nmDataInput <- rhandsontable::renderRHandsontable({
    
    nmDataInput <- rhandsontable::rhandsontable(data = nmDataInput_init,
                                                height = 600,
                                                rowHeaders = NULL,
                                                width = "100%"#,
                                                # overflow = "visible"
                                                # stretchH = "all"
    ) |>
      rhandsontable::hot_col(col = colnames(nmDataInput_init), halign = "htCenter") |>
      rhandsontable::hot_col(
        col = "Year",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "MinJanTemp",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "MaxJulyTemp",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "Moisture.F",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "Light.L",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "Nitrogen.N",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "Reaction.R",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "Salinity.S",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_col(
        col = "Disturbance.D",
        readOnly = FALSE,
        type = "numeric"
      ) |>
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, stretchH = "all") |>
      htmlwidgets::onRender("
        function(el, x) {
          var hot = this.hot
          $('a[data-value=\"nmDataInput_panel\"').on('click', function(){
            setTimeout(function() {hot.render();}, 0);
          })
        }")
    
    return(nmDataInput)
    
  })
    
  # Ensure table is created whilst hidden.
  outputOptions(output, "nmDataInput", suspendWhenHidden = FALSE)
  
  # return(nmDataInput_rval)

}
