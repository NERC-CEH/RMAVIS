surveyDataSummaryUI <- function(id){
  
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h5("Data Structure"),
      
      # shiny::div(shiny::br()),
      
      shiny::markdown(
        "
        NVC similarities for groups and years are not calculated when there are
        less than two sample plots per year-group and per year.
        Smaller sample sizes may also result in the failure of the composition of representative floristic tables.
        
        Below the number of quadrats per year, along with the number of quadrats per group and year are displayed.
        Whether similarity values will be calculated by `RMAVIS` for each
        year and year-group are displayed in the 'Similarities.Calculable?' column.

        "
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("quadratsPerYearTable"))
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("quadratsPerYearGroupTable"))
      ),
      
      shiny::div(shiny::br()),
      
      shiny::h5("Species Data Availability"),
      
      # shiny::div(shiny::br()),
      
      shiny::markdown(
        "
        Below the availability of data for each species is displayed,
        two datasets are currently used: Hill-Ellenberg values and the NVC Floristic
        tables.
        
        If there are no Hill-Ellenberg values for a species that species does
        not contribute to the mean Hill-Ellenberg values calculated for each
        Quadrat, Group, and Site; or the CCA scores found in the MVA section.
        
        If a species is not present in the NVC Floristic tables it still
        contributes to (dis)similarity metrics used in the NVC assignment process.
        "
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("speciesDataAvailabilityTable"))
      ),
      
      shiny::div(shiny::br())
      
    )
  )
}