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
        year and year-group are displayed in the 'Czekanowski.Similarities.Calculable?' column.

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
        Below the availability of data for each species is displayed.
        
        If there are no EIVs for a species that species does
        not contribute to the mean EIVs calculated for each
        Quadrat, Group, and Site; or the CCA scores found in the MVA section.
        
        If a species is not present in the VC Floristic tables it still
        contributes to (dis)similarity metrics used in the VC assignment process.
        
        If a species is not present in the phylogenetic tree is does not contribute
        to the calculation of phylogenetic diversity in the diversity section.
        "
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("speciesDataAvailabilitySummaryTable"))
      ),
      
      shiny::div(shiny::br()),
      
      shiny::div(
        reactable::reactableOutput(outputId = ns("speciesDataAvailabilityTable"))
      ),
      
      shiny::div(shiny::br())
      
    )
  )
}