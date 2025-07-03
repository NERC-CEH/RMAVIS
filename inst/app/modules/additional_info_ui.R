additionalInfoUI <- function(id) {
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h1("GBNVPD"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          If you use RMAVIS please consider submitting your data to the
          <a href="https://www.ceh.ac.uk/data/gbnvpd" target="_blank">GBNVPD</a> - 
          the National Vegetation Plot Database for Great Britain.
          '
        )
      ),
      
      shiny::br(),
      
      shiny::h1("JNCC"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          The Joint Nature Conservation Committee (JNCC) website contains copies of the original and updated NVC floristic tables and other information regarding the NVC.
          See the <a href="https://jncc.gov.uk/our-work/nvc/" target="_blank">JNCC website</a> for more details.
          '
          )
      ),
      
      shiny::br(),
      
      shiny::h1("Floodplain Meadows Partnership"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          The <a href="https://floodplainmeadows.org.uk/" target="_blank">Floodplain Meadows Partnership</a> actively work to update the classifications of wet mesotrophic grassland communities.
          RMAVIS contains these updated communities as described in the Natural England Report <a href="https://publications.naturalengland.org.uk/publication/5839929072943104" target="_blank">A review of the National Vegetation Classification for the Calthion group of plant communities in England and Wales (JP021)</a>
          and the British Wildlife article <a href="https://www.britishwildlife.com/article/article-volume-35-number-3-page-193-200/" target="_blank">An exploration of oceanic wet grasslands in the Scottish coastal lowlands</a>.
          '
        )
      ),
      
      shiny::br(),
      
      shiny::h1("IVC"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          The Irish Vegetation Classification provides a modern, well-organised vegetation classification system for Ireland.
          See the <a href="https://biodiversityireland.ie/projects/ivc-classification-explorer/" target="_blank">website</a> for more details.
          '
        )
      )
      
      
      
    )
  )
  
}