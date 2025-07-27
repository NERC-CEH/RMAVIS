additionalInfoUI <- function(id) {
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      
      shiny::h2("GBNVPD"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          If you use RMAVIS please consider submitting vegetation plot to the
          <a href="https://www.ceh.ac.uk/data/gbnvpd" target="_blank">GBNVPD</a> - 
          the National Vegetation Plot Database for Great Britain.
          '
        )
      ),
      
      shiny::br(),
      
      shiny::h2("UKSI"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          Version 1.1.0 of `RMAVIS` currently uses version 20250703a of the UKSI to form its taxonomic backbone.
          The UKSI can be accessed from several sources:
          * The versioned releases available via the NHM data portal (https://data.nhm.ac.uk/dataset/uk-species-inventory-simplified-copy)
          * The UKSI sandbox (https://uksi-sandbox.nhm.ac.uk/)
          * The NBN atlas species search (https://species.nbnatlas.org/)
          * The NHM UKSI website (https://www.nhm.ac.uk/our-science/data/uk-species/index)

          This backbone is also available as a lightweight R package - <a href="https://github.com/NERC-CEH/UKVegTB" target="_blank">UKVegTB</a>
          '
        )
      ),
      
      shiny::br(),
      
      shiny::h2("JNCC"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          The Joint Nature Conservation Committee (JNCC) website contains copies of the original NVC floristic tables and other information regarding the NVC.
          See the <a href="https://jncc.gov.uk/our-work/nvc/" target="_blank">JNCC website</a> for more details.
          '
          )
      ),
      
      shiny::br(),
      
      shiny::h2("EUNIS"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          The European Nature Information System (EUNIS) contains information on species, habitats, and designated nature conservation sites in Europe.
          See the <a href="https://eunis.eea.europa.eu/index.jsp" target="_blank">EUNIS website</a> for more details.
          '
        )
      ),
      
      shiny::br(),
      
      shiny::h2("FloraVeg.EU"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          FloraVeg.EU contains an array of detailed autoecological species information and
          an interface to explore the phytosociological <a href="https://doi.org/10.1111/avsc.12257" target="_blank">European Vegetation Classification</a>
          and the <a href="https://doi.org/10.1111/avsc.12519" target="_blank">EUNIS Habitat Classification</a>. 
          See the <a href="https://floraveg.eu/" target="_blank">FloraVeg.EU website</a> for more details.
          '
        )
      ),
      
      shiny::br(),
      
      shiny::h2("Floodplain Meadows Partnership"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          The <a href="https://floodplainmeadows.org.uk/" target="_blank">Floodplain Meadows Partnership</a> actively work to update the classifications of wet mesotrophic grassland communities.
          RMAVIS contains these updated communities as described in 
          the British and Irish Botany paper <a href="https://doi.org/10.33928/bib.2023.05.001" target="_blank">Phytosociology informs the conservation of species-rich meadows in hydrologically dynamic habitats: an example from British floodplains in a wider European context</a>,
          the Natural England Report <a href="https://publications.naturalengland.org.uk/publication/5839929072943104" target="_blank">A review of the National Vegetation Classification for the Calthion group of plant communities in England and Wales (JP021)</a>,
          and the British Wildlife article <a href="https://www.britishwildlife.com/article/article-volume-35-number-3-page-193-200/" target="_blank">An exploration of oceanic wet grasslands in the Scottish coastal lowlands</a>.
          '
        )
      ),
      
      shiny::br(),
      
      shiny::h2("IVC"),
      
      shiny::div(
        
        # shiny::br(),
        
        shiny::markdown(
          '
          The Irish Vegetation Classification (IVC) provides a modern, well-organised vegetation classification system for Ireland.
          See the <a href="https://biodiversityireland.ie/projects/ivc-classification-explorer/" target="_blank">website</a> for more details.
          '
        )
      )
      
      
      
    )
  )
  
}