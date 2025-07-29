newsUI <- function(id) {
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      
      width = 12,
      
      shiny::div(
        
        shiny::h1("News"),
        
        shiny::br(),
        
        shiny::markdown(
        '
        The latest versions of RMAVIS (v1.1.3, v1.1.2, v1.1.1, and v1.1.0) contain a number of breaking changes relative to the v1.0.1 release.
        
        First, is a major update to the taxonomy, which is now aligned with version 20250703a of the UKSI, available <a href="https://data.nhm.ac.uk/dataset/uk-species-inventory-simplified-copy/" target="_blank">here</a>.
        A csv of the taxonomic backbone can also be downloaded from the Data Entry section of RMAVIS.
        Second, is the recreation of the NVC datasets, which have been re-exported and re-organised from the original MS Access database and Floodplain Meadows datasets, and seperated into individual datasets (Original, Calthion, and Scottish Oceanic Wet Grassland)
        which can be selected independently.
        Third, is a reduction in the number of pseudo-quadrats used to match quadrats with the Jaccard similarity method,
        this has improved the speed at which quadrats are matched.
        Fourth, is the ability to optionally filter out taxa which occurred at a frequency of less than 5% of the survey plots (only applies when there are 20 or more plots in a group), 
        which is tuned on by default, but can be turned off.
        
        The re-export of the original tables has also resulted in a number of fixes to the previous set of tables used in RMAVIS, which did not include many tree species in the shrub layer.
        This change emphasises the need to add the correct (c), (s), and (g) suffixes for tree and shrub species, to ensure they are matched. 
        Please check the Floristic Tables and NVC Taxon Name Updates tabs in the NVC Information section to inspect how the strata suffixes appear in the floristic tables.
        Future work will separate out the strata into an additional data entry column, to ensure there is only one taxon name for each tree and shrub species.
        
        Together these changes mean that users results may differ slightly from previous versions.
        It also means that the taxon names in datasets previously entered into RMAVIS will need to be updated, to do this please download the Taxon Lookup object from the data entry section.
        
        Please see the Release Log notes below for a complete list of changes.
        
        Previous versions of RMAVIS can be accessed from 
        <a href="https://github.com/NERC-CEH/RMAVIS/releases" target="_blank">Github</a>
        or
        <a href="https://zenodo.org/search?q=parent.id%3A10818640&f=allversions%3Atrue&l=list&p=1&s=10&sort=version" target="_blank">Zenodo</a>.
        
        Special thanks again to Colin Conroy and Barry Jobson for testing v1.1.2, v1.1.1, and v1.1.0 of RMAVIS!
        '
        )
        
      ),
      
      shiny::div(
        
        shiny::hr()
        
      ),
      
      shiny::div(
        
        shiny::h1("Release Log"),
        
        shiny::br(),
        
        shiny::includeMarkdown(gsub(pattern = "#", replacement = "###", x = readLines("NEWS.md"))),
        
      )
    )
  )
  
}
