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
        The latest version of RMAVIS (v1.1.0) contains a number of breaking changes.
        
        First, is a major update to the taxonomy, which is now aligned with version 20250703a of the UKSI, available <a href="https://data.nhm.ac.uk/dataset/uk-species-inventory-simplified-copy/" target="_blank">here</a>.
        A csv of the taxonomic backbone can also be downloaded from the Data Entry section of RMAVIS.
        Second, is the recreation of the NVC datasets, which have been re-exported and re-organised from the original MS Access database and Floodplain Meadows datasets, and seperated into individual datasets (Original, Calthion, and Scottish Oceanic Wet Grassland)
        which can be selected independently.
        Third, is a reduction in the number of pseudo-quadrats used to match quadrats with the Jaccard similarity method,
        this has improved the speed at which quadrats are matched.
        
        Together these changes mean that users results may differ slightly from previous versions.
        It also means that the taxon names in datasets previously entered into RMAVIS will need to be updated, to do this please download the Taxon Lookup object from the data entry section.
        
        Please see the Release Log notes below for a complete list of changes.
        
        Previous versions of RMAVIS can be accessed from 
        <a href="https://github.com/NERC-CEH/RMAVIS/releases" target="_blank">Github</a>
        or
        <a href="https://zenodo.org/search?q=parent.id%3A10818640&f=allversions%3Atrue&l=list&p=1&s=10&sort=version" target="_blank">Zenodo</a>.
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