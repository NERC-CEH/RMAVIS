additionalInfoUI <- function(id) {
  
  ns <- NS(id)
  
  shiny::fluidRow(
    
    shiny::div(
      
      id = ns("gbnvc_ai_div"),
      
      shiny::column(
        width = 12,
        
        shiny::h2("GBNVPD"),
        
        shiny::div(
          
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
          
          shiny::markdown(
          '
          Version 1.2.0 of `RMAVIS` currently uses version 20250703a of the UKSI to form its taxonomic backbone.
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
          
          shiny::markdown(
          '
          The Irish Vegetation Classification (IVC) provides a modern, well-organised vegetation classification system for Ireland.
          See the <a href="https://biodiversityireland.ie/projects/ivc-classification-explorer/" target="_blank">website</a> for more details.
          '
          )
        )
        
      )
      
      
    ),
    
    shiny::div(
      
      id = ns("mnnpc_ai_div"),
      
      shiny::column(
        width = 12,
        
        shiny::h2("MNNPC"),
        
        shiny::div(
          
          shiny::markdown(
          '
          The Minnesota Native Plant Classification was published in 2003, with two additional field guides released in 2005, 
          to provide a common language for vegetation conservation and management across Minnesota. It is used by multiple DNR divisions; 
          federal agencies such as the U.S. Forest Service, Natural Resources Conservation Service, National Park Service, and U.S. Fish and Wildlife Service; 
          county land departments; nonprofit organizations; educators; researchers; and environmental consultants. The current classification hierarchy aligns 
          with Minnesota’s Ecological Classification System (ECS) provinces and sections, allowing vegetation management issues to be assessed consistently 
          from broad landscapes down to individual stands. 
          
          The published MN NPC is based on relevé data collected from 1964 through the early 2000s, 
          replacing an older expert based descriptive system with an empirically derived one. A total of 5,523 plots were used in the original Field Guide development. 
          Since then, an additional 4,144 relevés have been collected, including data from ecological systems that were under represented or absent in the original analysis. 
          An update to the classification is currently underway, with a target completion date of 2027, and will incorporate these newer data to fill geographic gaps and 
          correct regional imbalances present in the original analysis.
          
          The MNNPC tool being developed for RMAVIS builds on the published classification but serves a different purpose. 
          The website presents the official, static classification, while the tool provides a data driven environment for exploring how new relevés, 
          updated analyses, and additional ecological systems relate to the existing framework. The tool supports tasks such as testing plot assignments, 
          examining fidelity and diversity metrics, and evaluating distinctions among similar communities. As new data continue to accumulate, 
          the tool will help inform future refinements such as improved field keys and clearer separation of ecotonal communities.

          ### Publications:
          
          Aaseng, N.E., Almendinger, J.C., Dana, R.P., Hanson, D.S., Lee, M.D., Rowe, E.R., Rusterholz, K.A. and Wovcha, D.S., 2011. Minnesota’s native plant community classification: A statewide classification of terrestrial and wetland vegetation based on numerical analysis of plot data. Biological Report, 108, pp.1-27. URL: Minnesota’s native plant community classification: A statewide classification of terrestrial and wetland vegetation based on numerical analysis of plot data.
          
          Minnesota Department of Natural Resources. 2003. Field guide to the native plant communities of Minnesota: The Laurentian Mixed Forest Province. Ecological Land Classification Program, Minnesota County Biological Survey, and Natural Heritage and Nongame Research Program, Minnesota Department of Natural Resources, St. Paul, MN, US.
          
          Minnesota Department of Natural Resources. 2005a. Field guide to the native plant communities of Minnesota: The Eastern Broadleaf Forest Province. Ecological Land Classification Program, Minnesota County Biological Survey, and Natural Heritage and Nongame Research Program, Minnesota Department of Natural Resources, St. Paul, MN, US.
          
          Minnesota Department of Natural Resources. 2005b. Field guide to the native plant communities of Minnesota: The Prairie Parkland and Tallgrass Aspen Parklands provinces. Ecological Land Classification Program, Minnesota County Biological Survey, and Natural Heritage and Nongame Research Program, Minnesota Department of Natural Resources, St. Paul, MN, US.
          
          Minnesota Department of Natural Resources. 2007. A handbook for collecting vegetation plot data in Minnesota: The relevé method. Minnesota County Biological Survey, Minnesota Natural Heritage and Nongame Research Program, and Ecological Land Classification Program, Biological Report 92. Minnesota Department of Natural Resources, St. Paul, MN, US. URL: http://www.dnr.state.mn.us/eco/mcbs/vegetation_sampling.html (accessed November 11, 2025).
          
          ### Terms of use
          
          To help highlight use of relevé data outside of the Minnesota Department of Natural Resources (DNR) and to protect the DNRs 
          copyright interest in these data, users are asked to acknowledge the DNRs Minnesota Biological Survey Program as the source 
          of information in any product derived from the relevé data, and to include in any product an appropriate copyright notice in 
          the name of the DNR (for example, Copyright 2026 State of Minnesota, Department of Natural Resources). Products include hard 
          copy or digital versions of studies, summaries, reports, maps, proposals, plans, analyses, interpretations, derivative works, 
          and compilations containing relevé data. If you have any questions about the data or its use, please contact:

          Daniel Wovcha
          500 Lafayette Rd., Box 25
          St. Paul, MN 55155
          
          (651) 259-5154
          Daniel.Wovcha@state.mn.us
          '
          )
          
        ),
        
        shiny::br(),
        
        shiny::h2("Ecological Classification System"),
        
        shiny::div(
          
          shiny::markdown(
          '
          The ecological land classification system: https://www.dnr.state.mn.us/ecs/index.html
          '
          )
          
        ),
        
        shiny::br(),
        
        shiny::h2("MNTaxa"),
        
        shiny::div(
          
          shiny::markdown(
          '
          The taxonomic backbone underpinning the MNNPC in RMAVIS is adapted from MNTaxa - The State of Minnesota Vascular Plant Checklist,
          which can be found on the Minnesota Department of Natural Resources website: https://www.dnr.state.mn.us/eco/mbs/plant-lists.html.
          '
          )
          
        ),
        
        shiny::br()
        
      )
      
    )
    
  )
  
}