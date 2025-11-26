vcInformation <- function(input, output, session,
                          region,
                          setupData
                          ){
  
  ns <- session$ns
  
# Reactively show/hide tabs -----------------------------------------------
  observe({
    
    show_taxonNameUpdates <- setupData()$regional_module_availability$taxonNameUpdates
    
    if(isTRUE(show_taxonNameUpdates)){
      shiny::showTab(inputId = "vcInfo_nct", target = "vcTaxonNamesLookup_panel")
    } else {
      shiny::hideTab(inputId = "vcInfo_nct", target = "vcTaxonNamesLookup_panel")
    }
    
  }) |>
    bindEvent(setupData(),
              ignoreInit = FALSE)
  

# Call submodules ---------------------------------------------------------
  vcCommNamesLookup <- shiny::callModule(module = vcCommNamesLookup,
                                         region = region,
                                         id = "vcCommNamesLookup_id_1")
  
  shiny::callModule(module = vcTaxonNamesLookup,
                    region = region,
                    id = "vcTaxonNamesLookup_id_1")
  
  vcFlorTabs <- shiny::callModule(module = vcFlorTabs,
                                  region = region,
                                  id = "vcFlorTabs_id_1")
  
  vcCommAttr <- shiny::callModule(module = vcCommAttr,
                                  region = region,
                                  id = "vcCommAttr_id_1")
  
  shiny::callModule(module = vcInfoSidebar,
                    id = "vcInfoSidebar_id_1",
                    region = region,
                    vcCommNamesLookup = vcCommNamesLookup, 
                    vcFlorTabs = vcFlorTabs, 
                    vcCommAttr = vcCommAttr)
  
}