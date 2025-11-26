vcInformationUI <- function(id){
  
  ns <- NS(id)
  
  bslib::nav_panel(
    
    title = "VC Information",
    
    bslib::layout_sidebar(
      
      sidebar = vcInfoSidebarUI(id = "vcInfoSidebar_id_1"),
      
      bslib::navset_card_tab(
        
        id = ns("vcInfo_nct"),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "vcCommNamesLookup_panel",
          
          bslib::card_header("Community Names"),
          
          vcCommNamesLookupUI(id = "vcCommNamesLookup_id_1")
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "vcFlorTabs_panel",
          
          bslib::card_header("Floristic Tables"),
          
          vcFlorTabsUI(id = "vcFlorTabs_id_1")
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "vcTaxonNamesLookup_panel",
          
          bslib::card_header("Taxon Name Updates"),
          
          vcTaxonNamesLookupUI(id = "vcTaxonNamesLookup_id_1")
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "vcCommAttr_panel",
          
          bslib::card_header("Community Attributes"),
          
          vcCommAttrUI(id = "vcCommAttr_id_1")
          
        )
        
      )
      
    )
    
  )
  
}