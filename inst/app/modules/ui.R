ui <- function(id){
  
  bslib::page_navbar(
    
    shinyjs::useShinyjs(),
    
    # Note using bslib::layout_columns() leads to additional bslib text in the tab title
    title = shiny::div(
      
      shiny::splitLayout(
        
        # App Title
        bslib::card_body(shiny::h1("RMAVIS"), padding = c(9, 0, 0, 0), class = "color: black !important;"),
        
        # Logo
        bslib::card_image(file = "www/UKCEH_Logo_Master_Black.png", fill = FALSE, width = "140px"),
        shinyjs::hidden(shiny::div(bslib::card_image(file = "www/DNR_Logo_RGB.png", fill = FALSE, width = "260px"), id = "mnnpc_logo_div")),
        
        # Cell widths
        cellWidths = c(140, 150, 270),
        
        # Cell args
        cellArgs = list(style = "vertical-align: middle !important; padding-right: 0px !important;")
        
      ),
      
      # Tab Title
      tags$head(tags$title(paste0(" | UK Centre for Ecology & Hydrology")),
                tags$link(rel = "shortcut icon", href = "https://brandroom.ceh.ac.uk/themes/custom/ceh/favicon.ico")
      )
      
    ),
    
    id = "nav",
    
    tags$head(includeCSS("www/style.css")),
    
    bslib::nav_spacer(),
    
    bslib::nav_panel(
      
      title = "Home",
      
      homeUI(id = "home_id_1")
      
    ),
    
    bslib::nav_panel(
      
      title = "Data Entry",
      
      bslib::layout_sidebar(
        
        sidebar = deSidebarUI(id = "deSidebar_id_1"),
        
        bslib::navset_card_tab(
          
          bslib::nav_panel(
            
            full_screen = TRUE,
            
            bslib::card_header("Data Input"),
            
            value = "surveyData_panel",
            
            surveyDataUI(id = "surveyData_id_1")
            
          ),
          
          bslib::nav_panel(
            
            full_screen = TRUE,
            
            bslib::card_header("Data Structure"),
            
            value = "dataStructure_panel",
            
            surveyDataSummaryUI(id = "surveyDataSummary_id_1")
            
          ),
          
          bslib::nav_panel(
            
            full_screen = TRUE,
            
            value = "taxonomicBackbone_panel",
            
            bslib::card_header("Taxonomic Backbone"),
            
            taxonomicBackboneUI(id = "taxonomicBackbone_id_1")
            
          ),
          
          bslib::nav_panel(
            
            full_screen = TRUE,
            
            value = "taxaLookup_panel",
            
            bslib::card_header("Taxa Lookup"),
            
            taxaLookupUI(id = "taxaLookup_id_1")
            
          )
          
        )
        
      )
      
    ),
    
    analysisUI(id = "analysis_id_1"),
    
    vcInformationUI(id = "vcInfo_id_1"),
    
    bslib::nav_panel(
      
      title = "Documentation",
      
      documentationUI(id = "docs_id_1")
      
    ),
    
    bslib::nav_panel(
      
      title = "News❗",
      
      newsUI(id = "news_id_1")
      
    ),
    
    bslib::nav_panel(
      
      title = "Additional Information",
      
      additionalInfoUI(id = "additional_info_id_1")
      
    ),
    
    bslib::nav_panel(
      
      title = "Privacy",
      
      privacyUI(id = "privacy_id_1")
      
    ),
    
    bslib::nav_item(
      
      regionSelectUI(id = "regionSelect_id_1"),
      
    )
    
  )
  
}
