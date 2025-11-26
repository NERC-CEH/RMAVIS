# UI
ui <- bslib::page_navbar(
  
  shinyjs::useShinyjs(),
  
  # Note using bslib::layout_columns() leads to additional bslib text in the tab title
  title = shiny::div(
    
    shiny::splitLayout(
      
      # CEH Logo
      bslib::card_image(file = "www/ukceh_logo_long_720x170_rgb.png", fill = FALSE, width = "300px"),
      
      # App Title
      shiny::div(shiny::h1("RMAVIS")),
      
      # Align cell contents in the middle vertically
      cellArgs = list(style = "vertical-align: middle !important; color: #565656 !important;")
      
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
