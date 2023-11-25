# UI
ui <- bslib::page_navbar(

  title = bslib::layout_columns(
    bslib::card_image(file = "www/ukceh_logo_long_720x170_rgb.png", fill = FALSE, width = "320px"),
    shiny::h2("MAVIS", 
              id = "title")
    ),
  
  id = "nav",
  
  sidebar = sidebarUI(id = "sidebar_id_1"),
  
  # sidebar = bslib::sidebar(
  # 
  #   shiny::conditionalPanel(
  #     "input.nav === 'Main'",
  #     sidebarUI(id = "sidebar_id_1")
  #   ),
  # 
  #   shiny::conditionalPanel(
  #     "input.nav === 'Documentation'",
  #     NULL
  #   )
  # 
  # ),

  tags$head(includeCSS("www/style.css")),
  
  bslib::nav_spacer(),
    
  bslib::nav_panel(
    
    "Main",
    
    bslib::navset_card_tab(
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        bslib::card_header("Main"),
        
        value = "main_panel",
        
        shiny::h5("Survey Data Table"),
        
        surveyTableUI(id = "surveyTable_id_1"),
        
        shiny::h5("Results"),
        
        assignNVCResultsUI(id = "assignNVCResults_id_1"),
        
        shiny::h5("Habitat Correspondence"),
        
        habCorUI(id = "habCor_id_1")
        
      ),
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        value = "floristicTables_panel",
        
        bslib::card_header("Floristic Tables"),
        
        floristicTablesUI(id = "floristicTables_id_1")
        
      )
    )
  ),
    
  bslib::nav_panel(
    
    "Documentation",
    
    bslib::card(
      
      full_screen = TRUE,
      
      bslib::card_header("Documentation"),
      
      documentationUI(id = "docs_id_1")
      
    )
  )

  # )
)
