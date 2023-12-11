# UI
ui <- bslib::page_navbar(
    
  # Note using bslib::layout_columns() leads to additional bslib text in the tab title
  title = shiny::div(

    shiny::splitLayout(

      # CEH Logo
      bslib::card_image(file = "www/ukceh_logo_long_720x170_rgb.png", fill = FALSE, width = "300px"),

      # App Title
      shiny::div(shiny::h1("pseudoMAVIS")),
      
      # Align cell contents in the middle vertically
      cellArgs = list(style = "vertical-align: middle !important;")

    ),

     # Tab Title
      shiny::tagList(tags$head(tags$title(paste0(" | UK Centre for Ecology & Hydrology")),
                       tags$link(rel = "shortcut icon", href = "https://brandroom.ceh.ac.uk/themes/custom/ceh/favicon.ico")))
  
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
  
  shinyjs::useShinyjs(),
  
  bslib::nav_spacer(),
    
  bslib::nav_panel(
    
    "Application",
    
    bslib::navset_card_tab(
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        bslib::card_header("Survey Data"),
        
        value = "surveyData_panel",
        
        surveyTableUI(id = "surveyTable_id_1")
        
      ),
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        value = "nvcAssignment_panel",
        
        bslib::card_header("NVC Assignment"),
        
        nvcAssignmentUI(id = "nvcAssignment_id_1")
        
      ),
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        value = "habCor_panel",
        
        bslib::card_header("Habitat Correspondence"),
        
        habCorUI(id = "habCor_id_1")
        
      ),
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        value = "floristicTables_panel",
        
        bslib::card_header("Floristic Tables"),
        
        floristicTablesUI(id = "floristicTables_id_1")
        
      ),
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        value = "speciesFreq_panel",
        
        bslib::card_header("Frequency"),
        
        speciesFreqUI(id = "speciesFreq_id_1")
        
      ),
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        value = "eivs_panel",
        
        bslib::card_header("EIVs"),
        
        calcAvgEIVsUI(id = "calcAvgEIVs_id_1")
        
      ),
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        value = "diversity_panel",
        
        bslib::card_header("Diversity"),
        
        diversityAnalysisUI(id = "diversityAnalysis_id_1")
        
      ),
      
      bslib::nav_panel(
        
        full_screen = TRUE,
        
        value = "mva_panel",
        
        bslib::card_header("MVA"),
        
        bslib::layout_columns(
          
          col_widths = c(12, 12, 12, 12),
          
          row_heights = c(1, 1, 1, 1),
          
          fill = FALSE,
          
          fillable = TRUE,
          
          mvaNationalRefUI(id = "mvaNationalRef_id_1"),
          
          mvaLocalRefRestrictedUI(id = "mvaLocalRefRestricted_id_1"),
          
          mvaLocalRefUnrestrictedUI(id = "mvaLocalRefUnrestricted_id_1")
          
        )
      )
    )
  ),
  
  bslib::nav_panel(
    
    "NVC Information",
    
    value = "nvcInfo",
    
    bslib::card(
      
      full_screen = FALSE,
      
      fill = TRUE,
      
      bslib::card_header("NVC Lookup"),
      
      nvcInfoUI(id = "nvcInfo_id_1")
      
    )
  ),
    
  bslib::nav_panel(
    
    "Documentation",
    
    bslib::card(
      
      full_screen = FALSE,
      
      fill = TRUE,
      
      # fillable = TRUE,
      
      bslib::card_header("Documentation"),
      
      documentationUI(id = "docs_id_1")
      
    )
  )
)
