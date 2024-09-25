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
      
      # bslib::card_image(file = "www/nationalReference.png", fill = FALSE, max_height = "62px"),
      
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
    
    "Home",
    
    homeUI(id = "home_id_1")
    
  ),
    
  bslib::nav_panel(
    
    "Core",
    
    bslib::layout_sidebar(

      sidebar = sidebarUI(id = "sidebar_id_1"),
      
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
            
            col_widths = c(6, 6, 6),
            
            row_heights = c(1, 1),
            
            fill = FALSE,
            
            fillable = TRUE,
            
            mvaNationalRefUI(id = "mvaNationalRef_id_1"),
            
            mvaLocalRefRestrictedUI(id = "mvaLocalRefRestricted_id_1"),
            
            mvaLocalRefUnrestrictedUI(id = "mvaLocalRefUnrestricted_id_1")
            
          )
        )
      )

    ),
    
  ),
  
  bslib::nav_panel(

    "ENM",

    bslib::layout_sidebar(

      sidebar = nmSidebarUI(id = "nmSidebar_id_1"),

      bslib::navset_card_tab(
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          bslib::card_header("Model Display"),
          
          value = "modelDisplay_panel",
          
          nmModelDisplayUI(id = "nmModelDisplay_id_1")
          
        ),

        bslib::nav_panel(

          full_screen = TRUE,

          bslib::card_header("Data Input"),

          value = "nmDataInput_panel",

          nmDataInputUI(id = "nmDataInput_id_1")

        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          bslib::card_header("Model Prediction"),
          
          value = "nmModelPred_panel",
          
          nmModelRunUI(id = "nmModelRun_id_1")
          
        )

      )

    ),

  ),
  
  bslib::nav_panel(
    
    "NVC Lookup",
    
    bslib::card(
      
      full_screen = FALSE,
      
      fill = TRUE,
      
      nvcInfoUI(id = "nvcInfo_id_1")
      
    )
  ),
    
  bslib::nav_panel(
    
    "Documentation",
    
    documentationUI(id = "docs_id_1")
    
  ),
  
  bslib::nav_panel(

    "News",

    newsUI(id = "news_id_1")

  ),
  
  bslib::nav_panel(
    
    "Privacy",
    
    privacyUI(id = "privacy_id_1")

  )
  
)
