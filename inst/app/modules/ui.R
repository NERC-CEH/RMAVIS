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
    
    "Home",
    
    homeUI(id = "home_id_1")
    
  ),
  
  bslib::nav_panel(
    
    "Data Entry",
    
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
          
          value = "rmavisTaxonNamesLookup_panel",
          
          bslib::card_header("Taxonomic Backbone"),
          
          rmavisTaxonNamesLookupUI(id = "rmavisTaxonNamesLookup_id_1")
          
        )
        
      )
      
    )
    
  ),
    
  bslib::nav_panel(
    
    "NVC Analysis",
    
    bslib::layout_sidebar(

      sidebar = sidebarUI(id = "sidebar_id_1"),
      
      bslib::navset_card_tab(
        
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

    )
    
  ),
  
  bslib::nav_panel(
    
    "NVC Information",
    
    bslib::layout_sidebar(
      
      sidebar = nvcInfoSidebarUI(id = "nvcInfoSidebar_id_1"),
      
      bslib::navset_card_tab(
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "nvcCommNamesLookup_panel",
          
          bslib::card_header("Community Names"),
          
          nvcCommNamesLookupUI(id = "nvcCommNamesLookup_id_1")
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "nvcFlorTabs_panel",
          
          bslib::card_header("Floristic Tables"),
          
          nvcFlorTabsUI(id = "nvcFlorTabs_id_1")
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "nvcTaxonNamesLookup_panel",
          
          bslib::card_header("NVC Taxon Name Updates"),
          
          nvcTaxonNamesLookupUI(id = "nvcTaxonNamesLookup_id_1")
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "nvcCommAttr_panel",
          
          bslib::card_header("Community Attributes"),
          
          nvcCommAttrUI(id = "nvcCommAttr_id_1")
          
        )
        
      )
      
      )
    
  ),
    
  bslib::nav_panel(
    
    "Documentation",
    
    documentationUI(id = "docs_id_1")
    
  ),
  
  bslib::nav_panel(

    "News❗",

    newsUI(id = "news_id_1")

  ),
  
  bslib::nav_panel(
    
    "Additional Information",
    
    additionalInfoUI(id = "additiona_info_id_1")
    
  ),
  
  bslib::nav_panel(
    
    "Privacy",
    
    privacyUI(id = "privacy_id_1")
    
  )
  
)
