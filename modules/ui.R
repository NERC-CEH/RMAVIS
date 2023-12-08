# UI
ui <- bslib::page_navbar(

  title = bslib::layout_columns(
    bslib::card_image(file = "www/ukceh_logo_long_720x170_rgb.png", fill = FALSE, width = "320px"),
    shiny::h1("MAVIS", 
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
        
        nvcAverageSimUI(id = "nvcAverageSim_id_1")
        
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
        
        value = "dca_panel",
        
        bslib::card_header("Multivariate Analysis"),
        
        bslib::layout_columns(
          
          col_widths = c(6, 6),
          
          mvaAllNVCUI(id = "mvaAllNVC_id_1"),
          
          dcaSubsetNVCUI(id = "dcaSubsetNVC_id_1")#,
          
          # dcaAllQuadratsUI(id = "dcaAllQuadrats_id_1")
          
        )
      )
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
  )#,
  
  # bslib::nav_panel(
  #   
  #   "About",
  #   
  #   bslib::card(
  #     
  #     full_screen = TRUE,
  #     
  #     bslib::card_header("Documentation"),
  #     
  #     documentationUI(id = "docs_id_1")
  #     
  #   )
  # )

  # )
)
