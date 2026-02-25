analysisUI <- function(id){
  
  ns <- NS(id)
  
  bslib::nav_panel(
    
    title = "Analysis",
    
    value = ns("analysis_panel"),
    
    bslib::layout_sidebar(
      
      sidebar = sidebarUI(id = ns("sidebar_id_1")),
      
      bslib::navset_card_tab(
        
        id = ns("analysis_nct"),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "vcAssignment_panel",
          
          bslib::card_header("VC Assignment"),
          
          vcAssignmentUI(id = ns("vcAssignment_id_1"))
          
        ),
        
        bslib::nav_panel(

          full_screen = TRUE,

          value = "habCor_panel",

          bslib::card_header("Habitat Correspondence"),

          habCorUI(id = ns("habCor_id_1"))

        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "floristicTables_panel",
          
          bslib::card_header("Floristic Tables"),
          
          floristicTablesUI(id = ns("floristicTables_id_1"))
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "speciesFreq_panel",
          
          bslib::card_header("Frequency"),
          
          speciesFreqUI(id = ns("speciesFreq_id_1"))
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "eivs_panel",
          
          bslib::card_header("EIVs"),
          
          calcAvgEIVsUI(id = ns("calcAvgEIVs_id_1"))
          
        ),
        
        bslib::nav_panel(
          
          full_screen = TRUE,
          
          value = "diversity_panel",
          
          bslib::card_header("Diversity"),
          
          diversityAnalysisUI(id = ns("diversityAnalysis_id_1"))
          
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
            
            mvaNationalRefUI(id = ns("mvaNationalRef_id_1")),
            
            mvaLocalRefRestrictedUI(id = ns("mvaLocalRefRestricted_id_1")),
            
            mvaLocalRefUnrestrictedUI(id = ns("mvaLocalRefUnrestricted_id_1"))
            
          )
        )
      )
      
    )
    
  )
  
}