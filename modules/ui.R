# UI
ui <- bslib::page_sidebar(

  title = "MAVIS",

  sidebar = sidebarUI(id = "sidebar_id_1"),

  tags$head(includeCSS("www/style.css")),
  
  bslib::card(
  # bslib::navset_card_pill(
  #   
  #   bslib::nav_panel(
      
      # title = "Basic Inputs",
      # value = "basic_inputs_panel",
      
      shiny::h5("Survey Data Table"),
    
      inputsUI(id = "inputs_id_1"),
      
      shiny::h5("Results"),
      
      resultsUI(id = "results_id_1"),
      
      shiny::h5("Habitat Correspondence"),
      
      habCorUI(id = "habCor_id_1")
      
#     )
  )
)
