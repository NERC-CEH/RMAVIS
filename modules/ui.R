# UI
ui <- bslib::page_sidebar(

  title = "MAVIS",

  sidebar = "Sidebar",

  tags$head(includeCSS("www/style.css")),

  inputsUI(id = "inputs_id_1")

)
