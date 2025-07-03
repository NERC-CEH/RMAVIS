newsUI <- function(id) {
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::column(
      
      width = 12,
      
      shiny::div(
        
        shiny::h1("News"),
        
        shiny::br(),
        
        shiny::markdown("The latest version of RMAVIS (v1.1.0) contains a number of breaking changes.
                         First, is a major update to the taxonomy, which is now aligned with version of the UKSI.
                         Second, is a reduction in the number of pseudo-quadrats used to match quadrats with the Jaccard similarity method.
                         This has improved the speed at which quadrats are matched, but means that your results may differ slightly from previous versions.")
        
      ),
      
      shiny::div(
        
        shiny::hr()
        
      ),
      
      shiny::div(
        
        shiny::h1("Release Log"),
        
        shiny::br(),
        
        shiny::includeMarkdown(gsub(pattern = "#", replacement = "###", x = readLines("NEWS.md"))),
        
      )
    )
  )
  
}