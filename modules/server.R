# Server
server <- function(input, output, session) {

  sidebar_options <- callModule(module = sidebar,
                                id = "sidebar_id_1")
  
  assignNVC_results <- callModule(module = inputs,
                                  id = "inputs_id_1",
                                  sidebar_options = sidebar_options)
  
  callModule(module = results,
             id = "results_id_1",
             assignNVC_results = assignNVC_results)
  
  callModule(module = habCor,
             id = "habCor_id_1",
             assignNVC_results = assignNVC_results,
             sidebar_options = sidebar_options)

}
