#' comparacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comparacion_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' comparacion Server Functions
#'
#' @noRd 
mod_comparacion_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_comparacion_ui("comparacion_1")
    
## To be copied in the server
# mod_comparacion_server("comparacion_1")

