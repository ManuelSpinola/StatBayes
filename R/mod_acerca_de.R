#' acerca_de UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' acerca_de Server Functions
#'
#' @noRd 
mod_acerca_de_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_acerca_de_ui("acerca_de_1")
    
## To be copied in the server
# mod_acerca_de_server("acerca_de_1")

