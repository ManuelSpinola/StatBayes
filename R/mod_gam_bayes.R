#' gam_bayes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gam_bayes_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' gam_bayes Server Functions
#'
#' @noRd 
mod_gam_bayes_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_gam_bayes_ui("gam_bayes_1")
    
## To be copied in the server
# mod_gam_bayes_server("gam_bayes_1")

