#' mixed_bayes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mixed_bayes_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mixed_bayes Server Functions
#'
#' @noRd 
mod_mixed_bayes_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mixed_bayes_ui("mixed_bayes_1")
    
## To be copied in the server
# mod_mixed_bayes_server("mixed_bayes_1")

