#' mcmc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mcmc_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mcmc Server Functions
#'
#' @noRd 
mod_mcmc_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mcmc_ui("mcmc_1")
    
## To be copied in the server
# mod_mcmc_server("mcmc_1")

