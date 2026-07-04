#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_prior_server("prior")
  mod_lm_bayes_server("lm_bayes")
  mod_glm_bayes_server("glm_bayes")
  mod_gam_bayes_server("gam_bayes")
  mod_mixed_bayes_server("mixed_bayes")
  mod_mcmc_server("mcmc")
  mod_acerca_de_server("acerca_de")

  session$onSessionEnded(function() {})
}
