#' The application User-Interface
#'
#' @param request Internal parameter for \code{{shiny}}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import shinyjs
#' @noRd
app_ui <- function(request) {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatBayes")
  )

  bslib::page_navbar(
    header = shinyjs::useShinyjs(),
    title  = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatBayes.png", height = "38px"),
      span("StatBayes", style = "font-weight: 600;")
    ),
    theme  = tema_app,
    lang   = "es",
    footer = div(
      class = "text-center small py-2",
      style = paste0("background:", colores$primario, "; color: white;"),
      "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
    ),

    bslib::nav_panel(
      title = "Distribuciones a priori",
      icon  = bsicons::bs_icon("sliders"),
      mod_prior_ui("prior")
    ),

    bslib::nav_panel(
      title = "Regresi\u00f3n lineal bayesiana",
      icon  = bsicons::bs_icon("graph-up"),
      mod_lm_bayes_ui("lm_bayes")
    ),

    bslib::nav_panel(
      title = "GLM bayesiano",
      icon  = bsicons::bs_icon("toggles"),
      mod_glm_bayes_ui("glm_bayes")
    ),

    bslib::nav_panel(
      title = "GAM bayesiano",
      icon  = bsicons::bs_icon("bezier2"),
      mod_gam_bayes_ui("gam_bayes")
    ),

    bslib::nav_panel(
      title = "Modelos mixtos bayesianos",
      icon  = bsicons::bs_icon("diagram-3"),
      mod_mixed_bayes_ui("mixed_bayes")
    ),

    bslib::nav_panel(
      title = "Inferencia MCMC",
      icon  = bsicons::bs_icon("activity"),
      mod_mcmc_ui("mcmc")
    ),

    bslib::nav_spacer(),

    bslib::nav_panel(
      title = "Acerca de",
      icon  = bsicons::bs_icon("info-circle"),
      mod_acerca_de_ui("acerca_de")
    ),

    bslib::nav_item(
      tags$span(class = "text-white-50 small", "StatBayes v0.1")
    )
  )
}
