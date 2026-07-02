# ============================================================
# mod_acerca_de.R — Information about StatBayes
# StatBayes · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 780px; margin: 0 auto;",

      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatBayes",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(class = "text-muted mb-4",
        "StatBayes es el m\u00f3dulo de an\u00e1lisis bayesiano de StatSuite, ",
        "desarrollado en el ICOMVIS de la Universidad Nacional, Costa Rica. ",
        "Permite ajustar modelos bayesianos de manera interactiva y did\u00e1ctica, ",
        "cubriendo desde la elecci\u00f3n de distribuciones a priori hasta la ",
        "comparaci\u00f3n de modelos, sin necesidad de conocimiento previo de ",
        "programaci\u00f3n en Stan."
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite \u2014 Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"),  " \u2014 Dise\u00f1o de estudios y muestreo"),
              tags$li(strong("StatFlow"),    " \u2014 Primeros an\u00e1lisis y visualizaci\u00f3n"),
              tags$li(strong("StatGeo"),     " \u2014 An\u00e1lisis espacial y mapas"),
              tags$li(strong("StatMonitor"), " \u2014 Monitoreo poblacional"),
              tags$li(strong("StatModels"), " \u2014 Modelos estad\u00edsticos frecuentistas"),
              tags$li(strong("StatBayes"),  " \u2014 An\u00e1lisis bayesiano \u2190 aqu\u00ed")
            )
          )
        ),

        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "Ecosistema R utilizado"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("brms"),
                      " \u2014 modelos bayesianos v\u00eda Stan"),
              tags$li(strong("bayesplot"),
                      " \u2014 visualizaci\u00f3n de posteriors y MCMC"),
              tags$li(strong("posterior"),
                      " \u2014 manipulaci\u00f3n de draws"),
              tags$li(strong("loo"),
                      " \u2014 validaci\u00f3n cruzada aproximada (LOO, WAIC)"),
              tags$li(strong("tidybayes"),
                      " \u2014 flujo tidy para modelos bayesianos"),
              tags$li(strong("parameters"), " + ", strong("performance"),
                      " \u2014 resumen y diagn\u00f3stico")
            )
          )
        )
      ),

      # Módulos
      card(
        class = "mt-3",
        card_header(bs_icon("grid", class = "me-1"), "M\u00f3dulos"),
        card_body(
          tags$ul(
            class = "small",
            tags$li(strong("Distribuciones a priori"),
                    " \u2014 qu\u00e9 son, c\u00f3mo elegirlas, prior predictive check"),
            tags$li(strong("Regresi\u00f3n lineal bayesiana"),
                    " \u2014 equivalente bayesiano del LM"),
            tags$li(strong("GLM bayesiano"),
                    " \u2014 familias binomial, Poisson, binomial negativa"),
            tags$li(strong("GAM bayesiano"),
                    " \u2014 splines bayesianos con brms"),
            tags$li(strong("Modelos mixtos bayesianos"),
                    " \u2014 efectos aleatorios con brms"),
            tags$li(strong("Diagn\u00f3stico MCMC"),
                    " \u2014 Rhat, ESS, traceplots, divergencias"),
            tags$li(strong("Comparaci\u00f3n de modelos"),
                    " \u2014 LOO, WAIC, Factor de Bayes")
          )
        )
      ),

      # Desarrollo
      card(
        class = "mt-3",
        card_header(bs_icon("code-slash", class = "me-1"), "Desarrollo"),
        card_body(
          p(class = "small mb-2",
            bs_icon("person-fill", class = "me-1"),
            strong("Autor:"), " Manuel Sp\u00ednola \u2014 ICOMVIS, ",
            "Universidad Nacional, Costa Rica."),
          p(class = "small mb-2",
            bs_icon("robot", class = "me-1"),
            strong("Asistencia en desarrollo:"), " StatBayes fue desarrollado ",
            "con asistencia de ", strong("Claude (Anthropic)"),
            " para la estructura de m\u00f3dulos, interfaz de usuario y ",
            "l\u00f3gica del servidor."),
          p(class = "small mb-0",
            bs_icon("building", class = "me-1"),
            strong("Instituci\u00f3n:"), " Instituto Internacional en ",
            "Conservaci\u00f3n y Manejo de Vida Silvestre (ICOMVIS), ",
            "Universidad Nacional de Costa Rica.")
        )
      ),

      div(
        class = "alert alert-info small mt-3 mb-0",
        bs_icon("envelope", class = "me-1"),
        "Contacto: ",
        tags$a(href = "mailto:manuel.spinola@una.ac.cr",
               "manuel.spinola@una.ac.cr")
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # no reactive logic
  })
}
