# ============================================================
# mod_mcmc.R — Diagnóstico MCMC y Factor de Bayes
# StatBayes · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. ¿Qué es MCMC?
#   2. Algoritmos (MH, HMC, NUTS)
#   3. Explorador interactivo
#   4. Diagnóstico
#   5. Factor de Bayes
# ============================================================

mod_mcmc_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(bs_icon("activity", class = "me-2"), "Inferencia MCMC",
         style = paste0("color:", colores$primario, "; font-weight:700;")),
      p(class = "text-muted mb-0",
        "M\u00e9todos de Monte Carlo v\u00eda cadenas de Markov (MCMC): ",
        "c\u00f3mo brms muestrea la distribuci\u00f3n posterior, c\u00f3mo diagnosticar ",
        "la convergencia y c\u00f3mo usar el Factor de Bayes para comparar hip\u00f3tesis.")
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es MCMC?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es MCMC?"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Monte Carlo v\u00eda cadenas de Markov"),
          p(class = "small text-muted mb-3",
            "El an\u00e1lisis bayesiano requiere calcular la distribuci\u00f3n posterior ",
            "P(\u03b8 | datos). En modelos simples esto se puede hacer anal\u00edticamente, ",
            "pero en modelos complejos (como los de brms) es imposible. ",
            strong("MCMC"), " resuelve esto generando una secuencia de muestras ",
            "que, colectivamente, aproximan la distribuci\u00f3n posterior."
          ),

          layout_columns(col_widths = c(6, 6),
            card(
              card_header(bs_icon("question-circle", class = "me-1"),
                          "\u00bfC\u00f3mo funciona?"),
              card_body(
                tags$ol(class = "small mb-0",
                  tags$li("Comienza en un valor inicial de \u03b8 (aleatorio o especificado)."),
                  tags$li("Propone un nuevo valor \u03b8* seg\u00fan alguna regla."),
                  tags$li("Acepta o rechaza \u03b8* con probabilidad proporcional al posterior."),
                  tags$li("Si acepta, se mueve a \u03b8*. Si rechaza, permanece en \u03b8."),
                  tags$li("Repite miles de veces. La secuencia de valores es la cadena MCMC."),
                  tags$li("Las muestras de la cadena aproximan el posterior.")
                ),
                br(),
                div(class = "alert alert-info small py-2 px-3 mb-0",
                  bs_icon("lightbulb", class = "me-1"),
                  strong("Warmup:"), " las primeras iteraciones se descartan porque ",
                  "la cadena a\u00fan no ha convergido al posterior. brms usa por defecto ",
                  "el 50% de las iteraciones como warmup.")
              )
            ),
            card(
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Conceptos clave"),
              card_body(
                tags$dl(class = "small mb-0",
                  tags$dt("Cadena (chain)"),
                  tags$dd(class = "text-muted mb-2",
                    "Secuencia de muestras generadas por el algoritmo. ",
                    "brms corre m\u00faltiples cadenas en paralelo (default: 4) ",
                    "para verificar convergencia."),
                  tags$dt("Iteraci\u00f3n"),
                  tags$dd(class = "text-muted mb-2",
                    "Cada paso del algoritmo. Con 2000 iteraciones y 50% warmup, ",
                    "quedan 1000 muestras por cadena = 4000 muestras totales."),
                  tags$dt("R\u0302 (R-hat)"),
                  tags$dd(class = "text-muted mb-2",
                    "Mide si las cadenas convergieron al mismo posterior. ",
                    "R\u0302 < 1.01 indica convergencia."),
                  tags$dt("ESS (Effective Sample Size)"),
                  tags$dd(class = "text-muted mb-0",
                    "Tama\u00f1o efectivo de muestra, corregido por autocorrelaci\u00f3n. ",
                    "ESS > 400 es aceptable. Puede superar el total de muestras.")
                )
              )
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfPor qu\u00e9 m\u00faltiples cadenas?"),
          p(class = "small text-muted mb-3",
            "Correr varias cadenas desde puntos de inicio diferentes permite ",
            "detectar si el algoritmo qued\u00f3 atrapado en una regi\u00f3n del espacio ",
            "de par\u00e1metros. Si todas las cadenas convergen al mismo posterior, ",
            "tenemos confianza en el resultado."),

          layout_columns(col_widths = c(4, 4, 4),
            div(class = "card-muestreo",
              style = paste0("border-left:4px solid ", colores$exito, ";"),
              p(class = "small mb-1 fw-bold",
                style = paste0("color:", colores$exito),
                bs_icon("check-circle-fill", class = "me-1"),
                "Convergencia correcta"),
              p(class = "small text-muted mb-0",
                "Las cadenas se mezclan como orugas peludas superpuestas. ",
                "R\u0302 < 1.01 para todos los par\u00e1metros.")
            ),
            div(class = "card-muestreo",
              style = paste0("border-left:4px solid ", colores$advertencia, ";"),
              p(class = "small mb-1 fw-bold",
                style = paste0("color:#856404"),
                bs_icon("exclamation-triangle-fill", class = "me-1"),
                "Convergencia lenta"),
              p(class = "small text-muted mb-0",
                "Las cadenas se mezclan pero lentamente. ",
                "R\u0302 entre 1.01 y 1.05. Aumenta iteraciones.")
            ),
            div(class = "card-muestreo",
              style = paste0("border-left:4px solid ", colores$peligro, ";"),
              p(class = "small mb-1 fw-bold",
                style = paste0("color:", colores$peligro),
                bs_icon("x-circle-fill", class = "me-1"),
                "Sin convergencia"),
              p(class = "small text-muted mb-0",
                "Las cadenas exploran regiones diferentes. ",
                "R\u0302 > 1.05. Reparametriza el modelo.")
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Algoritmos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("cpu", class = "me-1"), "Algoritmos"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "M\u00e9todos para aproximar el posterior"),
          p(class = "small text-muted mb-2",
            "Todos estos m\u00e9todos resuelven el mismo problema: ",
            strong("calcular la distribuci\u00f3n posterior P(\u03b8 | datos)"),
            ". La diferencia es ", em("c\u00f3mo"), " lo hacen \u2014 ",
            "con diferente balance entre exactitud y velocidad."),
          div(
            class = "p-3 mb-4 rounded small",
            style = paste0("background:", colores$fondo,
                           "; border: 1px solid ", colores$borde,
                           "; font-family: monospace; white-space: pre;",
                           " color:", colores$texto, "; line-height:1.7;"),
            "Problema: calcular P(\u03b8 | datos)\n",
            "         \u2193\n",
            "Soluciones:\n",
            "  \u251c\u2500\u2500 Exacta (solo modelos conjugados muy simples)\n",
            "  \u251c\u2500\u2500 Aproximaci\u00f3n determin\u00edsta\n",
            "  \u2502   \u251c\u2500\u2500 Laplace \u2014 aproxima el posterior con una Normal\n",
            "  \u2502   \u2514\u2500\u2500 INLA \u2014 aproximaci\u00f3n num\u00e9rica anidada (R-INLA)\n",
            "  \u2514\u2500\u2500 Muestreo estoc\u00e1stico (MCMC)\n",
            "      \u251c\u2500\u2500 Metropolis (1953) \u2014 el original\n",
            "      \u251c\u2500\u2500 Metropolis-Hastings (1970) \u2014 generalizaci\u00f3n\n",
            "      \u251c\u2500\u2500 Gibbs (BUGS, JAGS) \u2014 muestrea par\u00e1metro por par\u00e1metro\n",
            "      \u251c\u2500\u2500 HMC \u2014 usa el gradiente del posterior\n",
            "      \u2514\u2500\u2500 NUTS \u2014 Stan / brms (el m\u00e1s moderno y eficiente)"
          ),
          p(class = "small text-muted mb-3",
            "Lo que cambi\u00f3 hist\u00f3ricamente fue la ",
            strong("eficiencia"), ": con Metropolis necesitabas millones de ",
            "iteraciones para lo que NUTS hace con 4000. Por eso brms + Stan ",
            "es el est\u00e1ndar hoy en la inferencia bayesiana aplicada."),

          div(style = "overflow-x:auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-4",
              style = "background:#fff;",
              tags$thead(
                style = paste0("background:", colores$primario, "; color:#fff;"),
                tags$tr(
                  tags$th("M\u00e9todo"), tags$th("C\u00f3mo funciona"),
                  tags$th("Exactitud"), tags$th("Velocidad"),
                  tags$th("Paquetes en R"), tags$th("Mejor para")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$span(class="badge",
                    style=paste0("background:",colores$primario), "MCMC")),
                  tags$td("Muestrea el posterior directamente"),
                  tags$td(tags$span(class="badge bg-success", "Alta")),
                  tags$td(tags$span(class="badge bg-danger", "Lento")),
                  tags$td(tags$code("brms, rstan, rstanarm")),
                  tags$td("Cualquier modelo, m\u00e1xima exactitud")
                ),
                tags$tr(style=paste0("background:",colores$fondo),
                  tags$td(tags$span(class="badge",
                    style=paste0("background:",colores$secundario), "Laplace")),
                  tags$td("Aproxima el posterior con una Normal centrada en la moda"),
                  tags$td(tags$span(class="badge bg-warning text-dark", "Media")),
                  tags$td(tags$span(class="badge bg-success", "R\u00e1pido")),
                  tags$td(tags$code("glm, lme4 (internamente)")),
                  tags$td("Posteriors aproximadamente normales")
                ),
                tags$tr(
                  tags$td(tags$span(class="badge",
                    style=paste0("background:",colores$acento), "INLA")),
                  tags$td("Aproximaci\u00f3n num\u00e9rica determinista anidada"),
                  tags$td(tags$span(class="badge bg-warning text-dark", "Media-alta")),
                  tags$td(tags$span(class="badge bg-success", "Muy r\u00e1pido")),
                  tags$td(tags$code("R-INLA, inlabru")),
                  tags$td("Modelos espaciales y temporales")
                ),
                tags$tr(style=paste0("background:",colores$fondo),
                  tags$td(tags$span(class="badge", style="background:#9F8B75",
                    "Variational Bayes")),
                  tags$td("Optimizaci\u00f3n en lugar de muestreo"),
                  tags$td(tags$span(class="badge bg-warning text-dark", "Media")),
                  tags$td(tags$span(class="badge bg-success", "Muy r\u00e1pido")),
                  tags$td(tags$code("Stan VB, rstanarm")),
                  tags$td("Datos muy grandes, exploraci\u00f3n r\u00e1pida")
                )
              )
            )
          ),

          # INLA en detalle
          div(class = "card-muestreo mb-4",
            style = paste0("border-left:4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento, "; font-weight:700;"),
                   "INLA \u2014 Integrated Nested Laplace Approximations")),
            p(class = "small text-muted mb-2",
              "El nombre describe exactamente c\u00f3mo funciona:"),
            tags$ul(class = "small text-muted mb-2",
              tags$li(strong("Integrated:"),
                      " integra (marginaliza) sobre los hiperpar\u00e1metros del modelo."),
              tags$li(strong("Nested:"),
                      " las aproximaciones est\u00e1n anidadas en dos niveles: ",
                      "primero aproxima los hiperpar\u00e1metros, luego los efectos latentes."),
              tags$li(strong("Laplace:"),
                      " usa la aproximaci\u00f3n de Laplace (Normal centrada en la moda) ",
                      "como bloque fundamental."),
              tags$li(strong("Approximations:"),
                      " es una aproximaci\u00f3n, no muestreo exacto.")
            ),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                strong("Ventaja:"), " lo que MCMC tarda horas, INLA lo hace en segundos. ",
                "Ideal para modelos espaciales (geoestadística, SPDE), ",
                "series de tiempo y modelos con muchos efectos aleatorios. ",
                "Desarrollado por H\u00e5vard Rue et al. (2009)."),
              p(class = "small text-muted mb-0",
                strong("Limitaci\u00f3n:"), " solo funciona para modelos ",
                strong("LGCM (Latent Gaussian Conditional Models)"),
                " \u2014 una clase amplia pero no universal. ",
                "La aproximaci\u00f3n puede fallar en posteriors muy no gaussianos. ",
                "brms usa MCMC porque es m\u00e1s general.")
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "De Metropolis-Hastings a NUTS"),
          p(class = "small text-muted mb-3",
            "Dentro de MCMC, los algoritmos han evolucionado desde propuestas simples ",
            "hasta samplers sofisticados. brms usa Stan, que implementa ",
            strong("NUTS"), " \u2014 el algoritmo m\u00e1s eficiente disponible."),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$texto, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                h6(class = "mb-0",
                   style = paste0("color:", colores$texto, "; font-weight:700;"),
                   "1. Metropolis (1953) y Metropolis-Hastings (1970)")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "Nicholas Metropolis propuso la idea original para f\u00edsica estad\u00edstica. ",
                "W.K. Hastings la generaliz\u00f3 en 1970 para distribuciones asim\u00e9tricas. ",
                "Propone un nuevo \u03b8* y acepta con probabilidad min(1, P(\u03b8*)/P(\u03b8)). ",
                "Simple pero ineficiente en espacios de alta dimensi\u00f3n."),
              p(class = "small text-muted mb-0",
                strong("Problema:"), " con 10+ par\u00e1metros necesita millones de ",
                "iteraciones. Hoy solo se usa como referencia did\u00e1ctica ",
                "o en modelos muy simples.")
            )
          ),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid #9F8B75;"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                h6(class = "mb-0",
                   style = "color:#9F8B75; font-weight:700;",
                   "2. Gibbs sampling \u2014 BUGS y JAGS")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "Nombrado en honor al f\u00edsico Josiah Willard Gibbs. ",
                "En lugar de proponer valores para todos los par\u00e1metros a la vez, ",
                "muestrea ", strong("cada par\u00e1metro por separado"),
                " condicionado a los valores actuales de los dem\u00e1s. ",
                "La tasa de aceptaci\u00f3n es siempre 100% \u2014 nunca rechaza. ",
                "Usado en BUGS, WinBUGS y ", strong("JAGS"),
                " (Just Another Gibbs Sampler)."),
              p(class = "small text-muted mb-0",
                strong("Limitaci\u00f3n:"), " requiere conocer las distribuciones ",
                "condicionales completas de cada par\u00e1metro, lo que no siempre ",
                "es posible. Con par\u00e1metros muy correlacionados, la mezcla ",
                "es lenta. NUTS supera a Gibbs en eficiencia para modelos modernos.")
            )
          ),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario, "; font-weight:700;"),
                   "3. Hamiltonian Monte Carlo (HMC) \u2014 el salto cu\u00e1ntico")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "Usa el gradiente del log-posterior para proponer saltos ",
                "inteligentes que exploran el espacio eficientemente. ",
                "Anal\u00f3gicamente: imagina una bola que rueda por el paisaje ",
                "del posterior \u2014 la gravedad (gradiente) la gu\u00eda hacia ",
                "regiones de alta probabilidad."),
              p(class = "small text-muted mb-0",
                strong("Ventaja:"), " mucho m\u00e1s eficiente que MH. ",
                "Con 1000 iteraciones de HMC se obtiene lo que MH lograría ",
                "con millones. ",
                strong("Desventaja:"), " requiere especificar el tama\u00f1o del paso ",
                "(\u03b5) y el n\u00famero de pasos (L) — si se eligen mal, es ineficiente.")
            )
          ),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("stars", style = paste0("color:", colores$primario,
                                                 "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "4. NUTS (No-U-Turn Sampler) \u2014 el que usa brms")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "NUTS es HMC con ajuste autom\u00e1tico del n\u00famero de pasos. ",
                "El algoritmo detecta cu\u00e1ndo la trayectoria empieza a dar ",
                "la vuelta (U-turn) y para justo antes, maximizando la ",
                "distancia recorrida. No requiere tuning manual."),
              p(class = "small text-muted mb-0",
                strong("Stan + NUTS:"), " Stan (el motor de brms) implementa ",
                "NUTS con adaptaci\u00f3n autom\u00e1tica del tama\u00f1o de paso durante ",
                "el warmup. El resultado es un sampler que funciona bien ",
                "en la mayor\u00eda de los modelos sin ajuste manual.")
            )
          ),

          div(class = "card-muestreo mb-0",
            style = paste0("border-left:4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("exclamation-triangle",
                        style = paste0("color:", colores$acento,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento, "; font-weight:700;"),
                   "Divergencias \u2014 una se\u00f1al de alerta")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "Las ", strong("divergencias"), " ocurren cuando NUTS no puede ",
                "seguir la trayectoria del posterior correctamente. ",
                "Indican que la geometr\u00eda del posterior es problem\u00e1tica ",
                "en alguna regi\u00f3n. ", strong("No son errores num\u00e9ricos"), " ",
                "\u2014 son advertencias de que las muestras en esa regi\u00f3n ",
                "pueden no ser confiables."),
              p(class = "small text-muted mb-0",
                strong("Si hay divergencias:"), br(),
                "1. Aumenta ", tags$code("adapt_delta"), " (default 0.8, prueba 0.95). ",
                br(),
                "2. Reparametriza el modelo.", br(),
                "3. Usa priors m\u00e1s informativos para regularizar.")
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Explorador interactivo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("sliders", class = "me-1"),
                        "Explorador interactivo"),
        card_body(

          p(class = "small text-muted mb-3",
            "Simula cadenas MCMC para una distribuci\u00f3n objetivo simple. ",
            "Visualiza c\u00f3mo el algoritmo explora el espacio de par\u00e1metros ",
            "y c\u00f3mo afectan el n\u00famero de iteraciones y el warmup."),

          layout_columns(col_widths = c(4, 8),

            card(
              card_header(bs_icon("gear", class = "me-1"), "Configuraci\u00f3n"),
              card_body(
                selectInput(ns("dist_objetivo_mcmc"),
                  "Distribuci\u00f3n objetivo:",
                  choices = c(
                    "Normal(0, 1)"          = "normal",
                    "Normal(2, 0.5)"        = "normal_estrecha",
                    "Mezcla de Normales"    = "mezcla",
                    "Beta(2, 5)"            = "beta",
                    "Gamma(2, 1)"           = "gamma"
                  ), selected = "normal"),
                sliderInput(ns("n_iter_mcmc"), "Iteraciones totales:",
                            min = 100, max = 5000, value = 1000, step = 100),
                sliderInput(ns("warmup_pct_mcmc"), "Warmup (%):",
                            min = 10, max = 70, value = 50, step = 10),
                sliderInput(ns("n_cadenas_mcmc"), "N\u00famero de cadenas:",
                            min = 1, max = 4, value = 2, step = 1),
                sliderInput(ns("paso_mcmc"), "Tama\u00f1o de paso (MH):",
                            min = 0.1, max = 3, value = 1, step = 0.1),
                actionButton(ns("simular_mcmc"), "Simular cadenas",
                             class = "btn-primary w-100", icon = icon("play"))
              )
            ),

            div(
              navset_pill(
                nav_panel(title = "Trazas", br(),
                  p(class = "small text-muted mb-2",
                    "Las l\u00edneas verticales separan warmup (gris) de sampling. ",
                    "Las cadenas deben mezclarse bien en la fase de sampling."),
                  plotOutput(ns("plot_trazas_mcmc"), height = "300px")
                ),
                nav_panel(title = "Posterior", br(),
                  p(class = "small text-muted mb-2",
                    "Densidad de las muestras post-warmup vs. distribuci\u00f3n objetivo. ",
                    "Con suficientes iteraciones deben coincidir."),
                  plotOutput(ns("plot_posterior_mcmc"), height = "300px")
                ),
                nav_panel(title = "Autocorrelaci\u00f3n", br(),
                  p(class = "small text-muted mb-2",
                    "Correlaci\u00f3n entre muestras consecutivas. Alta autocorrelaci\u00f3n ",
                    "reduce el ESS. El tama\u00f1o de paso afecta la autocorrelaci\u00f3n."),
                  plotOutput(ns("plot_acf_mcmc"), height = "300px")
                ),
                nav_panel(title = "M\u00e9tricas", br(),
                  uiOutput(ns("metricas_mcmc"))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard2-check", class = "me-1"),
                        "Diagn\u00f3stico"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Gu\u00eda de diagn\u00f3stico MCMC"),
          p(class = "small text-muted mb-3",
            "Un diagn\u00f3stico completo verifica que las cadenas convergieron, ",
            "que las muestras son suficientemente independientes y que el modelo ",
            "reproduce bien los datos observados."),

          layout_columns(col_widths = c(6, 6),

            div(
              h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "1. Verificar convergencia (R\u0302)"),
              div(class = "card-muestreo mb-3",
                style = paste0("border-left:4px solid ", colores$primario, ";"),
                tags$table(class = "table table-sm small mb-0",
                  tags$thead(
                    style = paste0("background:", colores$primario, "; color:#fff;"),
                    tags$tr(tags$th("R\u0302"), tags$th("Interpretaci\u00f3n"),
                            tags$th("Acci\u00f3n"))
                  ),
                  tags$tbody(
                    tags$tr(tags$td("< 1.01"),
                            tags$td("Convergencia correcta"),
                            tags$td(tags$span(class="badge bg-success", "\u2713 OK"))),
                    tags$tr(style = paste0("background:", colores$fondo),
                            tags$td("1.01 - 1.05"),
                            tags$td("Convergencia marginal"),
                            tags$td(tags$span(class="badge bg-warning text-dark",
                                             "M\u00e1s iter."))),
                    tags$tr(tags$td("> 1.05"),
                            tags$td("Sin convergencia"),
                            tags$td(tags$span(class="badge bg-danger",
                                             "Reparametriza")))
                  )
                )
              ),

              h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "2. Verificar ESS"),
              div(class = "card-muestreo mb-3",
                style = paste0("border-left:4px solid ", colores$secundario, ";"),
                tags$table(class = "table table-sm small mb-0",
                  tags$thead(
                    style = paste0("background:", colores$secundario, "; color:#fff;"),
                    tags$tr(tags$th("ESS"), tags$th("Interpretaci\u00f3n"),
                            tags$th("Acci\u00f3n"))
                  ),
                  tags$tbody(
                    tags$tr(tags$td("> 1000"),
                            tags$td("Excelente"),
                            tags$td(tags$span(class="badge bg-success", "\u2713 OK"))),
                    tags$tr(style = paste0("background:", colores$fondo),
                            tags$td("400 - 1000"),
                            tags$td("Aceptable"),
                            tags$td(tags$span(class="badge bg-success", "\u2713 OK"))),
                    tags$tr(tags$td("100 - 400"),
                            tags$td("Marginal"),
                            tags$td(tags$span(class="badge bg-warning text-dark",
                                             "M\u00e1s iter."))),
                    tags$tr(style = paste0("background:", colores$fondo),
                            tags$td("< 100"),
                            tags$td("Insuficiente"),
                            tags$td(tags$span(class="badge bg-danger",
                                             "Problema")))
                  )
                )
              )
            ),

            div(
              h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "3. Traceplots"),
              div(class = "card-muestreo mb-3",
                style = paste0("border-left:4px solid ", colores$acento, ";"),
                p(class = "small text-muted mb-2",
                  "Las cadenas deben parecerse a ", strong("orugas peludas"),
                  " superpuestas. Patrones problem\u00e1ticos:"),
                tags$ul(class = "small text-muted mb-0",
                  tags$li(strong("Cadena atascada:"),
                          " permanece en el mismo valor por muchas iteraciones."),
                  tags$li(strong("Deriva:"),
                          " la cadena sube o baja sistem\u00e1ticamente."),
                  tags$li(strong("Cadenas separadas:"),
                          " exploran regiones diferentes del posterior.")
                )
              ),

              h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "4. Posterior predictive check"),
              div(class = "card-muestreo mb-3",
                style = paste0("border-left:4px solid ", colores$peligro, ";"),
                p(class = "small text-muted mb-0",
                  "Compara datos simulados desde el posterior con los observados. ",
                  "Si el modelo est\u00e1 bien especificado, las distribuciones simuladas ",
                  "deben parecerse a la observada. Discrepancias indican que ",
                  "la familia o la estructura del modelo no es adecuada.")
              ),

              h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "5. Divergencias"),
              div(class = "card-muestreo mb-0",
                style = paste0("border-left:4px solid ", colores$advertencia, ";"),
                p(class = "small text-muted mb-0",
                  "Cualquier divergencia es preocupante. ",
                  "Si hay divergencias, las muestras en esa regi\u00f3n no son confiables. ",
                  "Soluciones: aumentar ", tags$code("adapt_delta"),
                  ", reparametrizar, o usar priors m\u00e1s informativos.")
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Factor de Bayes
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard2-data", class = "me-1"),
                        "Factor de Bayes"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfQu\u00e9 es el Factor de Bayes?"),
          p(class = "small text-muted mb-3",
            "El ", strong("Factor de Bayes (BF)"), " cuantifica cu\u00e1ntas veces ",
            "un modelo (o hip\u00f3tesis) es m\u00e1s probable que otro dado los datos. ",
            "Es el equivalente bayesiano de la prueba de hip\u00f3tesis frecuentista, ",
            "pero con una interpretaci\u00f3n m\u00e1s directa: ",
            strong("BF = 10"), " significa que los datos son 10 veces m\u00e1s compatibles ",
            "con H\u2081 que con H\u2080."
          ),

          layout_columns(col_widths = c(6, 6),
            card(
              card_header(bs_icon("table", class = "me-1"),
                          "Escala de Jeffreys (interpretaci\u00f3n)"),
              card_body(
                tags$table(class = "table table-sm small mb-0",
                  tags$thead(
                    style = paste0("background:", colores$primario, "; color:#fff;"),
                    tags$tr(tags$th("BF\u2081\u2080"), tags$th("Evidencia a favor de H\u2081"))
                  ),
                  tags$tbody(
                    tags$tr(tags$td("1 - 3"),
                            tags$td("Anecdótica / no concluyente")),
                    tags$tr(style = paste0("background:", colores$fondo),
                            tags$td("3 - 10"),
                            tags$td("Moderada")),
                    tags$tr(tags$td("10 - 30"),
                            tags$td("Fuerte")),
                    tags$tr(style = paste0("background:", colores$fondo),
                            tags$td("30 - 100"),
                            tags$td("Muy fuerte")),
                    tags$tr(tags$td("> 100"),
                            tags$td("Extrema")),
                    tags$tr(style = paste0("background:", colores$fondo),
                            tags$td("< 1/3"),
                            tags$td("Evidencia a favor de H\u2080"))
                  )
                )
              )
            ),
            card(
              card_header(bs_icon("exclamation-triangle", class = "me-1"),
                          "Limitaciones importantes"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li(strong("Sensible a los priors:"),
                          " el BF depende fuertemente de la especificaci\u00f3n del prior. ",
                          "Priors difusos producen BF que penalizan modelos complejos ",
                          "(paradoja de Bartlett)."),
                  tags$li(strong("Costoso computacionalmente:"),
                          " requiere calcular la evidencia marginal, que MCMC ",
                          "no proporciona directamente."),
                  tags$li(strong("Cu\u00e1ndo preferir LOO:"),
                          " para comparar modelos predictivos, LOO es m\u00e1s robusto ",
                          "y menos sensible a los priors."),
                  tags$li(strong("Cu\u00e1ndo preferir BF:"),
                          " para contrastar hip\u00f3tesis te\u00f3ricas espec\u00edficas, ",
                          "especialmente con priors informativos bien justificados.")
                )
              )
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Calculadora de Factor de Bayes"),
          p(class = "small text-muted mb-3",
            "Calcula el Factor de Bayes para pruebas simples usando el paquete ",
            strong("BayesFactor"), ". Selecciona el tipo de prueba, ",
            "carga los datos e interpreta el resultado."),

          layout_columns(col_widths = c(4, 8),
            card(
              card_header(bs_icon("gear", class = "me-1"),
                          "Configuraci\u00f3n"),
              card_body(
                selectInput(ns("tipo_bf"),
                  "Tipo de prueba:",
                  choices = c(
                    "t de una muestra (H\u2081: \u03bc \u2260 0)"          = "t_una",
                    "t de dos muestras (H\u2081: \u03bc\u2081 \u2260 \u03bc\u2082)" = "t_dos",
                    "Correlaci\u00f3n (H\u2081: \u03c1 \u2260 0)"             = "correlacion",
                    "Regresi\u00f3n lineal simple"                         = "regresion"
                  ),
                  selected = "t_una"),
                tags$hr(),
                uiOutput(ns("inputs_bf")),
                tags$hr(),
                numericInput(ns("prior_escala_bf"),
                  label = tagList(
                    "Escala del prior (r):",
                    tags$small(class = "text-muted d-block mt-1",
                      "Controla la amplitud del prior sobre el tama\u00f1o del efecto. ",
                      "Default: 0.707 (medio). Rango: 0.1 \u2013 2.")
                  ),
                  value = 0.707, min = 0.1, max = 2, step = 0.1),
                actionButton(ns("calcular_bf"),
                             "Calcular Factor de Bayes",
                             class = "btn-primary w-100",
                             icon = icon("calculator"))
              )
            ),

            div(
              card(class = "mb-3",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Resultado"),
                card_body(uiOutput(ns("resultado_bf")))
              ),
              card(class = "mb-0",
                card_header(bs_icon("code-slash", class = "me-1"),
                            "C\u00f3digo R"),
                card_body(verbatimTextOutput(ns("codigo_bf")))
              )
            )
          )
        )
      )

    ) # fin navset_card_tab
  )
}

# ── SERVER ────────────────────────────────────────────────
mod_mcmc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Explorador MCMC ───────────────────────────────
    cadenas_sim <- eventReactive(input$simular_mcmc, {
      n_iter   <- input$n_iter_mcmc
      warmup   <- round(n_iter * input$warmup_pct_mcmc / 100)
      n_chains <- input$n_cadenas_mcmc
      paso     <- input$paso_mcmc

      # función log-posterior según distribución objetivo
      log_post <- switch(input$dist_objetivo_mcmc,
        normal         = function(x) dnorm(x, 0, 1, log = TRUE),
        normal_estrecha = function(x) dnorm(x, 2, 0.5, log = TRUE),
        mezcla         = function(x) log(0.5 * dnorm(x, -2, 0.5) +
                                          0.5 * dnorm(x, 2, 0.5)),
        beta           = function(x) if (x > 0 && x < 1)
                                       dbeta(x, 2, 5, log = TRUE)
                                     else -Inf,
        gamma          = function(x) if (x > 0)
                                       dgamma(x, 2, 1, log = TRUE)
                                     else -Inf
      )

      # inicio según distribución
      inicio_range <- switch(input$dist_objetivo_mcmc,
        normal          = c(-3, 3),
        normal_estrecha = c(0, 4),
        mezcla          = c(-4, 4),
        beta            = c(0.1, 0.9),
        gamma           = c(0.5, 5)
      )

      # MH sampler
      lapply(seq_len(n_chains), function(chain) {
        set.seed(chain * 42)
        theta <- runif(1, inicio_range[1], inicio_range[2])
        muestras <- numeric(n_iter)
        aceptadas <- 0
        for (i in seq_len(n_iter)) {
          propuesta <- theta + rnorm(1, 0, paso)
          log_ratio <- log_post(propuesta) - log_post(theta)
          if (log(runif(1)) < log_ratio) {
            theta <- propuesta
            aceptadas <- aceptadas + 1
          }
          muestras[i] <- theta
        }
        list(muestras = muestras, warmup = warmup,
             tasa_aceptacion = aceptadas / n_iter)
      })
    })

    output$plot_trazas_mcmc <- renderPlot({
      req(cadenas_sim())
      chains <- cadenas_sim()
      n_iter <- input$n_iter_mcmc
      warmup <- round(n_iter * input$warmup_pct_mcmc / 100)

      df <- do.call(rbind, lapply(seq_along(chains), function(i) {
        data.frame(
          iter   = seq_len(n_iter),
          valor  = chains[[i]]$muestras,
          cadena = paste0("Cadena ", i)
        )
      }))

      ggplot(df, aes(x = iter, y = valor, color = cadena)) +
        annotate("rect", xmin = 0, xmax = warmup,
                 ymin = -Inf, ymax = Inf,
                 fill = "grey80", alpha = 0.4) +
        geom_line(linewidth = 0.4, alpha = 0.8) +
        scale_color_manual(values = colores$tableau[seq_along(chains)]) +
        geom_vline(xintercept = warmup, linetype = "dashed",
                   color = colores$texto) +
        labs(x = "Iteraci\u00f3n", y = "\u03b8", color = NULL,
             caption = paste0("Zona gris = warmup (", warmup, " iter.)")) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "top",
              plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    output$plot_posterior_mcmc <- renderPlot({
      req(cadenas_sim())
      chains  <- cadenas_sim()
      n_iter  <- input$n_iter_mcmc
      warmup  <- round(n_iter * input$warmup_pct_mcmc / 100)
      muestras_post <- unlist(lapply(chains, function(c)
        c$muestras[(warmup + 1):n_iter]))

      # distribución objetivo
      x_range <- switch(input$dist_objetivo_mcmc,
        normal          = seq(-4, 4, length.out = 500),
        normal_estrecha = seq(-1, 5, length.out = 500),
        mezcla          = seq(-5, 5, length.out = 500),
        beta            = seq(0.001, 0.999, length.out = 500),
        gamma           = seq(0.001, 8, length.out = 500)
      )
      y_obj <- switch(input$dist_objetivo_mcmc,
        normal          = dnorm(x_range, 0, 1),
        normal_estrecha = dnorm(x_range, 2, 0.5),
        mezcla          = 0.5 * dnorm(x_range, -2, 0.5) +
                          0.5 * dnorm(x_range, 2, 0.5),
        beta            = dbeta(x_range, 2, 5),
        gamma           = dgamma(x_range, 2, 1)
      )
      df_obj <- data.frame(x = x_range, y = y_obj)
      df_mc  <- data.frame(x = muestras_post)

      ggplot() +
        geom_density(data = df_mc, aes(x = x),
                     fill = colores$primario, alpha = 0.3,
                     color = colores$primario, linewidth = 0.8) +
        geom_line(data = df_obj, aes(x = x, y = y),
                  color = colores$acento, linewidth = 1.2,
                  linetype = "dashed") +
        labs(x = "\u03b8", y = "Densidad",
             caption = paste0("Azul = muestras MCMC | Naranja = distribuci\u00f3n objetivo",
                              " (", length(muestras_post), " muestras post-warmup)")) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    output$plot_acf_mcmc <- renderPlot({
      req(cadenas_sim())
      chains  <- cadenas_sim()
      n_iter  <- input$n_iter_mcmc
      warmup  <- round(n_iter * input$warmup_pct_mcmc / 100)
      muestras <- chains[[1]]$muestras[(warmup + 1):n_iter]

      acf_vals <- acf(muestras, lag.max = 30, plot = FALSE)
      df_acf   <- data.frame(lag = acf_vals$lag[-1],
                              acf = acf_vals$acf[-1])
      ic <- qnorm(0.975) / sqrt(length(muestras))

      ggplot(df_acf, aes(x = lag, y = acf)) +
        geom_hline(yintercept = 0, color = colores$texto) +
        geom_hline(yintercept = c(-ic, ic), linetype = "dashed",
                   color = colores$acento) +
        geom_col(fill = colores$primario, alpha = 0.7, width = 0.6) +
        labs(x = "Lag", y = "Autocorrelaci\u00f3n",
             caption = "L\u00edneas punteadas = l\u00edmites de significancia (95%)") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    output$metricas_mcmc <- renderUI({
      req(cadenas_sim())
      chains  <- cadenas_sim()
      n_iter  <- input$n_iter_mcmc
      warmup  <- round(n_iter * input$warmup_pct_mcmc / 100)
      muestras_list <- lapply(chains, function(c)
        c$muestras[(warmup + 1):n_iter])
      muestras_all  <- unlist(muestras_list)

      # R-hat manual
      n_post <- length(muestras_list[[1]])
      n_ch   <- length(muestras_list)
      medias <- sapply(muestras_list, mean)
      vars   <- sapply(muestras_list, var)
      B      <- n_post * var(medias)
      W      <- mean(vars)
      Vhat   <- (1 - 1/n_post) * W + B / n_post
      rhat   <- sqrt(Vhat / W)

      # ESS manual
      acf_v  <- acf(muestras_all, lag.max = 50, plot = FALSE)$acf[-1]
      rho    <- acf_v[acf_v > 0]
      ess    <- length(muestras_all) / (1 + 2 * sum(rho))

      tasa   <- mean(sapply(chains, function(c) c$tasa_aceptacion))

      clase_rhat <- if (rhat < 1.01) "sem-ok"
                   else if (rhat < 1.05) "sem-warn" else "sem-bad"
      clase_ess  <- if (ess > 400) "sem-ok"
                   else if (ess > 100) "sem-warn" else "sem-bad"
      clase_tasa <- if (tasa > 0.2 && tasa < 0.7) "sem-ok"
                   else "sem-warn"

      tagList(
        br(),
        div(class = paste("p-2 rounded mb-2", clase_rhat),
            bs_icon(if (clase_rhat == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            strong("R\u0302 = "), round(rhat, 4), br(),
            span(class = "text-muted small",
                 if (clase_rhat == "sem-ok") "Convergencia correcta"
                 else if (clase_rhat == "sem-warn") "Convergencia marginal"
                 else "Sin convergencia")),
        div(class = paste("p-2 rounded mb-2", clase_ess),
            bs_icon(if (clase_ess == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            strong("ESS \u2248 "), round(ess, 0), br(),
            span(class = "text-muted small",
                 if (clase_ess == "sem-ok") "ESS adecuado (> 400)"
                 else if (clase_ess == "sem-warn") "ESS marginal"
                 else "ESS insuficiente")),
        div(class = paste("p-2 rounded mb-2", clase_tasa),
            bs_icon("percent", class = "me-1"),
            strong("Tasa de aceptaci\u00f3n: "), scales::percent(tasa, 1), br(),
            span(class = "text-muted small",
                 "Ideal: 20-70%. Ajusta el tama\u00f1o de paso si est\u00e1 fuera de rango.")),
        div(class = "p-2 rounded mb-0 sem-ok",
            bs_icon("calculator", class = "me-1"),
            strong("Media posterior: "), round(mean(muestras_all), 3), br(),
            strong("DE posterior: "), round(sd(muestras_all), 3))
      )
    })

    # ── Factor de Bayes ───────────────────────────────
    output$inputs_bf <- renderUI({
      switch(input$tipo_bf,
        t_una = tagList(
          p(class = "small text-muted mb-2",
            "Ingresa los datos separados por comas o espacios:"),
          textAreaInput(ns("datos_bf_x"), "Datos (x):",
                        value = "2.3, 1.8, 3.1, 2.7, 1.5, 2.9, 3.4, 2.1",
                        rows = 3),
          numericInput(ns("mu0_bf"), "H\u2080: \u03bc =", value = 0, step = 0.5)
        ),
        t_dos = tagList(
          p(class = "small text-muted mb-2", "Ingresa los dos grupos:"),
          textAreaInput(ns("datos_bf_x"), "Grupo 1:",
                        value = "2.3, 1.8, 3.1, 2.7, 1.5", rows = 2),
          textAreaInput(ns("datos_bf_y"), "Grupo 2:",
                        value = "1.2, 0.9, 1.7, 1.4, 0.8", rows = 2)
        ),
        correlacion = tagList(
          p(class = "small text-muted mb-2", "Ingresa dos variables:"),
          textAreaInput(ns("datos_bf_x"), "Variable X:",
                        value = "1.2, 2.3, 3.1, 4.0, 5.2", rows = 2),
          textAreaInput(ns("datos_bf_y"), "Variable Y:",
                        value = "2.1, 3.8, 4.2, 6.1, 7.3", rows = 2)
        ),
        regresion = tagList(
          p(class = "small text-muted mb-2", "Ingresa predictor y respuesta:"),
          textAreaInput(ns("datos_bf_x"), "Predictor (X):",
                        value = "1, 2, 3, 4, 5, 6, 7, 8", rows = 2),
          textAreaInput(ns("datos_bf_y"), "Respuesta (Y):",
                        value = "2.1, 3.8, 4.2, 6.1, 7.3, 8.9, 9.1, 11.2",
                        rows = 2)
        )
      )
    })

    parse_datos <- function(txt) {
      txt <- gsub("[,;\\s]+", " ", trimws(txt))
      as.numeric(strsplit(txt, "\\s+")[[1]])
    }

    observeEvent(input$calcular_bf, {
      req(input$tipo_bf)
      tryCatch({
        x <- parse_datos(input$datos_bf_x)
        resultado <- switch(input$tipo_bf,
          t_una = {
            mu0 <- input$mu0_bf %||% 0
            BayesFactor::ttestBF(x = x, mu = mu0,
                                  rscale = input$prior_escala_bf)
          },
          t_dos = {
            y <- parse_datos(input$datos_bf_y)
            BayesFactor::ttestBF(x = x, y = y,
                                  rscale = input$prior_escala_bf)
          },
          correlacion = {
            y <- parse_datos(input$datos_bf_y)
            BayesFactor::correlationBF(y = x, x = y,
                                        rscale = input$prior_escala_bf)
          },
          regresion = {
            y <- parse_datos(input$datos_bf_y)
            df_reg <- data.frame(x = x, y = y)
            BayesFactor::lmBF(y ~ x, data = df_reg,
                               rscale = input$prior_escala_bf)
          }
        )

        bf_val <- as.numeric(BayesFactor::extractBF(resultado)$bf)

        interpretacion <- if (bf_val > 100) "Evidencia extrema a favor de H\u2081"
          else if (bf_val > 30) "Evidencia muy fuerte a favor de H\u2081"
          else if (bf_val > 10) "Evidencia fuerte a favor de H\u2081"
          else if (bf_val > 3)  "Evidencia moderada a favor de H\u2081"
          else if (bf_val > 1)  "Evidencia anecdt\u00f3tica a favor de H\u2081"
          else if (bf_val > 1/3) "Evidencia anecdt\u00f3tica a favor de H\u2080"
          else if (bf_val > 1/10) "Evidencia moderada a favor de H\u2080"
          else "Evidencia fuerte o mayor a favor de H\u2080"

        clase <- if (bf_val > 10 || bf_val < 1/10) "sem-ok"
                 else if (bf_val > 3 || bf_val < 1/3) "sem-warn"
                 else "sem-bad"

        output$resultado_bf <- renderUI({
          tagList(
            div(class = paste("p-3 rounded mb-3", clase),
                h3(style = paste0("color:", colores$primario,
                                   "; font-weight:700;"),
                   paste0("BF\u2081\u2080 = ", round(bf_val, 3))),
                p(class = "small mb-1", strong(interpretacion)),
                p(class = "small text-muted mb-0",
                  "1/BF = ", round(1/bf_val, 3),
                  " (evidencia a favor de H\u2080)")
            ),
            div(class = "alert alert-warning small",
                bs_icon("exclamation-triangle", class = "me-1"),
                strong("Recuerda:"), " el BF es sensible al prior (r = ",
                input$prior_escala_bf, "). Cambia r y observa c\u00f3mo ",
                "var\u00eda el resultado.")
          )
        })

      }, error = function(e) {
        output$resultado_bf <- renderUI({
          div(class = "alert alert-danger small", e$message)
        })
      })
    })

    output$codigo_bf <- renderText({
      switch(input$tipo_bf,
        t_una = paste0(
          "library(BayesFactor)\n\n",
          "x <- c(2.3, 1.8, 3.1, ...)\n\n",
          "# Factor de Bayes: t de una muestra\n",
          "bf <- ttestBF(x = x, mu = ", input$mu0_bf %||% 0, ",\n",
          "              rscale = ", input$prior_escala_bf, ")\n",
          "bf\nextractBF(bf)$bf  # valor num\u00e9rico"
        ),
        t_dos = paste0(
          "library(BayesFactor)\n\n",
          "x <- c(2.3, 1.8, ...); y <- c(1.2, 0.9, ...)\n\n",
          "# Factor de Bayes: t de dos muestras\n",
          "bf <- ttestBF(x = x, y = y,\n",
          "              rscale = ", input$prior_escala_bf, ")\n",
          "bf\nextractBF(bf)$bf"
        ),
        correlacion = paste0(
          "library(BayesFactor)\n\n",
          "x <- c(1.2, 2.3, ...); y <- c(2.1, 3.8, ...)\n\n",
          "# Factor de Bayes: correlaci\u00f3n\n",
          "bf <- correlationBF(y = y, x = x,\n",
          "                    rscale = ", input$prior_escala_bf, ")\n",
          "bf\nextractBF(bf)$bf"
        ),
        regresion = paste0(
          "library(BayesFactor)\n\n",
          "datos <- data.frame(x = c(1,2,...), y = c(2.1,3.8,...))\n\n",
          "# Factor de Bayes: regresi\u00f3n lineal\n",
          "bf <- lmBF(y ~ x, data = datos,\n",
          "           rscale = ", input$prior_escala_bf, ")\n",
          "bf\nextractBF(bf)$bf"
        )
      )
    })

  })
}
