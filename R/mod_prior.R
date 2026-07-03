# ============================================================
# mod_prior.R — Distribuciones a priori
# StatBayes · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. Estadística bayesiana
#   2. ¿Qué es un prior?
#   3. Fundamentos (Teorema de Bayes)
#   4. Explorador de priors
#   5. Prior predictive check
#   6. Quiz
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_prior_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("sliders", class = "me-2"),
        "Distribuciones a priori",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "El prior representa nuestro conocimiento sobre los par\u00e1metros ",
        "antes de observar los datos. Combinado con la verosimilitud, ",
        "produce la distribuci\u00f3n posterior: ",
        strong("P(\u03b8 | datos) \u221d P(datos | \u03b8) \u00d7 P(\u03b8)"), "."
      )
    ),

    navset_card_tab(

      # ══════════════════════════════════════════════════
      # PESTAÑA 1: Estadística bayesiana
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("globe", class = "me-1"),
                        "Estad\u00edstica bayesiana"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfQu\u00e9 es la estad\u00edstica bayesiana?"),
          p(class = "small text-muted mb-3",
            "La estad\u00edstica bayesiana es un enfoque para el an\u00e1lisis de datos ",
            "que combina ", strong("informaci\u00f3n previa"), " (lo que sabemos antes de ",
            "ver los datos) con ", strong("la evidencia de los datos"),
            " para obtener conclusiones actualizadas. A diferencia del enfoque ",
            "frecuentista, en el bayesiano los par\u00e1metros son tratados como ",
            "variables aleatorias con distribuciones de probabilidad propias."
          ),

          # ── Tabla comparativa ─────────────────────────
          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Frecuentista vs. Bayesiano"),
          p(class = "small text-muted mb-2",
            "Ambos enfoques buscan aprender de los datos, pero difieren ",
            "fundamentalmente en c\u00f3mo interpretan la probabilidad y los par\u00e1metros."
          ),

          div(
            style = "overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-4",
              style = "background: #ffffff;",
              tags$thead(
                style = paste0("background:", colores$primario, "; color: #ffffff;"),
                tags$tr(
                  tags$th("Aspecto"),
                  tags$th("Frecuentista"),
                  tags$th("Bayesiano")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(strong("Par\u00e1metros")),
                  tags$td("Fijos pero desconocidos"),
                  tags$td("Variables aleatorias con distribuci\u00f3n")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Probabilidad")),
                  tags$td("Frecuencia a largo plazo"),
                  tags$td("Grado de creencia o incertidumbre")
                ),
                tags$tr(
                  tags$td(strong("Conocimiento previo")),
                  tags$td("No se incorpora formalmente"),
                  tags$td("Se incorpora mediante el prior")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Resultado")),
                  tags$td("Estimaci\u00f3n puntual + intervalo de confianza"),
                  tags$td("Distribuci\u00f3n posterior completa")
                ),
                tags$tr(
                  tags$td(strong("Intervalo")),
                  tags$td(
                    "IC 95%: en el 95% de las muestras repetidas, ",
                    "el intervalo contendr\u00e1 el par\u00e1metro"
                  ),
                  tags$td(
                    "IC credible 95%: hay 95% de probabilidad de que ",
                    "el par\u00e1metro est\u00e9 en ese intervalo"
                  )
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Inferencia")),
                  tags$td("p-valores, pruebas de hip\u00f3tesis"),
                  tags$td("Distribuci\u00f3n posterior, factor de Bayes")
                ),
                tags$tr(
                  tags$td(strong("Software")),
                  tags$td("lm, glm, lme4, mgcv"),
                  tags$td("brms, rstanarm, Stan")
                )
              )
            )
          ),

          # ── Cuándo usar bayesiano ─────────────────────
          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfCu\u00e1ndo usar el enfoque bayesiano?"),

          layout_columns(
            col_widths = c(6, 6),

            div(
              div(
                class = "card-muestreo mb-3",
                style = paste0("border-left: 4px solid ", colores$primario, ";"),
                div(class = "d-flex align-items-center gap-2 mb-2",
                    bs_icon("check-circle-fill",
                            style = paste0("color:", colores$primario,
                                           "; font-size:1.1rem")),
                    h6(class = "mb-0",
                       style = paste0("color:", colores$primario, "; font-weight:700;"),
                       "Muestras peque\u00f1as")),
                p(class = "small text-muted mb-0",
                  "Cuando los datos son escasos, incorporar conocimiento ",
                  "previo mediante el prior estabiliza las estimaciones y ",
                  "evita sobreajuste.")
              ),
              div(
                class = "card-muestreo mb-3",
                style = paste0("border-left: 4px solid ", colores$primario, ";"),
                div(class = "d-flex align-items-center gap-2 mb-2",
                    bs_icon("check-circle-fill",
                            style = paste0("color:", colores$primario,
                                           "; font-size:1.1rem")),
                    h6(class = "mb-0",
                       style = paste0("color:", colores$primario, "; font-weight:700;"),
                       "Conocimiento previo disponible")),
                p(class = "small text-muted mb-0",
                  "Estudios previos, literatura cient\u00edfica o experiencia de ",
                  "expertos pueden integrarse formalmente en el an\u00e1lisis.")
              ),
              div(
                class = "card-muestreo mb-0",
                style = paste0("border-left: 4px solid ", colores$primario, ";"),
                div(class = "d-flex align-items-center gap-2 mb-2",
                    bs_icon("check-circle-fill",
                            style = paste0("color:", colores$primario,
                                           "; font-size:1.1rem")),
                    h6(class = "mb-0",
                       style = paste0("color:", colores$primario, "; font-weight:700;"),
                       "Modelos complejos")),
                p(class = "small text-muted mb-0",
                  "Modelos jer\u00e1rquicos, con muchos par\u00e1metros o datos ",
                  "desbalanceados se benefician del enfoque bayesiano.")
              )
            ),

            div(
              div(
                class = "card-muestreo mb-3",
                style = paste0("border-left: 4px solid ", colores$secundario, ";"),
                div(class = "d-flex align-items-center gap-2 mb-2",
                    bs_icon("check-circle-fill",
                            style = paste0("color:", colores$secundario,
                                           "; font-size:1.1rem")),
                    h6(class = "mb-0",
                       style = paste0("color:", colores$secundario, "; font-weight:700;"),
                       "Incertidumbre completa")),
                p(class = "small text-muted mb-0",
                  "El posterior cuantifica la incertidumbre sobre ",
                  "todos los par\u00e1metros de forma natural, sin ",
                  "necesidad de aproximaciones asint\u00f3ticas.")
              ),
              div(
                class = "card-muestreo mb-3",
                style = paste0("border-left: 4px solid ", colores$secundario, ";"),
                div(class = "d-flex align-items-center gap-2 mb-2",
                    bs_icon("check-circle-fill",
                            style = paste0("color:", colores$secundario,
                                           "; font-size:1.1rem")),
                    h6(class = "mb-0",
                       style = paste0("color:", colores$secundario, "; font-weight:700;"),
                       "Predicciones")),
                p(class = "small text-muted mb-0",
                  "Las predicciones bayesianas integran la incertidumbre ",
                  "en los par\u00e1metros, produciendo intervalos de predicci\u00f3n ",
                  "m\u00e1s honestos.")
              ),
              div(
                class = "card-muestreo mb-0",
                style = paste0("border-left: 4px solid ", colores$secundario, ";"),
                div(class = "d-flex align-items-center gap-2 mb-2",
                    bs_icon("check-circle-fill",
                            style = paste0("color:", colores$secundario,
                                           "; font-size:1.1rem")),
                    h6(class = "mb-0",
                       style = paste0("color:", colores$secundario, "; font-weight:700;"),
                       "Comparaci\u00f3n de modelos")),
                p(class = "small text-muted mb-0",
                  "LOO, WAIC y el factor de Bayes permiten comparar modelos ",
                  "de manera m\u00e1s intuitiva que el AIC o las pruebas de raz\u00f3n ",
                  "de verosimilitud.")
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # PESTAÑA 2: ¿Qué es un prior?
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es un prior?"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "La distribuci\u00f3n a priori"),
          p(class = "small text-muted mb-3",
            "Un ", strong("prior"), " (o distribuci\u00f3n a priori) es una distribuci\u00f3n ",
            "de probabilidad que representa nuestro conocimiento o creencia sobre ",
            "un par\u00e1metro ", strong("antes"), " de observar los datos. ",
            "No es una opini\u00f3n arbitraria: debe reflejar informaci\u00f3n real ",
            "disponible, ya sea de estudios previos, teor\u00eda o conocimiento experto."
          ),

          # ── Tipos de prior ────────────────────────────
          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Tipos de distribuciones a priori"),

          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("circle-fill",
                        style = paste0("color:", colores$primario,
                                       "; font-size:0.8rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Prior informativo")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("\u00bfQu\u00e9 es?")),
                p(class = "small text-muted mb-0",
                  "Refleja conocimiento sustancial sobre el par\u00e1metro. ",
                  "Es estrecho y centrado en valores plausibles seg\u00fan ",
                  "la literatura o experiencia previa.")
              ),
              div(
                p(class = "small mb-1", strong("Ejemplo")),
                p(class = "small text-muted mb-0",
                  "Si estudios previos indican que la masa corporal de una especie ",
                  "de ave promedia 45 g con poca variaci\u00f3n, podemos expresarlo como ",
                  "una distribuci\u00f3n Normal: ",
                  tags$code("Normal(45, 5)"),
                  ", donde 45 es la media y 5 es la desviaci\u00f3n est\u00e1ndar. ",
                  "En las distribuciones a priori, el segundo par\u00e1metro de la Normal ",
                  "siempre representa la desviaci\u00f3n est\u00e1ndar — no el error est\u00e1ndar ",
                  "ni la precisi\u00f3n.")
              )
            )
          ),

          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("circle-fill",
                        style = paste0("color:", colores$secundario,
                                       "; font-size:0.8rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario, "; font-weight:700;"),
                   "Prior d\u00e9bilmente informativo")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("\u00bfQu\u00e9 es?")),
                p(class = "small text-muted mb-0",
                  "Proporciona informaci\u00f3n moderada, descartando valores ",
                  "extremos pero sin sesgar fuertemente el resultado. ",
                  "Es la opci\u00f3n recomendada en la mayor\u00eda de los casos.")
              ),
              div(
                p(class = "small mb-1", strong("Ejemplo")),
                p(class = "small text-muted mb-0",
                  "Para un coeficiente de regresi\u00f3n en escala estandarizada: ",
                  tags$code("Normal(0, 1)"), " o ",
                  tags$code("Student-t(3, 0, 2.5)"),
                  " — valores por defecto en brms.")
              )
            )
          ),

          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("circle-fill",
                        style = paste0("color:", colores$acento,
                                       "; font-size:0.8rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento, "; font-weight:700;"),
                   "Prior no informativo (difuso)")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("\u00bfQu\u00e9 es?")),
                p(class = "small text-muted mb-0",
                  "Asigna probabilidad aproximadamente igual a un rango ",
                  "muy amplio de valores. Deja que los datos hablen solos. ",
                  "Puede causar problemas num\u00e9ricos con muestras peque\u00f1as.")
              ),
              div(
                p(class = "small mb-1", strong("Ejemplo")),
                p(class = "small text-muted mb-0",
                  tags$code("Normal(0, 100)"), " o ",
                  tags$code("Uniforme(-\u221e, +\u221e)"),
                  " — en la pr\u00e1ctica rara vez recomendado.")
              )
            )
          ),

          div(
            class = "alert alert-info small mt-2 mb-0",
            bs_icon("lightbulb", class = "me-2"),
            strong("Recomendaci\u00f3n:"), " En la mayor\u00eda de los an\u00e1lisis con brms, ",
            "los priors d\u00e9bilmente informativos son la mejor opci\u00f3n de partida. ",
            "Son los valores por defecto de brms para muchos modelos."
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # PESTAÑA 3: Fundamentos (Teorema de Bayes)
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("journal-bookmark", class = "me-1"),
                        "Fundamentos"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "El teorema de Bayes"),
          p(class = "small text-muted mb-3",
            "Todo el an\u00e1lisis bayesiano descansa en una sola ecuaci\u00f3n, ",
            "el ", strong("teorema de Bayes"), ", que describe c\u00f3mo actualizar ",
            "nuestras creencias sobre un par\u00e1metro \u03b8 al observar datos:"
          ),

          div(
            class = "text-center py-3 mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-radius: 8px; border: 1px solid ",
                           colores$borde, ";"),
            p(class = "mb-1",
              style = "font-size: 1.15rem; font-weight: 600;",
              "P(\u03b8 | datos) = P(datos | \u03b8) \u00d7 P(\u03b8) / P(datos)"
            ),
            p(class = "small text-muted mb-0",
              "Posterior = Verosimilitud \u00d7 Prior / Evidencia"
            )
          ),

          # ── Los tres componentes ──────────────────────
          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Los tres componentes"),

          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-1",
                bs_icon("arrow-left-circle-fill",
                        style = paste0("color:", colores$primario)),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Prior — P(\u03b8)")),
            p(class = "small text-muted mb-0",
              "Lo que sabemos sobre el par\u00e1metro ", strong("antes"),
              " de ver los datos. Puede ser informativo (basado en estudios previos) ",
              "o d\u00e9bilmente informativo (solo descarta valores absurdos).")
          ),

          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-1",
                bs_icon("database-fill",
                        style = paste0("color:", colores$acento)),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento, "; font-weight:700;"),
                   "Verosimilitud — P(datos | \u03b8)")),
            p(class = "small text-muted mb-0",
              "Qu\u00e9 tan probable es observar los datos que tenemos, ",
              strong("dado"), " un valor espec\u00edfico del par\u00e1metro \u03b8. ",
              "Es la misma cantidad que maximiza la regresi\u00f3n frecuentista, ",
              "pero aqu\u00ed se combina con el prior.")
          ),

          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-1",
                bs_icon("arrow-right-circle-fill",
                        style = paste0("color:", colores$secundario)),
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario, "; font-weight:700;"),
                   "Posterior — P(\u03b8 | datos)")),
            p(class = "small text-muted mb-0",
              "Lo que sabemos sobre el par\u00e1metro ", strong("despu\u00e9s"),
              " de ver los datos. Es el resultado del an\u00e1lisis bayesiano: ",
              "una distribuci\u00f3n completa que refleja toda la incertidumbre ",
              "restante sobre \u03b8.")
          ),

          div(
            class = "card-muestreo mb-0",
            style = paste0("border-left: 4px solid ", colores$texto, ";"),
            div(class = "d-flex align-items-center gap-2 mb-1",
                bs_icon("slash-circle",
                        style = paste0("color:", colores$texto)),
                h6(class = "mb-0",
                   style = paste0("color:", colores$texto, "; font-weight:700;"),
                   "Evidencia — P(datos)")),
            p(class = "small text-muted mb-0",
              "Una constante de normalizaci\u00f3n que garantiza que el posterior ",
              "sume 1. En la pr\u00e1ctica, MCMC no necesita calcularla expl\u00edcitamente, ",
              "por lo que usamos la proporci\u00f3n: ",
              strong("Posterior \u221d Verosimilitud \u00d7 Prior"), ".")
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # PESTAÑA 4: Explorador de priors
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("sliders", class = "me-1"),
                        "Explorador"),
        card_body(

          p(class = "small text-muted mb-3",
            "Selecciona una distribuci\u00f3n y ajusta sus par\u00e1metros para ",
            "visualizar c\u00f3mo cambia la forma del prior. Observa qu\u00e9 valores ",
            "del par\u00e1metro considera plausibles cada configuraci\u00f3n."
          ),

          layout_columns(
            col_widths = c(4, 8),

            # ── Panel de controles ──────────────────────
            card(
              card_header(bs_icon("gear", class = "me-1"),
                          "Configuraci\u00f3n del prior"),
              card_body(

                selectInput(
                  ns("dist_prior"),
                  "Distribuci\u00f3n:",
                  choices = c(
                    "Normal"          = "normal",
                    "Student-t"       = "student_t",
                    "Cauchy"          = "cauchy",
                    "Beta"            = "beta",
                    "Gamma"           = "gamma",
                    "Exponencial"     = "exponential",
                    "Uniforme"        = "uniform"
                  ),
                  selected = "normal"
                ),

                # Parámetros dinámicos
                uiOutput(ns("params_ui")),

                hr(),

                div(
                  class = "alert alert-info small py-2 px-3 mb-0",
                  bs_icon("info-circle", class = "me-1"),
                  uiOutput(ns("info_dist"))
                )
              )
            ),

            # ── Gráfico ─────────────────────────────────
            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Distribuci\u00f3n a priori"),
              card_body(
                plotOutput(ns("plot_prior"), height = "320px"),
                hr(),
                div(
                  class = "small text-muted",
                  strong("Código brms:"),
                  verbatimTextOutput(ns("codigo_prior"))
                )
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # PESTAÑA 5: Prior predictive check
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("check2-circle", class = "me-1"),
                        "Prior predictive check"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfQu\u00e9 es un prior predictive check?"),
          p(class = "small text-muted mb-3",
            "Antes de ajustar el modelo a los datos, podemos simular datos ",
            strong("directamente desde el prior"), " para verificar que nuestra ",
            "elecci\u00f3n de priors produce predicciones razonables. Si el prior ",
            "genera datos absurdos (pesos negativos, probabilidades mayores a 1), ",
            "debemos revisarlo."
          ),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("gear", class = "me-1"),
                          "Configuraci\u00f3n"),
              card_body(

                selectInput(
                  ns("modelo_ppc"),
                  "Tipo de modelo:",
                  choices = c(
                    "Regresi\u00f3n lineal (gaussian)" = "gaussian",
                    "Regresi\u00f3n log\u00edstica (binomial)" = "binomial",
                    "Regresi\u00f3n de Poisson"         = "poisson"
                  )
                ),

                sliderInput(
                  ns("prior_intercept_mu"),
                  "Prior intercepto — media:",
                  min = -10, max = 10, value = 0, step = 0.5
                ),

                sliderInput(
                  ns("prior_intercept_sd"),
                  "Prior intercepto — DE:",
                  min = 0.5, max = 10, value = 2.5, step = 0.5
                ),

                sliderInput(
                  ns("prior_beta_sd"),
                  "Prior coeficiente — DE:",
                  min = 0.5, max = 10, value = 1, step = 0.5
                ),

                sliderInput(
                  ns("n_sim_ppc"),
                  "N\u00famero de simulaciones:",
                  min = 50, max = 500, value = 200, step = 50
                ),

                actionButton(
                  ns("btn_ppc"),
                  "Simular",
                  icon  = icon("play"),
                  class = "btn-primary w-100"
                )
              )
            ),

            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Datos simulados desde el prior"),
              card_body(
                plotOutput(ns("plot_ppc"), height = "320px"),
                uiOutput(ns("msg_ppc"))
              )
            )
          )
        )
      ),

      # ══════════════════════════════════════════════════
      # PESTAÑA 6: Quiz
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("patch-question", class = "me-1"), "Quiz"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Pon a prueba tu comprensi\u00f3n"),
          p(class = "small text-muted mb-4",
            "Selecciona la respuesta correcta en cada pregunta."),

          # ── Pregunta 1 ──────────────────────────────
          div(
            class = "mb-4",
            p(class = "small mb-2", strong(
              "1. \u00bfQu\u00e9 representa la distribuci\u00f3n a priori en el an\u00e1lisis bayesiano?"
            )),
            div(id = ns("q1"),
                div(class = "quiz-opt", id = ns("q1_a"),
                    onclick = paste0("Shiny.setInputValue('", ns("q1_resp"), "', 'a', {priority: 'event'})"),
                    "a) La distribuci\u00f3n de los datos observados"),
                div(class = "quiz-opt", id = ns("q1_b"),
                    onclick = paste0("Shiny.setInputValue('", ns("q1_resp"), "', 'b', {priority: 'event'})"),
                    "b) El conocimiento sobre el par\u00e1metro antes de ver los datos"),
                div(class = "quiz-opt", id = ns("q1_c"),
                    onclick = paste0("Shiny.setInputValue('", ns("q1_resp"), "', 'c', {priority: 'event'})"),
                    "c) La probabilidad de los datos dado el modelo"),
                div(class = "quiz-opt", id = ns("q1_d"),
                    onclick = paste0("Shiny.setInputValue('", ns("q1_resp"), "', 'd', {priority: 'event'})"),
                    "d) El resultado final del an\u00e1lisis")
            ),
            uiOutput(ns("fb_q1"))
          ),

          # ── Pregunta 2 ──────────────────────────────
          div(
            class = "mb-4",
            p(class = "small mb-2", strong(
              "2. Un prior d\u00e9bilmente informativo Normal(0, 1) para un coeficiente estandarizado significa que:"
            )),
            div(id = ns("q2"),
                div(class = "quiz-opt", id = ns("q2_a"),
                    onclick = paste0("Shiny.setInputValue('", ns("q2_resp"), "', 'a', {priority: 'event'})"),
                    "a) El coeficiente debe ser exactamente 0"),
                div(class = "quiz-opt", id = ns("q2_b"),
                    onclick = paste0("Shiny.setInputValue('", ns("q2_resp"), "', 'b', {priority: 'event'})"),
                    "b) Esperamos que el coeficiente est\u00e9 cerca de 0, pero valores hasta \u00b12 son plausibles"),
                div(class = "quiz-opt", id = ns("q2_c"),
                    onclick = paste0("Shiny.setInputValue('", ns("q2_resp"), "', 'c', {priority: 'event'})"),
                    "c) No sabemos nada sobre el coeficiente"),
                div(class = "quiz-opt", id = ns("q2_d"),
                    onclick = paste0("Shiny.setInputValue('", ns("q2_resp"), "', 'd', {priority: 'event'})"),
                    "d) El coeficiente sigue una distribuci\u00f3n uniforme")
            ),
            uiOutput(ns("fb_q2"))
          ),

          # ── Pregunta 3 ──────────────────────────────
          div(
            class = "mb-4",
            p(class = "small mb-2", strong(
              "3. \u00bfCu\u00e1l es el prop\u00f3sito del prior predictive check?"
            )),
            div(id = ns("q3"),
                div(class = "quiz-opt", id = ns("q3_a"),
                    onclick = paste0("Shiny.setInputValue('", ns("q3_resp"), "', 'a', {priority: 'event'})"),
                    "a) Verificar que el modelo ajusta bien los datos observados"),
                div(class = "quiz-opt", id = ns("q3_b"),
                    onclick = paste0("Shiny.setInputValue('", ns("q3_resp"), "', 'b', {priority: 'event'})"),
                    "b) Simular datos desde el prior para verificar que produce predicciones razonables"),
                div(class = "quiz-opt", id = ns("q3_c"),
                    onclick = paste0("Shiny.setInputValue('", ns("q3_resp"), "', 'c', {priority: 'event'})"),
                    "c) Calcular el p-valor del modelo"),
                div(class = "quiz-opt", id = ns("q3_d"),
                    onclick = paste0("Shiny.setInputValue('", ns("q3_resp"), "', 'd', {priority: 'event'})"),
                    "d) Comparar modelos con AIC")
            ),
            uiOutput(ns("fb_q3"))
          )
        )
      )
    )
  )
}

# ── SERVER ────────────────────────────────────────────────
mod_prior_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Explorador: parámetros dinámicos ──────────────
    output$params_ui <- renderUI({
      switch(input$dist_prior,
        normal = tagList(
          sliderInput(ns("p1"), "Media (\u03bc):",  min = -10, max = 10, value = 0,   step = 0.5),
          sliderInput(ns("p2"), "DE (\u03c3):",     min = 0.1, max = 10, value = 1,   step = 0.1)
        ),
        student_t = tagList(
          sliderInput(ns("p1"), "Grados de libertad (\u03bd):", min = 1, max = 30, value = 3, step = 1),
          sliderInput(ns("p2"), "Media (\u03bc):",  min = -10, max = 10, value = 0,   step = 0.5),
          sliderInput(ns("p3"), "Escala (\u03c3):", min = 0.1, max = 10, value = 2.5, step = 0.1)
        ),
        cauchy = tagList(
          sliderInput(ns("p1"), "Ubicaci\u00f3n (x\u2080):", min = -10, max = 10, value = 0, step = 0.5),
          sliderInput(ns("p2"), "Escala (\u03b3):",          min = 0.1, max = 10, value = 1, step = 0.1)
        ),
        beta = tagList(
          sliderInput(ns("p1"), "Forma \u03b1:", min = 0.1, max = 20, value = 2, step = 0.1),
          sliderInput(ns("p2"), "Forma \u03b2:", min = 0.1, max = 20, value = 2, step = 0.1)
        ),
        gamma = tagList(
          sliderInput(ns("p1"), "Forma (\u03b1):", min = 0.1, max = 20, value = 2,   step = 0.1),
          sliderInput(ns("p2"), "Tasa (\u03b2):",  min = 0.1, max = 10, value = 0.5, step = 0.1)
        ),
        exponential = tagList(
          sliderInput(ns("p1"), "Tasa (\u03bb):", min = 0.1, max = 10, value = 1, step = 0.1)
        ),
        uniform = tagList(
          sliderInput(ns("p1"), "M\u00ednimo (a):", min = -20, max = 0,  value = -10, step = 1),
          sliderInput(ns("p2"), "M\u00e1ximo (b):", min = 0,   max = 20, value = 10,  step = 1)
        )
      )
    })

    # ── Explorador: información de la distribución ────
    output$info_dist <- renderUI({
      switch(input$dist_prior,

        normal = tagList(
          strong("Distribuci\u00f3n Normal"), " — la m\u00e1s com\u00fan para coeficientes de regresi\u00f3n.",
          tags$ul(class = "mb-0 mt-1",
            tags$li(strong("\u03bc (media):"), " valor central del prior, el efecto m\u00e1s esperado."),
            tags$li(strong("\u03c3 (desviaci\u00f3n est\u00e1ndar):"), " controla la amplitud. Valores grandes ",
                    "= prior m\u00e1s difuso; valores peque\u00f1os = prior m\u00e1s concentrado.")
          )
        ),

        student_t = tagList(
          strong("Distribuci\u00f3n Student-t"), " — m\u00e1s robusta que la Normal, con colas m\u00e1s pesadas.",
          tags$ul(class = "mb-0 mt-1",
            tags$li(strong("\u03bd (grados de libertad):"), " controla el grosor de las colas. ",
                    "Valores bajos (ej. 3) producen colas muy pesadas, permitiendo efectos grandes. ",
                    "A medida que \u03bd aumenta, se aproxima a la Normal."),
            tags$li(strong("\u03bc (media):"), " valor central del prior."),
            tags$li(strong("\u03c3 (escala):"), " similar a la desviaci\u00f3n est\u00e1ndar pero no id\u00e9ntica; ",
                    "controla la amplitud sin determinar la varianza exacta, ya que esta depende tambi\u00e9n de \u03bd.")
          )
        ),

        cauchy = tagList(
          strong("Distribuci\u00f3n de Cauchy"), " — colas muy pesadas. \u00datil cuando se esperan efectos grandes o at\u00edpicos.",
          tags$ul(class = "mb-0 mt-1",
            tags$li(strong("x\u2080 (ubicaci\u00f3n):"), " valor central del prior (equivalente a la media en la Normal)."),
            tags$li(strong("\u03b3 (escala):"), " controla la amplitud. La escala en Cauchy ",
                    "no equivale a la desviaci\u00f3n est\u00e1ndar — la Cauchy no tiene media ni varianza definidas.")
          )
        ),

        beta = tagList(
          strong("Distribuci\u00f3n Beta"), " — definida en [0, 1]. Ideal para proporciones y probabilidades.",
          tags$ul(class = "mb-0 mt-1",
            tags$li(strong("\u03b1 (forma 1):"), " junto con \u03b2 determina la forma. ",
                    "Con \u03b1 = \u03b2 la distribuci\u00f3n es sim\u00e9trica; \u03b1 > \u03b2 desplaza la masa hacia 1."),
            tags$li(strong("\u03b2 (forma 2):"), " \u03b2 > \u03b1 desplaza la masa hacia 0. ",
                    "Con \u03b1 = \u03b2 = 1 se obtiene una Uniforme(0,1). ",
                    "Valores grandes de ambos producen una distribuci\u00f3n m\u00e1s concentrada.")
          )
        ),

        gamma = tagList(
          strong("Distribuci\u00f3n Gamma"), " — definida en (0, \u221e). Para par\u00e1metros estrictamente positivos.",
          tags$ul(class = "mb-0 mt-1",
            tags$li(strong("\u03b1 (forma):"), " controla la asimetr\u00eda. Valores peque\u00f1os producen ",
                    "distribuciones muy sesgadas a la derecha; valores grandes la hacen m\u00e1s sim\u00e9trica."),
            tags$li(strong("\u03b2 (tasa):"), " inverso de la escala. Valores grandes de \u03b2 ",
                    "concentran la distribuci\u00f3n cerca de cero; valores peque\u00f1os la hacen m\u00e1s dispersa. ",
                    "La media de la distribuci\u00f3n es \u03b1/\u03b2.")
          )
        ),

        exponential = tagList(
          strong("Distribuci\u00f3n Exponencial"), " — definida en (0, \u221e). Caso especial de la Gamma con \u03b1 = 1.",
          tags$ul(class = "mb-0 mt-1",
            tags$li(strong("\u03bb (tasa):"), " controla la velocidad de decaimiento. ",
                    "La media de la distribuci\u00f3n es 1/\u03bb. Un valor \u03bb = 1 produce una media de 1; ",
                    "\u03bb = 0.5 produce una media de 2. Valores grandes de \u03bb concentran la masa cerca de cero.")
          )
        ),

        uniform = tagList(
          strong("Distribuci\u00f3n Uniforme"), " — asigna la misma probabilidad a todos los valores en el rango [a, b].",
          tags$ul(class = "mb-0 mt-1",
            tags$li(strong("a (m\u00ednimo):"), " valor m\u00ednimo posible del par\u00e1metro."),
            tags$li(strong("b (m\u00e1ximo):"), " valor m\u00e1ximo posible del par\u00e1metro.")
          ),
          tags$p(class = "mb-0 mt-1",
                 "\u26a0\ufe0f Generalmente no recomendada: puede causar problemas num\u00e9ricos y ",
                 "produce resultados poco confiables con muestras peque\u00f1as.")
        )
      )
    })

    # ── Explorador: gráfico ───────────────────────────
    output$plot_prior <- renderPlot({

      req(input$dist_prior)

      x_range <- switch(input$dist_prior,
        normal      = seq(-10, 10, length.out = 500),
        student_t   = seq(-15, 15, length.out = 500),
        cauchy      = seq(-15, 15, length.out = 500),
        beta        = seq(0.001, 0.999, length.out = 500),
        gamma       = seq(0.001, 20,   length.out = 500),
        exponential = seq(0.001, 10,   length.out = 500),
        uniform     = {
          a <- input$p1 %||% -10
          b <- input$p2 %||%  10
          seq(a - 1, b + 1, length.out = 500)
        }
      )

      y_vals <- switch(input$dist_prior,
        normal      = dnorm(x_range, input$p1 %||% 0, input$p2 %||% 1),
        student_t   = {
          nu <- input$p1 %||% 3
          mu <- input$p2 %||% 0
          sg <- input$p3 %||% 2.5
          dt((x_range - mu) / sg, df = nu) / sg
        },
        cauchy      = dcauchy(x_range, input$p1 %||% 0, input$p2 %||% 1),
        beta        = dbeta(x_range,   input$p1 %||% 2, input$p2 %||% 2),
        gamma       = dgamma(x_range,  shape = input$p1 %||% 2,
                                        rate  = input$p2 %||% 0.5),
        exponential = dexp(x_range, rate = input$p1 %||% 1),
        uniform     = dunif(x_range, input$p1 %||% -10, input$p2 %||% 10)
      )

      df_plot <- data.frame(x = x_range, y = y_vals)

      ggplot(df_plot, aes(x = x, y = y)) +
        geom_area(fill = colores$primario, alpha = 0.2) +
        geom_line(color = colores$primario, linewidth = 1) +
        labs(x = "Valor del parámetro", y = "Densidad") +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          plot.background  = element_rect(fill = colores$fondo, color = NA)
        )
    })

    # ── Explorador: código brms ───────────────────────
    output$codigo_prior <- renderText({
      switch(input$dist_prior,
        normal      = paste0('prior(normal(', input$p1 %||% 0, ', ',
                              input$p2 %||% 1, '), class = b)'),
        student_t   = paste0('prior(student_t(', input$p1 %||% 3, ', ',
                              input$p2 %||% 0, ', ', input$p3 %||% 2.5,
                              '), class = b)'),
        cauchy      = paste0('prior(cauchy(', input$p1 %||% 0, ', ',
                              input$p2 %||% 1, '), class = b)'),
        beta        = paste0('prior(beta(', input$p1 %||% 2, ', ',
                              input$p2 %||% 2, '), class = b)'),
        gamma       = paste0('prior(gamma(', input$p1 %||% 2, ', ',
                              input$p2 %||% 0.5, '), class = b)'),
        exponential = paste0('prior(exponential(', input$p1 %||% 1,
                              '), class = sigma)'),
        uniform     = paste0('prior(uniform(', input$p1 %||% -10, ', ',
                              input$p2 %||% 10, '), class = b)')
      )
    })

    # ── Prior predictive check ────────────────────────
    ppc_data <- eventReactive(input$btn_ppc, {

      n    <- input$n_sim_ppc
      mu_i <- input$prior_intercept_mu
      sd_i <- input$prior_intercept_sd
      sd_b <- input$prior_beta_sd
      x    <- rnorm(100)

      replicate(n, {
        intercept <- rnorm(1, mu_i, sd_i)
        beta      <- rnorm(1, 0, sd_b)
        eta       <- intercept + beta * x

        switch(input$modelo_ppc,
          gaussian  = rnorm(100, eta, 1),
          binomial  = rbinom(100, 1, plogis(eta)),
          poisson   = rpois(100, exp(pmax(pmin(eta, 10), -10)))
        )
      })
    })

    output$plot_ppc <- renderPlot({
      req(ppc_data())
      mat  <- ppc_data()
      df   <- data.frame(
        y    = as.vector(mat),
        sim  = rep(seq_len(ncol(mat)), each = nrow(mat))
      )
      n_show <- ncol(mat)
      df_sub <- df

      ggplot(df_sub, aes(x = y, group = sim)) +
        geom_density(color = colores$primario, alpha = 0.1,
                     linewidth = 0.3) +
        labs(
          x = "Valores simulados",
          y = "Densidad",
          subtitle = paste0(n_show, " distribuciones simuladas desde el prior")
        ) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          plot.background  = element_rect(fill = colores$fondo, color = NA)
        )
    })

    output$msg_ppc <- renderUI({
      req(ppc_data())
      mat    <- ppc_data()
      rango  <- range(mat, na.rm = TRUE)
      clase  <- if (abs(rango[1]) > 1e4 || abs(rango[2]) > 1e4) "sem-bad"
                else if (abs(rango[1]) > 100 || abs(rango[2]) > 100) "sem-warn"
                else "sem-ok"
      icono  <- if (clase == "sem-ok") "check-circle-fill"
                else if (clase == "sem-warn") "exclamation-triangle-fill"
                else "x-circle-fill"
      msg    <- if (clase == "sem-ok")
                  "Los datos simulados tienen rangos razonables. El prior parece adecuado."
                else if (clase == "sem-warn")
                  "Los datos simulados tienen rangos amplios. Considera un prior más restrictivo."
                else
                  "Los datos simulados tienen rangos extremos. El prior es demasiado difuso."

      div(
        class = paste("small p-2 mt-2 rounded", clase),
        bs_icon(icono, class = "me-1"),
        msg,
        br(),
        span(class = "text-muted",
             paste0("Rango simulado: [",
                    round(rango[1], 2), ", ",
                    round(rango[2], 2), "]"))
      )
    })

    # ── Quiz ──────────────────────────────────────────
    correctas <- c(q1 = "b", q2 = "b", q3 = "b")

    explicaciones <- list(
      q1 = "El prior representa el conocimiento sobre el parámetro ANTES de ver los datos. El posterior lo actualiza después.",
      q2 = "Normal(0,1) significa que esperamos valores cercanos a 0, pero no imposibilita efectos moderados (±2 DE cubren ~95%).",
      q3 = "El prior predictive check simula datos ANTES de ajustar el modelo, para verificar que el prior tiene sentido."
    )

    lapply(1:3, function(i) {
      q_id <- paste0("q", i)
      observeEvent(input[[paste0(q_id, "_resp")]], {
        resp     <- input[[paste0(q_id, "_resp")]]
        correcta <- correctas[[q_id]]
        es_ok    <- resp == correcta

        output[[paste0("fb_", q_id)]] <- renderUI({
          div(
            class = paste("small p-2 rounded mt-1",
                          if (es_ok) "sem-ok" else "sem-bad"),
            bs_icon(if (es_ok) "check-circle-fill" else "x-circle-fill",
                    class = "me-1"),
            if (es_ok) strong("¡Correcto! ") else
              strong(paste0("Incorrecto. La respuesta correcta es (", correcta, "). ")),
            explicaciones[[q_id]]
          )
        })
      })
    })

  })
}
