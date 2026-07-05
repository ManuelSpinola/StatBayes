# ============================================================
# mod_lm_bayes.R — Regresión lineal bayesiana
# StatBayes · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1.  ¿Qué es?
#   2.  Fundamentos
#   3.  Los datos (Datos de ejemplo + Mis datos + Tipos de variables)
#   4.  Explorar
#   5.  Priors
#   6.  Ajustar modelo
#   7.  Diagnóstico MCMC
#   8.  Performance
#   9.  Parámetros
#   10. Gráficos
#   11. Efectos marginales
#   12. Comparar modelos
#   13. Código R
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_lm_bayes_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("graph-up", class = "me-2"),
        "Regresión lineal bayesiana",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Versión bayesiana del modelo lineal general: misma ecuación ",
        strong("Y = β₀ + β₁X₁ + … + ε"),
        ", pero ahora los parámetros tienen distribuciones de probabilidad ",
        "completas. El resultado es una distribución posterior que cuantifica ",
        "la incertidumbre sobre cada coeficiente."
      )
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Variable respuesta en la regresión lineal bayesiana"),
          p(class = "small text-muted mb-3",
            "Al igual que en el modelo lineal frecuentista, la variable ",
            "respuesta (Y) debe ser ", strong("numérica y continua"),
            " — como el peso en gramos, la temperatura o la abundancia de ",
            "una especie. La diferencia es que ahora los coeficientes no son ",
            "valores fijos: son distribuciones de probabilidad que reflejan ",
            "nuestra incertidumbre sobre el verdadero efecto."
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Frecuentista vs. bayesiano en la regresión lineal"),
          p(class = "small text-muted mb-2",
            "Ambos enfoques estiman los mismos coeficientes β, pero la ",
            "interpretación y el resultado son fundamentalmente distintos."
          ),

          div(
            style = "overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-4",
              style = "background: #ffffff;",
              tags$thead(
                style = paste0("background:", colores$primario,
                               "; color: #ffffff;"),
                tags$tr(
                  tags$th("Aspecto"),
                  tags$th("Frecuentista (lm)"),
                  tags$th("Bayesiano (brms)")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(strong("Resultado de β")),
                  tags$td("Estimación puntual + IC 95%"),
                  tags$td("Distribución posterior completa")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Intervalo")),
                  tags$td("IC 95%: en el 95% de muestras repetidas, contiene β"),
                  tags$td("IC credible 95%: hay 95% de prob. de que β esté ahí")
                ),
                tags$tr(
                  tags$td(strong("Prior")),
                  tags$td("No se usa"),
                  tags$td("Se especifica para cada parámetro")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Ajuste")),
                  tags$td("Mínimos cuadrados / máxima verosimilitud"),
                  tags$td("MCMC (cadenas de Markov Monte Carlo)")
                ),
                tags$tr(
                  tags$td(strong("Comparación")),
                  tags$td("AIC, BIC, R²"),
                  tags$td("LOO, WAIC, R² bayesiano")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Velocidad")),
                  tags$td("Muy rápido"),
                  tags$td("Más lento (minutos por modelo)")
                )
              )
            )
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "¿Cuándo usar la regresión lineal bayesiana?"),

          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("check-circle-fill", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Muestra pequeña"), br(),
              "Los priors estabilizan las estimaciones cuando n es pequeño."
            ),
            div(
              class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("check-circle-fill", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Conocimiento previo"), br(),
              "Tienes estudios anteriores que informan el rango plausible de β."
            ),
            div(
              class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("check-circle-fill", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Incertidumbre completa"), br(),
              "Necesitas la distribución completa del efecto, no solo un punto."
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "¿Cuándo NO usar la regresión lineal bayesiana?"),
          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Y binaria (sí/no)"), br(),
              "Usa ", strong("GLM bayesiano con familia binomial"), "."
            ),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Y = conteos (0, 1, 2…)"), br(),
              "Usa ", strong("GLM bayesiano con familia Poisson"), "."
            ),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Datos agrupados o repetidos"), br(),
              "Usa ", strong("modelos mixtos bayesianos"), "."
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("journal-bookmark", class = "me-1"),
                        "Fundamentos"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Supuestos de la regresión lineal bayesiana"),
          p(class = "small text-muted mb-3",
            "Los supuestos sobre la estructura de los datos son los mismos ",
            "que en la regresión frecuentista. La diferencia es que en el ",
            "enfoque bayesiano ", strong("también especificamos supuestos sobre los parámetros"),
            " mediante los priors. Si un supuesto falla, el ",
            strong("posterior predictive check"), " en la pestaña ",
            strong("Diagnóstico MCMC"), " lo revelará."
          ),

          # Supuesto 1
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("graph-up",
                        style = paste0("color:", colores$primario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "1. Linealidad")),
            layout_columns(
              col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "La relación entre cada predictor (X) y la variable respuesta (Y) ",
                "debe ser aproximadamente lineal. Una curva no lineal viola este supuesto."),
              p(class = "small text-muted mb-0",
                strong("¿Cómo verificarlo?"), " Gráfico de residuos vs. valores ajustados ",
                "y posterior predictive check. Si falla: transforma X o usa un GAM bayesiano.")
            )
          ),

          # Supuesto 2
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("bar-chart",
                        style = paste0("color:", colores$secundario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario, "; font-weight:700;"),
                   "2. Normalidad de los residuos")),
            layout_columns(
              col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "Los errores del modelo deben seguir una distribución normal. ",
                strong("No es Y quien debe ser normal"), ", sino los residuos. ",
                "En Bayes esto se especifica explícitamente: Y ~ Normal(μ, σ)."),
              p(class = "small text-muted mb-0",
                strong("¿Cómo verificarlo?"), " Q-Q plot de residuos y posterior ",
                "predictive check. Si falla: considera una familia Student-t para ",
                "manejar valores atípicos.")
            )
          ),

          # Supuesto 3
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("arrows-expand",
                        style = paste0("color:", colores$acento,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento, "; font-weight:700;"),
                   "3. Homocedasticidad")),
            layout_columns(
              col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "La varianza de los errores debe ser constante en toda la escala ",
                "de predicción. La heterocedasticidad produce intervalos credibles ",
                "poco confiables."),
              p(class = "small text-muted mb-0",
                strong("¿Cómo verificarlo?"), " Scale-location plot. Si falla: ",
                "transforma Y o modela σ como función de los predictores ",
                "(posible con brms usando bf(..., sigma ~ ...)).")
            )
          ),

          # Supuesto 4
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$peligro, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("link-45deg",
                        style = paste0("color:", colores$peligro,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$peligro, "; font-weight:700;"),
                   "4. Independencia de los errores")),
            layout_columns(
              col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "Cada observación debe ser independiente de las demás. ",
                "Se viola con medidas repetidas, datos temporales o espaciales, ",
                "o individuos del mismo grupo."),
              p(class = "small text-muted mb-0",
                strong("Si falla:"), " usa modelos mixtos bayesianos para ",
                "manejar la estructura jerárquica o de agrupamiento.")
            )
          ),

          # Supuesto 5 — Prior (exclusivo Bayes)
          div(
            class = "card-muestreo mb-0",
            style = paste0("border-left: 4px solid #9F8B75;"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("sliders",
                        style = "color:#9F8B75; font-size:1.1rem"),
                h6(class = "mb-0",
                   style = "color:#9F8B75; font-weight:700;",
                   "5. Especificación del prior (exclusivo Bayes)")),
            layout_columns(
              col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "Los priors deben ser coherentes con el problema. Un prior ",
                "informativo basado en literatura previa mejora las estimaciones. ",
                "Un prior demasiado difuso puede producir resultados inestables ",
                "con muestras pequeñas."),
              p(class = "small text-muted mb-0",
                strong("¿Cómo verificarlo?"), " Prior predictive check en la ",
                "pestaña ", strong("Priors"), ": simula datos desde el prior ",
                "antes de ajustar el modelo para verificar que son razonables.")
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(
          navset_pill(

            # ── Sub 1: Datos de ejemplo ─────────────
            nav_panel(
              title = tagList(bs_icon("collection", class = "me-1"),
                              "Datos de ejemplo"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                div(
                  radioButtons(
                    ns("fuente_datos_lmb"),
                    label   = tagList(bs_icon("database", class = "me-1"),
                                      "Seleccionar dataset:"),
                    choices = c(
                      "Densidad de especie de ave (Loyn, 1987)"    = "birdabundance_lm",
                      "Peso al nacer \u2014 salud perinatal (Hosmer)" = "birthwt_lm"
                    ),
                    selected = "birdabundance_lm"
                  ),
                  tags$hr(),
                  uiOutput(ns("info_dataset_lmb"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_lmb")),
                    br(),
                    DTOutput(ns("tabla_preview_lmb"))
                  )
                )
              )
            ),

            # ── Sub 2: Mis datos ─────────────────────
            nav_panel(
              title = tagList(bs_icon("folder2-open", class = "me-1"),
                              "Mis datos"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                div(
                  p(class = "small text-muted mb-3",
                    bs_icon("info-circle", class = "me-1"),
                    "Sube un archivo CSV o Excel. ",
                    "La primera fila debe contener los nombres de las columnas."),
                  fileInput(
                    ns("archivo_lmb"),
                    label       = "Seleccionar archivo:",
                    accept      = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Buscar\u2026",
                    placeholder = "CSV o Excel"
                  ),
                  selectInput(
                    ns("separador_lmb"),
                    label   = "Separador (CSV):",
                    choices = c(
                      "Coma (,)"         = ",",
                      "Punto y coma (;)" = ";",
                      "Tabulador"        = "\t"
                    )
                  ),
                  tags$hr(),
                  uiOutput(ns("resumen_datos_propio_lmb"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_propio_lmb")),
                    br(),
                    DTOutput(ns("tabla_preview_propio_lmb"))
                  )
                )
              )
            ),

            # ── Sub 3: Tipos de variables ────────────
            nav_panel(
              title = tagList(bs_icon("sliders2", class = "me-1"),
                              "Tipos de variables"),
              br(),
              p(class = "small text-muted mb-3",
                "Verifica que cada variable tenga el tipo correcto. ",
                "Las variables ", strong("categ\u00f3ricas"),
                " deben ser ", strong("Factor"), ". ",
                "Las variables codificadas como n\u00fameros pero que ",
                "representan grupos deben cambiarse a Factor antes de modelar."
              ),
              layout_columns(
                col_widths = c(10, 2),
                uiOutput(ns("tabla_tipos_lmb")),
                div(
                  class = "pt-2",
                  actionButton(ns("aplicar_tipos_lmb"), "Aplicar tipos",
                               class = "btn-primary w-100",
                               icon  = icon("check")),
                  br(), br(),
                  actionButton(ns("resetear_tipos_lmb"), "Restaurar",
                               class = "btn-outline-secondary w-100 btn-sm",
                               icon  = icon("rotate-left"))
                )
              ),
              uiOutput(ns("tipos_aplicados_msg_lmb"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Explorar
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("zoom-in", class = "me-1"), "Explorar"),
        card_body(
          p(class = "small text-muted mb-3",
            "Visualiza las relaciones entre variables antes de ajustar el modelo. ",
            "Ayuda a identificar predictores relevantes y elegir priors adecuados."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_x_lmb")),
                uiOutput(ns("sel_color_lmb")),
                checkboxInput(ns("mostrar_linea_lmb"),
                              "Mostrar línea de regresión",
                              value = TRUE),
                checkboxInput(ns("linea_por_grupo_lmb"),
                              "Línea por grupo (si hay color)",
                              value = FALSE),
                tags$hr(),
                uiOutput(ns("cards_correlacion_lmb"))
              )
            ),
            div(
              plotOutput(ns("plot_scatter_lmb"), height = "380px"),
              uiOutput(ns("insight_scatter_lmb"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Priors
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("sliders", class = "me-1"), "Priors"),
        card_body(

          p(class = "small text-muted mb-3",
            "Define las distribuciones a priori para los parámetros del modelo. ",
            "Los priors por defecto son ", strong("débilmente informativos"),
            " y funcionan bien en la mayoría de los casos. ",
            "Ajústalos si tienes conocimiento previo sobre el rango plausible ",
            "de los efectos."
          ),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("gear", class = "me-1"),
                          "Configuración de priors"),
              card_body(

                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Intercepto — β₀"),
                selectInput(
                  ns("prior_intercept_dist_lmb"),
                  "Distribución:",
                  choices = c("Normal" = "normal",
                              "Student-t" = "student_t",
                              "Cauchy" = "cauchy"),
                  selected = "student_t"
                ),
                fluidRow(
                  column(6, numericInput(ns("prior_intercept_mu_lmb"),
                               "Media:", value = 0, step = 0.5)),
                  column(6, numericInput(ns("prior_intercept_sd_lmb"),
                               "Escala:", value = 2.5, min = 0.1, step = 0.5))
                ),

                tags$hr(),

                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Coeficientes — β"),
                selectInput(
                  ns("prior_b_dist_lmb"),
                  "Distribución:",
                  choices = c("Normal" = "normal",
                              "Student-t" = "student_t",
                              "Cauchy" = "cauchy"),
                  selected = "normal"
                ),
                fluidRow(
                  column(6, numericInput(ns("prior_b_mu_lmb"),
                               "Media:", value = 0, step = 0.5)),
                  column(6, numericInput(ns("prior_b_sd_lmb"),
                               "DE:", value = 1, min = 0.1, step = 0.5))
                ),

                tags$hr(),

                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Error residual — σ"),
                selectInput(
                  ns("prior_sigma_dist_lmb"),
                  "Distribución:",
                  choices = c("Exponencial" = "exponential",
                              "Cauchy (semi)" = "cauchy",
                              "Student-t (semi)" = "student_t"),
                  selected = "exponential"
                ),
                numericInput(ns("prior_sigma_rate_lmb"),
                             "Tasa (λ):", value = 1, min = 0.1, step = 0.1),

                tags$hr(),
                actionButton(
                  ns("ver_ppc_lmb"),
                  "Prior predictive check",
                  icon  = icon("eye"),
                  class = "btn-outline-primary w-100 btn-sm"
                )
              )
            ),

            div(
              card(
                class = "mb-3",
                card_header(bs_icon("code-slash", class = "me-1"),
                            "Código de priors"),
                card_body(
                  verbatimTextOutput(ns("codigo_priors_lmb"))
                )
              ),
              card(
                class = "mb-0",
                card_header(bs_icon("eye", class = "me-1"),
                            "Prior predictive check",
                            span(class = "text-muted small ms-2",
                                 "— datos simulados desde el prior")),
                card_body(
                  plotOutput(ns("plot_ppc_prior_lmb"), height = "280px"),
                  uiOutput(ns("msg_ppc_prior_lmb"))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 6: Ajustar modelo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("gear", class = "me-1"), "Ajustar modelo"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("toggles", class = "me-1"),
                          "Especificar el modelo"),
              card_body(
                p(class = "small text-muted",
                  "Selecciona la variable respuesta y los predictores. ",
                  "El modelo se ajusta con MCMC — puede tardar unos minutos."),
                uiOutput(ns("sel_var_y_lmb")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores numéricos"),
                uiOutput(ns("checks_numericos_lmb")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores categóricos"),
                uiOutput(ns("checks_categoricos_lmb")),
                tags$hr(),
                conditionalPanel(
                  condition = paste0(
                    "(input['", ns("preds_num_lmb"), "'] !== null && ",
                    "input['", ns("preds_num_lmb"), "'].length + ",
                    "(input['", ns("preds_cat_lmb"), "'] !== null ? ",
                    "input['", ns("preds_cat_lmb"), "'].length : 0)) >= 2"
                  ),
                  div(
                    p(class = "small fw-bold text-muted mb-1",
                      bs_icon("diagram-2", class = "me-1"),
                      "Interacciones (opcional)"),
                    uiOutput(ns("checks_interacciones_lmb")),
                    tags$hr()
                  )
                ),
                checkboxInput(
                  ns("estandarizar_lmb"),
                  label = tagList(
                    "Estandarizar predictores numéricos",
                    tags$small(class = "text-muted d-block mt-1",
                               "Permite comparar el peso relativo de β en unidades de DE.")
                  ),
                  value = FALSE
                ),
                tags$hr(),
                # Opciones MCMC
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   bs_icon("activity", class = "me-1"), "Opciones MCMC"),
                fluidRow(
                  column(6, numericInput(ns("mcmc_chains_lmb"), "Cadenas:",
                               value = 4, min = 1, max = 8)),
                  column(6, numericInput(ns("mcmc_iter_lmb"), "Iteraciones:",
                               value = 2000, min = 500, max = 10000, step = 500))
                ),
                actionButton(
                  ns("ajustar_lmb"),
                  "Ajustar modelo",
                  class = "btn-primary w-100",
                  icon  = icon("play")
                ),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  bs_icon("floppy", class = "me-1"), "Guardar para comparar"),
                p(class = "small text-muted mb-2",
                  "Dale un nombre y guárdalo. Cambia los predictores, ",
                  "reajusta y guarda otro para comparar en ",
                  strong("Comparar modelos"), "."),
                textInput(ns("nombre_modelo_lmb"), label = NULL,
                          placeholder = "Ej: solo_area, area+habitat…"),
                actionButton(
                  ns("guardar_modelo_lmb"),
                  "Guardar modelo",
                  class = "btn-outline-primary w-100 btn-sm",
                  icon  = icon("floppy-disk")
                )
              )
            ),

            div(
              uiOutput(ns("cards_metricas_lmb")),
              br(),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header(bs_icon("bullseye", class = "me-1"),
                              "Predichos vs. observados"),
                  card_body(
                    p(class = "small text-muted",
                      "Puntos cerca de la diagonal = buenas predicciones."),
                    plotOutput(ns("plot_predobs_lmb"), height = "240px")
                  )
                ),
                card(
                  card_header(bs_icon("lightbulb", class = "me-1"),
                              "Interpretación"),
                  card_body(uiOutput(ns("texto_modelo_lmb")))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 7: Diagnóstico MCMC
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("activity", class = "me-1"),
                        "Diagnóstico MCMC"),
        card_body(

          p(class = "small text-muted mb-3",
            "Verifica que las cadenas MCMC convergieron correctamente. ",
            "Un modelo bien ajustado debe tener ", strong("R̂ < 1.01"),
            " y ", strong("ESS > 400"), " para todos los parámetros. ",
            "El posterior predictive check compara datos simulados con los observados."
          ),

          layout_columns(
            col_widths = c(4, 8),

            # Semáforo
            card(
              card_header(bs_icon("stopwatch", class = "me-1"),
                          "Diagnóstico de convergencia"),
              card_body(uiOutput(ns("semaforo_mcmc_lmb")))
            ),

            div(
              navset_pill(
                nav_panel(
                  title = "Traceplots",
                  br(),
                  p(class = "small text-muted mb-2",
                    "Las cadenas deben mezclarse bien — como ",
                    strong("orugas peludas"), " superpuestas. ",
                    "Cadenas atascadas o que divergen indican problemas."),
                  selectInput(ns("param_trace_lmb"), "Parámetro:",
                              choices = NULL),
                  plotOutput(ns("plot_trace_lmb"), height = "280px")
                ),
                nav_panel(
                  title = "Densidades",
                  br(),
                  p(class = "small text-muted mb-2",
                    "Las densidades de las cadenas deben superponerse. ",
                    "Distribuciones muy diferentes entre cadenas indican ",
                    "falta de convergencia."),
                  plotOutput(ns("plot_dens_mcmc_lmb"), height = "280px")
                ),
                nav_panel(
                  title = "Posterior predictive check",
                  br(),
                  p(class = "small text-muted mb-2",
                    "Compara la distribución de los datos observados (línea oscura) ",
                    "con réplicas simuladas desde el posterior. ",
                    "Deben parecerse — si no, el modelo está mal especificado."),
                  plotOutput(ns("plot_ppc_post_lmb"), height = "280px")
                ),
                nav_panel(
                  title = "R\u0302 y ESS",
                  br(),
                  p(class = "small text-muted mb-2",
                    strong("R\u0302 (R-hat):"), " mide convergencia entre cadenas. ",
                    "Valores < 1.01 indican convergencia. ",
                    strong("ESS:"), " tama\u00f1o efectivo de muestra. ",
                    "Valores > 400 son aceptables, > 1000 son buenos."),
                  div(
                    class = "alert alert-info small mb-3",
                    bs_icon("info-circle", class = "me-1"),
                    strong("\u00bfC\u00f3mo se relaciona el ESS con las iteraciones?"), br(),
                    "Con ", strong("2000 iteraciones"), " y warmup del 50%, quedan ",
                    strong("1000 muestras de sampling por cadena"), ". ",
                    "Con 4 cadenas hay ", strong("4000 muestras totales del posterior"), ". ",
                    "El warmup se descarta intencionalmente \u2014 es el per\u00edodo de adaptaci\u00f3n ",
                    "del sampler antes de converger. ",
                    "Un ESS bulk cercano a 4000 indica cadenas que se mezclan bien. ",
                    "Si el ESS ", strong("supera las 4000 muestras"), " (por ejemplo 6000), ",
                    "es completamente normal y positivo: indica anticorrelaci\u00f3n entre ",
                    "muestras consecutivas, lo que aumenta la eficiencia del muestreo. ",
                    "Si el ESS fuera muy bajo (< 400), indicar\u00eda alta autocorrelaci\u00f3n ",
                    "\u2014 habr\u00eda que aumentar iteraciones o reparametrizar el modelo."
                  ),
                  DTOutput(ns("tabla_rhat_lmb"))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 8: Performance
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("speedometer2", class = "me-1"),
                        "Performance"),
        card_body(

          p(class = "small text-muted mb-3",
            "Métricas de rendimiento del modelo bayesiano. ",
            strong("R² bayesiano"), " con incertidumbre, ",
            strong("RMSE"), " calculado desde el posterior completo, y ",
            strong("LOO / WAIC"), " para evaluar la capacidad predictiva ",
            "en datos nuevos."
          ),

          layout_columns(
            col_widths = c(6, 6),

            card(
              card_header(
                bs_icon("speedometer2", class = "me-1"),
                "Métricas del modelo",
                span(class = "text-muted small ms-2",
                     "— brms · loo")
              ),
              card_body(uiOutput(ns("tabla_performance_lmb")))
            ),

            div(
              card(
                class = "mb-3",
                card_header(
                  bs_icon("bullseye", class = "me-1"),
                  "Predicho vs. observado",
                  span(class = "text-muted small ms-2",
                       "— media posterior")
                ),
                card_body(
                  plotOutput(ns("plot_predobs_perf_lmb"), height = "240px")
                )
              ),
              card(
                class = "mb-0",
                card_header(
                  bs_icon("info-circle", class = "me-1"),
                  "Interpretación de métricas"
                ),
                card_body(
                  tags$ul(
                    class = "small text-muted mb-0",
                    tags$li(strong("R² bayesiano:"),
                            " proporción de varianza explicada por el modelo. ",
                            "Se reporta con su intervalo credible 95%."),
                    tags$li(strong("RMSE posterior:"),
                            " error cuadrático medio calculado desde el ",
                            "posterior predictivo. Menor = mejor ajuste."),
                    tags$li(strong("ELPD-LOO:"),
                            " log-densidad predictiva esperada por validación ",
                            "cruzada aproximada. Más alto (menos negativo) = mejor."),
                    tags$li(strong("ELPD-WAIC:"),
                            " alternativa al LOO basada en el criterio de ",
                            "información de Watanabe. Similar interpretación que LOO.")
                  )
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 9: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Parámetros"),
        div(
          class = "p-3",
          p(class = "small text-muted mb-3",
            "Cada coeficiente β muestra la media posterior, la desviación estándar ",
            "posterior y el ", strong("intervalo credible 95% (IC)"),
            " — el rango donde hay 95% de probabilidad de encontrar el verdadero valor. ",
            "A diferencia del p-valor frecuentista, el IC bayesiano tiene una ",
            "interpretación directa: si no incluye el cero, el efecto es ",
            "prácticamente relevante."
          ),
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(
                bs_icon("layout-text-sidebar", class = "me-1"),
                "Tabla de coeficientes",
                span(class = "text-muted small ms-2",
                     "— distribución posterior")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_params_ui_lmb"))
              )
            ),
            card(
              card_header(
                bs_icon("bar-chart-fill", class = "me-1"),
                "Forest plot",
                span(class = "text-muted small ms-2",
                     "— media posterior ± IC 95%")
              ),
              card_body(
                p(class = "small text-muted",
                  "Si el intervalo credible no cruza el cero (línea punteada), ",
                  "el efecto es prácticamente relevante."),
                plotOutput(ns("plot_forest_lmb"), height = "300px")
              )
            )
          ),
          div(
            class = "mt-3",
            card(
              card_header(
                bs_icon("bar-chart-steps", class = "me-1"),
                "Importancia de variables",
                span(class = "text-muted small ms-2",
                     "— probabilidad de dirección (pd)")
              ),
              card_body(
                p(class = "small text-muted mb-2",
                  "La ", strong("probabilidad de dirección (pd)"),
                  " indica qué tan consistente es la dirección del efecto ",
                  "(positivo o negativo) en toda la distribución posterior. ",
                  "pd > 95% es equivalente aproximado a p < 0.05."),
                plotOutput(ns("plot_pd_lmb"), height = "220px")
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 10: Gráficos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"),
                        "Gráficos"),
        card_body(
          navset_pill(
            nav_panel(
              title = "Distribuciones posteriores",
              br(),
              p(class = "small text-muted mb-3",
                "Distribución posterior de cada coeficiente. ",
                "El área sombreada representa el intervalo credible 95%."),
              plotOutput(ns("plot_areas_lmb"), height = "380px")
            ),
            nav_panel(
              title = "Predicho vs. observado",
              br(),
              p(class = "small text-muted mb-3",
                "Comparación entre los valores observados y los predichos ",
                "por el modelo. Los puntos deben estar cerca de la diagonal."),
              plotOutput(ns("plot_predobs_graf_lmb"), height = "380px")
            ),
            nav_panel(
              title = "Residuos",
              br(),
              p(class = "small text-muted mb-3",
                "Residuos del modelo (diferencia entre observado y predicho). ",
                "Deben distribuirse aleatoriamente alrededor del cero."),
              plotOutput(ns("plot_resid_lmb"), height = "380px")
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 11: Efectos marginales
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrows-angle-expand", class = "me-1"),
                        "Efectos marginales"),
        card_body(

          p(class = "small text-muted mb-3",
            "Visualiza el efecto de cada predictor sobre Y, manteniendo ",
            "el resto de variables en sus valores típicos. La banda sombreada ",
            "representa el ", strong("intervalo credible 95%"),
            " del posterior — más ancho = más incertidumbre."
          ),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_pred_marginal_lmb")),
                tags$hr(),
                checkboxInput(ns("marginal_ci_lmb"),
                              "Mostrar intervalo credible 95%",
                              value = TRUE),
                checkboxInput(ns("marginal_puntos_lmb"),
                              "Mostrar datos observados",
                              value = TRUE),
                tags$hr(),
                uiOutput(ns("marginal_valores_tipicos_lmb"))
              )
            ),

            div(
              card(
                card_header(
                  bs_icon("graph-up-arrow", class = "me-1"),
                  "Efecto marginal",
                  span(class = "text-muted small ms-2",
                       "— posterior predictive")
                ),
                card_body(
                  plotOutput(ns("plot_marginal_lmb"), height = "380px")
                )
              ),
              br(),
              uiOutput(ns("marginal_interpretacion_lmb"))
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Predicción puntual"),
          p(class = "small text-muted mb-3",
            "Ingresa valores específicos para cada predictor y obtén ",
            "la predicción bayesiana con su intervalo credible 95%."
          ),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"),
                          "Valores de los predictores"),
              card_body(
                uiOutput(ns("inputs_prediccion_lmb")),
                br(),
                actionButton(
                  ns("calcular_prediccion_lmb"),
                  "Calcular predicción",
                  class = "btn-primary w-100",
                  icon  = icon("calculator")
                )
              )
            ),
            card(
              card_header(bs_icon("bullseye", class = "me-1"), "Resultado"),
              card_body(uiOutput(ns("resultado_prediccion_lmb")))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 12: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrow-left-right", class = "me-1"),
                        "Comparar modelos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Ajusta distintos modelos en la pestaña ", strong("Ajustar modelo"),
            ", guarda cada uno con un nombre descriptivo y compáralos aquí. ",
            "Se usan ", strong("LOO (Leave-One-Out)"), " y ", strong("WAIC"),
            " — equivalentes bayesianos del AIC. Menor ELPD (más negativo) = mejor modelo."
          ),

          div(
            class = "alert alert-info small mb-3",
            bs_icon("info-circle", class = "me-1"),
            strong("¿Cómo interpretar?"), " La diferencia de ELPD-LOO entre modelos ",
            "indica cuál predice mejor datos nuevos. Una diferencia > 4 puntos ",
            "con SE < diferencia/2 se considera sustancial."
          ),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("list-check", class = "me-1"),
                          "Modelos guardados"),
              card_body(
                uiOutput(ns("lista_modelos_guardados_lmb")),
                tags$hr(),
                actionButton(ns("limpiar_modelos_lmb"),
                             "Limpiar todos",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("trash"))
              )
            ),

            div(
              card(
                class = "mb-3",
                card_header(
                  bs_icon("table", class = "me-1"),
                  "Tabla comparativa",
                  span(class = "text-muted small ms-2", "— LOO y WAIC")
                ),
                card_body(uiOutput(ns("tabla_comparacion_lmb")))
              ),
              card(
                class = "mb-0",
                card_header(
                  bs_icon("bar-chart-fill", class = "me-1"),
                  "Gráfico de comparación LOO",
                  span(class = "text-muted small ms-2",
                       "— mayor ELPD = mejor predicción")
                ),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Las barras de error muestran la incertidumbre del LOO. ",
                    "Requiere al menos 2 modelos guardados."),
                  plotOutput(ns("plot_comparacion_lmb"), height = "300px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 13: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
        card_body(
          p(class = "text-muted small mb-3",
            "Script que reproduce este análisis en R usando ",
            strong("brms"), " y ", strong("bayesplot / posterior"),
            ". Se actualiza automáticamente según las selecciones activas."
          ),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " Script reproducible"),
              downloadButton(
                ns("descargar_script_lmb"),
                label = "Descargar .R",
                icon  = bs_icon("download"),
                class = "btn-sm btn-outline-primary"
              )
            ),
            verbatimTextOutput(ns("codigo_r_lmb"))
          )
        )
      )

    ) # fin navset_card_tab
  )
}

# ── SERVER ────────────────────────────────────────────────
mod_lm_bayes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos reactivos ───────────────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_lmb
      req(!is.null(fuente) && nchar(fuente) > 0)
      tryCatch({
        e <- new.env()
        load(system.file("app/data", paste0(fuente, ".rda"),
                         package = "StatBayes"), envir = e)
        df <- get(ls(e)[1], envir = e)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(err) {
        showNotification(paste("Error al cargar dataset:", err$message),
                         type = "error", duration = 6)
        NULL
      })
    })

    datos_mod <- reactiveVal(NULL)
    observeEvent(datos(), { datos_mod(datos()) })

    # ── Info dataset ──────────────────────────────────
    output$info_dataset_lmb <- renderUI({
      fuente <- input$fuente_datos_lmb
      if (is.null(fuente)) return(NULL)
      switch(fuente,
        birdabundance_lm = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Densidad de especie de ave (Loyn, 1987)."),
          " Abundancia de aves en ",
          strong("56 fragmentos de bosque"),
          " de Victoria, Australia. Variables: ",
          strong("densidad_especie"), " (aves/ha), ",
          strong("area_ha"), " (ha), ",
          strong("distancia_m"), " (m al fragmento m\u00e1s cercano), ",
          strong("altitud_m"), " (m s.n.m.) y ",
          strong("pastoreo"), " (5 niveles de intensidad). ",
          "Fuente: Quinn & Keough (2002). ",
          em("Experimental Design and Data Analysis for Biologists.")
        ),
        birthwt_lm = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Peso al nacer \u2014 salud perinatal (Hosmer & Lemeshow)."),
          " Datos de ",
          strong("189 neonatos"),
          " del Baystate Medical Center, Springfield, MA (1986). Variables: ",
          strong("peso_g"), " (peso al nacer en gramos), ",
          strong("edad_madre"), " (a\u00f1os), ",
          strong("peso_madre"), " (libras), ",
          strong("tabaco"), " y ", strong("hta"), " (factores de riesgo). ",
          "Fuente: MASS::birthwt."
        )
      )
    })

    # ── Vista previa datos de ejemplo ────────────────
    output$cards_datos_lmb <- renderUI({
      req(datos())
      d    <- datos()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(
        col_widths = c(4, 4, 4),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$primario, "; font-weight:700;"),
                  nrow(d)),
               p(class = "small text-muted mb-0", "Observaciones"))),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$acento, "; font-weight:700;"),
                  nnum),
               p(class = "small text-muted mb-0", "Num\u00e9ricas"))),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$secundario, "; font-weight:700;"),
                  ncat),
               p(class = "small text-muted mb-0", "Categ\u00f3ricas")))
      )
    })

    output$tabla_preview_lmb <- renderDT({
      req(datos())
      datatable(datos(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Datos propios ─────────────────────────────────
    datos_propio_lmb <- reactive({
      req(input$archivo_lmb)
      ext <- tools::file_ext(input$archivo_lmb$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo_lmb$datapath)
        else
          readr::read_delim(input$archivo_lmb$datapath,
                            delim = input$separador_lmb %||% ",",
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(e) {
        showNotification(paste("Error al leer archivo:", e$message),
                         type = "error", duration = 6)
        NULL
      })
    })

    observeEvent(datos_propio_lmb(), { datos_mod(datos_propio_lmb()) })

    output$resumen_datos_propio_lmb <- renderUI({
      req(datos_propio_lmb())
      d <- datos_propio_lmb()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_lmb <- renderUI({
      req(datos_propio_lmb())
      d    <- datos_propio_lmb()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(
        col_widths = c(4, 4, 4),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$primario, "; font-weight:700;"),
                  nrow(d)),
               p(class = "small text-muted mb-0", "Observaciones"))),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$acento, "; font-weight:700;"),
                  nnum),
               p(class = "small text-muted mb-0", "Num\u00e9ricas"))),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$secundario, "; font-weight:700;"),
                  ncat),
               p(class = "small text-muted mb-0", "Categ\u00f3ricas")))
      )
    })

    output$tabla_preview_propio_lmb <- renderDT({
      req(datos_propio_lmb())
      datatable(datos_propio_lmb(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Tipos de variables ────────────────────────────
    tipos_usuario_lmb <- reactiveVal(NULL)

    output$tabla_tipos_lmb <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_lmb()
      filas <- lapply(names(d), function(nm) {
        col    <- d[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor" else "numeric"
        icono  <- if (actual == "factor")
          bs_icon("tag-fill", style = paste0("color:", colores$acento))
        else
          bs_icon("123", style = paste0("color:", colores$primario))
        sel <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else actual
        tags$tr(
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  div(class = "d-flex align-items-center gap-2",
                      icono, strong(nm))),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  tags$span(class = "badge",
                            style = paste0("background:",
                              if (actual == "factor") colores$acento
                              else colores$primario, "; font-size:0.75rem;"),
                            if (actual == "factor") "Factor" else "Num\u00e9rico")),
          tags$td(style = "padding:5px 8px;",
                  selectInput(
                    inputId  = ns(paste0("tipo_", nm)),
                    label    = NULL,
                    choices  = c("Num\u00e9rico"          = "numeric",
                                 "Factor (categ\u00f3rico)" = "factor",
                                 "Excluir"              = "excluir"),
                    selected = sel,
                    width    = "180px"
                  )),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  if (!is.null(tu) && !is.null(tu[[nm]]) && tu[[nm]] != actual)
                    tags$span(class = "badge",
                              style = paste0("background:", colores$exito),
                              "Modificado")
                  else
                    tags$span(class = "text-muted small", "Sin cambios"))
        )
      })
      tags$table(
        class = "table table-sm table-hover small mb-0",
        tags$thead(
          style = paste0("background:", colores$primario,
                         " !important; color:#fff !important;"),
          tags$tr(
            tags$th(style = "padding:7px 8px;", "Variable"),
            tags$th(style = "padding:7px 8px;", "Tipo detectado"),
            tags$th(style = "padding:7px 8px;", "Tipo a usar"),
            tags$th(style = "padding:7px 8px;", "Estado")
          )
        ),
        tags$tbody(filas)
      )
    })

    observeEvent(input$aplicar_tipos_lmb, {
      req(datos_mod())
      d  <- datos_mod()
      tu <- setNames(
        lapply(names(d), function(nm) input[[paste0("tipo_", nm)]]),
        names(d)
      )
      tipos_usuario_lmb(tu)
      for (nm in names(d)) {
        nuevo_tipo <- tu[[nm]]
        if (!is.null(nuevo_tipo) && nuevo_tipo != "excluir") {
          d[[nm]] <- switch(nuevo_tipo,
            numeric = as.numeric(d[[nm]]),
            factor  = as.factor(d[[nm]])
          )
        }
      }
      # excluir variables marcadas
      excluir <- names(tu)[sapply(tu, function(t) !is.null(t) && t == "excluir")]
      if (length(excluir) > 0) d <- d[, !names(d) %in% excluir, drop = FALSE]
      datos_mod(d)
    })

    output$tipos_aplicados_msg_lmb <- renderUI({
      tu <- tipos_usuario_lmb()
      if (is.null(tu)) return(NULL)
      d <- datos_mod()
      n_cambios <- sum(sapply(names(tu), function(nm) {
        if (!nm %in% names(d)) return(FALSE)
        actual <- if (is.factor(d[[nm]]) || is.character(d[[nm]])) "factor"
                  else "numeric"
        !is.null(tu[[nm]]) && tu[[nm]] != actual && tu[[nm]] != "excluir"
      }))
      n_excl <- sum(sapply(tu, function(t) !is.null(t) && t == "excluir"))
      if (n_cambios == 0 && n_excl == 0) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle-fill", class = "me-1",
                  style = paste0("color:", colores$exito)),
          if (n_cambios > 0) paste0(n_cambios, " variable(s) convertida(s). "),
          if (n_excl > 0) paste0(n_excl, " variable(s) excluida(s). "),
          "El modelo usar\u00e1 estos tipos.")
    })

    observeEvent(input$resetear_tipos_lmb, {
      tipos_usuario_lmb(NULL)
      datos_mod(datos())
    })

    # ── Exploración ───────────────────────────────────
    vars_num <- reactive({
      req(datos_mod())
      names(which(sapply(datos_mod(), is.numeric)))
    })

    vars_cat <- reactive({
      req(datos_mod())
      names(which(sapply(datos_mod(), function(x)
        is.factor(x) || is.character(x))))
    })

    output$sel_var_x_lmb <- renderUI({
      selectInput(ns("var_x_lmb"), "Variable X (predictor):",
                  choices = vars_num())
    })

    output$sel_color_lmb <- renderUI({
      selectInput(ns("var_color_lmb"), "Color por grupo (opcional):",
                  choices = c("Ninguno" = "ninguno", vars_cat()))
    })

    output$plot_scatter_lmb <- renderPlot({
      req(datos_mod(), input$var_x_lmb)
      d   <- datos_mod()
      x   <- input$var_x_lmb
      col <- input$var_color_lmb

      # necesitamos una Y — usar primera numérica que no sea X
      ys <- setdiff(vars_num(), x)
      req(length(ys) > 0)
      y <- ys[1]

      p <- ggplot(d, aes(.data[[x]], .data[[y]])) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = colores$fondo, color = NA))

      if (!is.null(col) && col != "ninguno") {
        p <- p + aes(color = .data[[col]]) +
          scale_color_tableau_cb()
        if (isTRUE(input$linea_por_grupo_lmb)) {
          p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8)
        }
      }

      p <- p + geom_point(alpha = 0.6, size = 2)

      if (isTRUE(input$mostrar_linea_lmb) &&
          (is.null(col) || col == "ninguno" ||
           !isTRUE(input$linea_por_grupo_lmb))) {
        p <- p + geom_smooth(method = "lm", se = TRUE,
                             color = colores$primario, linewidth = 0.8)
      }

      p + labs(x = x, y = y)
    })

    output$cards_correlacion_lmb <- renderUI({
      req(datos_mod(), input$var_x_lmb)
      d <- datos_mod()
      x <- input$var_x_lmb
      ys <- setdiff(vars_num(), x)
      if (length(ys) == 0) return(NULL)
      y   <- ys[1]
      cor_val <- cor(d[[x]], d[[y]], use = "complete.obs")
      div(
        class = paste("small p-2 rounded",
                      if (abs(cor_val) > 0.7) "sem-ok"
                      else if (abs(cor_val) > 0.3) "sem-warn"
                      else "sem-bad"),
        bs_icon("arrow-left-right", class = "me-1"),
        strong("r = "), round(cor_val, 3)
      )
    })

    # ── Priors: código ────────────────────────────────
    output$codigo_priors_lmb <- renderText({
      dist_int <- switch(input$prior_intercept_dist_lmb,
        normal    = paste0("normal(", input$prior_intercept_mu_lmb,
                           ", ", input$prior_intercept_sd_lmb, ")"),
        student_t = paste0("student_t(3, ", input$prior_intercept_mu_lmb,
                           ", ", input$prior_intercept_sd_lmb, ")"),
        cauchy    = paste0("cauchy(", input$prior_intercept_mu_lmb,
                           ", ", input$prior_intercept_sd_lmb, ")")
      )
      dist_b <- switch(input$prior_b_dist_lmb,
        normal    = paste0("normal(", input$prior_b_mu_lmb,
                           ", ", input$prior_b_sd_lmb, ")"),
        student_t = paste0("student_t(3, ", input$prior_b_mu_lmb,
                           ", ", input$prior_b_sd_lmb, ")"),
        cauchy    = paste0("cauchy(", input$prior_b_mu_lmb,
                           ", ", input$prior_b_sd_lmb, ")")
      )
      dist_sigma <- switch(input$prior_sigma_dist_lmb,
        exponential = paste0("exponential(", input$prior_sigma_rate_lmb, ")"),
        cauchy      = paste0("cauchy(0, ", input$prior_sigma_rate_lmb, ")"),
        student_t   = paste0("student_t(3, 0, ", input$prior_sigma_rate_lmb, ")")
      )
      paste0(
        "c(\n",
        "  prior(", dist_int, ", class = Intercept),\n",
        "  prior(", dist_b, ", class = b),\n",
        "  prior(", dist_sigma, ", class = sigma)\n",
        ")"
      )
    })

    # ── Priors: PPC ───────────────────────────────────
    observeEvent(input$ver_ppc_lmb, {
      req(datos_mod())
      n   <- 200
      x   <- rnorm(100)
      mat <- replicate(n, {
        b0  <- rnorm(1, input$prior_intercept_mu_lmb,
                     input$prior_intercept_sd_lmb)
        b1  <- rnorm(1, input$prior_b_mu_lmb, input$prior_b_sd_lmb)
        sig <- rexp(1, input$prior_sigma_rate_lmb)
        rnorm(100, b0 + b1 * x, sig)
      })

      output$plot_ppc_prior_lmb <- renderPlot({
        df <- data.frame(
          y   = as.vector(mat),
          sim = rep(seq_len(ncol(mat)), each = nrow(mat))
        )
        ggplot(df, aes(x = y, group = sim)) +
          geom_density(color = colores$primario, alpha = 0.08,
                       linewidth = 0.3) +
          labs(
            x        = "Valores simulados de Y (escala de la variable respuesta)",
            y        = "Densidad de probabilidad",
            subtitle = paste0(n, " conjuntos de datos simulados desde el prior — ",
                              "cada curva representa un escenario posible ")
          ) +
          theme_minimal(base_size = 13) +
          theme(panel.grid.minor = element_blank(),
                plot.background  = element_rect(fill = colores$fondo,
                                                color = NA))
      })

      rango <- range(mat, na.rm = TRUE)
      output$msg_ppc_prior_lmb <- renderUI({
        clase <- if (abs(rango[1]) > 1e4 || abs(rango[2]) > 1e4) "sem-bad"
                 else if (abs(rango[1]) > 100 || abs(rango[2]) > 100) "sem-warn"
                 else "sem-ok"
        icono <- if (clase == "sem-ok") "check-circle-fill"
                 else if (clase == "sem-warn") "exclamation-triangle-fill"
                 else "x-circle-fill"
        msg <- if (clase == "sem-ok")
                 "Rangos razonables. El prior parece adecuado."
               else if (clase == "sem-warn")
                 "Rangos amplios. Considera priors más restrictivos."
               else
                 "Rangos extremos. El prior es demasiado difuso."
        div(class = paste("small p-2 mt-2 rounded", clase),
            bs_icon(icono, class = "me-1"), msg)
      })
    })

    # ── Ajuste del modelo ─────────────────────────────
    modelo_actual <- reactiveVal(NULL)
    modelos_guardados <- reactiveVal(list())

    output$sel_var_y_lmb <- renderUI({
      selectInput(ns("var_y_lmb"), "Variable respuesta (Y):",
                  choices = vars_num())
    })

    output$checks_numericos_lmb <- renderUI({
      ys <- input$var_y_lmb %||% vars_num()[1]
      ops <- setdiff(vars_num(), ys)
      if (length(ops) == 0)
        return(p(class = "small text-muted", "No hay más variables numéricas."))
      checkboxGroupInput(ns("preds_num_lmb"), label = NULL, choices = ops)
    })

    output$checks_categoricos_lmb <- renderUI({
      ops <- vars_cat()
      if (length(ops) == 0)
        return(p(class = "small text-muted", "No hay variables categóricas."))
      checkboxGroupInput(ns("preds_cat_lmb"), label = NULL, choices = ops)
    })

    output$checks_interacciones_lmb <- renderUI({
      preds <- c(input$preds_num_lmb, input$preds_cat_lmb)
      if (length(preds) < 2) return(NULL)
      pares <- combn(preds, 2, function(x) paste(x, collapse = " × "),
                     simplify = TRUE)
      checkboxGroupInput(ns("interacciones_lmb"), label = NULL,
                         choices = setNames(
                           gsub(" × ", ":", pares), pares
                         ))
    })

    formula_lmb <- reactive({
      req(input$var_y_lmb)
      preds <- c(input$preds_num_lmb, input$preds_cat_lmb,
                 input$interacciones_lmb)
      if (length(preds) == 0) preds <- "1"
      as.formula(paste(input$var_y_lmb, "~",
                       paste(preds, collapse = " + ")))
    })

    priors_lmb <- reactive({
      dist_int <- switch(input$prior_intercept_dist_lmb,
        normal    = brms::prior(normal(0, 2.5), class = Intercept),
        student_t = brms::prior(student_t(3, 0, 2.5), class = Intercept),
        cauchy    = brms::prior(cauchy(0, 2.5), class = Intercept)
      )
      dist_sigma <- switch(input$prior_sigma_dist_lmb,
        exponential = brms::prior(exponential(1), class = sigma),
        cauchy      = brms::prior(cauchy(0, 1), class = sigma),
        student_t   = brms::prior(student_t(3, 0, 1), class = sigma)
      )
      # solo agregar prior de b si hay predictores seleccionados
      preds <- c(input$preds_num_lmb, input$preds_cat_lmb)
      if (length(preds) > 0) {
        dist_b <- switch(input$prior_b_dist_lmb,
          normal    = brms::prior(normal(0, 1), class = b),
          student_t = brms::prior(student_t(3, 0, 1), class = b),
          cauchy    = brms::prior(cauchy(0, 1), class = b)
        )
        c(dist_int, dist_b, dist_sigma)
      } else {
        c(dist_int, dist_sigma)
      }
    })

    observeEvent(input$ajustar_lmb, {
      req(datos_mod(), input$var_y_lmb)
      d   <- datos_mod()
      frm <- formula_lmb()

      if (isTRUE(input$estandarizar_lmb)) {
        num_vars <- c(input$preds_num_lmb)
        for (v in num_vars) {
          if (v %in% names(d)) d[[v]] <- scale(d[[v]])[, 1]
        }
      }

      withProgress(message = "Ajustando modelo bayesiano (MCMC)…",
                   detail = "Esto puede tardar unos minutos.", value = 0.1, {
        tryCatch({
          fit <- brms::brm(
            formula = frm,
            data    = d,
            family  = gaussian(),
            prior   = priors_lmb(),
            chains  = input$mcmc_chains_lmb,
            iter    = input$mcmc_iter_lmb,
            cores   = parallel::detectCores(),
            refresh = 0,
            silent  = 2
          )
          modelo_actual(fit)
          setProgress(1)
        }, error = function(e) {
          showNotification(paste("Error al ajustar:", e$message),
                           type = "error")
        })
      })
    })

    # ── Métricas tras ajuste ──────────────────────────
    output$cards_metricas_lmb <- renderUI({
      req(modelo_actual())
      fit <- modelo_actual()

      # R² bayesiano
      r2   <- tryCatch(round(brms::bayes_R2(fit)[1, "Estimate"], 3),
                       error = function(e) "—")

      # RMSE desde el posterior completo
      rmse <- tryCatch({
        pe <- brms::predictive_error(fit)
        round(sqrt(mean(pe^2)), 3)
      }, error = function(e) "—")

      # LOO
      loo_val <- tryCatch({
        l <- loo::loo(fit)
        round(l$estimates["elpd_loo", "Estimate"], 1)
      }, error = function(e) "—")

      # WAIC
      waic_val <- tryCatch({
        w <- loo::waic(fit)
        round(w$estimates["elpd_waic", "Estimate"], 1)
      }, error = function(e) "—")

      layout_columns(
        col_widths = c(3, 3, 3, 3),
        card(
          class = "text-center",
          card_body(class = "p-2",
                    h4(style = paste0("color:", colores$primario,
                                      "; font-weight:700;"), r2),
                    p(class = "small text-muted mb-0", "R² bayesiano"))
        ),
        card(
          class = "text-center",
          card_body(class = "p-2",
                    h4(style = paste0("color:", colores$acento,
                                      "; font-weight:700;"), rmse),
                    p(class = "small text-muted mb-0", "RMSE posterior"))
        ),
        card(
          class = "text-center",
          card_body(class = "p-2",
                    h4(style = paste0("color:", colores$secundario,
                                      "; font-weight:700;"), loo_val),
                    p(class = "small text-muted mb-0", "ELPD-LOO"))
        ),
        card(
          class = "text-center",
          card_body(class = "p-2",
                    h4(style = paste0("color:", colores$secundario,
                                      "; font-weight:700;"), waic_val),
                    p(class = "small text-muted mb-0", "ELPD-WAIC"))
        )
      )
    })

    output$plot_predobs_lmb <- renderPlot({
      req(modelo_actual())
      fit <- modelo_actual()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- model.response(model.frame(fit))
      df    <- data.frame(obs = obs, pred = preds)
      ggplot(df, aes(obs, pred)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                    color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2) +
        labs(x = "Observado", y = "Predicho") +
        theme_minimal(base_size = 12) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    output$texto_modelo_lmb <- renderUI({
      req(modelo_actual())
      fit  <- modelo_actual()
      pars <- posterior::summarise_draws(fit,
                                         mean, ~quantile(.x, c(0.025, 0.975)))
      pars <- pars[grep("^b_", pars$variable), ]
      items <- lapply(seq_len(nrow(pars)), function(i) {
        nm  <- gsub("^b_", "", pars$variable[i])
        est <- round(pars$mean[i], 3)
        lo  <- round(pars$`2.5%`[i], 3)
        hi  <- round(pars$`97.5%`[i], 3)
        dir <- if (lo > 0) "positivo" else if (hi < 0) "negativo" else "incierto"
        tags$li(class = "small mb-1",
                strong(nm), ": β = ", est,
                " [IC 95%: ", lo, ", ", hi, "] — efecto ", strong(dir))
      })
      tagList(
        p(class = "small text-muted mb-2",
          "Resumen de los coeficientes posteriores:"),
        tags$ul(items)
      )
    })

    # ── Diagnóstico MCMC ──────────────────────────────
    output$semaforo_mcmc_lmb <- renderUI({
      req(modelo_actual())
      fit   <- modelo_actual()
      draws <- posterior::as_draws_df(fit)
      sumas <- posterior::summarise_draws(
        draws,
        rhat     = posterior::rhat,
        ess_bulk = posterior::ess_bulk
      )
      max_rhat <- max(as.numeric(sumas$rhat),     na.rm = TRUE)
      min_ess  <- min(as.numeric(sumas$ess_bulk), na.rm = TRUE)

      clase_rhat <- if (max_rhat < 1.01) "sem-ok"
                   else if (max_rhat < 1.05) "sem-warn"
                   else "sem-bad"
      clase_ess  <- if (min_ess > 400) "sem-ok"
                   else if (min_ess > 100) "sem-warn"
                   else "sem-bad"

      # mean_PPD vs Y observada
      ppd_card <- tryCatch({
        pp       <- posterior_predict(fit)
        mean_ppd <- round(mean(pp), 2)
        y_mean   <- round(mean(model.response(model.frame(fit)),
                               na.rm = TRUE), 2)
        pct_diff <- abs(mean_ppd - y_mean) / abs(y_mean) * 100
        clase_ppd <- if (pct_diff < 5) "sem-ok"
                     else if (pct_diff < 15) "sem-warn"
                     else "sem-bad"
        div(class = paste("p-2 rounded mb-2", clase_ppd),
            bs_icon(if (clase_ppd == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            strong("mean_PPD: "), mean_ppd,
            " \u2014 Y observada: ", y_mean, br(),
            span(class = "text-muted small",
                 paste0("Diferencia: ", round(pct_diff, 1), "% \u2014 ",
                        if (clase_ppd == "sem-ok")
                          "el modelo reproduce bien la media observada"
                        else if (clase_ppd == "sem-warn")
                          "diferencia moderada, revisa la especificaci\u00f3n"
                        else
                          "diferencia grande, modelo posiblemente mal especificado")))
      }, error = function(e) NULL)

      tagList(
        div(class = paste("p-2 rounded mb-2", clase_rhat),
            bs_icon(if (clase_rhat == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            strong("R\u0302 m\u00e1ximo: "), round(max_rhat, 4), br(),
            span(class = "text-muted small",
                 if (clase_rhat == "sem-ok") "Convergencia correcta (< 1.01)"
                 else if (clase_rhat == "sem-warn") "Convergencia marginal (< 1.05)"
                 else "Sin convergencia (\u2265 1.05)")),
        div(class = paste("p-2 rounded mb-2", clase_ess),
            bs_icon(if (clase_ess == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            strong("ESS m\u00ednimo: "), round(min_ess, 0), br(),
            span(class = "text-muted small",
                 if (clase_ess == "sem-ok") "ESS adecuado (> 400)"
                 else if (clase_ess == "sem-warn") "ESS marginal (> 100)"
                 else "ESS insuficiente (\u2264 100)")),
        ppd_card
      )
    })

    observe({
      req(modelo_actual())
      pars <- brms::variables(modelo_actual())
      pars <- pars[grep("^b_|^sigma", pars)]
      updateSelectInput(session, "param_trace_lmb", choices = pars)
    })

    output$plot_trace_lmb <- renderPlot({
      req(modelo_actual(), input$param_trace_lmb)
      bayesplot::mcmc_trace(
        modelo_actual(),
        pars  = input$param_trace_lmb,
        facet_args = list(ncol = 1)
      ) + theme_minimal(base_size = 12)
    })

    output$plot_dens_mcmc_lmb <- renderPlot({
      req(modelo_actual())
      pars <- brms::variables(modelo_actual())
      pars <- pars[grep("^b_|^sigma", pars)]
      bayesplot::mcmc_dens_overlay(modelo_actual(), pars = pars) +
        theme_minimal(base_size = 12)
    })

    output$plot_ppc_post_lmb <- renderPlot({
      req(modelo_actual())
      bayesplot::pp_check(modelo_actual(), ndraws = 100) +
        theme_minimal(base_size = 12) +
        scale_color_manual(values = c(colores$texto, colores$primario))
    })

    output$tabla_rhat_lmb <- renderDT({
      req(modelo_actual())
      fit   <- modelo_actual()
      draws <- posterior::as_draws_df(fit)
      sumas <- posterior::summarise_draws(
        draws,
        rhat     = posterior::rhat,
        ess_bulk = posterior::ess_bulk,
        ess_tail = posterior::ess_tail
      )
      df <- data.frame(
        Parametro  = sumas$variable,
        Rhat       = round(as.numeric(sumas$rhat), 4),
        ESS_bulk   = round(as.numeric(sumas$ess_bulk), 0),
        ESS_tail   = round(as.numeric(sumas$ess_tail), 0),
        check.names = FALSE
      )
      df <- df[grep("^b_|^sigma", df$Parametro), ]
      names(df) <- c("Par\u00e1metro", "R\u0302", "ESS bulk", "ESS tail")
      datatable(df, options = list(pageLength = 10), rownames = FALSE) |>
        DT::formatStyle("R\u0302",
          backgroundColor = DT::styleInterval(
            c(1.01, 1.05),
            c("#f0f9f5", "#fffbf0", "#fff0f2")
          )
        )
    })

    # ── Performance ───────────────────────────────────
    output$tabla_performance_lmb <- renderUI({
      req(modelo_actual())
      fit <- modelo_actual()

      r2 <- tryCatch({
        r <- brms::bayes_R2(fit)
        paste0(round(r[1, "Estimate"], 3),
               " [", round(r[1, "Q2.5"], 3),
               ", ", round(r[1, "Q97.5"], 3), "]")
      }, error = function(e) "—")

      rmse <- tryCatch({
        pe <- brms::predictive_error(fit)
        round(sqrt(mean(pe^2)), 3)
      }, error = function(e) "—")

      loo_res <- tryCatch({
        l <- loo::loo(fit)
        paste0(round(l$estimates["elpd_loo", "Estimate"], 1),
               " (SE = ", round(l$estimates["elpd_loo", "SE"], 1), ")")
      }, error = function(e) "—")

      waic_res <- tryCatch({
        w <- loo::waic(fit)
        paste0(round(w$estimates["elpd_waic", "Estimate"], 1),
               " (SE = ", round(w$estimates["elpd_waic", "SE"], 1), ")")
      }, error = function(e) "—")

      p_waic <- tryCatch({
        w <- loo::waic(fit)
        round(w$estimates["p_waic", "Estimate"], 1)
      }, error = function(e) "—")

      # mean_PPD
      mean_ppd <- tryCatch({
        pp  <- posterior_predict(fit)
        round(mean(pp), 2)
      }, error = function(e) "—")

      y_obs_mean <- tryCatch({
        round(mean(model.response(model.frame(fit)), na.rm = TRUE), 2)
      }, error = function(e) "—")

      ppd_diff <- tryCatch({
        diff <- abs(as.numeric(mean_ppd) - as.numeric(y_obs_mean))
        pct  <- round(diff / abs(as.numeric(y_obs_mean)) * 100, 1)
        paste0(round(diff, 2), " (", pct, "%)")
      }, error = function(e) "—")

      tags$table(
        class = "table table-sm small",
        tags$thead(
          style = paste0("background:", colores$primario, "; color:#fff;"),
          tags$tr(tags$th("M\u00e9trica"), tags$th("Valor"))
        ),
        tags$tbody(
          tags$tr(
            tags$td(tagList(strong("R\u00b2 bayesiano"),
                            tags$small(class = "text-muted ms-1", "\u00b1 IC 95%"))),
            tags$td(r2)
          ),
          tags$tr(style = paste0("background:", colores$fondo),
            tags$td(tagList(strong("RMSE"),
                            tags$small(class = "text-muted ms-1",
                                       "posterior predictivo"))),
            tags$td(rmse)
          ),
          tags$tr(
            tags$td(tagList(strong("Media Y observada"))),
            tags$td(y_obs_mean)
          ),
          tags$tr(style = paste0("background:", colores$fondo),
            tags$td(tagList(strong("mean_PPD"),
                            tags$small(class = "text-muted ms-1",
                                       "media del posterior predictivo"))),
            tags$td(mean_ppd)
          ),
          tags$tr(
            tags$td(tagList(strong("Diferencia PPD vs. Y"),
                            tags$small(class = "text-muted ms-1",
                                       "debe ser peque\u00f1a"))),
            tags$td(ppd_diff)
          ),
          tags$tr(style = paste0("background:", colores$fondo),
            tags$td(tagList(strong("ELPD-LOO"),
                            tags$small(class = "text-muted ms-1", "\u00b1 SE"))),
            tags$td(loo_res)
          ),
          tags$tr(
            tags$td(tagList(strong("ELPD-WAIC"),
                            tags$small(class = "text-muted ms-1", "\u00b1 SE"))),
            tags$td(waic_res)
          ),
          tags$tr(style = paste0("background:", colores$fondo),
            tags$td(tagList(strong("p_waic"),
                            tags$small(class = "text-muted ms-1",
                                       "n\u00b0 efectivo de par\u00e1metros"))),
            tags$td(p_waic)
          )
        )
      )
    })

    output$plot_predobs_perf_lmb <- renderPlot({
      req(modelo_actual())
      fit   <- modelo_actual()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- model.response(model.frame(fit))
      df    <- data.frame(obs = obs, pred = preds)
      ggplot(df, aes(obs, pred)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                    color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2.5) +
        geom_smooth(method = "lm", se = FALSE,
                    color = colores$acento, linewidth = 0.8) +
        labs(x = "Observado", y = "Predicho (media posterior)") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background  = element_rect(fill = colores$fondo,
                                              color = NA))
    })

    # ── Parámetros ────────────────────────────────────
    output$tabla_params_ui_lmb <- renderUI({
      req(modelo_actual())
      fit  <- modelo_actual()
      pars <- parameters::model_parameters(fit, ci = 0.95)
      DTOutput(ns("tabla_params_lmb"))
    })

    output$tabla_params_lmb <- renderDT({
      req(modelo_actual())
      pars <- parameters::model_parameters(modelo_actual(), ci = 0.95)
      df   <- as.data.frame(pars)
      df   <- df[, intersect(c("Parameter", "Median", "Mean", "SD",
                                "CI_low", "CI_high", "pd", "Rhat"),
                              names(df))]
      df[sapply(df, is.numeric)] <- lapply(
        df[sapply(df, is.numeric)], round, 3)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
    })

    output$plot_forest_lmb <- renderPlot({
      req(modelo_actual())
      fit  <- modelo_actual()
      pars <- posterior::summarise_draws(
        fit, mean, ~quantile(.x, c(0.025, 0.975)))
      pars <- pars[grep("^b_", pars$variable), ]
      pars$variable <- gsub("^b_", "", pars$variable)
      names(pars)[3:4] <- c("lo", "hi")

      ggplot(pars, aes(x = mean, y = reorder(variable, mean),
                       xmin = lo, xmax = hi,
                       color = (lo > 0 | hi < 0))) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   color = colores$texto) +
        geom_errorbarh(height = 0.25, linewidth = 0.6) +
        geom_point(size = 2.5) +
        scale_color_manual(
          values = c("FALSE" = colores$acento,
                     "TRUE"  = colores$primario),
          guide = "none"
        ) +
        labs(x = "Media posterior (IC 95%)", y = NULL) +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo,
                                              color = NA))
    })

    output$plot_pd_lmb <- renderPlot({
      req(modelo_actual())
      pars <- parameters::model_parameters(modelo_actual(), ci = 0.95)
      df   <- as.data.frame(pars)
      df   <- df[grep("^b_", df$Parameter), ]
      if (!"pd" %in% names(df)) return(NULL)
      df$Parameter <- gsub("^b_", "", df$Parameter)

      # pd viene en escala 0-1 para brms — convertir a 0-100
      if (max(df$pd, na.rm = TRUE) <= 1) df$pd <- df$pd * 100

      ggplot(df, aes(x = pd,
                     y = reorder(Parameter, pd),
                     color = pd > 95)) +
        geom_vline(xintercept = 95, linetype = "dashed",
                   color = colores$texto) +
        geom_segment(aes(x = 0, xend = pd,
                         yend = reorder(Parameter, pd)),
                     linewidth = 0.8) +
        geom_point(size = 4) +
        scale_color_manual(values = c("FALSE" = colores$acento,
                                      "TRUE"  = colores$primario),
                           guide = "none") +
        scale_x_continuous(limits = c(0, 105),
                           breaks = c(0, 25, 50, 75, 95, 100),
                           labels = function(x) paste0(x, "%")) +
        labs(x = "Probabilidad de dirección (%)", y = NULL,
             caption = "Línea punteada = 95% (equivalente aproximado a p < 0.05)") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background  = element_rect(fill = colores$fondo,
                                              color = NA))
    })

    # ── Gráficos ──────────────────────────────────────
    output$plot_areas_lmb <- renderPlot({
      req(modelo_actual())
      pars <- brms::variables(modelo_actual())
      pars <- pars[grep("^b_", pars)]
      bayesplot::mcmc_areas(modelo_actual(), pars = pars, prob = 0.95) +
        theme_minimal(base_size = 13)
    })

    output$plot_predobs_graf_lmb <- renderPlot({
      req(modelo_actual())
      fit   <- modelo_actual()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- model.response(model.frame(fit))
      df    <- data.frame(obs = obs, pred = preds)
      ggplot(df, aes(obs, pred)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                    color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2.5) +
        labs(x = "Observado", y = "Predicho") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo,
                                              color = NA))
    })

    output$plot_resid_lmb <- renderPlot({
      req(modelo_actual())
      fit   <- modelo_actual()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- model.response(model.frame(fit))
      df    <- data.frame(ajustado = preds, residuo = obs - preds)
      ggplot(df, aes(ajustado, residuo)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2) +
        geom_smooth(method = "loess", se = FALSE,
                    color = colores$acento, linewidth = 0.8) +
        labs(x = "Valores ajustados", y = "Residuos") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo,
                                              color = NA))
    })

    # ── Efectos marginales ────────────────────────────
    output$sel_pred_marginal_lmb <- renderUI({
      req(modelo_actual())
      preds <- c(input$preds_num_lmb, input$preds_cat_lmb)
      selectInput(ns("pred_marginal_lmb"), "Predictor focal:", choices = preds)
    })

    output$plot_marginal_lmb <- renderPlot({
      req(modelo_actual(), input$pred_marginal_lmb)
      fit  <- modelo_actual()
      pred <- input$pred_marginal_lmb
      tryCatch({
        ef <- brms::conditional_effects(fit, effects = pred)
        plot(ef, plot = FALSE)[[1]] +
          theme_minimal(base_size = 13) +
          theme(plot.background = element_rect(fill = colores$fondo,
                                                color = NA))
      }, error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("Error:", e$message)) +
          theme_void()
      })
    })

    output$marginal_interpretacion_lmb <- renderUI({
      req(modelo_actual(), input$pred_marginal_lmb)
      fit  <- modelo_actual()
      pred <- input$pred_marginal_lmb
      pars <- posterior::summarise_draws(
        fit, mean, ~quantile(.x, c(0.025, 0.975)))
      par_nm <- paste0("b_", pred)
      row    <- pars[pars$variable == par_nm, ]
      if (nrow(row) == 0) return(NULL)
      est <- round(row$mean, 3)
      lo  <- round(row$`2.5%`, 3)
      hi  <- round(row$`97.5%`, 3)
      dir <- if (lo > 0) "positivo y estadísticamente relevante"
             else if (hi < 0) "negativo y estadísticamente relevante"
             else "incierto (el IC credible incluye el cero)"
      div(
        class = "alert alert-info small",
        bs_icon("lightbulb", class = "me-1"),
        strong("Interpretación: "), "Por cada unidad adicional de ",
        strong(pred), ", Y cambia en promedio ", strong(est),
        " unidades [IC 95%: ", lo, ", ", hi, "]. El efecto es ", strong(dir), "."
      )
    })

    output$inputs_prediccion_lmb <- renderUI({
      req(modelo_actual())
      preds <- c(input$preds_num_lmb, input$preds_cat_lmb)
      d     <- datos_mod()
      lapply(preds, function(p) {
        if (is.numeric(d[[p]])) {
          numericInput(ns(paste0("pred_val_", p)), p,
                       value = round(mean(d[[p]], na.rm = TRUE), 2))
        } else {
          selectInput(ns(paste0("pred_val_", p)), p,
                      choices = levels(as.factor(d[[p]])))
        }
      })
    })

    observeEvent(input$calcular_prediccion_lmb, {
      req(modelo_actual())
      preds <- c(input$preds_num_lmb, input$preds_cat_lmb)
      nuevos <- setNames(
        lapply(preds, function(p) input[[paste0("pred_val_", p)]]),
        preds
      )
      nuevos_df <- as.data.frame(lapply(nuevos, function(x) {
        v <- suppressWarnings(as.numeric(x))
        if (is.na(v)) x else v
      }))
      tryCatch({
        pred <- fitted(modelo_actual(), newdata = nuevos_df,
                       probs = c(0.025, 0.975))
        output$resultado_prediccion_lmb <- renderUI({
          div(
            class = "alert alert-success",
            h5(class = "mb-1", round(pred[, "Estimate"], 3)),
            p(class = "small mb-0",
              "IC credible 95%: [",
              round(pred[, "Q2.5"], 3), ", ",
              round(pred[, "Q97.5"], 3), "]")
          )
        })
      }, error = function(e) {
        output$resultado_prediccion_lmb <- renderUI({
          div(class = "alert alert-danger small", e$message)
        })
      })
    })

    # ── Comparar modelos ──────────────────────────────
    observeEvent(input$guardar_modelo_lmb, {
      req(modelo_actual(), input$nombre_modelo_lmb != "")
      nm   <- input$nombre_modelo_lmb
      lista <- modelos_guardados()
      lista[[nm]] <- modelo_actual()
      modelos_guardados(lista)
      showNotification(paste("Modelo", nm, "guardado."),
                       type = "message")
    })

    output$lista_modelos_guardados_lmb <- renderUI({
      lista <- modelos_guardados()
      if (length(lista) == 0)
        return(p(class = "small text-muted",
                 "Aún no hay modelos guardados."))
      tags$ul(
        class = "small",
        lapply(names(lista), function(nm) tags$li(bs_icon("check2",
                                                           class = "me-1"), nm))
      )
    })

    observeEvent(input$limpiar_modelos_lmb, {
      modelos_guardados(list())
    })

    output$tabla_comparacion_lmb <- renderUI({
      lista <- modelos_guardados()
      if (length(lista) < 2)
        return(div(class = "alert alert-info small",
                   "Guarda al menos 2 modelos para compararlos."))
      DTOutput(ns("dt_comparacion_lmb"))
    })

    output$dt_comparacion_lmb <- renderDT({
      lista <- modelos_guardados()
      req(length(lista) >= 2)
      tryCatch({
        loos <- lapply(lista, loo::loo)
        comp <- loo::loo_compare(loos)
        df   <- as.data.frame(round(comp, 2))
        datatable(df, options = list(pageLength = 10, scrollX = TRUE))
      }, error = function(e) {
        data.frame(Error = e$message)
      })
    })

    output$plot_comparacion_lmb <- renderPlot({
      lista <- modelos_guardados()
      req(length(lista) >= 2)
      tryCatch({
        loos <- lapply(lista, loo::loo)
        comp <- loo::loo_compare(loos)
        df   <- as.data.frame(comp)
        df$modelo <- rownames(df)
        ggplot(df, aes(x = elpd_diff,
                       y = reorder(modelo, elpd_diff),
                       xmin = elpd_diff - se_diff,
                       xmax = elpd_diff + se_diff)) +
          geom_vline(xintercept = 0, linetype = "dashed",
                     color = colores$texto) +
          geom_errorbarh(height = 0.25, color = colores$primario) +
          geom_point(size = 3, color = colores$primario) +
          labs(x = "Diferencia ELPD-LOO (± SE)", y = NULL,
               caption = "Mayor ELPD = mejor predicción") +
          theme_minimal(base_size = 13) +
          theme(plot.background = element_rect(fill = colores$fondo,
                                                color = NA))
      }, error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("Error:", e$message)) +
          theme_void()
      })
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_lmb <- renderText({
      req(input$var_y_lmb)
      preds <- c(input$preds_num_lmb, input$preds_cat_lmb,
                 input$interacciones_lmb)
      if (length(preds) == 0) preds <- "1"
      formula_str <- paste(input$var_y_lmb, "~",
                           paste(preds, collapse = " + "))
      nm_datos <- if (input$fuente_datos_lmb != "propio")
                    input$fuente_datos_lmb
                  else "mis_datos"
      dist_int <- switch(input$prior_intercept_dist_lmb,
        normal    = paste0("normal(", input$prior_intercept_mu_lmb, ", ",
                           input$prior_intercept_sd_lmb, ")"),
        student_t = paste0("student_t(3, ", input$prior_intercept_mu_lmb, ", ",
                           input$prior_intercept_sd_lmb, ")"),
        cauchy    = paste0("cauchy(", input$prior_intercept_mu_lmb, ", ",
                           input$prior_intercept_sd_lmb, ")")
      )
      dist_b <- switch(input$prior_b_dist_lmb,
        normal    = paste0("normal(", input$prior_b_mu_lmb, ", ",
                           input$prior_b_sd_lmb, ")"),
        student_t = paste0("student_t(3, ", input$prior_b_mu_lmb, ", ",
                           input$prior_b_sd_lmb, ")"),
        cauchy    = paste0("cauchy(", input$prior_b_mu_lmb, ", ",
                           input$prior_b_sd_lmb, ")")
      )
      dist_sigma <- switch(input$prior_sigma_dist_lmb,
        exponential = paste0("exponential(", input$prior_sigma_rate_lmb, ")"),
        cauchy      = paste0("cauchy(0, ", input$prior_sigma_rate_lmb, ")"),
        student_t   = paste0("student_t(3, 0, ", input$prior_sigma_rate_lmb, ")")
      )

      paste0(
        "# ── Regresión lineal bayesiana con brms ──────────────\n",
        "library(brms)\n",
        "library(bayesplot)\n",
        "library(posterior)\n",
        "library(loo)\n\n",
        "# Datos\n",
        "# datos <- readr::read_csv('mis_datos.csv')\n",
        "data('", nm_datos, "', package = 'StatBayes')\n\n",
        "# Priors\n",
        "mis_priors <- c(\n",
        "  prior(", dist_int, ", class = Intercept),\n",
        "  prior(", dist_b, ", class = b),\n",
        "  prior(", dist_sigma, ", class = sigma)\n",
        ")\n\n",
        "# Ajustar modelo\n",
        "fit <- brm(\n",
        "  formula = ", formula_str, ",\n",
        "  data    = ", nm_datos, ",\n",
        "  family  = gaussian(),\n",
        "  prior   = mis_priors,\n",
        "  chains  = ", input$mcmc_chains_lmb, ",\n",
        "  iter    = ", input$mcmc_iter_lmb, ",\n",
        "  cores   = parallel::detectCores()\n",
        ")\n\n",
        "# Resumen\n",
        "summary(fit)\n\n",
        "# Diagnóstico MCMC\n",
        "mcmc_trace(fit)        # traceplots\n",
        "pp_check(fit, ndraws = 100)  # posterior predictive check\n\n",
        "# Parámetros\n",
        "library(parameters)\n",
        "model_parameters(fit, ci = 0.95)\n\n",
        "# Efectos marginales\n",
        "conditional_effects(fit)\n\n",
        "# LOO\n",
        "loo(fit)\n"
      )
    })

    output$descargar_script_lmb <- downloadHandler(
      filename = function() paste0("StatBayes_lm_bayes_",
                                    Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_lmb(), file)
    )

  })
}
