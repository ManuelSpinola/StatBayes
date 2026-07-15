# ============================================================
# mod_gam_bayes.R — GAM bayesiano
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
mod_gam_bayes_ui <- function(id) {
  ns <- NS(id)

  tagList(

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(

          div(
            class = "px-1 pb-2",
            h4(bs_icon("bezier2", class = "me-2"), "GAM bayesiano",
               style = paste0("color:", colores$primario, "; font-weight:700;")),
            p(class = "text-muted mb-0",
              "Versi\u00f3n bayesiana del modelo aditivo generalizado. ",
              "Usa la misma sintaxis de splines ", tags$code("s()"),
              " que mgcv, pero ajustada con MCMC via Stan (brms). ",
              "Distribuciones posteriores completas para cada curva suavizada.")
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Del GAM frecuentista al GAM bayesiano"),
          p(class = "small text-muted mb-3",
            "El GAM bayesiano usa la misma sintaxis de splines que mgcv \u2014 ",
            tags$code("s(x, k = 10)"), " \u2014 pero el ajuste es bayesiano. ",
            "Internamente, brms convierte los t\u00e9rminos ", tags$code("s()"),
            " en efectos aleatorios gaussianos, lo que permite estimar ",
            "la distribuci\u00f3n posterior completa de cada curva suavizada."
          ),

          layout_columns(col_widths = c(6, 6),
            card(
              fill = FALSE,
              card_header(bs_icon("arrow-left-right", class = "me-1"),
                          "Comparaci\u00f3n de enfoques"),
              card_body(
                tags$table(
                  class = "table table-sm small mb-0",
                  tags$thead(
                    style = paste0("background:", colores$primario, "; color:#fff;"),
                    tags$tr(tags$th("Aspecto"), tags$th("mgcv"), tags$th("brms"))
                  ),
                  tags$tbody(
                    tags$tr(tags$td(strong("Ajuste")),
                            tags$td("M\u00e1xima verosimilitud (REML)"),
                            tags$td("MCMC (Stan)")),
                    tags$tr(style = paste0("background:", colores$fondo),
                            tags$td(strong("Resultado")),
                            tags$td("Estimaci\u00f3n puntual + IC aprox."),
                            tags$td("Distribuci\u00f3n posterior completa")),
                    tags$tr(tags$td(strong("Suavizado")),
                            tags$td("Par\u00e1metro de suavizado \u03bb"),
                            tags$td("Prior sobre los coeficientes del spline")),
                    tags$tr(style = paste0("background:", colores$fondo),
                            tags$td(strong("Familias")),
                            tags$td("gaussian, Poisson, binomial\u2026"),
                            tags$td("Todas las de brms + Beta, ZIP\u2026")),
                    tags$tr(tags$td(strong("Velocidad")),
                            tags$td("Muy r\u00e1pido"),
                            tags$td("M\u00e1s lento (minutos)"))
                  )
                )
              )
            ),
            card(
              fill = FALSE,
              card_header(bs_icon("question-circle", class = "me-1"),
                          "\u00bfCu\u00e1ndo usar el GAM bayesiano?"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li("Necesitas cuantificar la ",
                          strong("incertidumbre completa"),
                          " sobre la forma de la curva."),
                  tags$li("Tienes ", strong("muestra peque\u00f1a"),
                          " y quieres estabilizar el suavizado con priors."),
                  tags$li("La variable respuesta requiere una familia ",
                          strong("no disponible en mgcv"),
                          " (Beta, ZIP, ZINB)."),
                  tags$li("Quieres comparar modelos con y sin t\u00e9rminos ",
                          "suaves usando ", strong("LOO / WAIC"), "."),
                  tags$li("Necesitas integrar el GAM con ",
                          strong("efectos aleatorios"),
                          " en un solo modelo.")
                )
              )
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "La f\u00f3rmula del GAM bayesiano"),
          p(class = "small text-muted mb-2",
            "La sintaxis es id\u00e9ntica a mgcv. Puedes combinar t\u00e9rminos ",
            "lineales y suaves en la misma f\u00f3rmula:"),

          layout_columns(col_widths = c(6, 6),
            div(
              class = "codigo-bloque",
              "# Solo t\u00e9rminos suaves\n",
              "y ~ s(x1) + s(x2)\n\n",
              "# Mezcla lineal + suave\n",
              "y ~ x1 + s(x2)\n\n",
              "# Controlar dimensi\u00f3n del spline\n",
              "y ~ s(x1, k = 5) + s(x2, k = 10)"
            ),
            div(
              class = "alert alert-info small",
              bs_icon("info-circle", class = "me-1"),
              strong("Par\u00e1metro k:"), " controla la flexibilidad m\u00e1xima ",
              "del spline (n\u00famero de nodos). brms usa por defecto k = 10. ",
              "Valores peque\u00f1os (k = 3-5) producen curvas m\u00e1s suaves. ",
              "El suavizado real est\u00e1 controlado por el prior."
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("journal-bookmark", class = "me-1"),
                        "Fundamentos"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Splines bayesianos en brms"),
          p(class = "small text-muted mb-3",
            "Los splines en brms se implementan como ",
            strong("efectos aleatorios gaussianos"),
            ". Cada t\u00e9rmino ", tags$code("s(x)"),
            " se descompone en una parte penalizada (controlada por un prior) ",
            "y una parte no penalizada. El grado de suavizado emerge ",
            "autom\u00e1ticamente del prior y los datos."
          ),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("bezier2",
                        style = paste0("color:", colores$primario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "1. Suavizado controlado por priors")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "El prior sobre los coeficientes del spline controla cu\u00e1nto ",
                "se permite que la curva sea irregular. Un prior m\u00e1s estrecho ",
                "produce curvas m\u00e1s suaves."),
              p(class = "small text-muted mb-0",
                strong("\u00bfC\u00f3mo verificarlo?"), " Prior predictive check: ",
                "simula curvas desde el prior y verifica que tienen ",
                "formas plausibles para el problema.")
            )
          ),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("graph-up",
                        style = paste0("color:", colores$secundario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario, "; font-weight:700;"),
                   "2. Incertidumbre sobre la curva")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "A diferencia de mgcv, el GAM bayesiano produce una ",
                strong("distribuci\u00f3n posterior de curvas"),
                ", no solo una curva estimada. Cada muestra MCMC es ",
                "una curva plausible dado los datos y el prior."),
              p(class = "small text-muted mb-0",
                "La banda de incertidumbre en los gr\u00e1ficos refleja ",
                "la variabilidad real sobre la forma de la relaci\u00f3n, ",
                "no solo un intervalo de confianza aproximado.")
            )
          ),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("toggles",
                        style = paste0("color:", colores$acento,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento, "; font-weight:700;"),
                   "3. Familia de distribuci\u00f3n")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "Como en el GLM bayesiano, puedes usar cualquier familia: ",
                "gaussian, binomial, Poisson, binomial negativa, Beta, ",
                "zero-inflated. El t\u00e9rmino suave opera en la escala del enlace."),
              p(class = "small text-muted mb-0",
                strong("Ejemplo:"), " para modelar cobertura vegetal (0-1) ",
                "con una relaci\u00f3n no lineal con la altitud: ",
                tags$code("brm(cobertura ~ s(altitud), family = Beta())"))
            )
          ),

          div(class = "card-muestreo mb-0",
            style = paste0("border-left:4px solid ", colores$peligro, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("exclamation-triangle",
                        style = paste0("color:", colores$peligro,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$peligro, "; font-weight:700;"),
                   "4. Consideraciones pr\u00e1cticas")),
            layout_columns(col_widths = c(6, 6),
              p(class = "small text-muted mb-0",
                "El GAM bayesiano es m\u00e1s lento que mgcv porque requiere MCMC. ",
                "Con m\u00e1s de 2-3 t\u00e9rminos suaves y muestras grandes puede ",
                "tardar varios minutos."),
              p(class = "small text-muted mb-0",
                strong("Recomendaci\u00f3n:"), " empieza con k peque\u00f1o (5-7) ",
                "para explorar, luego aumenta si la curva parece muy r\u00edgida. ",
                "Usa 2 cadenas para explorar, 4 para el an\u00e1lisis final.")
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(navset_pill(

          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("collection", class = "me-1"),
                            "Datos de ejemplo"),
            br(),
            layout_columns(col_widths = c(4, 8),
              div(
                radioButtons(ns("fuente_datos_gamb"),
                  label = tagList(bs_icon("database", class = "me-1"),
                                  "Seleccionar dataset:"),
                  choices = c(
                    "Densidad de especie de ave (Loyn, 1987)"       = "birdabundance_lm",
                    "Peso al nacer \u2014 salud perinatal (Hosmer)"  = "birthwt_lm",
                    "Presencia de \u00e1caros NPRA \u2014 binomial"  = "mite_logistic",
                    "Abundancia de \u00e1caros Brachy \u2014 Poisson" = "mite_counts",
                    "Riqueza de hormigas \u2014 Poisson / BN"        = "ants_glm",
                    "Cangrejos herradura \u2014 Poisson / BN"        = "hcrabs_glm"
                  ),
                  selected = "birdabundance_lm"
                ),
                tags$hr(),
                uiOutput(ns("info_dataset_gamb"))
              ),
              card(
                fill = FALSE,
                card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                card_body(style = "overflow:auto;",
                  uiOutput(ns("cards_datos_gamb")), br(),
                  DTOutput(ns("tabla_preview_gamb"))
                )
              )
            )
          ),

          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("folder2-open", class = "me-1"),
                            "Mis datos"),
            br(),
            layout_columns(col_widths = c(4, 8),
              div(
                p(class = "small text-muted mb-3",
                  bs_icon("info-circle", class = "me-1"),
                  "Sube un archivo CSV o Excel. ",
                  "La primera fila debe contener los nombres de las columnas."),
                fileInput(ns("archivo_gamb"),
                  label = "Seleccionar archivo:",
                  accept = c(".csv", ".xlsx", ".xls"),
                  buttonLabel = "Buscar\u2026",
                  placeholder = "CSV o Excel"),
                selectInput(ns("separador_gamb"), "Separador (CSV):",
                  choices = c("Coma (,)" = ",", "Punto y coma (;)" = ";",
                              "Tabulador" = "\t")),
                tags$hr(),
                uiOutput(ns("resumen_datos_propio_gamb"))
              ),
              card(
                fill = FALSE,
                card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                card_body(style = "overflow:auto;",
                  uiOutput(ns("cards_datos_propio_gamb")), br(),
                  DTOutput(ns("tabla_preview_propio_gamb"))
                )
              )
            )
          ),

          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("sliders2", class = "me-1"),
                            "Tipos de variables"),
            br(),
            p(class = "small text-muted mb-3",
              "Verifica que cada variable tenga el tipo correcto. ",
              "Los predictores para t\u00e9rminos suaves ", tags$code("s()"),
              " deben ser ", strong("num\u00e9ricos"), "."),
            layout_columns(col_widths = c(10, 2),
              uiOutput(ns("tabla_tipos_gamb")),
              div(class = "pt-2",
                actionButton(ns("aplicar_tipos_gamb"), "Aplicar tipos",
                             class = "btn-primary w-100", icon = icon("check")),
                br(), br(),
                actionButton(ns("resetear_tipos_gamb"), "Restaurar",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon = icon("rotate-left"))
              )
            ),
            uiOutput(ns("tipos_aplicados_msg_gamb")),

            tags$hr(),
            layout_columns(
              col_widths = c(4, 8),
              radioButtons(
                ns("manejo_na_gamb"),
                label    = tagList(bs_icon("exclamation-diamond", class = "me-1"),
                                   "Valores perdidos (NA)"),
                choices  = c(
                  "Conservar"             = "conservar",
                  "Eliminar filas con NA" = "eliminar"
                ),
                selected = "conservar"
              ),
              uiOutput(ns("na_info_gamb"))
            )
          )
        ))
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Explorar
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("zoom-in", class = "me-1"), "Explorar"),
        card_body(
          p(class = "small text-muted mb-3",
            "Visualiza las relaciones entre variables. En el GAM bayesiano, ",
            "los t\u00e9rminos suaves son \u00fatiles cuando la relaci\u00f3n ",
            "es claramente no lineal (curva, unimodal, con umbral)."),
          layout_columns(col_widths = c(4, 8), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_x_gamb")),
                uiOutput(ns("sel_var_y_gamb")),
                uiOutput(ns("sel_color_gamb")),
                checkboxInput(ns("mostrar_suave_gamb"),
                              "Mostrar curva suavizada", value = TRUE),
                tags$hr(),
                uiOutput(ns("sugerencia_no_lineal_gamb"))
              )
            ),
            plotOutput(ns("plot_scatter_gamb"), height = "380px")
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Priors
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("sliders", class = "me-1"), "Priors"),
        card_body(
          p(class = "small text-muted mb-3",
            "En el GAM bayesiano los priors controlan el ",
            strong("grado de suavizado"), " de las curvas. ",
            "brms usa priors t-Student por defecto para los coeficientes ",
            "de los splines, lo que permite curvas m\u00e1s flexibles. ",
            "Un prior m\u00e1s estrecho produce curvas m\u00e1s suaves."),
          layout_columns(col_widths = c(4, 8),
            card(
              fill = FALSE,
              card_header(bs_icon("gear", class = "me-1"),
                          "Configuraci\u00f3n de priors"),
              card_body(
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Intercepto \u2014 \u03b2\u2080"),
                selectInput(ns("prior_intercept_dist_gamb"), "Distribuci\u00f3n:",
                            choices = c("Normal" = "normal",
                                        "Student-t" = "student_t",
                                        "Cauchy" = "cauchy"),
                            selected = "student_t"),
                fluidRow(
                  column(6, numericInput(ns("prior_intercept_mu_gamb"),
                                         "Media:", value = 0, step = 0.5)),
                  column(6, numericInput(ns("prior_intercept_sd_gamb"),
                                         "Escala:", value = 2.5,
                                         min = 0.1, step = 0.5))
                ),
                tags$hr(),
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Efectos lineales \u2014 \u03b2"),
                selectInput(ns("prior_b_dist_gamb"), "Distribuci\u00f3n:",
                            choices = c("Normal" = "normal",
                                        "Student-t" = "student_t",
                                        "Cauchy" = "cauchy"),
                            selected = "normal"),
                fluidRow(
                  column(6, numericInput(ns("prior_b_mu_gamb"),
                                         "Media:", value = 0, step = 0.5)),
                  column(6, numericInput(ns("prior_b_sd_gamb"),
                                         "DE:", value = 1, min = 0.1, step = 0.5))
                ),
                tags$hr(),
                div(class = "alert alert-info small py-2 px-3 mb-2",
                  bs_icon("info-circle", class = "me-1"),
                  strong("Splines:"), " brms gestiona autom\u00e1ticamente los priors ",
                  "de los t\u00e9rminos suaves ", tags$code("s()"), ". El suavizado ",
                  "emerge de la interacci\u00f3n entre los datos y el prior."),
                actionButton(ns("ver_ppc_gamb"), "Prior predictive check",
                             icon = icon("eye"),
                             class = "btn-outline-primary w-100 btn-sm")
              )
            ),
            div(
              card(class = "mb-3",
                card_header(bs_icon("code-slash", class = "me-1"),
                            "C\u00f3digo de priors"),
                card_body(verbatimTextOutput(ns("codigo_priors_gamb")))
              ),
              card(class = "mb-0",
                card_header(bs_icon("eye", class = "me-1"),
                            "Prior predictive check",
                            span(class = "text-muted small ms-2",
                                 "\u2014 datos simulados desde el prior")),
                card_body(
                  plotOutput(ns("plot_ppc_prior_gamb"), height = "280px"),
                  uiOutput(ns("msg_ppc_prior_gamb"))
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
        fillable = FALSE,
        title = tagList(bs_icon("gear", class = "me-1"), "Ajustar modelo"),
        card_body(
          layout_columns(col_widths = c(4, 8),
            card(
              fill = FALSE,
              card_header(bs_icon("toggles", class = "me-1"),
                          "Especificar el modelo"),
              card_body(
                p(class = "small text-muted",
                  "Selecciona la familia, la variable respuesta y los predictores. ",
                  "Los predictores num\u00e9ricos pueden ser lineales o suaves."),
                selectInput(ns("familia_gamb"), "Familia de distribuci\u00f3n:",
                  choices = c(
                    "Gaussian (continua)"             = "gaussian",
                    "Binomial (log\u00edstica)"        = "binomial",
                    "Poisson"                         = "poisson",
                    "Binomial negativa"               = "negbinomial",
                    "Beta (proporciones)"             = "beta"
                  ), selected = "gaussian"),
                uiOutput(ns("info_familia_gamb")),
                tags$hr(),
                uiOutput(ns("sel_var_y_mod_gamb")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores num\u00e9ricos"),
                p(class = "small text-muted mb-1",
                  bs_icon("info-circle", class = "me-1"),
                  "Marca ", strong("Suave"), " para usar ", tags$code("s()"),
                  " o deja sin marcar para efecto lineal."),
                uiOutput(ns("checks_numericos_gamb")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores categ\u00f3ricos (siempre lineales)"),
                uiOutput(ns("checks_categoricos_gamb")),
                tags$hr(),
                numericInput(ns("k_spline_gamb"),
                  label = tagList("Par\u00e1metro k (dimensi\u00f3n del spline):",
                    tags$small(class = "text-muted d-block mt-1",
                      "Controla la flexibilidad m\u00e1xima. Default: 10. ",
                      "Rango recomendado: 5-15.")),
                  value = 10, min = 3, max = 20),
                tags$hr(),
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   bs_icon("activity", class = "me-1"), "Opciones MCMC"),
                fluidRow(
                  column(6, numericInput(ns("mcmc_chains_gamb"), "Cadenas:",
                               value = 4, min = 1, max = 8)),
                  column(6, numericInput(ns("mcmc_iter_gamb"), "Iteraciones:",
                               value = 2000, min = 500, max = 10000, step = 500))
                ),
                actionButton(ns("ajustar_gamb"), "Ajustar modelo",
                             class = "btn-primary w-100", icon = icon("play")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  bs_icon("floppy", class = "me-1"), "Guardar para comparar"),
                textInput(ns("nombre_modelo_gamb"), label = NULL,
                          placeholder = "Ej: suave_area, lineal_area\u2026"),
                actionButton(ns("guardar_modelo_gamb"), "Guardar modelo",
                             class = "btn-outline-primary w-100 btn-sm",
                             icon = icon("floppy-disk"))
              )
            ),
            div(
              uiOutput(ns("cards_metricas_gamb")), br(),
              layout_columns(col_widths = c(6, 6),
                card(
                  fill = FALSE,
                  card_header(bs_icon("bullseye", class = "me-1"),
                              "Predichos vs. observados"),
                  card_body(
                    p(class = "small text-muted",
                      "Puntos cerca de la diagonal = buen ajuste."),
                    plotOutput(ns("plot_predobs_gamb"), height = "240px")
                  )
                ),
                card(
                  fill = FALSE,
                  card_header(bs_icon("code-slash", class = "me-1"),
                              "F\u00f3rmula ajustada"),
                  card_body(verbatimTextOutput(ns("formula_ajustada_gamb")))
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
        fillable = FALSE,
        title = tagList(bs_icon("activity", class = "me-1"),
                        "Diagn\u00f3stico MCMC"),
        card_body(
          p(class = "small text-muted mb-3",
            "Verifica convergencia: ", strong("R\u0302 < 1.01"), " y ",
            strong("ESS > 400"), ". Los splines generan m\u00e1s par\u00e1metros ",
            "que un GLM, por lo que es especialmente importante verificar ",
            "la convergencia de todos los t\u00e9rminos."),
          layout_columns(col_widths = c(4, 8),
            card(
              fill = FALSE,
              card_header(bs_icon("stopwatch", class = "me-1"),
                          "Diagn\u00f3stico de convergencia"),
              card_body(uiOutput(ns("semaforo_mcmc_gamb")))
            ),
            div(navset_pill(
              nav_panel(title = "Traceplots", fillable = FALSE, br(),
                p(class = "small text-muted mb-2",
                  "Las cadenas deben mezclarse como ",
                  strong("orugas peludas"), " superpuestas."),
                selectInput(ns("param_trace_gamb"), "Par\u00e1metro:",
                            choices = NULL),
                plotOutput(ns("plot_trace_gamb"), height = "280px")
              ),
              nav_panel(title = "Densidades", fillable = FALSE, br(),
                p(class = "small text-muted mb-2",
                  "Las densidades de las cadenas deben superponerse."),
                plotOutput(ns("plot_dens_mcmc_gamb"), height = "280px")
              ),
              nav_panel(title = "Posterior predictive check", fillable = FALSE, br(),
                p(class = "small text-muted mb-2",
                  "Datos observados (l\u00ednea oscura) vs. r\u00e9plicas del posterior."),
                plotOutput(ns("plot_ppc_post_gamb"), height = "280px")
              ),
              nav_panel(title = "R\u0302 y ESS", fillable = FALSE, br(),
                div(class = "alert alert-info small mb-3",
                  bs_icon("info-circle", class = "me-1"),
                  strong("Nota sobre splines:"), " los t\u00e9rminos suaves generan ",
                  "m\u00faltiples par\u00e1metros (uno por coeficiente del spline). ",
                  "Es normal ver muchos par\u00e1metros en la tabla. ",
                  "ESS puede superar las 4000 muestras por anticorrelaci\u00f3n."),
                DTOutput(ns("tabla_rhat_gamb"))
              )
            ))
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 8: Performance
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("speedometer2", class = "me-1"), "Performance"),
        card_body(
          p(class = "small text-muted mb-3",
            "M\u00e9tricas de rendimiento del GAM bayesiano. ",
            "LOO y WAIC permiten comparar modelos con y sin t\u00e9rminos suaves."),
          layout_columns(col_widths = c(6, 6),
            card(
              fill = FALSE,
              card_header(bs_icon("speedometer2", class = "me-1"),
                          "M\u00e9tricas del modelo",
                          span(class = "text-muted small ms-2",
                               "\u2014 brms \u00b7 loo")),
              card_body(uiOutput(ns("tabla_performance_gamb")))
            ),
            div(
              card(class = "mb-3",
                card_header(bs_icon("bullseye", class = "me-1"),
                            "Predicho vs. observado",
                            span(class = "text-muted small ms-2",
                                 "\u2014 media posterior")),
                card_body(plotOutput(ns("plot_predobs_perf_gamb"),
                                     height = "240px"))
              ),
              card(class = "mb-0",
                card_header(bs_icon("info-circle", class = "me-1"),
                            "Interpretaci\u00f3n de m\u00e9tricas"),
                card_body(tags$ul(class = "small text-muted mb-0",
                  tags$li(strong("R\u00b2 bayesiano:"),
                          " proporci\u00f3n de varianza explicada."),
                  tags$li(strong("ELPD-LOO:"),
                          " capacidad predictiva. M\u00e1s alto = mejor."),
                  tags$li(strong("RMSE:"),
                          " error cuadr\u00e1tico medio posterior."),
                  tags$li(strong("mean_PPD:"),
                          " debe estar cerca de la media de Y.")
                ))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 9: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("table", class = "me-1"), "Par\u00e1metros"),
        div(class = "p-3",
          p(class = "small text-muted mb-3",
            "Los efectos lineales se interpretan igual que en el LM/GLM bayesiano. ",
            "Los t\u00e9rminos suaves ", tags$code("s(x)"),
            " generan m\u00faltiples par\u00e1metros \u2014 su efecto total se visualiza ",
            "mejor en la pesta\u00f1a ", strong("Gr\u00e1ficos"), "."),
          layout_columns(col_widths = c(6, 6), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("layout-text-sidebar", class = "me-1"),
                          "Tabla de coeficientes",
                          span(class = "text-muted small ms-2",
                               "\u2014 distribuci\u00f3n posterior")),
              card_body(style = "overflow:visible; height:auto;",
                uiOutput(ns("tabla_params_ui_gamb")))
            ),
            card(
              fill = FALSE,
              card_header(bs_icon("bar-chart-fill", class = "me-1"),
                          "Forest plot",
                          span(class = "text-muted small ms-2",
                               "\u2014 efectos lineales \u00b1 IC 95%")),
              card_body(
                p(class = "small text-muted",
                  "Solo se muestran los efectos lineales (b_). ",
                  "Los splines se visualizan en Gr\u00e1ficos."),
                plotOutput(ns("plot_forest_gamb"), height = "300px")
              )
            )
          ),
          div(class = "mt-3",
            card(
              fill = FALSE,
              card_header(bs_icon("bar-chart-steps", class = "me-1"),
                          "Importancia de variables",
                          span(class = "text-muted small ms-2",
                               "\u2014 probabilidad de direcci\u00f3n (pd)")),
              card_body(
                p(class = "small text-muted mb-2",
                  "Solo para efectos lineales. pd > 95% \u2248 p < 0.05."),
                plotOutput(ns("plot_pd_gamb"), height = "200px")
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 10: Gráficos
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"),
                        "Gr\u00e1ficos"),
        card_body(navset_pill(
          nav_panel(title = "Curvas suavizadas", fillable = FALSE, br(),
            p(class = "small text-muted mb-3",
              "Curvas suavizadas posteriores para cada t\u00e9rmino ",
              tags$code("s()"), ". La banda sombreada es el IC credible 95%. ",
              "Cada curva representa el efecto parcial del predictor."),
            plotOutput(ns("plot_smooth_gamb"), height = "400px")
          ),
          nav_panel(title = "Distribuciones posteriores", fillable = FALSE, br(),
            p(class = "small text-muted mb-3",
              "Distribuci\u00f3n posterior de los efectos lineales."),
            plotOutput(ns("plot_areas_gamb"), height = "380px")
          ),
          nav_panel(title = "Predicho vs. observado", fillable = FALSE, br(),
            plotOutput(ns("plot_predobs_graf_gamb"), height = "380px")
          ),
          nav_panel(title = "Residuos", fillable = FALSE, br(),
            plotOutput(ns("plot_resid_gamb"), height = "380px")
          )
        ))
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 11: Efectos marginales
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("arrows-angle-expand", class = "me-1"),
                        "Efectos marginales"),
        card_body(
          p(class = "small text-muted mb-3",
            "Efecto de cada predictor sobre Y manteniendo el resto en sus ",
            "valores t\u00edpicos. Para t\u00e9rminos suaves, muestra la curva ",
            "posterior con IC credible 95%."),
          layout_columns(col_widths = c(4, 8),
            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_pred_marginal_gamb")),
                tags$hr(),
                checkboxInput(ns("marginal_ci_gamb"),
                              "Mostrar IC credible 95%", value = TRUE),
                checkboxInput(ns("marginal_puntos_gamb"),
                              "Mostrar datos observados", value = TRUE)
              )
            ),
            div(
              card(
                fill = FALSE,
                card_header(bs_icon("graph-up-arrow", class = "me-1"),
                            "Efecto marginal posterior"),
                card_body(plotOutput(ns("plot_marginal_gamb"), height = "380px"))
              ),
              br(),
              uiOutput(ns("marginal_tipo_gamb"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 12: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("arrow-left-right", class = "me-1"),
                        "Comparar modelos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Compara modelos con y sin t\u00e9rminos suaves usando ",
            strong("LOO"), " y ", strong("WAIC"), ". ",
            "Esto permite evaluar si el suavizado mejora la predicci\u00f3n."),
          div(class = "alert alert-info small mb-3",
            bs_icon("lightbulb", class = "me-1"),
            strong("Tip:"), " ajusta el mismo modelo con ",
            tags$code("y ~ s(x)"), " (suave) y ",
            tags$code("y ~ x"), " (lineal) y comp\u00e1ralos. ",
            "Si LOO no mejora, la relaci\u00f3n es suficientemente lineal."),
          layout_columns(col_widths = c(4, 8),
            card(
              fill = FALSE,
              card_header(bs_icon("list-check", class = "me-1"),
                          "Modelos guardados"),
              card_body(
                uiOutput(ns("lista_modelos_guardados_gamb")), tags$hr(),
                actionButton(ns("limpiar_modelos_gamb"), "Limpiar todos",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon = icon("trash"))
              )
            ),
            div(
              card(class = "mb-3",
                card_header(bs_icon("table", class = "me-1"),
                            "Tabla comparativa",
                            span(class = "text-muted small ms-2",
                                 "\u2014 LOO y WAIC")),
                card_body(uiOutput(ns("tabla_comparacion_gamb")))
              ),
              card(class = "mb-0",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Gr\u00e1fico comparativo LOO"),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Mayor ELPD = mejor predicci\u00f3n. M\u00ednimo 2 modelos."),
                  plotOutput(ns("plot_comparacion_gamb"), height = "300px")
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
        fillable = FALSE,
        title = tagList(bs_icon("code-slash", class = "me-1"), "C\u00f3digo R"),
        card_body(
          p(class = "text-muted small mb-3",
            "Script reproducible con ", strong("brms"),
            ". Se actualiza seg\u00fan las selecciones activas."),
          card(
            fill = FALSE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " Script reproducible"),
              downloadButton(ns("descargar_script_gamb"),
                             label = "Descargar .R",
                             icon = bs_icon("download"),
                             class = "btn-sm btn-outline-primary")
            ),
            verbatimTextOutput(ns("codigo_r_gamb"))
          )
        )
      )

    ) # fin navset_card_tab
  )
}

# ── SERVER ────────────────────────────────────────────────
mod_gam_bayes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos de ejemplo ──────────────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_gamb
      req(!is.null(fuente) && nchar(fuente) > 0)
      tryCatch({
        e <- new.env()
        load(system.file("app/data", paste0(fuente, ".rda"),
                         package = "StatBayes"), envir = e)
        df <- get(ls(e)[1], envir = e)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(err) {
        showNotification(paste("Error:", err$message), type = "error"); NULL
      })
    })

    datos_mod <- reactiveVal(NULL)
    observeEvent(datos(), { datos_mod(datos()) })

    # ── Info dataset ──────────────────────────────────
    output$info_dataset_gamb <- renderUI({
      fuente <- input$fuente_datos_gamb
      if (is.null(fuente)) return(NULL)
      textos <- list(
        birdabundance_lm = tagList(
          strong("Densidad de especie de ave (Loyn, 1987)."),
          " 56 fragmentos de bosque. Variables continuas ideales para splines: ",
          strong("area_ha"), ", ", strong("distancia_m"), ", ", strong("altitud_m"), "."
        ),
        birthwt_lm = tagList(
          strong("Peso al nacer (Hosmer & Lemeshow)."),
          " 189 neonatos. Relaciones no lineales entre ",
          strong("edad_madre"), " / ", strong("peso_madre"), " y peso al nacer."
        ),
        mite_logistic = tagList(
          strong("Presencia de \u00e1caros NPRA (Borcard & Legendre, 1994)."),
          " 70 muestras. Relaciones no lineales con ",
          strong("densidad_sustrato"), " y ", strong("contenido_agua"), "."
        ),
        mite_counts = tagList(
          strong("Abundancia de \u00e1caros Brachy."),
          " 70 muestras. Relaciones unimodales con ",
          strong("densidad_sustrato"), " y ", strong("contenido_agua"), "."
        ),
        ants_glm = tagList(
          strong("Riqueza de hormigas (GLMsData)."),
          " 44 sitios. Gradiente latitudinal potencialmente no lineal."
        ),
        hcrabs_glm = tagList(
          strong("Cangrejos herradura (Brockmann, 1996)."),
          " 173 hembras. Relaci\u00f3n no lineal entre ancho y sat\u00e9lites."
        )
      )
      info <- textos[[fuente]]
      if (is.null(info)) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mb-0",
          bs_icon("info-circle-fill", class = "me-1"), info)
    })

    output$cards_datos_gamb <- renderUI({
      req(datos())
      d <- datos()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(col_widths = c(4, 4, 4),
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

    output$tabla_preview_gamb <- renderDT({
      req(datos())
      datatable(datos(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Datos propios ─────────────────────────────────
    datos_propio_gamb <- reactive({
      req(input$archivo_gamb)
      ext <- tools::file_ext(input$archivo_gamb$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo_gamb$datapath)
        else
          readr::read_delim(input$archivo_gamb$datapath,
                            delim = input$separador_gamb %||% ",",
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error"); NULL
      })
    })

    observeEvent(datos_propio_gamb(), { datos_mod(datos_propio_gamb()) })

    output$resumen_datos_propio_gamb <- renderUI({
      req(datos_propio_gamb())
      d <- datos_propio_gamb()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_gamb <- renderUI({
      req(datos_propio_gamb())
      d <- datos_propio_gamb()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(col_widths = c(4, 4, 4),
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

    output$tabla_preview_propio_gamb <- renderDT({
      req(datos_propio_gamb())
      datatable(datos_propio_gamb(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Tipos de variables ────────────────────────────
    tipos_usuario_gamb <- reactiveVal(NULL)

    output$tabla_tipos_gamb <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_gamb()
      filas <- lapply(names(d), function(nm) {
        col    <- d[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor" else "numeric"
        icono  <- if (actual == "factor")
          bs_icon("tag-fill", style = paste0("color:", colores$acento))
        else bs_icon("123", style = paste0("color:", colores$primario))
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
                  selectInput(ns(paste0("tipo_", nm)), label = NULL,
                    choices = c("Num\u00e9rico" = "numeric",
                                "Factor" = "factor",
                                "Excluir" = "excluir"),
                    selected = sel, width = "160px")),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  if (!is.null(tu) && !is.null(tu[[nm]]) && tu[[nm]] != actual)
                    tags$span(class = "badge",
                              style = paste0("background:", colores$exito),
                              "Modificado")
                  else tags$span(class = "text-muted small", "Sin cambios"))
        )
      })
      tags$table(class = "table table-sm table-hover small mb-0",
        tags$thead(
          style = paste0("background:", colores$primario,
                         " !important; color:#fff !important;"),
          tags$tr(tags$th(style = "padding:7px 8px;", "Variable"),
                  tags$th(style = "padding:7px 8px;", "Tipo detectado"),
                  tags$th(style = "padding:7px 8px;", "Tipo a usar"),
                  tags$th(style = "padding:7px 8px;", "Estado"))
        ),
        tags$tbody(filas)
      )
    })

    observeEvent(input$aplicar_tipos_gamb, {
      req(datos_mod())
      d  <- datos_mod()
      tu <- setNames(lapply(names(d), function(nm) input[[paste0("tipo_", nm)]]),
                     names(d))
      tipos_usuario_gamb(tu)
      for (nm in names(d)) {
        nuevo <- tu[[nm]]
        if (!is.null(nuevo) && nuevo != "excluir")
          d[[nm]] <- switch(nuevo,
            numeric = as.numeric(d[[nm]]),
            factor  = as.factor(d[[nm]]))
      }
      excluir <- names(tu)[sapply(tu, function(t) !is.null(t) && t == "excluir")]
      if (length(excluir) > 0)
        d <- d[, !names(d) %in% excluir, drop = FALSE]
      datos_mod(d)
      showNotification("Tipos aplicados.", type = "message", duration = 2)
    })

    output$tipos_aplicados_msg_gamb <- renderUI({
      tu <- tipos_usuario_gamb(); if (is.null(tu)) return(NULL)
      n_excl <- sum(sapply(tu, function(t) !is.null(t) && t == "excluir"))
      if (n_excl == 0) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          paste0(n_excl, " variable(s) excluida(s)."))
    })

    observeEvent(input$resetear_tipos_gamb, {
      tipos_usuario_gamb(NULL); datos_mod(datos())
    })

    # ── Manejo de NAs ────────────────────────────────────────────────────────
    datos_finales_gamb <- reactive({
      df <- datos_mod()
      req(df)
      if (isTRUE(input$manejo_na_gamb == "eliminar")) {
        df <- tidyr::drop_na(df)
      }
      df
    })

    output$na_info_gamb <- renderUI({
      df_orig  <- datos_mod()
      df_final <- datos_finales_gamb()
      req(df_orig)
      n_na <- sum(!stats::complete.cases(df_orig))
      if (n_na == 0) return(
        div(class = "alert alert-success small py-2 px-3 mb-0",
            bs_icon("check-circle", class = "me-1"), "Sin valores perdidos.")
      )
      n_elim <- nrow(df_orig) - nrow(df_final)
      if (input$manejo_na_gamb == "eliminar")
        div(class = "alert alert-warning small py-2 px-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            paste0(n_elim, " fila(s) eliminadas. Quedan ", nrow(df_final), " filas."))
      else
        div(class = "alert alert-info small py-2 px-3 mb-0",
            bs_icon("info-circle", class = "me-1"),
            paste0(n_na, " fila(s) con NA. El modelo puede fallar o excluirlas ",
                   "autom\u00e1ticamente \u2014 pod\u00e9s eliminarlas arriba para mayor control."))
    })

    # ── Variables reactivas ───────────────────────────
    vars_num <- reactive({
      req(datos_finales_gamb())
      names(which(sapply(datos_finales_gamb(), is.numeric)))
    })
    vars_cat <- reactive({
      req(datos_finales_gamb())
      names(which(sapply(datos_finales_gamb(),
                         function(x) is.factor(x) || is.character(x))))
    })

    # ── Exploración ───────────────────────────────────
    output$sel_var_x_gamb <- renderUI({
      selectInput(ns("var_x_gamb"), "Variable X (predictor):",
                  choices = vars_num())
    })
    output$sel_var_y_gamb <- renderUI({
      selectInput(ns("var_y_exp_gamb"), "Variable Y (respuesta):",
                  choices = c(vars_num(), vars_cat()))
    })
    output$sel_color_gamb <- renderUI({
      selectInput(ns("var_color_gamb"), "Color por grupo (opcional):",
                  choices = c("Ninguno" = "ninguno", vars_cat()))
    })

    output$plot_scatter_gamb <- renderPlot({
      req(datos_finales_gamb(), input$var_x_gamb, input$var_y_exp_gamb)
      d <- datos_finales_gamb()
      x <- input$var_x_gamb; y <- input$var_y_exp_gamb
      req(x %in% names(d), y %in% names(d))
      p <- ggplot(d, aes(.data[[x]], as.numeric(.data[[y]]))) +
        geom_point(color = colores$primario, alpha = 0.5, size = 2) +
        labs(x = x, y = y) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = colores$fondo, color = NA))
      if (!is.null(input$var_color_gamb) &&
          input$var_color_gamb != "ninguno" &&
          input$var_color_gamb %in% names(d))
        p <- p + aes(color = .data[[input$var_color_gamb]]) +
          scale_color_tableau_cb()
      if (isTRUE(input$mostrar_suave_gamb))
        p <- p + geom_smooth(method = "gam", formula = y ~ s(x, k = 6),
                             se = TRUE, color = colores$acento, linewidth = 0.9)
      p
    })

    output$sugerencia_no_lineal_gamb <- renderUI({
      req(datos_finales_gamb(), input$var_x_gamb, input$var_y_exp_gamb)
      d <- datos_finales_gamb()
      x <- input$var_x_gamb; y <- input$var_y_exp_gamb
      if (!x %in% names(d) || !y %in% names(d) ||
          !is.numeric(d[[x]]) || !is.numeric(d[[y]])) return(NULL)
      cor_lin <- tryCatch(cor(d[[x]], d[[y]], use = "complete.obs"), error = function(e) NA)
      if (is.na(cor_lin)) return(NULL)
      clase <- if (abs(cor_lin) > 0.7) "sem-ok" else if (abs(cor_lin) > 0.3) "sem-warn" else "sem-bad"
      div(class = paste("small p-2 rounded", clase),
          bs_icon("arrow-left-right", class = "me-1"),
          strong("r = "), round(cor_lin, 3), br(),
          span(class = "text-muted small",
               if (abs(cor_lin) > 0.7)
                 "Relaci\u00f3n lineal fuerte \u2014 el LM bayesiano puede ser suficiente."
               else if (abs(cor_lin) > 0.3)
                 "Relaci\u00f3n moderada \u2014 verifica la curva para decidir."
               else
                 "Relaci\u00f3n d\u00e9bil o no lineal \u2014 el GAM bayesiano puede ser \u00fatil."))
    })

    # ── Priors ────────────────────────────────────────
    output$codigo_priors_gamb <- renderText({
      dist_int <- switch(input$prior_intercept_dist_gamb,
        normal    = paste0("normal(", input$prior_intercept_mu_gamb, ", ",
                           input$prior_intercept_sd_gamb, ")"),
        student_t = paste0("student_t(3, ", input$prior_intercept_mu_gamb, ", ",
                           input$prior_intercept_sd_gamb, ")"),
        cauchy    = paste0("cauchy(", input$prior_intercept_mu_gamb, ", ",
                           input$prior_intercept_sd_gamb, ")")
      )
      dist_b <- switch(input$prior_b_dist_gamb,
        normal    = paste0("normal(", input$prior_b_mu_gamb, ", ",
                           input$prior_b_sd_gamb, ")"),
        student_t = paste0("student_t(3, ", input$prior_b_mu_gamb, ", ",
                           input$prior_b_sd_gamb, ")"),
        cauchy    = paste0("cauchy(", input$prior_b_mu_gamb, ", ",
                           input$prior_b_sd_gamb, ")")
      )
      paste0("c(\n  prior(", dist_int, ", class = Intercept),\n",
             "  prior(", dist_b, ", class = b)\n",
             ")\n# Nota: brms gestiona automáticamente\n",
             "# los priors de los términos s().")
    })

    observeEvent(input$ver_ppc_gamb, {
      req(datos_finales_gamb())
      n <- 100; x <- seq(0, 1, length.out = 50)
      mat <- replicate(n, {
        b0 <- rnorm(1, input$prior_intercept_mu_gamb,
                    input$prior_intercept_sd_gamb)
        rnorm(50, b0 + sin(2 * pi * x) * rnorm(1, 0, input$prior_b_sd_gamb), 0.5)
      })
      output$plot_ppc_prior_gamb <- renderPlot({
        df <- data.frame(y = as.vector(mat),
                         sim = rep(seq_len(n), each = 50))
        ggplot(df, aes(x = y, group = sim)) +
          geom_density(color = colores$primario, alpha = 0.06, linewidth = 0.3) +
          labs(x = "Valores simulados de Y", y = "Densidad de probabilidad",
               subtitle = paste0(n, " simulaciones desde el prior")) +
          theme_minimal(base_size = 13) +
          theme(panel.grid.minor = element_blank(),
                plot.background = element_rect(fill = colores$fondo, color = NA))
      })
      rango <- range(mat, na.rm = TRUE)
      output$msg_ppc_prior_gamb <- renderUI({
        clase <- if (abs(rango[1]) > 1e4 || abs(rango[2]) > 1e4) "sem-bad"
                 else if (abs(rango[1]) > 100 || abs(rango[2]) > 100) "sem-warn"
                 else "sem-ok"
        div(class = paste("small p-2 mt-2 rounded", clase),
            bs_icon(if (clase == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            if (clase == "sem-ok") "Rangos razonables. Prior adecuado."
            else if (clase == "sem-warn") "Rangos amplios. Considera priors m\u00e1s restrictivos."
            else "Rangos extremos. Prior demasiado difuso.")
      })
    })

    # ── Info familia ──────────────────────────────────
    output$info_familia_gamb <- renderUI({
      req(input$familia_gamb)
      switch(input$familia_gamb,
        gaussian    = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "Y continua. Los splines modelan relaciones no lineales ",
                          "en la escala original."),
        binomial    = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "Y binaria (0/1). Los splines operan en escala logit. ",
                          "exp(\u03b2) = odds ratio (OR) para efectos lineales."),
        poisson     = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "Y = conteos (0, 1, 2\u2026). Los splines operan en escala log. ",
                          "exp(\u03b2) = raz\u00f3n de tasas (IRR) para efectos lineales. ",
                          "Asume varianza = media."),
        negbinomial = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "Conteos sobredispersados. Los splines operan en escala log. ",
                          "exp(\u03b2) = IRR para efectos lineales. ",
                          "Par\u00e1metro de forma \u03b8 estimado autom\u00e1ticamente. ",
                          "En el mundo bayesiano no existe quasipoisson \u2014 la binomial ",
                          "negativa es la alternativa m\u00e1s rigurosa porque es una ",
                          "distribuci\u00f3n probabil\u00edstica real."),
        beta        = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "Y = proporciones en el intervalo abierto (0, 1). ",
                          strong("Sin ceros ni unos exactos:"),
                          " la distribuci\u00f3n Beta no est\u00e1 definida en los extremos. ",
                          "Si tienes valores de exactamente 0 o 1, usa una ",
                          strong("Beta inflada en cero/uno"), ". ",
                          "Los splines operan en escala logit.")
      )
    })

    # ── Ajuste ────────────────────────────────────────
    modelo_actual_gamb     <- reactiveVal(NULL)
    modelos_guardados_gamb <- reactiveVal(list())
    smooth_vars_gamb       <- reactiveVal(character(0))

    output$sel_var_y_mod_gamb <- renderUI({
      selectInput(ns("var_y_gamb"), "Variable respuesta (Y):",
                  choices = c(vars_num(), vars_cat()))
    })

    output$checks_numericos_gamb <- renderUI({
      ys  <- input$var_y_gamb %||% ""
      ops <- setdiff(vars_num(), ys)
      if (length(ops) == 0)
        return(p(class = "small text-muted", "No hay variables num\u00e9ricas."))
      tagList(lapply(ops, function(v) {
        div(class = "d-flex align-items-center gap-3 mb-1",
            checkboxInput(ns(paste0("lineal_", v)), label = v, value = FALSE),
            checkboxInput(ns(paste0("suave_", v)),
                          label = tagList(tags$code("s()"), " suave"),
                          value = FALSE)
        )
      }))
    })

    output$checks_categoricos_gamb <- renderUI({
      ys  <- input$var_y_gamb %||% ""
      ops <- setdiff(vars_cat(), ys)
      if (length(ops) == 0)
        return(p(class = "small text-muted", "No hay variables categ\u00f3ricas."))
      checkboxGroupInput(ns("preds_cat_gamb"), label = NULL, choices = ops)
    })

    formula_gamb <- reactive({
      req(input$var_y_gamb)
      ops <- setdiff(vars_num(), input$var_y_gamb %||% "")
      k   <- input$k_spline_gamb %||% 10
      terminos <- c()
      for (v in ops) {
        if (isTRUE(input[[paste0("suave_", v)]])) {
          terminos <- c(terminos, paste0("s(", v, ", k = ", k, ")"))
        } else if (isTRUE(input[[paste0("lineal_", v)]])) {
          terminos <- c(terminos, v)
        }
      }
      terminos <- c(terminos, input$preds_cat_gamb)
      if (length(terminos) == 0) terminos <- "1"
      smooth_vars_gamb(grep("^s\\(", terminos, value = TRUE))
      as.formula(paste(input$var_y_gamb, "~", paste(terminos, collapse = " + ")))
    })

    familia_brms_gamb <- reactive({
      switch(input$familia_gamb,
        gaussian    = gaussian(),
        binomial    = binomial(),
        poisson     = poisson(),
        negbinomial = brms::negbinomial(),
        beta        = brms::Beta()
      )
    })

    priors_gamb <- reactive({
      dist_int <- switch(input$prior_intercept_dist_gamb,
        normal    = brms::prior(normal(0, 2.5), class = Intercept),
        student_t = brms::prior(student_t(3, 0, 2.5), class = Intercept),
        cauchy    = brms::prior(cauchy(0, 2.5), class = Intercept)
      )
      c(dist_int)
    })

    observeEvent(input$ajustar_gamb, {
      req(datos_finales_gamb(), input$var_y_gamb)
      frm <- tryCatch(formula_gamb(), error = function(e) NULL)
      req(frm)
      d   <- datos_finales_gamb()
      withProgress(message = "Ajustando GAM bayesiano (MCMC)\u2026",
                   detail = "Puede tardar varios minutos.", value = 0.1, {
        tryCatch({
          fit <- brms::brm(
            formula = frm, data = d,
            family  = familia_brms_gamb(),
            prior   = priors_gamb(),
            chains  = input$mcmc_chains_gamb,
            iter    = input$mcmc_iter_gamb,
            cores   = parallel::detectCores(),
            refresh = 0, silent = 2
          )
          modelo_actual_gamb(fit); setProgress(1)
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 10)
        })
      })
    })

    output$formula_ajustada_gamb <- renderText({
      req(modelo_actual_gamb())
      deparse(formula_gamb())
    })

    # ── Métricas ──────────────────────────────────────
    output$cards_metricas_gamb <- renderUI({
      req(modelo_actual_gamb())
      fit <- modelo_actual_gamb()
      loo_val <- tryCatch({
        l <- loo::loo(fit)
        round(l$estimates["elpd_loo", "Estimate"], 1)
      }, error = function(e) "\u2014")
      waic_val <- tryCatch({
        w <- loo::waic(fit)
        round(w$estimates["elpd_waic", "Estimate"], 1)
      }, error = function(e) "\u2014")
      r2 <- tryCatch(round(brms::bayes_R2(fit)[1, "Estimate"], 3),
                     error = function(e) "\u2014")
      layout_columns(col_widths = c(4, 4, 4),
        card(class = "text-center",
          card_body(class = "p-2",
            h4(style = paste0("color:", colores$primario, "; font-weight:700;"), r2),
            p(class = "small text-muted mb-0", "R\u00b2 bayesiano"))),
        card(class = "text-center",
          card_body(class = "p-2",
            h4(style = paste0("color:", colores$secundario, "; font-weight:700;"), loo_val),
            p(class = "small text-muted mb-0", "ELPD-LOO"))),
        card(class = "text-center",
          card_body(class = "p-2",
            h4(style = paste0("color:", colores$acento, "; font-weight:700;"), waic_val),
            p(class = "small text-muted mb-0", "ELPD-WAIC")))
      )
    })

    output$plot_predobs_gamb <- renderPlot({
      req(modelo_actual_gamb())
      fit   <- modelo_actual_gamb()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      ggplot(data.frame(obs = obs, pred = preds), aes(obs, pred)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                    color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2) +
        labs(x = "Observado", y = "Predicho") +
        theme_minimal(base_size = 12) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    # ── Diagnóstico MCMC ──────────────────────────────
    output$semaforo_mcmc_gamb <- renderUI({
      req(modelo_actual_gamb())
      fit   <- modelo_actual_gamb()
      draws <- posterior::as_draws_df(fit)
      sumas <- posterior::summarise_draws(draws,
                 rhat = posterior::rhat, ess_bulk = posterior::ess_bulk)
      max_rhat <- max(as.numeric(sumas$rhat), na.rm = TRUE)
      min_ess  <- min(as.numeric(sumas$ess_bulk), na.rm = TRUE)
      clase_rhat <- if (max_rhat < 1.01) "sem-ok"
                   else if (max_rhat < 1.05) "sem-warn" else "sem-bad"
      clase_ess  <- if (min_ess > 400) "sem-ok"
                   else if (min_ess > 100) "sem-warn" else "sem-bad"
      ppd_card <- tryCatch({
        pp       <- posterior_predict(fit)
        mean_ppd <- round(mean(pp), 2)
        y_mean   <- round(mean(as.numeric(
          model.response(model.frame(fit))), na.rm = TRUE), 2)
        pct_diff <- abs(mean_ppd - y_mean) / abs(y_mean) * 100
        clase_ppd <- if (pct_diff < 5) "sem-ok"
                     else if (pct_diff < 15) "sem-warn" else "sem-bad"
        div(class = paste("p-2 rounded mb-2", clase_ppd),
            bs_icon(if (clase_ppd == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            strong("mean_PPD: "), mean_ppd, " \u2014 Y obs: ", y_mean, br(),
            span(class = "text-muted small",
                 paste0("Diferencia: ", round(pct_diff, 1), "%")))
      }, error = function(e) NULL)
      tagList(
        div(class = paste("p-2 rounded mb-2", clase_rhat),
            bs_icon(if (clase_rhat == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            strong("R\u0302 m\u00e1ximo: "), round(max_rhat, 4), br(),
            span(class = "text-muted small",
                 if (clase_rhat == "sem-ok") "Convergencia correcta (< 1.01)"
                 else if (clase_rhat == "sem-warn") "Convergencia marginal"
                 else "Sin convergencia")),
        div(class = paste("p-2 rounded mb-2", clase_ess),
            bs_icon(if (clase_ess == "sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class = "me-1"),
            strong("ESS m\u00ednimo: "), round(min_ess, 0), br(),
            span(class = "text-muted small",
                 if (clase_ess == "sem-ok") "ESS adecuado (> 400)"
                 else if (clase_ess == "sem-warn") "ESS marginal"
                 else "ESS insuficiente")),
        ppd_card
      )
    })

    observe({
      req(modelo_actual_gamb())
      pars <- brms::variables(modelo_actual_gamb())
      pars <- pars[grep("^b_|^sds_", pars)]
      updateSelectInput(session, "param_trace_gamb", choices = pars)
    })

    output$plot_trace_gamb <- renderPlot({
      req(modelo_actual_gamb(), input$param_trace_gamb)
      bayesplot::mcmc_trace(modelo_actual_gamb(),
                            pars = input$param_trace_gamb,
                            facet_args = list(ncol = 1)) +
        theme_minimal(base_size = 12)
    })

    output$plot_dens_mcmc_gamb <- renderPlot({
      req(modelo_actual_gamb())
      pars <- brms::variables(modelo_actual_gamb())
      pars <- pars[grep("^b_", pars)]
      if (length(pars) == 0) return(NULL)
      bayesplot::mcmc_dens_overlay(modelo_actual_gamb(), pars = pars) +
        theme_minimal(base_size = 12)
    })

    output$plot_ppc_post_gamb <- renderPlot({
      req(modelo_actual_gamb())
      bayesplot::pp_check(modelo_actual_gamb(), ndraws = 100) +
        theme_minimal(base_size = 12)
    })

    output$tabla_rhat_gamb <- renderDT({
      req(modelo_actual_gamb())
      fit   <- modelo_actual_gamb()
      draws <- posterior::as_draws_df(fit)
      sumas <- posterior::summarise_draws(draws,
                 rhat = posterior::rhat,
                 ess_bulk = posterior::ess_bulk,
                 ess_tail = posterior::ess_tail)
      df <- data.frame(
        Parametro = sumas$variable,
        Rhat      = round(as.numeric(sumas$rhat), 4),
        ESS_bulk  = round(as.numeric(sumas$ess_bulk), 0),
        ESS_tail  = round(as.numeric(sumas$ess_tail), 0),
        check.names = FALSE
      )
      df <- df[grep("^b_|^sds_|^sigma", df$Parametro), ]
      names(df) <- c("Par\u00e1metro", "R\u0302", "ESS bulk", "ESS tail")
      datatable(df, options = list(pageLength = 10), rownames = FALSE) |>
        DT::formatStyle("R\u0302",
          backgroundColor = DT::styleInterval(c(1.01, 1.05),
            c("#f0f9f5", "#fffbf0", "#fff0f2")))
    })

    # ── Performance ───────────────────────────────────
    output$tabla_performance_gamb <- renderUI({
      req(modelo_actual_gamb())
      fit <- modelo_actual_gamb()
      r2 <- tryCatch({
        r <- brms::bayes_R2(fit)
        paste0(round(r[1, "Estimate"], 3), " [",
               round(r[1, "Q2.5"], 3), ", ",
               round(r[1, "Q97.5"], 3), "]")
      }, error = function(e) "\u2014")
      rmse <- tryCatch({
        pe <- brms::predictive_error(fit)
        round(sqrt(mean(pe^2)), 3)
      }, error = function(e) "\u2014")
      loo_res <- tryCatch({
        l <- loo::loo(fit)
        paste0(round(l$estimates["elpd_loo", "Estimate"], 1),
               " (SE=", round(l$estimates["elpd_loo", "SE"], 1), ")")
      }, error = function(e) "\u2014")
      waic_res <- tryCatch({
        w <- loo::waic(fit)
        paste0(round(w$estimates["elpd_waic", "Estimate"], 1),
               " (SE=", round(w$estimates["elpd_waic", "SE"], 1), ")")
      }, error = function(e) "\u2014")
      mean_ppd <- tryCatch(round(mean(posterior_predict(fit)), 2),
                           error = function(e) "\u2014")
      y_mean   <- tryCatch(
        round(mean(as.numeric(model.response(model.frame(fit))), na.rm = TRUE), 2),
        error = function(e) "\u2014")
      tags$table(class = "table table-sm small",
        tags$thead(style = paste0("background:", colores$primario, "; color:#fff;"),
                   tags$tr(tags$th("M\u00e9trica"), tags$th("Valor"))),
        tags$tbody(
          tags$tr(tags$td(strong("R\u00b2 bayesiano \u00b1 IC 95%")), tags$td(r2)),
          tags$tr(style = paste0("background:", colores$fondo),
                  tags$td(strong("RMSE posterior")), tags$td(rmse)),
          tags$tr(tags$td(strong("ELPD-LOO \u00b1 SE")), tags$td(loo_res)),
          tags$tr(style = paste0("background:", colores$fondo),
                  tags$td(strong("ELPD-WAIC \u00b1 SE")), tags$td(waic_res)),
          tags$tr(tags$td(strong("Media Y observada")), tags$td(y_mean)),
          tags$tr(style = paste0("background:", colores$fondo),
                  tags$td(strong("mean_PPD")), tags$td(mean_ppd))
        )
      )
    })

    output$plot_predobs_perf_gamb <- renderPlot({
      req(modelo_actual_gamb())
      fit   <- modelo_actual_gamb()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      ggplot(data.frame(obs = obs, pred = preds), aes(obs, pred)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                    color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2.5) +
        geom_smooth(method = "lm", se = FALSE,
                    color = colores$acento, linewidth = 0.8) +
        labs(x = "Observado", y = "Predicho (media posterior)") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    # ── Parámetros ────────────────────────────────────
    output$tabla_params_ui_gamb <- renderUI({
      req(modelo_actual_gamb())
      DTOutput(ns("tabla_params_gamb"))
    })

    output$tabla_params_gamb <- renderDT({
      req(modelo_actual_gamb())
      pars <- parameters::model_parameters(modelo_actual_gamb(), ci = 0.95)
      df   <- as.data.frame(pars)
      df   <- df[, intersect(c("Parameter", "Median", "Mean", "SD",
                                "CI_low", "CI_high", "pd", "Rhat"),
                              names(df))]
      df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], round, 3)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
    })

    output$plot_forest_gamb <- renderPlot({
      req(modelo_actual_gamb())
      pars <- posterior::summarise_draws(modelo_actual_gamb(),
                mean, ~quantile(.x, c(0.025, 0.975)))
      pars <- pars[grep("^b_", pars$variable), ]
      if (nrow(pars) == 0) return(NULL)
      pars$variable <- gsub("^b_", "", pars$variable)
      names(pars)[3:4] <- c("lo", "hi")
      ggplot(pars, aes(x = mean, y = reorder(variable, mean),
                       xmin = lo, xmax = hi, color = (lo > 0 | hi < 0))) +
        geom_vline(xintercept = 0, linetype = "dashed", color = colores$texto) +
        geom_errorbarh(height = 0.25, linewidth = 0.6) +
        geom_point(size = 2.5) +
        scale_color_manual(values = c("FALSE" = colores$acento,
                                      "TRUE"  = colores$primario),
                           guide = "none") +
        labs(x = "Media posterior (IC 95%)", y = NULL) +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    output$plot_pd_gamb <- renderPlot({
      req(modelo_actual_gamb())
      pars <- parameters::model_parameters(modelo_actual_gamb(), ci = 0.95)
      df   <- as.data.frame(pars)
      df   <- df[grep("^b_", df$Parameter), ]
      if (!"pd" %in% names(df) || nrow(df) == 0) return(NULL)
      df$Parameter <- gsub("^b_", "", df$Parameter)
      if (max(df$pd, na.rm = TRUE) <= 1) df$pd <- df$pd * 100
      ggplot(df, aes(x = pd, y = reorder(Parameter, pd), color = pd > 95)) +
        geom_vline(xintercept = 95, linetype = "dashed", color = colores$texto) +
        geom_segment(aes(x = 0, xend = pd, yend = reorder(Parameter, pd)),
                     linewidth = 0.8) +
        geom_point(size = 4) +
        scale_color_manual(values = c("FALSE" = colores$acento,
                                      "TRUE"  = colores$primario),
                           guide = "none") +
        scale_x_continuous(limits = c(0, 105),
                           breaks = c(0, 25, 50, 75, 95, 100),
                           labels = function(x) paste0(x, "%")) +
        labs(x = "Probabilidad de direcci\u00f3n (%)", y = NULL) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    # ── Gráficos ──────────────────────────────────────
    output$plot_smooth_gamb <- renderPlot({
      req(modelo_actual_gamb())
      tryCatch({
        ef <- brms::conditional_effects(modelo_actual_gamb())
        plots <- plot(ef, plot = FALSE)
        if (length(plots) == 1) {
          plots[[1]] + theme_minimal(base_size = 13) +
            theme(plot.background = element_rect(fill = colores$fondo, color = NA))
        } else {
          # múltiples efectos — mostrar el primero suave
          idx <- which(grepl("s\\(", names(plots)))
          if (length(idx) > 0) {
            plots[[idx[1]]] + theme_minimal(base_size = 13) +
              theme(plot.background = element_rect(fill = colores$fondo, color = NA))
          } else {
            plots[[1]] + theme_minimal(base_size = 13)
          }
        }
      }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = paste("Error:", e$message)) + theme_void()
      })
    })

    output$plot_areas_gamb <- renderPlot({
      req(modelo_actual_gamb())
      pars <- brms::variables(modelo_actual_gamb())
      pars <- pars[grep("^b_", pars)]
      if (length(pars) == 0) return(NULL)
      bayesplot::mcmc_areas(modelo_actual_gamb(), pars = pars, prob = 0.95) +
        theme_minimal(base_size = 13)
    })

    output$plot_predobs_graf_gamb <- renderPlot({
      req(modelo_actual_gamb())
      fit   <- modelo_actual_gamb()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      ggplot(data.frame(obs = obs, pred = preds), aes(obs, pred)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                    color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2.5) +
        labs(x = "Observado", y = "Predicho") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    output$plot_resid_gamb <- renderPlot({
      req(modelo_actual_gamb())
      fit   <- modelo_actual_gamb()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      df    <- data.frame(ajustado = preds, residuo = obs - preds)
      ggplot(df, aes(ajustado, residuo)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2) +
        geom_smooth(method = "loess", se = FALSE,
                    color = colores$acento, linewidth = 0.8) +
        labs(x = "Valores ajustados", y = "Residuos") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    # ── Efectos marginales ────────────────────────────
    output$sel_pred_marginal_gamb <- renderUI({
      req(modelo_actual_gamb())
      ops <- setdiff(vars_num(), input$var_y_gamb %||% "")
      ops <- c(ops, input$preds_cat_gamb)
      selectInput(ns("pred_marginal_gamb"), "Predictor focal:", choices = ops)
    })

    output$plot_marginal_gamb <- renderPlot({
      req(modelo_actual_gamb(), input$pred_marginal_gamb)
      tryCatch({
        ef <- brms::conditional_effects(modelo_actual_gamb(),
                                         effects = input$pred_marginal_gamb)
        plot(ef, plot = FALSE)[[1]] +
          theme_minimal(base_size = 13) +
          theme(plot.background = element_rect(fill = colores$fondo, color = NA))
      }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = paste("Error:", e$message)) + theme_void()
      })
    })

    output$marginal_tipo_gamb <- renderUI({
      req(modelo_actual_gamb(), input$pred_marginal_gamb)
      pred <- input$pred_marginal_gamb
      sv   <- smooth_vars_gamb()
      es_suave <- any(grepl(paste0("s\\(", pred), sv))
      if (es_suave) {
        div(class = "alert alert-info small",
            bs_icon("bezier2", class = "me-1"),
            strong("T\u00e9rmino suave "), tags$code(paste0("s(", pred, ")")),
            ": la curva muestra el efecto no lineal del predictor sobre Y. ",
            "La banda sombreada es el IC credible 95% de la curva.")
      } else {
        div(class = "alert alert-info small",
            bs_icon("arrow-up-right", class = "me-1"),
            strong("Efecto lineal: "),
            "la l\u00ednea muestra el efecto constante del predictor sobre Y.")
      }
    })

    # ── Comparar modelos ──────────────────────────────
    observeEvent(input$guardar_modelo_gamb, {
      req(modelo_actual_gamb(), input$nombre_modelo_gamb != "")
      nm    <- input$nombre_modelo_gamb
      lista <- modelos_guardados_gamb()
      lista[[nm]] <- modelo_actual_gamb()
      modelos_guardados_gamb(lista)
      showNotification(paste("Modelo", nm, "guardado."), type = "message")
    })

    output$lista_modelos_guardados_gamb <- renderUI({
      lista <- modelos_guardados_gamb()
      if (length(lista) == 0)
        return(p(class = "small text-muted", "A\u00fan no hay modelos guardados."))
      tags$ul(class = "small",
              lapply(names(lista), function(nm)
                tags$li(bs_icon("check2", class = "me-1"), nm)))
    })

    observeEvent(input$limpiar_modelos_gamb, { modelos_guardados_gamb(list()) })

    output$tabla_comparacion_gamb <- renderUI({
      lista <- modelos_guardados_gamb()
      if (length(lista) < 2)
        return(div(class = "alert alert-info small",
                   "Guarda al menos 2 modelos para compararlos."))
      DTOutput(ns("dt_comparacion_gamb"))
    })

    output$dt_comparacion_gamb <- renderDT({
      lista <- modelos_guardados_gamb()
      req(length(lista) >= 2)
      tryCatch({
        loos <- lapply(lista, loo::loo)
        comp <- loo::loo_compare(loos)
        datatable(as.data.frame(round(comp, 2)),
                  options = list(pageLength = 10, scrollX = TRUE))
      }, error = function(e) data.frame(Error = e$message))
    })

    output$plot_comparacion_gamb <- renderPlot({
      lista <- modelos_guardados_gamb()
      req(length(lista) >= 2)
      tryCatch({
        loos <- lapply(lista, loo::loo)
        comp <- loo::loo_compare(loos)
        df   <- as.data.frame(comp); df$modelo <- rownames(df)
        ggplot(df, aes(x = elpd_diff, y = reorder(modelo, elpd_diff),
                       xmin = elpd_diff - se_diff,
                       xmax = elpd_diff + se_diff)) +
          geom_vline(xintercept = 0, linetype = "dashed", color = colores$texto) +
          geom_errorbarh(height = 0.25, color = colores$primario) +
          geom_point(size = 3, color = colores$primario) +
          labs(x = "Diferencia ELPD-LOO (\u00b1 SE)", y = NULL,
               caption = "Mayor ELPD = mejor predicci\u00f3n") +
          theme_minimal(base_size = 13) +
          theme(plot.background = element_rect(fill = colores$fondo, color = NA))
      }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = paste("Error:", e$message)) + theme_void()
      })
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_gamb <- renderText({
      req(input$var_y_gamb)
      frm_str <- tryCatch(deparse(formula_gamb()),
                          error = function(e) paste(input$var_y_gamb, "~ s(x)"))
      nm_datos <- input$fuente_datos_gamb %||% "mis_datos"
      familia_str <- switch(input$familia_gamb,
        gaussian    = "gaussian()",
        binomial    = "binomial()",
        poisson     = "poisson()",
        negbinomial = "negbinomial()",
        beta        = "Beta()"
      )
      paste0(
        "# ── GAM bayesiano con brms ───────────────────────────\n",
        "library(brms)\nlibrary(bayesplot)\nlibrary(posterior)\nlibrary(loo)\n\n",
        "# Datos\ndata('", nm_datos, "', package = 'StatBayes')\n\n",
        "# Ajustar modelo\nfit <- brm(\n",
        "  formula = ", frm_str, ",\n",
        "  data    = ", nm_datos, ",\n",
        "  family  = ", familia_str, ",\n",
        "  chains  = ", input$mcmc_chains_gamb, ",\n",
        "  iter    = ", input$mcmc_iter_gamb, ",\n",
        "  cores   = parallel::detectCores()\n)\n\n",
        "# Resumen\nsummary(fit)\n\n",
        "# Curvas suavizadas\nconditional_effects(fit)\n\n",
        "# Diagnóstico MCMC\nmcmc_trace(fit)\npp_check(fit, ndraws = 100)\n\n",
        "# Performance\nbayes_R2(fit)\nloo(fit)\n"
      )
    })

    output$descargar_script_gamb <- downloadHandler(
      filename = function() paste0("StatBayes_gam_bayes_", Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_gamb(), file)
    )

  })
}
