# ============================================================
# mod_mixed_bayes.R — Modelos mixtos bayesianos
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
mod_mixed_bayes_ui <- function(id) {
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
            h4(bs_icon("diagram-3", class = "me-2"),
               "Modelos mixtos bayesianos",
               style = paste0("color:", colores$primario, "; font-weight:700;")),
            p(class = "text-muted mb-0",
              "LMM y GLMM bayesianos con brms. Una sola funci\u00f3n ",
              tags$code("brm()"), " cubre ambos tipos \u2014 solo cambia la ",
              tags$code("family"), ". Efectos aleatorios con distribuci\u00f3n ",
              "posterior completa.")
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Modelos mixtos bayesianos con brms"),
          p(class = "small text-muted mb-3",
            "Los modelos mixtos (o jer\u00e1rquicos) incluyen tanto ",
            strong("efectos fijos"), " \u2014 que afectan a toda la poblaci\u00f3n \u2014 ",
            "como ", strong("efectos aleatorios"), " \u2014 que capturan la ",
            "variabilidad entre grupos (parcelas, individuos, sitios, etc.). ",
            "En brms, LMM y GLMM se ajustan con la misma funci\u00f3n ",
            tags$code("brm()"), ", simplemente cambiando el argumento ",
            tags$code("family"), "."
          ),

          layout_columns(col_widths = c(6, 6), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("arrow-left-right", class = "me-1"),
                          "lme4 vs. brms"),
              card_body(
                tags$table(
                  class = "table table-sm small mb-0",
                  tags$thead(
                    style = paste0("background:", colores$primario, "; color:#fff;"),
                    tags$tr(tags$th("Aspecto"), tags$th("lme4"), tags$th("brms"))
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$td(strong("Funci\u00f3n")),
                      tags$td(tagList(tags$code("lmer()"), " / ", tags$code("glmer()"))),
                      tags$td(tags$code("brm()"))
                    ),
                    tags$tr(style = paste0("background:", colores$fondo),
                      tags$td(strong("Interfaz")),
                      tags$td("Dos funciones distintas"),
                      tags$td("Una sola funci\u00f3n, cambia family")
                    ),
                    tags$tr(
                      tags$td(strong("Resultado")),
                      tags$td("Estimaci\u00f3n puntual + IC aprox."),
                      tags$td("Distribuci\u00f3n posterior completa")
                    ),
                    tags$tr(style = paste0("background:", colores$fondo),
                      tags$td(strong("Familias")),
                      tags$td("gaussian, Poisson, binomial"),
                      tags$td("Todas + Beta, ZIP, ZINB\u2026")
                    ),
                    tags$tr(
                      tags$td(strong("Priors")),
                      tags$td("No se especifican"),
                      tags$td("Se especifican expl\u00edcitamente")
                    ),
                    tags$tr(style = paste0("background:", colores$fondo),
                      tags$td(strong("Efectos aleatorios")),
                      tags$td("Valores puntuales (BLUPs)"),
                      tags$td("Distribuci\u00f3n posterior por grupo")
                    ),
                    tags$tr(
                      tags$td(strong("Velocidad")),
                      tags$td("Muy r\u00e1pido"),
                      tags$td("M\u00e1s lento (MCMC)")
                    )
                  )
                )
              )
            ),
            card(
              fill = FALSE,
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Estructura de la f\u00f3rmula"),
              card_body(
                p(class = "small text-muted mb-2",
                  "La f\u00f3rmula de brms para modelos mixtos es id\u00e9ntica ",
                  "a lme4:"),
                div(class = "codigo-bloque mb-3",
                  "# Intercepto aleatorio por grupo\n",
                  "y ~ x + (1 | grupo)\n\n",
                  "# Intercepto y pendiente aleatoria\n",
                  "y ~ x + (1 + x | grupo)\n\n",
                  "# Solo pendiente aleatoria\n",
                  "y ~ x + (0 + x | grupo)\n\n",
                  "# Dos niveles de agrupamiento\n",
                  "y ~ x + (1 | sitio/parcela)"
                ),
                div(class = "alert alert-info small py-2 px-3 mb-0",
                  bs_icon("lightbulb", class = "me-1"),
                  strong("Ventaja de brms:"), " los efectos aleatorios tienen ",
                  "distribuci\u00f3n posterior completa, no solo valores puntuales (BLUPs). ",
                  "Puedes visualizar la incertidumbre sobre cada grupo.")
              )
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfCu\u00e1ndo usar modelos mixtos bayesianos?"),
          layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
            div(class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("check-circle-fill", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Datos agrupados"), br(),
              "Individuos en sitios, plantas en parcelas, ",
              "medidas repetidas en sujetos."),
            div(class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("check-circle-fill", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Pocos grupos"), br(),
              "Con < 5 grupos, los efectos aleatorios bayesianos ",
              "son m\u00e1s estables que los frecuentistas."),
            div(class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("check-circle-fill", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Familias no gaussianas"), br(),
              "GLMM bayesiano sin los problemas de convergencia ",
              "frecuentes en glmer().")
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
             "Efectos fijos y efectos aleatorios"),
          p(class = "small text-muted mb-3",
            "La diferencia clave entre efectos fijos y aleatorios es ",
            "c\u00f3mo se modela la variabilidad entre grupos."),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("person-fill",
                        style = paste0("color:", colores$primario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Efectos fijos (\u03b2)")),
            layout_columns(col_widths = c(6, 6), fill = FALSE,
              p(class = "small text-muted mb-0",
                "Representan efectos ", strong("constantes para toda la poblaci\u00f3n"),
                ". El efecto del tratamiento, la temperatura o la edad son ",
                "efectos fijos si asumes que son los mismos en todos los grupos."),
              p(class = "small text-muted mb-0",
                strong("Ejemplo:"), " el efecto del \u00e1rea del fragmento sobre ",
                "la abundancia de aves es el mismo independientemente del sitio. ",
                "Se estima un solo coeficiente \u03b2.")
            )
          ),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("diagram-3",
                        style = paste0("color:", colores$acento,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento, "; font-weight:700;"),
                   "Efectos aleatorios (u)")),
            layout_columns(col_widths = c(6, 6), fill = FALSE,
              p(class = "small text-muted mb-0",
                "Representan la ", strong("variabilidad entre grupos"),
                ". Cada grupo tiene su propio intercepto o pendiente, ",
                "pero todos provienen de una distribuci\u00f3n com\u00fan ",
                tags$code("Normal(0, \u03c3_grupo)"), "."),
              p(class = "small text-muted mb-0",
                strong("Ejemplo:"), " cada sitio tiene su propia abundancia base, ",
                "pero todos los sitios pertenecen a la misma poblaci\u00f3n. ",
                "Se estima \u03c3_sitio, no un coeficiente por sitio.")
            )
          ),

          div(class = "card-muestreo mb-3",
            style = paste0("border-left:4px solid ", colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("arrows-collapse",
                        style = paste0("color:", colores$secundario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario, "; font-weight:700;"),
                   "Shrinkage (contracci\u00f3n bayesiana)")),
            layout_columns(col_widths = c(6, 6), fill = FALSE,
              p(class = "small text-muted mb-0",
                "Los efectos aleatorios bayesianos aplican ",
                strong("contracci\u00f3n"), " hacia la media poblacional. ",
                "Los grupos con pocos datos son \"jalados\" hacia la media, ",
                "lo que mejora las predicciones y evita el sobreajuste."),
              p(class = "small text-muted mb-0",
                strong("Ventaja sobre lme4:"), " en brms la contracci\u00f3n ",
                "emerge naturalmente del posterior. Con grupos peque\u00f1os, ",
                "brms produce estimaciones m\u00e1s estables que lme4.")
            )
          ),

          div(class = "card-muestreo mb-0",
            style = "border-left:4px solid #9F8B75;",
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("sliders", style = "color:#9F8B75; font-size:1.1rem"),
                h6(class = "mb-0", style = "color:#9F8B75; font-weight:700;",
                   "Priors en modelos mixtos")),
            layout_columns(col_widths = c(6, 6), fill = FALSE,
              p(class = "small text-muted mb-0",
                "Adem\u00e1s de los priors sobre \u03b2, hay que especificar priors ",
                "sobre las desviaciones est\u00e1ndar de los efectos aleatorios (\u03c3). ",
                "brms usa por defecto ", tags$code("student_t(3, 0, 2.5)"),
                " para \u03c3, lo que es razonablemente difuso."),
              p(class = "small text-muted mb-0",
                strong("Recomendaci\u00f3n:"), " usa ", tags$code("exponential(1)"),
                " o ", tags$code("half_normal(0, 1)"),
                " para \u03c3 si esperas variabilidad moderada entre grupos. ",
                "Evita priors completamente planos sobre \u03c3.")
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
            layout_columns(col_widths = c(4, 8), fill = FALSE,
              div(
                radioButtons(ns("fuente_datos_mxb"),
                  label = tagList(bs_icon("database", class = "me-1"),
                                  "Seleccionar dataset:"),
                  choices = c(
                    "Pl\u00e1ntulas en parcelas (plantulas_lmm) \u2014 LMM"   = "plantulas_lmm",
                    "Sue\u00f1o y privaci\u00f3n (sleepstudy_lmm) \u2014 LMM"   = "sleepstudy_lmm",
                    "Aves en sitios (aves_glmm) \u2014 GLMM"                  = "aves_glmm",
                    "Ranas en sitios (ranas_glmm) \u2014 GLMM"               = "ranas_glmm"
                  ),
                  selected = "plantulas_lmm"
                ),
                tags$hr(),
                uiOutput(ns("info_dataset_mxb"))
              ),
              card(
                fill = FALSE,
                card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                card_body(style = "overflow:auto;",
                  uiOutput(ns("cards_datos_mxb")), br(),
                  DTOutput(ns("tabla_preview_mxb"))
                )
              )
            )
          ),

          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("folder2-open", class = "me-1"),
                            "Mis datos"),
            br(),
            layout_columns(col_widths = c(4, 8), fill = FALSE,
              div(
                p(class = "small text-muted mb-3",
                  bs_icon("info-circle", class = "me-1"),
                  "Sube un archivo CSV o Excel con una columna de agrupamiento. ",
                  "La primera fila debe contener los nombres de las columnas."),
                fileInput(ns("archivo_mxb"),
                  label = "Seleccionar archivo:",
                  accept = c(".csv", ".xlsx", ".xls"),
                  buttonLabel = "Buscar\u2026",
                  placeholder = "CSV o Excel"),
                selectInput(ns("separador_mxb"), "Separador (CSV):",
                  choices = c("Coma (,)" = ",",
                              "Punto y coma (;)" = ";",
                              "Tabulador" = "\t")),
                tags$hr(),
                uiOutput(ns("resumen_datos_propio_mxb"))
              ),
              card(
                fill = FALSE,
                card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                card_body(style = "overflow:auto;",
                  uiOutput(ns("cards_datos_propio_mxb")), br(),
                  DTOutput(ns("tabla_preview_propio_mxb"))
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
              "Verifica que la variable de agrupamiento sea ",
              strong("Factor"), ". Las variables de respuesta y predictores ",
              "num\u00e9ricos deben ser ", strong("Num\u00e9rico"), "."),
            layout_columns(col_widths = c(10, 2), fill = FALSE,
              uiOutput(ns("tabla_tipos_mxb")),
              div(class = "pt-2",
                actionButton(ns("aplicar_tipos_mxb"), "Aplicar tipos",
                             class = "btn-primary w-100", icon = icon("check")),
                br(), br(),
                actionButton(ns("resetear_tipos_mxb"), "Restaurar",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon = icon("rotate-left"))
              )
            ),
            uiOutput(ns("tipos_aplicados_msg_mxb")),

            tags$hr(),
            layout_columns(
              col_widths = c(4, 8),
              fill = FALSE,
              radioButtons(
                ns("manejo_na_mxb"),
                label    = tagList(bs_icon("exclamation-diamond", class = "me-1"),
                                   "Valores perdidos (NA)"),
                choices  = c(
                  "Conservar"             = "conservar",
                  "Eliminar filas con NA" = "eliminar"
                ),
                selected = "conservar"
              ),
              uiOutput(ns("na_info_mxb"))
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
            "Visualiza la variabilidad entre grupos antes de ajustar. ",
            "Si las l\u00edneas por grupo son paralelas, un intercepto aleatorio ",
            "es suficiente. Si las pendientes var\u00edan, considera pendiente aleatoria."),
          layout_columns(col_widths = c(4, 8), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_x_mxb")),
                uiOutput(ns("sel_var_y_mxb")),
                uiOutput(ns("sel_grupo_mxb")),
                checkboxInput(ns("linea_por_grupo_mxb"),
                              "L\u00ednea por grupo", value = TRUE),
                checkboxInput(ns("linea_global_mxb"),
                              "L\u00ednea global", value = FALSE),
                tags$hr(),
                uiOutput(ns("n_grupos_mxb"))
              )
            ),
            plotOutput(ns("plot_scatter_mxb"), height = "380px")
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
            "En modelos mixtos hay priors sobre los ",
            strong("efectos fijos"), " (\u03b2) y sobre las ",
            strong("desviaciones est\u00e1ndar de los efectos aleatorios"),
            " (\u03c3_grupo). brms usa por defecto ",
            tags$code("student_t(3, 0, 2.5)"), " para ambos."),
          layout_columns(col_widths = c(4, 8), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("gear", class = "me-1"),
                          "Configuraci\u00f3n de priors"),
              card_body(
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Intercepto \u2014 \u03b2\u2080"),
                selectInput(ns("prior_intercept_dist_mxb"), "Distribuci\u00f3n:",
                            choices = c("Normal" = "normal",
                                        "Student-t" = "student_t",
                                        "Cauchy" = "cauchy"),
                            selected = "student_t"),
                fluidRow(
                  column(6, numericInput(ns("prior_intercept_mu_mxb"),
                                         "Media:", value = 0, step = 0.5)),
                  column(6, numericInput(ns("prior_intercept_sd_mxb"),
                                         "Escala:", value = 2.5,
                                         min = 0.1, step = 0.5))
                ),
                tags$hr(),
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "Efectos fijos \u2014 \u03b2"),
                selectInput(ns("prior_b_dist_mxb"), "Distribuci\u00f3n:",
                            choices = c("Normal" = "normal",
                                        "Student-t" = "student_t",
                                        "Cauchy" = "cauchy"),
                            selected = "normal"),
                fluidRow(
                  column(6, numericInput(ns("prior_b_mu_mxb"),
                                         "Media:", value = 0, step = 0.5)),
                  column(6, numericInput(ns("prior_b_sd_mxb"),
                                         "DE:", value = 1, min = 0.1, step = 0.5))
                ),
                tags$hr(),
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "DE efectos aleatorios \u2014 \u03c3_grupo"),
                selectInput(ns("prior_sd_dist_mxb"), "Distribuci\u00f3n:",
                            choices = c("Exponencial" = "exponential",
                                        "Student-t (semi)" = "student_t",
                                        "Cauchy (semi)" = "cauchy"),
                            selected = "exponential"),
                numericInput(ns("prior_sd_rate_mxb"), "Tasa (\u03bb):",
                             value = 1, min = 0.1, step = 0.1),
                tags$hr(),
                actionButton(ns("ver_ppc_mxb"), "Prior predictive check",
                             icon = icon("eye"),
                             class = "btn-outline-primary w-100 btn-sm")
              )
            ),
            div(
              card(class = "mb-3",
                card_header(bs_icon("code-slash", class = "me-1"),
                            "C\u00f3digo de priors"),
                card_body(verbatimTextOutput(ns("codigo_priors_mxb")))
              ),
              card(class = "mb-0",
                card_header(bs_icon("eye", class = "me-1"),
                            "Prior predictive check"),
                card_body(
                  plotOutput(ns("plot_ppc_prior_mxb"), height = "280px"),
                  uiOutput(ns("msg_ppc_prior_mxb"))
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
          layout_columns(col_widths = c(4, 8), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("toggles", class = "me-1"),
                          "Especificar el modelo"),
              card_body(
                p(class = "small text-muted",
                  "Selecciona la familia, la variable respuesta, los predictores ",
                  "y la estructura de efectos aleatorios."),
                selectInput(ns("familia_mxb"), "Familia de distribuci\u00f3n:",
                  choices = c(
                    "Gaussian (LMM)"              = "gaussian",
                    "Binomial (GLMM log\u00edstico)" = "binomial",
                    "Poisson (GLMM)"              = "poisson",
                    "Binomial negativa (GLMM)"    = "negbinomial",
                    "Beta (GLMM)"                 = "beta",
                    "Zero-inflated Poisson"       = "zero_inflated_poisson"
                  ), selected = "gaussian"),
                uiOutput(ns("info_familia_mxb")),
                tags$hr(),
                uiOutput(ns("sel_var_y_mod_mxb")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores num\u00e9ricos (efectos fijos)"),
                uiOutput(ns("checks_numericos_mxb")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores categ\u00f3ricos (efectos fijos)"),
                uiOutput(ns("checks_categoricos_mxb")),
                tags$hr(),
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   bs_icon("diagram-3", class = "me-1"),
                   "Estructura de efectos aleatorios"),
                uiOutput(ns("sel_grupo_mod_mxb")),
                radioButtons(ns("estructura_aleatoria_mxb"),
                  label = "Tipo de efecto aleatorio:",
                  choices = c(
                    "Solo intercepto: (1 | grupo)"           = "intercepto",
                    "Intercepto + pendiente: (1 + x | grupo)" = "pendiente",
                    "Solo pendiente: (0 + x | grupo)"         = "solo_pendiente"
                  ),
                  selected = "intercepto"
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("estructura_aleatoria_mxb"),
                                     "'] !== 'intercepto'"),
                  uiOutput(ns("sel_pendiente_mxb"))
                ),
                tags$hr(),
                h6(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   bs_icon("activity", class = "me-1"), "Opciones MCMC"),
                fluidRow(
                  column(6, numericInput(ns("mcmc_chains_mxb"), "Cadenas:",
                               value = 4, min = 1, max = 8)),
                  column(6, numericInput(ns("mcmc_iter_mxb"), "Iteraciones:",
                               value = 2000, min = 500, max = 10000, step = 500))
                ),
                actionButton(ns("ajustar_mxb"), "Ajustar modelo",
                             class = "btn-primary w-100", icon = icon("play")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  bs_icon("floppy", class = "me-1"), "Guardar para comparar"),
                textInput(ns("nombre_modelo_mxb"), label = NULL,
                          placeholder = "Ej: intercepto, pendiente\u2026"),
                actionButton(ns("guardar_modelo_mxb"), "Guardar modelo",
                             class = "btn-outline-primary w-100 btn-sm",
                             icon = icon("floppy-disk"))
              )
            ),
            div(
              uiOutput(ns("cards_metricas_mxb")), br(),
              layout_columns(col_widths = c(6, 6), fill = FALSE,
                card(
                  fill = FALSE,
                  card_header(bs_icon("bullseye", class = "me-1"),
                              "Predichos vs. observados"),
                  card_body(
                    plotOutput(ns("plot_predobs_mxb"), height = "240px")
                  )
                ),
                card(
                  fill = FALSE,
                  card_header(bs_icon("code-slash", class = "me-1"),
                              "F\u00f3rmula ajustada"),
                  card_body(verbatimTextOutput(ns("formula_ajustada_mxb")))
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
            "Los modelos mixtos generan m\u00e1s par\u00e1metros que los simples. ",
            "Verifica ", strong("R\u0302 < 1.01"), " y ", strong("ESS > 400"),
            " para todos los par\u00e1metros, incluyendo las varianzas aleatorias."),
          layout_columns(col_widths = c(4, 8), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("stopwatch", class = "me-1"),
                          "Diagn\u00f3stico de convergencia"),
              card_body(uiOutput(ns("semaforo_mcmc_mxb")))
            ),
            div(navset_pill(
              nav_panel(title = "Traceplots", fillable = FALSE, br(),
                selectInput(ns("param_trace_mxb"), "Par\u00e1metro:", choices = NULL),
                plotOutput(ns("plot_trace_mxb"), height = "280px")
              ),
              nav_panel(title = "Densidades", fillable = FALSE, br(),
                plotOutput(ns("plot_dens_mcmc_mxb"), height = "280px")
              ),
              nav_panel(title = "Posterior predictive check", fillable = FALSE, br(),
                plotOutput(ns("plot_ppc_post_mxb"), height = "280px")
              ),
              nav_panel(title = "R\u0302 y ESS", fillable = FALSE, br(),
                div(class = "alert alert-info small mb-3",
                  bs_icon("info-circle", class = "me-1"),
                  strong("Par\u00e1metros de efectos aleatorios:"),
                  " busca ", tags$code("sd_grupo__Intercept"),
                  " en la tabla \u2014 es la DE del efecto aleatorio. ",
                  "Si R\u0302 es alto para este par\u00e1metro, considera ",
                  "m\u00e1s iteraciones o un prior m\u00e1s informativo."),
                DTOutput(ns("tabla_rhat_mxb"))
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
            "M\u00e9tricas de rendimiento. El R\u00b2 bayesiano en modelos mixtos ",
            "puede descomponerse en R\u00b2 marginal (solo efectos fijos) y ",
            "R\u00b2 condicional (efectos fijos + aleatorios)."),
          layout_columns(col_widths = c(6, 6), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("speedometer2", class = "me-1"),
                          "M\u00e9tricas del modelo"),
              card_body(uiOutput(ns("tabla_performance_mxb")))
            ),
            div(
              card(class = "mb-3",
                card_header(bs_icon("bullseye", class = "me-1"),
                            "Predicho vs. observado"),
                card_body(plotOutput(ns("plot_predobs_perf_mxb"), height = "240px"))
              ),
              card(class = "mb-0",
                card_header(bs_icon("info-circle", class = "me-1"),
                            "Interpretaci\u00f3n"),
                card_body(tags$ul(class = "small text-muted mb-0",
                  tags$li(strong("R\u00b2 bayesiano:"),
                          " varianza total explicada."),
                  tags$li(strong("ELPD-LOO:"),
                          " capacidad predictiva fuera de muestra."),
                  tags$li(strong("mean_PPD:"),
                          " media del posterior predictivo."),
                  tags$li(strong("DE aleatoria (\u03c3_grupo):"),
                          " variabilidad entre grupos.")
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
            "Los ", strong("efectos fijos"), " (\u03b2) se interpretan igual que en LM/GLM. ",
            "Los ", strong("efectos aleatorios"), " muestran la variabilidad entre grupos ",
            "(par\u00e1metro ", tags$code("sd_grupo__Intercept"), "). ",
            "Un valor grande indica alta variabilidad entre grupos."),
          layout_columns(col_widths = c(6, 6), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("layout-text-sidebar", class = "me-1"),
                          "Tabla de coeficientes"),
              card_body(style = "overflow:visible; height:auto;",
                uiOutput(ns("tabla_params_ui_mxb")))
            ),
            card(
              fill = FALSE,
              card_header(bs_icon("bar-chart-fill", class = "me-1"),
                          "Forest plot \u2014 efectos fijos"),
              card_body(
                plotOutput(ns("plot_forest_mxb"), height = "300px")
              )
            )
          ),
          div(class = "mt-3",
            card(
              fill = FALSE,
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Efectos por grupo",
                          span(class = "text-muted small ms-2",
                               "\u2014 distribuci\u00f3n posterior")),
              card_body(
                p(class = "small text-muted mb-2",
                  "Interceptos (y/o pendientes) posteriores para cada grupo. ",
                  "A diferencia de lme4, cada grupo tiene una distribuci\u00f3n ",
                  "completa, no solo un valor puntual (BLUP)."),
                plotOutput(ns("plot_ranef_mxb"), height = "300px")
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
          nav_panel(title = "Efectos por grupo", fillable = FALSE, br(),
            p(class = "small text-muted mb-3",
              "Predicciones del modelo para cada grupo con su IC credible 95%. ",
              "Muestra c\u00f3mo var\u00edan los efectos entre grupos."),
            plotOutput(ns("plot_grupos_gamb"), height = "400px")
          ),
          nav_panel(title = "Distribuciones posteriores", fillable = FALSE, br(),
            plotOutput(ns("plot_areas_mxb"), height = "380px")
          ),
          nav_panel(title = "Predicho vs. observado", fillable = FALSE, br(),
            plotOutput(ns("plot_predobs_graf_mxb"), height = "380px")
          ),
          nav_panel(title = "Residuos", fillable = FALSE, br(),
            plotOutput(ns("plot_resid_mxb"), height = "380px")
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
            "Efecto marginal promediado sobre los grupos (efectos fijos). ",
            "La banda es el IC credible 95%."),
          layout_columns(col_widths = c(4, 8), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_pred_marginal_mxb")),
                tags$hr(),
                checkboxInput(ns("marginal_ci_mxb"),
                              "Mostrar IC credible 95%", value = TRUE),
                checkboxInput(ns("marginal_puntos_mxb"),
                              "Mostrar datos observados", value = TRUE)
              )
            ),
            div(
              card(
                fill = FALSE,
                card_header(bs_icon("graph-up-arrow", class = "me-1"),
                            "Efecto marginal posterior"),
                card_body(plotOutput(ns("plot_marginal_mxb"), height = "380px"))
              )
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
            "Compara modelos con diferente estructura de efectos aleatorios ",
            "o diferentes predictores usando ", strong("LOO"), " y ",
            strong("WAIC"), "."),
          div(class = "alert alert-info small mb-3",
            bs_icon("lightbulb", class = "me-1"),
            strong("Tip:"), " compara ", tags$code("y ~ x + (1|grupo)"),
            " vs. ", tags$code("y ~ x + (1 + x|grupo)"),
            " para evaluar si la pendiente aleatoria mejora el modelo."),
          layout_columns(col_widths = c(4, 8), fill = FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("list-check", class = "me-1"),
                          "Modelos guardados"),
              card_body(
                uiOutput(ns("lista_modelos_guardados_mxb")), tags$hr(),
                actionButton(ns("limpiar_modelos_mxb"), "Limpiar todos",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon = icon("trash"))
              )
            ),
            div(
              card(class = "mb-3",
                card_header(bs_icon("table", class = "me-1"),
                            "Tabla comparativa \u2014 LOO y WAIC"),
                card_body(uiOutput(ns("tabla_comparacion_mxb")))
              ),
              card(class = "mb-0",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Gr\u00e1fico comparativo LOO"),
                card_body(
                  plotOutput(ns("plot_comparacion_mxb"), height = "300px")
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
            "Script reproducible con ", strong("brms"), "."),
          card(
            fill = FALSE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " Script reproducible"),
              downloadButton(ns("descargar_script_mxb"),
                             label = "Descargar .R",
                             icon = bs_icon("download"),
                             class = "btn-sm btn-outline-primary")
            ),
            verbatimTextOutput(ns("codigo_r_mxb"))
          )
        )
      )

    ) # fin navset_card_tab
  )
}

# ── SERVER ────────────────────────────────────────────────
mod_mixed_bayes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos de ejemplo ──────────────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_mxb
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

    output$info_dataset_mxb <- renderUI({
      fuente <- input$fuente_datos_mxb
      if (is.null(fuente)) return(NULL)
      textos <- list(
        plantulas_lmm = tagList(
          strong("Pl\u00e1ntulas BTS \u2014 LMM (densidad continua)."),
          " 60 parcelas en 10 fragmentos de bosque tropical seco, Costa Rica. ",
          "Y: ", strong("densidad_plantulas"), " (ind/m\u00b2) \u2014 continua gaussiana. ",
          "Predictores: cobertura_dosel (%), pendiente (\u00b0), dist_agua (m).",
          tags$br(), tags$br(),
          bs_icon("diagram-3", class = "me-1",
                  style = paste0("color:", colores$primario)),
          strong("Parametrizaci\u00f3n:"), " parcelas anidadas en fragmentos:", tags$br(),
          tags$code("brm(densidad_plantulas ~ cobertura_dosel + pendiente +"),
          tags$br(),
          tags$code("      dist_agua + (1 | fragmento/parcela),"),
          tags$br(),
          tags$code("    family = gaussian())"), tags$br(),
          "Estima variabilidad entre fragmentos y entre parcelas dentro de cada fragmento. ",
          "Si el efecto del dosel var\u00eda entre fragmentos: ",
          tags$code("(1 + cobertura_dosel | fragmento)"), "."
        ),
        sleepstudy_lmm = tagList(
          strong("Privaci\u00f3n de sue\u00f1o \u2014 LMM (Belenky et al., 2003)."),
          " 180 observaciones: 18 sujetos \u00d7 10 d\u00edas. ",
          "Y: ", strong("Reaction"), " (ms) \u2014 continua. ",
          "Predictor: Days. Agrupamiento: Subject.",
          tags$br(), tags$br(),
          bs_icon("diagram-3", class = "me-1",
                  style = paste0("color:", colores$primario)),
          strong("Parametrizaci\u00f3n:"), " ejemplo cl\u00e1sico de pendiente aleatoria:", tags$br(),
          tags$code("brm(Reaction ~ Days + (1 + Days | Subject),"),
          tags$br(),
          tags$code("    family = gaussian())"), tags$br(),
          "Cada sujeto tiene diferente tiempo base ", em("y"),
          " diferente tasa de deterioro con la privaci\u00f3n de sue\u00f1o."
        ),
        aves_glmm = tagList(
          strong("Aves en fragmentos \u2014 GLMM Poisson/BN (simulado)."),
          " 72 puntos de conteo (6 por fragmento) en 12 fragmentos. ",
          "Y: ", strong("n_aves"), " (conteos). Offset: log(area_ha). ",
          "Agrupamiento: fragmento.",
          tags$br(), tags$br(),
          bs_icon("diagram-3", class = "me-1",
                  style = paste0("color:", colores$primario)),
          strong("Parametrizaci\u00f3n:"), " puntos anidados en fragmentos:", tags$br(),
          tags$code("brm(n_aves ~ cobertura_dosel + dist_borde + NDVI +"),
          tags$br(),
          tags$code("      offset(log(area_ha)) + (1 | fragmento),"),
          tags$br(),
          tags$code("    family = negbinomial())"), tags$br(),
          "En brms se usa binomial negativa directamente \u2014 no existe quasipoisson ",
          "en el mundo bayesiano."
        ),
        ranas_glmm = tagList(
          strong("Ranas en charcas \u2014 GLMM binomial (simulado)."),
          " 120 visitas (8 por charca) a 15 charcas temporales. ",
          "Y: ", strong("presencia"), " (0/1). Agrupamiento: charca.",
          tags$br(), tags$br(),
          bs_icon("diagram-3", class = "me-1",
                  style = paste0("color:", colores$primario)),
          strong("Parametrizaci\u00f3n:"), " visitas repetidas a las mismas charcas:", tags$br(),
          tags$code("brm(presencia ~ hidroperiodo + cobertura + dist_bosque + pH +"),
          tags$br(),
          tags$code("      (1 | charca), family = binomial())"), tags$br(),
          "Si el efecto del pH var\u00eda entre charcas: ",
          tags$code("(1 + pH | charca)"), ". ",
          "Con solo 15 charcas, los priors bayesianos estabilizan mejor las estimaciones."
        )
      )
      info <- textos[[fuente]]
      if (is.null(info)) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mb-0",
          bs_icon("info-circle-fill", class = "me-1"), info)
    })

    output$cards_datos_mxb <- renderUI({
      req(datos())
      d <- datos()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
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

    output$tabla_preview_mxb <- renderDT({
      req(datos())
      datatable(datos(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Datos propios ─────────────────────────────────
    datos_propio_mxb <- reactive({
      req(input$archivo_mxb)
      ext <- tools::file_ext(input$archivo_mxb$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo_mxb$datapath)
        else
          readr::read_delim(input$archivo_mxb$datapath,
                            delim = input$separador_mxb %||% ",",
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error"); NULL
      })
    })

    observeEvent(datos_propio_mxb(), { datos_mod(datos_propio_mxb()) })

    output$resumen_datos_propio_mxb <- renderUI({
      req(datos_propio_mxb())
      d <- datos_propio_mxb()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_mxb <- renderUI({
      req(datos_propio_mxb())
      d <- datos_propio_mxb()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
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

    output$tabla_preview_propio_mxb <- renderDT({
      req(datos_propio_mxb())
      datatable(datos_propio_mxb(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Tipos de variables ────────────────────────────
    tipos_usuario_mxb <- reactiveVal(NULL)

    output$tabla_tipos_mxb <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_mxb()
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

    observeEvent(input$aplicar_tipos_mxb, {
      req(datos_mod())
      d  <- datos_mod()
      tu <- setNames(lapply(names(d), function(nm) input[[paste0("tipo_", nm)]]),
                     names(d))
      tipos_usuario_mxb(tu)
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

    output$tipos_aplicados_msg_mxb <- renderUI({
      tu <- tipos_usuario_mxb(); if (is.null(tu)) return(NULL)
      n_excl <- sum(sapply(tu, function(t) !is.null(t) && t == "excluir"))
      if (n_excl == 0) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          paste0(n_excl, " variable(s) excluida(s)."))
    })

    observeEvent(input$resetear_tipos_mxb, {
      tipos_usuario_mxb(NULL); datos_mod(datos())
    })

    # ── Manejo de NAs ────────────────────────────────────────────────────────
    datos_finales_mxb <- reactive({
      df <- datos_mod()
      req(df)
      if (isTRUE(input$manejo_na_mxb == "eliminar")) {
        df <- tidyr::drop_na(df)
      }
      df
    })

    output$na_info_mxb <- renderUI({
      df_orig  <- datos_mod()
      df_final <- datos_finales_mxb()
      req(df_orig)
      n_na <- sum(!stats::complete.cases(df_orig))
      if (n_na == 0) return(
        div(class = "alert alert-success small py-2 px-3 mb-0",
            bs_icon("check-circle", class = "me-1"), "Sin valores perdidos.")
      )
      n_elim <- nrow(df_orig) - nrow(df_final)
      if (input$manejo_na_mxb == "eliminar")
        div(class = "alert alert-warning small py-2 px-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            paste0(n_elim, " fila(s) eliminadas. Quedan ", nrow(df_final), " filas."))
      else
        div(class = "alert alert-info small py-2 px-3 mb-0",
            bs_icon("info-circle", class = "me-1"),
            paste0(n_na, " fila(s) con NA. El modelo puede fallar o excluirlas ",
                   "autom\u00e1ticamente \u2014 pod\u00e9s eliminarlas a la izquierda para mayor control."))
    })

    # ── Variables reactivas ───────────────────────────
    vars_num <- reactive({
      req(datos_finales_mxb())
      names(which(sapply(datos_finales_mxb(), is.numeric)))
    })
    vars_cat <- reactive({
      req(datos_finales_mxb())
      names(which(sapply(datos_finales_mxb(),
                         function(x) is.factor(x) || is.character(x))))
    })

    # ── Exploración ───────────────────────────────────
    output$sel_var_x_mxb <- renderUI({
      selectInput(ns("var_x_mxb"), "Variable X (predictor):", choices = vars_num())
    })
    output$sel_var_y_mxb <- renderUI({
      selectInput(ns("var_y_exp_mxb"), "Variable Y (respuesta):",
                  choices = c(vars_num(), vars_cat()))
    })
    output$sel_grupo_mxb <- renderUI({
      selectInput(ns("var_grupo_exp_mxb"), "Variable de agrupamiento:",
                  choices = vars_cat())
    })

    output$n_grupos_mxb <- renderUI({
      req(datos_finales_mxb(), input$var_grupo_exp_mxb)
      d <- datos_finales_mxb(); g <- input$var_grupo_exp_mxb
      if (!g %in% names(d)) return(NULL)
      n_grupos <- length(unique(d[[g]]))
      clase <- if (n_grupos < 5) "sem-warn"
               else if (n_grupos < 20) "sem-ok" else "sem-ok"
      div(class = paste("small p-2 rounded", clase),
          bs_icon("diagram-3", class = "me-1"),
          strong(n_grupos), " grupos en ", strong(g), br(),
          span(class = "text-muted small",
               if (n_grupos < 5)
                 "Pocos grupos \u2014 los efectos aleatorios bayesianos son especialmente \u00fatiles."
               else "N\u00famero adecuado de grupos para efectos aleatorios."))
    })

    output$plot_scatter_mxb <- renderPlot({
      req(datos_finales_mxb(), input$var_x_mxb, input$var_y_exp_mxb)
      d <- datos_finales_mxb()
      x <- input$var_x_mxb; y <- input$var_y_exp_mxb
      g <- input$var_grupo_exp_mxb
      req(x %in% names(d), y %in% names(d))
      p <- ggplot(d, aes(.data[[x]], as.numeric(.data[[y]]))) +
        geom_point(aes(color = if (!is.null(g) && g %in% names(d))
                                 .data[[g]] else NULL),
                   alpha = 0.6, size = 2) +
        scale_color_tableau_cb() +
        labs(x = x, y = y, color = g) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              plot.background = element_rect(fill = colores$fondo, color = NA))
      if (isTRUE(input$linea_por_grupo_mxb) && !is.null(g) && g %in% names(d))
        p <- p + geom_smooth(aes(group = .data[[g]], color = .data[[g]]),
                             method = "lm", se = FALSE, linewidth = 0.7)
      if (isTRUE(input$linea_global_mxb))
        p <- p + geom_smooth(method = "lm", se = TRUE,
                             color = "black", linewidth = 1,
                             inherit.aes = FALSE,
                             aes(x = .data[[x]], y = as.numeric(.data[[y]])))
      p
    })

    # ── Priors ────────────────────────────────────────
    output$codigo_priors_mxb <- renderText({
      dist_int <- switch(input$prior_intercept_dist_mxb,
        normal    = paste0("normal(", input$prior_intercept_mu_mxb, ", ",
                           input$prior_intercept_sd_mxb, ")"),
        student_t = paste0("student_t(3, ", input$prior_intercept_mu_mxb, ", ",
                           input$prior_intercept_sd_mxb, ")"),
        cauchy    = paste0("cauchy(", input$prior_intercept_mu_mxb, ", ",
                           input$prior_intercept_sd_mxb, ")")
      )
      dist_b <- switch(input$prior_b_dist_mxb,
        normal    = paste0("normal(", input$prior_b_mu_mxb, ", ",
                           input$prior_b_sd_mxb, ")"),
        student_t = paste0("student_t(3, ", input$prior_b_mu_mxb, ", ",
                           input$prior_b_sd_mxb, ")"),
        cauchy    = paste0("cauchy(", input$prior_b_mu_mxb, ", ",
                           input$prior_b_sd_mxb, ")")
      )
      dist_sd <- switch(input$prior_sd_dist_mxb,
        exponential = paste0("exponential(", input$prior_sd_rate_mxb, ")"),
        student_t   = paste0("student_t(3, 0, ", input$prior_sd_rate_mxb, ")"),
        cauchy      = paste0("cauchy(0, ", input$prior_sd_rate_mxb, ")")
      )
      paste0("c(\n",
             "  prior(", dist_int, ", class = Intercept),\n",
             "  prior(", dist_b, ", class = b),\n",
             "  prior(", dist_sd, ", class = sd)\n",
             ")")
    })

    observeEvent(input$ver_ppc_mxb, {
      req(datos_finales_mxb())
      n_grupos <- 10; n_obs <- 20; n_sim <- 100
      mat <- replicate(n_sim, {
        b0   <- rnorm(1, input$prior_intercept_mu_mxb,
                      input$prior_intercept_sd_mxb)
        sd_g <- rexp(1, input$prior_sd_rate_mxb)
        u    <- rnorm(n_grupos, 0, sd_g)
        x    <- rnorm(n_grupos * n_obs)
        b1   <- rnorm(1, input$prior_b_mu_mxb, input$prior_b_sd_mxb)
        g    <- rep(1:n_grupos, each = n_obs)
        rnorm(n_grupos * n_obs, b0 + u[g] + b1 * x, 1)
      })
      output$plot_ppc_prior_mxb <- renderPlot({
        df <- data.frame(y = as.vector(mat),
                         sim = rep(seq_len(ncol(mat)), each = nrow(mat)))
        ggplot(df, aes(x = y, group = sim)) +
          geom_density(color = colores$primario, alpha = 0.05, linewidth = 0.3) +
          labs(x = "Valores simulados de Y", y = "Densidad de probabilidad",
               subtitle = paste0(n_sim, " simulaciones desde el prior (", n_grupos,
                                  " grupos, ", n_obs, " obs por grupo)")) +
          theme_minimal(base_size = 13) +
          theme(panel.grid.minor = element_blank(),
                plot.background = element_rect(fill = colores$fondo, color = NA))
      })
      rango <- range(mat, na.rm = TRUE)
      output$msg_ppc_prior_mxb <- renderUI({
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

    # ── Ajuste ────────────────────────────────────────
    modelo_actual_mxb     <- reactiveVal(NULL)
    modelos_guardados_mxb <- reactiveVal(list())

    output$sel_var_y_mod_mxb <- renderUI({
      selectInput(ns("var_y_mxb"), "Variable respuesta (Y):",
                  choices = c(vars_num(), vars_cat()))
    })

    output$checks_numericos_mxb <- renderUI({
      ys  <- input$var_y_mxb %||% ""
      ops <- setdiff(vars_num(), ys)
      if (length(ops) == 0)
        return(p(class = "small text-muted", "No hay variables num\u00e9ricas."))
      checkboxGroupInput(ns("preds_num_mxb"), label = NULL, choices = ops)
    })

    output$checks_categoricos_mxb <- renderUI({
      ys  <- input$var_y_mxb %||% ""
      g   <- input$var_grupo_mod_mxb %||% ""
      ops <- setdiff(vars_cat(), c(ys, g))
      if (length(ops) == 0)
        return(p(class = "small text-muted", "No hay variables categ\u00f3ricas."))
      checkboxGroupInput(ns("preds_cat_mxb"), label = NULL, choices = ops)
    })

    output$sel_grupo_mod_mxb <- renderUI({
      selectInput(ns("var_grupo_mod_mxb"),
                  "Variable de agrupamiento (grupo):",
                  choices = vars_cat())
    })

    output$sel_pendiente_mxb <- renderUI({
      ops <- setdiff(vars_num(), input$var_y_mxb %||% "")
      selectInput(ns("var_pendiente_mxb"),
                  "Variable con pendiente aleatoria:",
                  choices = ops)
    })

    output$info_familia_mxb <- renderUI({
      req(input$familia_mxb)
      switch(input$familia_mxb,
        gaussian    = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "LMM: Y continua con efectos aleatorios."),
        binomial    = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "GLMM log\u00edstico: Y binaria (0/1) con efectos aleatorios."),
        poisson     = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "GLMM Poisson: conteos con efectos aleatorios."),
        negbinomial = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "GLMM BN: conteos con sobredispersi\u00f3n y efectos aleatorios."),
        beta        = div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "GLMM Beta: proporciones (0,1) con efectos aleatorios."),
        zero_inflated_poisson = div(
                          class = "alert alert-info small py-2 px-3 mt-2 mb-0",
                          bs_icon("info-circle", class = "me-1"),
                          "GLMM ZIP: conteos con exceso de ceros y efectos aleatorios.")
      )
    })

    formula_mxb <- reactive({
      req(input$var_y_mxb, input$var_grupo_mod_mxb)
      preds <- c(input$preds_num_mxb, input$preds_cat_mxb)
      if (length(preds) == 0) preds <- "1"
      grupo <- input$var_grupo_mod_mxb
      ef_aleatorio <- switch(input$estructura_aleatoria_mxb,
        intercepto    = paste0("(1 | ", grupo, ")"),
        pendiente     = paste0("(1 + ", input$var_pendiente_mxb %||% preds[1],
                               " | ", grupo, ")"),
        solo_pendiente = paste0("(0 + ", input$var_pendiente_mxb %||% preds[1],
                                " | ", grupo, ")")
      )
      as.formula(paste(input$var_y_mxb, "~",
                       paste(preds, collapse = " + "),
                       "+", ef_aleatorio))
    })

    familia_brms_mxb <- reactive({
      switch(input$familia_mxb,
        gaussian              = gaussian(),
        binomial              = binomial(),
        poisson               = poisson(),
        negbinomial           = brms::negbinomial(),
        beta                  = brms::Beta(),
        zero_inflated_poisson = brms::zero_inflated_poisson()
      )
    })

    priors_mxb <- reactive({
      dist_int <- switch(input$prior_intercept_dist_mxb,
        normal    = brms::prior(normal(0, 2.5), class = Intercept),
        student_t = brms::prior(student_t(3, 0, 2.5), class = Intercept),
        cauchy    = brms::prior(cauchy(0, 2.5), class = Intercept)
      )
      preds <- c(input$preds_num_mxb, input$preds_cat_mxb)
      dist_sd <- switch(input$prior_sd_dist_mxb,
        exponential = brms::prior(exponential(1), class = sd),
        student_t   = brms::prior(student_t(3, 0, 2.5), class = sd),
        cauchy      = brms::prior(cauchy(0, 1), class = sd)
      )
      if (length(preds) > 0) {
        dist_b <- switch(input$prior_b_dist_mxb,
          normal    = brms::prior(normal(0, 1), class = b),
          student_t = brms::prior(student_t(3, 0, 1), class = b),
          cauchy    = brms::prior(cauchy(0, 1), class = b)
        )
        c(dist_int, dist_b, dist_sd)
      } else {
        c(dist_int, dist_sd)
      }
    })

    observeEvent(input$ajustar_mxb, {
      req(datos_finales_mxb(), input$var_y_mxb, input$var_grupo_mod_mxb)
      frm <- tryCatch(formula_mxb(), error = function(e) NULL)
      req(frm)
      d <- datos_finales_mxb()
      withProgress(message = "Ajustando modelo mixto bayesiano (MCMC)\u2026",
                   detail = "Puede tardar varios minutos.", value = 0.1, {
        tryCatch({
          fit <- brms::brm(
            formula = frm, data = d,
            family  = familia_brms_mxb(),
            prior   = priors_mxb(),
            chains  = input$mcmc_chains_mxb,
            iter    = input$mcmc_iter_mxb,
            cores   = parallel::detectCores(),
            refresh = 0, silent = 2
          )
          modelo_actual_mxb(fit); setProgress(1)
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 10)
        })
      })
    })

    output$formula_ajustada_mxb <- renderText({
      req(modelo_actual_mxb())
      deparse(formula_mxb())
    })

    # ── Métricas ──────────────────────────────────────
    output$cards_metricas_mxb <- renderUI({
      req(modelo_actual_mxb())
      fit <- modelo_actual_mxb()
      r2 <- tryCatch(round(brms::bayes_R2(fit)[1, "Estimate"], 3),
                     error = function(e) "\u2014")
      loo_val <- tryCatch({
        l <- loo::loo(fit)
        round(l$estimates["elpd_loo", "Estimate"], 1)
      }, error = function(e) "\u2014")
      rmse <- tryCatch({
        pe <- brms::predictive_error(fit)
        round(sqrt(mean(pe^2)), 3)
      }, error = function(e) "\u2014")
      layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
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
            h4(style = paste0("color:", colores$acento, "; font-weight:700;"), rmse),
            p(class = "small text-muted mb-0", "RMSE posterior")))
      )
    })

    output$plot_predobs_mxb <- renderPlot({
      req(modelo_actual_mxb())
      fit   <- modelo_actual_mxb()
      preds <- fitted(fit)[, "Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      ggplot(data.frame(obs = obs, pred = preds), aes(obs, pred)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                    color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.5, size = 1.8) +
        labs(x = "Observado", y = "Predicho") +
        theme_minimal(base_size = 12) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    # ── Diagnóstico MCMC ──────────────────────────────
    output$semaforo_mcmc_mxb <- renderUI({
      req(modelo_actual_mxb())
      fit   <- modelo_actual_mxb()
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
      req(modelo_actual_mxb())
      pars <- brms::variables(modelo_actual_mxb())
      pars <- pars[grep("^b_|^sd_|^sigma", pars)]
      updateSelectInput(session, "param_trace_mxb", choices = pars)
    })

    output$plot_trace_mxb <- renderPlot({
      req(modelo_actual_mxb(), input$param_trace_mxb)
      bayesplot::mcmc_trace(modelo_actual_mxb(),
                            pars = input$param_trace_mxb,
                            facet_args = list(ncol = 1)) +
        theme_minimal(base_size = 12)
    })

    output$plot_dens_mcmc_mxb <- renderPlot({
      req(modelo_actual_mxb())
      pars <- brms::variables(modelo_actual_mxb())
      pars <- pars[grep("^b_|^sd_", pars)]
      if (length(pars) == 0) return(NULL)
      bayesplot::mcmc_dens_overlay(modelo_actual_mxb(), pars = pars) +
        theme_minimal(base_size = 12)
    })

    output$plot_ppc_post_mxb <- renderPlot({
      req(modelo_actual_mxb())
      bayesplot::pp_check(modelo_actual_mxb(), ndraws = 100) +
        theme_minimal(base_size = 12)
    })

    output$tabla_rhat_mxb <- renderDT({
      req(modelo_actual_mxb())
      fit   <- modelo_actual_mxb()
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
      df <- df[grep("^b_|^sd_|^sigma|^cor_", df$Parametro), ]
      names(df) <- c("Par\u00e1metro", "R\u0302", "ESS bulk", "ESS tail")
      datatable(df, options = list(pageLength = 10), rownames = FALSE) |>
        DT::formatStyle("R\u0302",
          backgroundColor = DT::styleInterval(c(1.01, 1.05),
            c("#f0f9f5", "#fffbf0", "#fff0f2")))
    })

    # ── Performance ───────────────────────────────────
    output$tabla_performance_mxb <- renderUI({
      req(modelo_actual_mxb())
      fit <- modelo_actual_mxb()
      r2 <- tryCatch({
        r <- brms::bayes_R2(fit)
        paste0(round(r[1, "Estimate"], 3), " [",
               round(r[1, "Q2.5"], 3), ", ", round(r[1, "Q97.5"], 3), "]")
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
      # DE efectos aleatorios
      sd_ranef <- tryCatch({
        pars <- posterior::summarise_draws(fit, mean)
        sd_row <- pars[grep("^sd_", pars$variable), ]
        if (nrow(sd_row) > 0)
          paste(round(sd_row$mean, 3), collapse = " / ")
        else "\u2014"
      }, error = function(e) "\u2014")
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
                  tags$td(strong("mean_PPD")), tags$td(mean_ppd)),
          tags$tr(tags$td(strong("DE efectos aleatorios (\u03c3)")),
                  tags$td(sd_ranef))
        )
      )
    })

    output$plot_predobs_perf_mxb <- renderPlot({
      req(modelo_actual_mxb())
      fit   <- modelo_actual_mxb()
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
    output$tabla_params_ui_mxb <- renderUI({
      req(modelo_actual_mxb())
      DTOutput(ns("tabla_params_mxb"))
    })

    output$tabla_params_mxb <- renderDT({
      req(modelo_actual_mxb())
      pars <- parameters::model_parameters(modelo_actual_mxb(), ci = 0.95)
      df   <- as.data.frame(pars)
      df   <- df[, intersect(c("Parameter", "Median", "Mean", "SD",
                                "CI_low", "CI_high", "pd", "Rhat"),
                              names(df))]
      df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], round, 3)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
    })

    output$plot_forest_mxb <- renderPlot({
      req(modelo_actual_mxb())
      pars <- posterior::summarise_draws(modelo_actual_mxb(),
                mean, ~quantile(.x, c(0.025, 0.975)))
      pars <- pars[grep("^b_", pars$variable), ]
      if (nrow(pars) == 0) return(NULL)
      pars$variable <- gsub("^b_", "", pars$variable)
      names(pars)[3:4] <- c("lo", "hi")
      ggplot(pars, aes(x = mean, y = reorder(variable, mean),
                       xmin = lo, xmax = hi,
                       color = (lo > 0 | hi < 0))) +
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

    output$plot_ranef_mxb <- renderPlot({
      req(modelo_actual_mxb())
      tryCatch({
        re   <- brms::ranef(modelo_actual_mxb())
        grp  <- names(re)[1]
        df   <- as.data.frame(re[[grp]][, , "Intercept"])
        df$grupo <- rownames(df)
        names(df)[1:4] <- c("est", "se", "lo", "hi")
        ggplot(df, aes(x = est, y = reorder(grupo, est),
                       xmin = lo, xmax = hi,
                       color = (lo > 0 | hi < 0))) +
          geom_vline(xintercept = 0, linetype = "dashed",
                     color = colores$texto) +
          geom_errorbarh(height = 0.3, linewidth = 0.5) +
          geom_point(size = 2.5) +
          scale_color_manual(values = c("FALSE" = colores$texto,
                                        "TRUE"  = colores$primario),
                             guide = "none") +
          labs(x = paste("Efecto aleatorio \u2014", grp),
               y = NULL,
               caption = "IC credible 95% del intercepto aleatorio por grupo") +
          theme_minimal(base_size = 13) +
          theme(plot.background = element_rect(fill = colores$fondo, color = NA))
      }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = paste("Error:", e$message)) + theme_void()
      })
    })

    # ── Gráficos ──────────────────────────────────────
    output$plot_grupos_gamb <- renderPlot({
      req(modelo_actual_mxb())
      tryCatch({
        ef <- brms::conditional_effects(modelo_actual_mxb())
        plot(ef, plot = FALSE)[[1]] +
          theme_minimal(base_size = 13) +
          theme(plot.background = element_rect(fill = colores$fondo, color = NA))
      }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = paste("Error:", e$message)) + theme_void()
      })
    })

    output$plot_areas_mxb <- renderPlot({
      req(modelo_actual_mxb())
      pars <- brms::variables(modelo_actual_mxb())
      pars <- pars[grep("^b_", pars)]
      if (length(pars) == 0) return(NULL)
      bayesplot::mcmc_areas(modelo_actual_mxb(), pars = pars, prob = 0.95) +
        theme_minimal(base_size = 13)
    })

    output$plot_predobs_graf_mxb <- renderPlot({
      req(modelo_actual_mxb())
      fit   <- modelo_actual_mxb()
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

    output$plot_resid_mxb <- renderPlot({
      req(modelo_actual_mxb())
      fit   <- modelo_actual_mxb()
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
    output$sel_pred_marginal_mxb <- renderUI({
      req(modelo_actual_mxb())
      preds <- c(input$preds_num_mxb, input$preds_cat_mxb)
      if (length(preds) == 0) preds <- "1"
      selectInput(ns("pred_marginal_mxb"), "Predictor focal:", choices = preds)
    })

    output$plot_marginal_mxb <- renderPlot({
      req(modelo_actual_mxb(), input$pred_marginal_mxb)
      tryCatch({
        ef <- brms::conditional_effects(modelo_actual_mxb(),
                                         effects = input$pred_marginal_mxb)
        plot(ef, plot = FALSE)[[1]] +
          theme_minimal(base_size = 13) +
          theme(plot.background = element_rect(fill = colores$fondo, color = NA))
      }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = paste("Error:", e$message)) + theme_void()
      })
    })

    # ── Comparar modelos ──────────────────────────────
    observeEvent(input$guardar_modelo_mxb, {
      req(modelo_actual_mxb(), input$nombre_modelo_mxb != "")
      nm    <- input$nombre_modelo_mxb
      lista <- modelos_guardados_mxb()
      lista[[nm]] <- modelo_actual_mxb()
      modelos_guardados_mxb(lista)
      showNotification(paste("Modelo", nm, "guardado."), type = "message")
    })

    output$lista_modelos_guardados_mxb <- renderUI({
      lista <- modelos_guardados_mxb()
      if (length(lista) == 0)
        return(p(class = "small text-muted", "A\u00fan no hay modelos guardados."))
      tags$ul(class = "small",
              lapply(names(lista), function(nm)
                tags$li(bs_icon("check2", class = "me-1"), nm)))
    })

    observeEvent(input$limpiar_modelos_mxb, { modelos_guardados_mxb(list()) })

    output$tabla_comparacion_mxb <- renderUI({
      lista <- modelos_guardados_mxb()
      if (length(lista) < 2)
        return(div(class = "alert alert-info small",
                   "Guarda al menos 2 modelos para compararlos."))
      DTOutput(ns("dt_comparacion_mxb"))
    })

    output$dt_comparacion_mxb <- renderDT({
      lista <- modelos_guardados_mxb()
      req(length(lista) >= 2)
      tryCatch({
        loos <- lapply(lista, loo::loo)
        comp <- loo::loo_compare(loos)
        datatable(as.data.frame(round(comp, 2)),
                  options = list(pageLength = 10, scrollX = TRUE))
      }, error = function(e) data.frame(Error = e$message))
    })

    output$plot_comparacion_mxb <- renderPlot({
      lista <- modelos_guardados_mxb()
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
    output$codigo_r_mxb <- renderText({
      req(input$var_y_mxb, input$var_grupo_mod_mxb)
      frm_str <- tryCatch(deparse(formula_mxb()),
                          error = function(e) paste(input$var_y_mxb,
                                                    "~ x + (1 | grupo)"))
      nm_datos <- input$fuente_datos_mxb %||% "mis_datos"
      familia_str <- switch(input$familia_mxb,
        gaussian              = "gaussian()",
        binomial              = "binomial()",
        poisson               = "poisson()",
        negbinomial           = "negbinomial()",
        beta                  = "Beta()",
        zero_inflated_poisson = "zero_inflated_poisson()"
      )
      dist_sd <- switch(input$prior_sd_dist_mxb,
        exponential = paste0("exponential(", input$prior_sd_rate_mxb, ")"),
        student_t   = paste0("student_t(3, 0, ", input$prior_sd_rate_mxb, ")"),
        cauchy      = paste0("cauchy(0, ", input$prior_sd_rate_mxb, ")")
      )
      paste0(
        "# ── Modelo mixto bayesiano con brms ─────────────────\n",
        "library(brms)\nlibrary(bayesplot)\nlibrary(posterior)\nlibrary(loo)\n\n",
        "# Datos\ndata('", nm_datos, "', package = 'StatBayes')\n\n",
        "# Priors\nmis_priors <- c(\n",
        "  prior(student_t(3, 0, 2.5), class = Intercept),\n",
        "  prior(normal(0, 1), class = b),\n",
        "  prior(", dist_sd, ", class = sd)\n)\n\n",
        "# Ajustar modelo\nfit <- brm(\n",
        "  formula = ", frm_str, ",\n",
        "  data    = ", nm_datos, ",\n",
        "  family  = ", familia_str, ",\n",
        "  prior   = mis_priors,\n",
        "  chains  = ", input$mcmc_chains_mxb, ",\n",
        "  iter    = ", input$mcmc_iter_mxb, ",\n",
        "  cores   = parallel::detectCores()\n)\n\n",
        "# Resumen\nsummary(fit)\n\n",
        "# Efectos aleatorios\nranef(fit)\n\n",
        "# Diagnóstico MCMC\nmcmc_trace(fit)\npp_check(fit, ndraws = 100)\n\n",
        "# Efectos marginales\nconditional_effects(fit)\n\n",
        "# Performance\nbayes_R2(fit)\nloo(fit)\n"
      )
    })

    output$descargar_script_mxb <- downloadHandler(
      filename = function() paste0("StatBayes_mixed_bayes_", Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_mxb(), file)
    )

  })
}
