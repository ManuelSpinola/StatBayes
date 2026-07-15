# ============================================================
# mod_glm_bayes.R — GLM bayesiano (PARTE 1: UI)
# StatBayes · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_glm_bayes_ui <- function(id) {
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
            h4(bs_icon("toggles", class = "me-2"), "GLM bayesiano",
               style = paste0("color:", colores$primario, "; font-weight:700;")),
            p(class = "text-muted mb-0",
              "Versi\u00f3n bayesiana del modelo lineal generalizado. ",
              "Cubre binomial, Poisson, binomial negativa, Beta y modelos zero-inflated.")
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Del GLM frecuentista al GLM bayesiano"),
          p(class = "small text-muted mb-3",
            "El GLM bayesiano usa las mismas familias y funciones de enlace ",
            "que el GLM frecuentista, pero incorpora distribuciones a priori ",
            "sobre los par\u00e1metros. El resultado es una distribuci\u00f3n posterior ",
            "completa para cada coeficiente. Adem\u00e1s, brms permite familias ",
            "que el GLM frecuentista no cubre: ", strong("Beta"),
            " y modelos ", strong("zero-inflated"), "."
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Familias disponibles"),

          div(style = "overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-4",
              style = "background: #ffffff;",
              tags$thead(
                style = paste0("background:", colores$primario, "; color:#fff;"),
                tags$tr(
                  tags$th("Familia"), tags$th("Variable Y"),
                  tags$th("Enlace"), tags$th("Par\u00e1metro"),
                  tags$th("Exclusivo brms"), tags$th("Ejemplo")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$span(class="badge",
                    style=paste0("background:",colores$primario),
                    "Binomial (log\u00edstica)")),
                  tags$td("Binaria (0/1)"), tags$td(tags$code("logit")),
                  tags$td("Odds ratio (OR)"), tags$td("\u2014"),
                  tags$td("\u00bfPresencia de especie? \u00bfTiene diabetes?")
                ),
                tags$tr(style=paste0("background:",colores$fondo),
                  tags$td(tags$span(class="badge",
                    style=paste0("background:",colores$acento), "Poisson")),
                  tags$td("Conteos (0,1,2\u2026)"), tags$td(tags$code("log")),
                  tags$td("Tasa (IRR)"), tags$td("\u2014"),
                  tags$td("\u00bfCu\u00e1ntas especies? \u00bfCu\u00e1ntos casos?")
                ),
                tags$tr(
                  tags$td(tags$span(class="badge",
                    style=paste0("background:",colores$peligro),
                    "Binomial negativa")),
                  tags$td("Conteos con sobredispersi\u00f3n"),
                  tags$td(tags$code("log")), tags$td("Tasa (IRR)"),
                  tags$td("\u2014"), tags$td("Conteos con alta variabilidad")
                ),
                tags$tr(style=paste0("background:",colores$fondo),
                  tags$td(tags$span(class="badge",
                    style=paste0("background:",colores$secundario), "Beta")),
                  tags$td("Proporciones (0,1)"), tags$td(tags$code("logit")),
                  tags$td("Efecto en logit"),
                  tags$td(tags$span(class="badge bg-success", "\u2713 brms")),
                  tags$td("Cobertura vegetal, proporciones")
                ),
                tags$tr(
                  tags$td(tags$span(class="badge", style="background:#9F8B75",
                    "Zero-inflated Poisson")),
                  tags$td("Conteos con exceso de ceros"),
                  tags$td(tags$code("log")), tags$td("Tasa (IRR)"),
                  tags$td(tags$span(class="badge bg-success", "\u2713 brms")),
                  tags$td("Abundancia de especies raras")
                ),
                tags$tr(style=paste0("background:",colores$fondo),
                  tags$td(tags$span(class="badge", style="background:#B85A0D",
                    "Zero-inflated BN")),
                  tags$td("Conteos: ceros + sobredispersi\u00f3n"),
                  tags$td(tags$code("log")), tags$td("Tasa (IRR)"),
                  tags$td(tags$span(class="badge bg-success", "\u2713 brms")),
                  tags$td("Conteos raros con alta variabilidad")
                )
              )
            )
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "La funci\u00f3n de enlace"),
          layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
            div(class="card-muestreo",
              style=paste0("border-left:4px solid ",colores$primario,";"),
              p(class="small mb-1", strong("Enlace logit \u2014 Binomial / Beta")),
              p(class="small text-muted mb-0",
                "exp(\u03b2) = odds ratio (OR). OR>1 aumenta la probabilidad, ",
                "OR<1 la disminuye, OR=1 sin efecto.")
            ),
            div(class="card-muestreo",
              style=paste0("border-left:4px solid ",colores$acento,";"),
              p(class="small mb-1", strong("Enlace log \u2014 Poisson / BN / ZIP / ZINB")),
              p(class="small text-muted mb-0",
                "exp(\u03b2) = raz\u00f3n de tasas (IRR). IRR>1 aumenta el conteo, ",
                "IRR<1 lo disminuye, IRR=1 sin efecto.")
            ),
            div(class="card-muestreo",
              style=paste0("border-left:4px solid ",colores$secundario,";"),
              p(class="small mb-1", strong("Ventajas del enfoque bayesiano")),
              p(class="small text-muted mb-0",
                "Intervalos credibles con interpretaci\u00f3n directa. ",
                "Estabilidad con muestras peque\u00f1as. ",
                "Familias extendidas con la misma sintaxis.")
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfCu\u00e1ndo ir m\u00e1s all\u00e1 del GLM bayesiano?"),
          layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
            div(class="alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class="me-2",
                style=paste0("color:",colores$peligro)),
              strong("Datos agrupados o repetidos"), br(),
              "Usa ", strong("modelos mixtos bayesianos"), "."),
            div(class="alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class="me-2",
                style=paste0("color:",colores$peligro)),
              strong("Relaci\u00f3n no lineal"), br(),
              "Usa ", strong("GAM bayesiano"), "."),
            div(class="alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class="me-2",
                style=paste0("color:",colores$peligro)),
              strong("Y ordinal o multinomial"), br(),
              "brms soporta familias ", strong("cumulative"),
              " y ", strong("categorical"), ".")
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("journal-bookmark", class="me-1"), "Fundamentos"),
        card_body(
          h5(style=paste0("color:",colores$primario,"; font-weight:700;"),
             "Supuestos del GLM bayesiano"),
          p(class="small text-muted mb-3",
            "Los supuestos sobre los datos son los mismos que en el GLM frecuentista, ",
            "pero se agrega la especificaci\u00f3n de priors. El ",
            strong("posterior predictive check"), " en Diagn\u00f3stico MCMC verifica ",
            "si el modelo reproduce bien la distribuci\u00f3n de Y."),

          div(class="card-muestreo mb-3",
            style=paste0("border-left:4px solid ",colores$primario,";"),
            div(class="d-flex align-items-center gap-2 mb-2",
              bs_icon("toggles", style=paste0("color:",colores$primario,"; font-size:1.1rem")),
              h6(class="mb-0", style=paste0("color:",colores$primario,"; font-weight:700;"),
                 "1. Familia correcta")),
            layout_columns(col_widths=c(6,6),
              p(class="small text-muted mb-0",
                "La familia elegida debe corresponder al tipo de Y. ",
                "Binomial para 0/1, Poisson o BN para conteos, Beta para proporciones."),
              p(class="small text-muted mb-0",
                strong("\u00bfC\u00f3mo verificarlo?"), " Posterior predictive check: ",
                "las distribuciones simuladas deben parecerse a la observada.")
            )
          ),

          div(class="card-muestreo mb-3",
            style=paste0("border-left:4px solid ",colores$secundario,";"),
            div(class="d-flex align-items-center gap-2 mb-2",
              bs_icon("graph-up", style=paste0("color:",colores$secundario,"; font-size:1.1rem")),
              h6(class="mb-0", style=paste0("color:",colores$secundario,"; font-weight:700;"),
                 "2. Linealidad en la escala del enlace")),
            layout_columns(col_widths=c(6,6),
              p(class="small text-muted mb-0",
                "La relaci\u00f3n entre los predictores y g(Y) debe ser lineal. ",
                "En log\u00edstica: relaci\u00f3n lineal entre X y el log-odds."),
              p(class="small text-muted mb-0",
                strong("Si falla:"), " transforma el predictor o usa GAM bayesiano.")
            )
          ),

          div(class="card-muestreo mb-3",
            style=paste0("border-left:4px solid ",colores$peligro,";"),
            div(class="d-flex align-items-center gap-2 mb-2",
              bs_icon("link-45deg", style=paste0("color:",colores$peligro,"; font-size:1.1rem")),
              h6(class="mb-0", style=paste0("color:",colores$peligro,"; font-weight:700;"),
                 "3. Independencia de las observaciones")),
            layout_columns(col_widths=c(6,6),
              p(class="small text-muted mb-0",
                "Las observaciones no deben estar correlacionadas. ",
                "Medidas repetidas o datos jer\u00e1rquicos violan este supuesto."),
              p(class="small text-muted mb-0",
                strong("Si falla:"), " usa modelos mixtos bayesianos.")
            )
          ),

          div(class="card-muestreo mb-3",
            style=paste0("border-left:4px solid ",colores$acento,";"),
            div(class="d-flex align-items-center gap-2 mb-2",
              bs_icon("arrows-expand", style=paste0("color:",colores$acento,"; font-size:1.1rem")),
              h6(class="mb-0", style=paste0("color:",colores$acento,"; font-weight:700;"),
                 "4. Sobredispersi\u00f3n (solo Poisson)")),
            layout_columns(col_widths=c(6,6),
              p(class="small text-muted mb-0",
                "En Poisson varianza = media. Si varianza > media, cambiar a BN."),
              p(class="small text-muted mb-0",
                strong("Posterior predictive check:"), " si Poisson subestima ",
                "la varianza observada, prueba binomial negativa.")
            )
          ),

          div(class="card-muestreo mb-3",
            style=paste0("border-left:4px solid ",colores$texto,";"),
            div(class="d-flex align-items-center gap-2 mb-2",
              bs_icon("0-circle", style=paste0("color:",colores$texto,"; font-size:1.1rem")),
              h6(class="mb-0", style=paste0("color:",colores$texto,"; font-weight:700;"),
                 "5. Inflaci\u00f3n de ceros (conteos)")),
            layout_columns(col_widths=c(6,6),
              p(class="small text-muted mb-0",
                "Si hay muchos m\u00e1s ceros de los esperados, el modelo los subestima. ",
                "Com\u00fan en datos de presencia de especies raras."),
              p(class="small text-muted mb-0",
                strong("Ventaja de brms:"), " modelos ZIP y ZINB disponibles ",
                "con la misma sintaxis que Poisson y BN.")
            )
          ),

          div(class="card-muestreo mb-0", style="border-left:4px solid #9F8B75;",
            div(class="d-flex align-items-center gap-2 mb-2",
              bs_icon("sliders", style="color:#9F8B75; font-size:1.1rem"),
              h6(class="mb-0", style="color:#9F8B75; font-weight:700;",
                 "6. Especificaci\u00f3n del prior (exclusivo Bayes)")),
            layout_columns(col_widths=c(6,6),
              p(class="small text-muted mb-0",
                "Los priors deben ser coherentes con la escala del enlace. ",
                "Para coeficientes en escala logit o log, ",
                tags$code("Normal(0, 2.5)"), " o ",
                tags$code("Student-t(3, 0, 2.5)"), " son razonables."),
              p(class="small text-muted mb-0",
                strong("\u00bfC\u00f3mo verificarlo?"), " Prior predictive check en ",
                "la pesta\u00f1a Priors.")
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("table", class="me-1"), "Los datos"),
        card_body(navset_pill(

          # ── Sub 1: Datos de ejemplo ─────────────────
          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("collection", class="me-1"),
                            "Datos de ejemplo"),
            br(),
            layout_columns(col_widths=c(4, 8),
              div(
                radioButtons(ns("fuente_datos_glmb"),
                  label = tagList(bs_icon("database", class="me-1"),
                                  "Seleccionar dataset:"),
                  choices = c(
                    "Presencia de \u00e1caros NPRA \u2014 binomial"       = "mite_logistic",
                    "Diabetes mujeres Pima \u2014 binomial"             = "pima_glm",
                    "Voluntariado Cowles \u2014 binomial"                = "cowles_glm",
                    "Abundancia de \u00e1caros Brachy \u2014 Poisson"   = "mite_counts",
                    "Riqueza de hormigas \u2014 Poisson / BN"           = "ants_glm",
                    "Reclamaciones de seguro \u2014 Poisson"            = "insurance_glm",
                    "C\u00e1ncer de pulm\u00f3n Dinamarca \u2014 Poisson" = "danish_glm",
                    "Cangrejos herradura \u2014 Poisson / BN"           = "hcrabs_glm"
                  ),
                  selected = "mite_logistic"
                ),
                tags$hr(),
                uiOutput(ns("info_dataset_glmb")),
                uiOutput(ns("resumen_datos_glmb"))
              ),
              card(
                fill = FALSE,
                card_header(bs_icon("eye", class="me-1"), "Vista previa"),
                card_body(style="overflow:auto;",
                  uiOutput(ns("cards_datos_glmb")), br(),
                  DTOutput(ns("tabla_preview_glmb"))
                )
              )
            )
          ),

          # ── Sub 2: Mis datos ─────────────────────────
          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("folder2-open", class="me-1"),
                            "Mis datos"),
            br(),
            layout_columns(col_widths=c(4, 8),
              div(
                p(class="small text-muted mb-3",
                  bs_icon("info-circle", class="me-1"),
                  "Sube un archivo CSV o Excel. ",
                  "La primera fila debe contener los nombres de las columnas."),
                fileInput(ns("archivo_glmb"),
                  label       = "Seleccionar archivo:",
                  accept      = c(".csv", ".xlsx", ".xls"),
                  buttonLabel = "Buscar\u2026",
                  placeholder = "CSV o Excel"
                ),
                selectInput(ns("separador_glmb"),
                  label   = "Separador (CSV):",
                  choices = c("Coma (,)"=",",
                              "Punto y coma (;)"=";",
                              "Tabulador"="\t")
                ),
                tags$hr(),
                uiOutput(ns("resumen_datos_propio_glmb"))
              ),
              card(
                fill = FALSE,
                card_header(bs_icon("eye", class="me-1"), "Vista previa"),
                card_body(style="overflow:auto;",
                  uiOutput(ns("cards_datos_propio_glmb")), br(),
                  DTOutput(ns("tabla_preview_propio_glmb"))
                )
              )
            )
          ),

          # ── Sub 3: Tipos de variables ────────────────
          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("sliders2", class="me-1"),
                            "Tipos de variables"),
            br(),
            p(class="small text-muted mb-3",
              "Verifica que cada variable tenga el tipo correcto. ",
              "Las variables ", strong("categ\u00f3ricas"),
              " deben ser ", strong("Factor"), "."),
            layout_columns(col_widths=c(10, 2),
              uiOutput(ns("tabla_tipos_glmb")),
              div(class="pt-2",
                actionButton(ns("aplicar_tipos_glmb"), "Aplicar tipos",
                             class="btn-primary w-100", icon=icon("check")),
                br(), br(),
                actionButton(ns("resetear_tipos_glmb"), "Restaurar",
                             class="btn-outline-secondary w-100 btn-sm",
                             icon=icon("rotate-left"))
              )
            ),
            uiOutput(ns("tipos_aplicados_msg_glmb")),

            tags$hr(),
            layout_columns(
              col_widths = c(4, 8),
              fill = FALSE,
              radioButtons(
                ns("manejo_na_glmb"),
                label    = tagList(bs_icon("exclamation-diamond", class = "me-1"),
                                   "Valores perdidos (NA)"),
                choices  = c(
                  "Conservar"             = "conservar",
                  "Eliminar filas con NA" = "eliminar"
                ),
                selected = "conservar"
              ),
              uiOutput(ns("na_info_glmb"))
            )
          )

        ))
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Explorar
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("zoom-in", class="me-1"), "Explorar"),
        card_body(
          p(class="small text-muted mb-3",
            "Visualiza las relaciones entre variables antes de ajustar. ",
            "Ayuda a identificar predictores relevantes y elegir la familia."),
          layout_columns(col_widths=c(4,8), fill=FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class="me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_x_glmb")),
                uiOutput(ns("sel_color_glmb")),
                checkboxInput(ns("mostrar_linea_glmb"),
                              "Mostrar l\u00ednea de tendencia", value=TRUE),
                tags$hr(),
                uiOutput(ns("info_y_glmb"))
              )
            ),
            plotOutput(ns("plot_scatter_glmb"), height="380px")
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Priors
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("sliders", class="me-1"), "Priors"),
        card_body(
          p(class="small text-muted mb-3",
            "Los priors est\u00e1n en la ", strong("escala del enlace"),
            " (logit para binomial/Beta, log para Poisson/BN). ",
            "Los defaults d\u00e9bilmente informativos funcionan bien en la mayor\u00eda de los casos."),
          layout_columns(col_widths=c(4,8),
            card(
              fill = FALSE,
              card_header(bs_icon("gear", class="me-1"), "Configuraci\u00f3n de priors"),
              card_body(
                h6(style=paste0("color:",colores$primario,"; font-weight:700;"),
                   "Intercepto \u2014 \u03b2\u2080"),
                selectInput(ns("prior_intercept_dist_glmb"), "Distribuci\u00f3n:",
                            choices=c("Normal"="normal","Student-t"="student_t",
                                      "Cauchy"="cauchy"), selected="student_t"),
                fluidRow(
                  column(6, numericInput(ns("prior_intercept_mu_glmb"),
                                         "Media:", value=0, step=0.5)),
                  column(6, numericInput(ns("prior_intercept_sd_glmb"),
                                         "Escala:", value=2.5, min=0.1, step=0.5))
                ),
                tags$hr(),
                h6(style=paste0("color:",colores$primario,"; font-weight:700;"),
                   "Coeficientes \u2014 \u03b2"),
                selectInput(ns("prior_b_dist_glmb"), "Distribuci\u00f3n:",
                            choices=c("Normal"="normal","Student-t"="student_t",
                                      "Cauchy"="cauchy"), selected="normal"),
                fluidRow(
                  column(6, numericInput(ns("prior_b_mu_glmb"),
                                         "Media:", value=0, step=0.5)),
                  column(6, numericInput(ns("prior_b_sd_glmb"),
                                         "DE:", value=1, min=0.1, step=0.5))
                ),
                tags$hr(),
                actionButton(ns("ver_ppc_glmb"), "Prior predictive check",
                             icon=icon("eye"), class="btn-outline-primary w-100 btn-sm")
              )
            ),
            div(
              card(class="mb-3",
                card_header(bs_icon("code-slash", class="me-1"), "C\u00f3digo de priors"),
                card_body(verbatimTextOutput(ns("codigo_priors_glmb")))
              ),
              card(class="mb-0",
                card_header(bs_icon("eye", class="me-1"), "Prior predictive check",
                            span(class="text-muted small ms-2",
                                 "\u2014 datos simulados desde el prior")),
                card_body(
                  plotOutput(ns("plot_ppc_prior_glmb"), height="280px"),
                  uiOutput(ns("msg_ppc_prior_glmb"))
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
        title = tagList(bs_icon("gear", class="me-1"), "Ajustar modelo"),
        card_body(
          layout_columns(col_widths=c(4,8),
            card(
              fill = FALSE,
              card_header(bs_icon("toggles", class="me-1"), "Especificar el modelo"),
              card_body(
                p(class="small text-muted",
                  "Selecciona la familia, variable respuesta y predictores."),
                selectInput(ns("familia_glmb"), "Familia de distribuci\u00f3n:",
                  choices=c(
                    "Binomial (log\u00edstica)"          = "binomial",
                    "Poisson"                           = "poisson",
                    "Binomial negativa"                 = "negbinomial",
                    "Beta (proporciones)"               = "beta",
                    "Zero-inflated Poisson"             = "zero_inflated_poisson",
                    "Zero-inflated binomial negativa"   = "zero_inflated_negbinomial"
                  ), selected="binomial"),
                uiOutput(ns("info_familia_glmb")),
                tags$hr(),
                uiOutput(ns("sel_var_y_glmb")),
                tags$hr(),
                p(class="small fw-bold text-muted mb-1", "Predictores num\u00e9ricos"),
                uiOutput(ns("checks_numericos_glmb")),
                tags$hr(),
                p(class="small fw-bold text-muted mb-1", "Predictores categ\u00f3ricos"),
                uiOutput(ns("checks_categoricos_glmb")),
                tags$hr(),
                conditionalPanel(
                  condition=paste0("(input['",ns("preds_num_glmb"),"'] !== null && ",
                    "input['",ns("preds_num_glmb"),"'].length + ",
                    "(input['",ns("preds_cat_glmb"),"'] !== null ? ",
                    "input['",ns("preds_cat_glmb"),"'].length : 0)) >= 2"),
                  div(p(class="small fw-bold text-muted mb-1",
                        bs_icon("diagram-2", class="me-1"), "Interacciones (opcional)"),
                      uiOutput(ns("checks_interacciones_glmb")), tags$hr())
                ),
                checkboxInput(ns("estandarizar_glmb"),
                  label=tagList("Estandarizar predictores num\u00e9ricos",
                    tags$small(class="text-muted d-block mt-1",
                      "Permite comparar el peso relativo de \u03b2 en unidades de DE.")),
                  value=FALSE),
                tags$hr(),
                h6(style=paste0("color:",colores$primario,"; font-weight:700;"),
                   bs_icon("activity", class="me-1"), "Opciones MCMC"),
                fluidRow(
                  column(6, numericInput(ns("mcmc_chains_glmb"), "Cadenas:",
                                         value=4, min=1, max=8)),
                  column(6, numericInput(ns("mcmc_iter_glmb"), "Iteraciones:",
                                         value=2000, min=500, max=10000, step=500))
                ),
                actionButton(ns("ajustar_glmb"), "Ajustar modelo",
                             class="btn-primary w-100", icon=icon("play")),
                tags$hr(),
                p(class="small fw-bold text-muted mb-1",
                  bs_icon("floppy", class="me-1"), "Guardar para comparar"),
                p(class="small text-muted mb-2",
                  "Guarda con un nombre descriptivo para comparar modelos."),
                textInput(ns("nombre_modelo_glmb"), label=NULL,
                          placeholder="Ej: logistica_full, poisson_age\u2026"),
                actionButton(ns("guardar_modelo_glmb"), "Guardar modelo",
                             class="btn-outline-primary w-100 btn-sm",
                             icon=icon("floppy-disk"))
              )
            ),
            div(
              uiOutput(ns("cards_metricas_glmb")), br(),
              layout_columns(col_widths=c(6,6),
                card(
                  fill = FALSE,
                  card_header(bs_icon("bullseye", class="me-1"),
                              "Predichos vs. observados"),
                  card_body(
                    p(class="small text-muted",
                      "Puntos cerca de la diagonal = buenas predicciones."),
                    plotOutput(ns("plot_predobs_glmb"), height="240px")
                  )
                ),
                card(
                  fill = FALSE,
                  card_header(bs_icon("lightbulb", class="me-1"), "Interpretaci\u00f3n"),
                  card_body(uiOutput(ns("texto_modelo_glmb")))
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
        title = tagList(bs_icon("activity", class="me-1"), "Diagn\u00f3stico MCMC"),
        card_body(
          p(class="small text-muted mb-3",
            "Verifica convergencia: ", strong("R\u0302 < 1.01"), " y ",
            strong("ESS > 400"), ". El PPC compara simulaciones con datos observados."),
          layout_columns(col_widths=c(4,8),
            card(
              fill = FALSE,
              card_header(bs_icon("stopwatch", class="me-1"),
                          "Diagn\u00f3stico de convergencia"),
              card_body(uiOutput(ns("semaforo_mcmc_glmb")))
            ),
            div(navset_pill(
              nav_panel(title="Traceplots", fillable = FALSE, br(),
                p(class="small text-muted mb-2",
                  "Las cadenas deben mezclarse como ",
                  strong("orugas peludas"), " superpuestas."),
                selectInput(ns("param_trace_glmb"), "Par\u00e1metro:", choices=NULL),
                plotOutput(ns("plot_trace_glmb"), height="280px")
              ),
              nav_panel(title="Densidades", fillable = FALSE, br(),
                p(class="small text-muted mb-2",
                  "Las densidades de las cadenas deben superponerse."),
                plotOutput(ns("plot_dens_mcmc_glmb"), height="280px")
              ),
              nav_panel(title="Posterior predictive check", fillable = FALSE, br(),
                p(class="small text-muted mb-2",
                  "Datos observados (l\u00ednea oscura) vs. r\u00e9plicas del posterior."),
                plotOutput(ns("plot_ppc_post_glmb"), height="280px")
              ),
              nav_panel(title="R\u0302 y ESS", fillable = FALSE, br(),
                div(class="alert alert-info small mb-3",
                  bs_icon("info-circle", class="me-1"),
                  strong("\u00bfESS puede superar las muestras totales?"), " S\u00ed. ",
                  "Con 2000 iter. y 4 cadenas hay 4000 muestras. ",
                  "ESS > 4000 indica anticorrelaci\u00f3n entre muestras — es positivo. ",
                  "ESS < 400 indica problemas."),
                DTOutput(ns("tabla_rhat_glmb"))
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
        title = tagList(bs_icon("speedometer2", class="me-1"), "Performance"),
        card_body(
          p(class="small text-muted mb-3",
            "M\u00e9tricas de rendimiento. LOO y WAIC para capacidad predictiva. ",
            "mean_PPD compara la media predicha con la observada."),
          layout_columns(col_widths=c(6,6),
            card(
              fill = FALSE,
              card_header(bs_icon("speedometer2", class="me-1"),
                          "M\u00e9tricas del modelo",
                          span(class="text-muted small ms-2", "\u2014 brms \u00b7 loo")),
              card_body(uiOutput(ns("tabla_performance_glmb")))
            ),
            div(
              card(class="mb-3",
                card_header(bs_icon("bullseye", class="me-1"),
                            "Predicho vs. observado",
                            span(class="text-muted small ms-2", "\u2014 media posterior")),
                card_body(plotOutput(ns("plot_predobs_perf_glmb"), height="240px"))
              ),
              card(class="mb-0",
                card_header(bs_icon("info-circle", class="me-1"),
                            "Interpretaci\u00f3n de m\u00e9tricas"),
                card_body(tags$ul(class="small text-muted mb-0",
                  tags$li(strong("ELPD-LOO:"), " m\u00e1s alto = mejor predicci\u00f3n."),
                  tags$li(strong("ELPD-WAIC:"), " alternativa al LOO."),
                  tags$li(strong("mean_PPD:"), " debe estar cerca de la media de Y."),
                  tags$li(strong("p_waic:"), " n\u00famero efectivo de par\u00e1metros.")
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
        title = tagList(bs_icon("table", class="me-1"), "Par\u00e1metros"),
        div(class="p-3",
          p(class="small text-muted mb-3",
            "Coeficientes en la ", strong("escala del enlace"), ". ",
            "exp(\u03b2) = OR (binomial) o IRR (Poisson/BN). ",
            "IC credible 95%: hay 95% de probabilidad de que \u03b2 est\u00e9 en ese rango."),
          layout_columns(col_widths=c(6,6), fill=FALSE,
            card(
              fill = FALSE,
              card_header(bs_icon("layout-text-sidebar", class="me-1"),
                          "Tabla de coeficientes",
                          span(class="text-muted small ms-2",
                               "\u2014 escala del enlace + OR/IRR")),
              card_body(style="overflow:visible; height:auto;",
                uiOutput(ns("tabla_params_ui_glmb")))
            ),
            card(
              fill = FALSE,
              card_header(bs_icon("bar-chart-fill", class="me-1"),
                          "Forest plot",
                          span(class="text-muted small ms-2",
                               "\u2014 media posterior \u00b1 IC 95%")),
              card_body(
                p(class="small text-muted",
                  "Escala del enlace. IC que no cruza cero = efecto relevante."),
                plotOutput(ns("plot_forest_glmb"), height="300px")
              )
            )
          ),
          div(class="mt-3",
            card(
              fill = FALSE,
              card_header(bs_icon("bar-chart-steps", class="me-1"),
                          "Importancia de variables",
                          span(class="text-muted small ms-2",
                               "\u2014 probabilidad de direcci\u00f3n (pd)")),
              card_body(
                p(class="small text-muted mb-2",
                  "La ", strong("pd"), " indica qu\u00e9 tan consistente es la ",
                  "direcci\u00f3n del efecto. pd > 95% \u2248 p < 0.05."),
                plotOutput(ns("plot_pd_glmb"), height="220px")
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
        title = tagList(bs_icon("graph-up-arrow", class="me-1"), "Gr\u00e1ficos"),
        card_body(navset_pill(
          nav_panel(title="Distribuciones posteriores", fillable = FALSE, br(),
            p(class="small text-muted mb-3",
              "Distribuci\u00f3n posterior de cada coeficiente (escala del enlace). ",
              "\u00c1rea sombreada = IC 95%."),
            plotOutput(ns("plot_areas_glmb"), height="380px")
          ),
          nav_panel(title="Predicho vs. observado", fillable = FALSE, br(),
            p(class="small text-muted mb-3",
              "Comparaci\u00f3n entre valores observados y predichos."),
            plotOutput(ns("plot_predobs_graf_glmb"), height="380px")
          ),
          nav_panel(title="Residuos", fillable = FALSE, br(),
            p(class="small text-muted mb-3",
              "Residuos del modelo. Deben distribuirse aleatoriamente."),
            plotOutput(ns("plot_resid_glmb"), height="380px")
          )
        ))
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 11: Efectos marginales
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("arrows-angle-expand", class="me-1"),
                        "Efectos marginales"),
        card_body(
          p(class="small text-muted mb-3",
            "Efecto de cada predictor en la ", strong("escala original"),
            " (probabilidad para binomial, conteo esperado para Poisson/BN). ",
            "Banda = IC credible 95%."),
          layout_columns(col_widths=c(4,8),
            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class="me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_pred_marginal_glmb")),
                tags$hr(),
                checkboxInput(ns("marginal_ci_glmb"),
                              "Mostrar IC credible 95%", value=TRUE),
                checkboxInput(ns("marginal_puntos_glmb"),
                              "Mostrar datos observados", value=TRUE)
              )
            ),
            div(
              card(
                fill = FALSE,
                card_header(bs_icon("graph-up-arrow", class="me-1"),
                            "Efecto marginal",
                            span(class="text-muted small ms-2",
                                 "\u2014 escala original")),
                card_body(plotOutput(ns("plot_marginal_glmb"), height="380px"))
              ),
              br(),
              uiOutput(ns("marginal_interpretacion_glmb"))
            )
          ),
          tags$hr(),
          h5(style=paste0("color:",colores$primario,"; font-weight:700;"),
             "Predicci\u00f3n puntual"),
          p(class="small text-muted mb-3",
            "Ingresa valores espec\u00edficos y obtiene la predicci\u00f3n bayesiana ",
            "con su IC 95% en escala original."),
          layout_columns(col_widths=c(4,8),
            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class="me-1"),
                          "Valores de los predictores"),
              card_body(
                uiOutput(ns("inputs_prediccion_glmb")), br(),
                actionButton(ns("calcular_prediccion_glmb"),
                             "Calcular predicci\u00f3n",
                             class="btn-primary w-100", icon=icon("calculator"))
              )
            ),
            card(
              fill = FALSE,
              card_header(bs_icon("bullseye", class="me-1"), "Resultado"),
              card_body(uiOutput(ns("resultado_prediccion_glmb")))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 12: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("arrow-left-right", class="me-1"),
                        "Comparar modelos"),
        card_body(
          p(class="small text-muted mb-3",
            "Ajusta distintos modelos (diferentes familias o predictores), ",
            "gu\u00e1rdalos y comp\u00e1ralos por LOO y WAIC."),
          div(class="alert alert-info small mb-3",
            bs_icon("lightbulb", class="me-1"),
            strong("Tip:"), " puedes comparar modelos con diferente familia ",
            "(ej. Poisson vs. binomial negativa)."),
          layout_columns(col_widths=c(4,8),
            card(
              fill = FALSE,
              card_header(bs_icon("list-check", class="me-1"), "Modelos guardados"),
              card_body(
                uiOutput(ns("lista_modelos_guardados_glmb")), tags$hr(),
                actionButton(ns("limpiar_modelos_glmb"), "Limpiar todos",
                             class="btn-outline-secondary w-100 btn-sm",
                             icon=icon("trash"))
              )
            ),
            div(
              card(class="mb-3",
                card_header(bs_icon("table", class="me-1"), "Tabla comparativa",
                            span(class="text-muted small ms-2", "\u2014 LOO y WAIC")),
                card_body(uiOutput(ns("tabla_comparacion_glmb")))
              ),
              card(class="mb-0",
                card_header(bs_icon("bar-chart-fill", class="me-1"),
                            "Gr\u00e1fico comparativo LOO"),
                card_body(
                  p(class="small text-muted mb-2",
                    "Mayor ELPD = mejor predicci\u00f3n. M\u00ednimo 2 modelos."),
                  plotOutput(ns("plot_comparacion_glmb"), height="300px")
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
        title = tagList(bs_icon("code-slash", class="me-1"), "C\u00f3digo R"),
        card_body(
          p(class="text-muted small mb-3",
            "Script reproducible con ", strong("brms"),
            ". Se actualiza seg\u00fan las selecciones activas."),
          card(
            fill = FALSE,
            card_header(
              class="d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " Script reproducible"),
              downloadButton(ns("descargar_script_glmb"), label="Descargar .R",
                             icon=bs_icon("download"),
                             class="btn-sm btn-outline-primary")
            ),
            verbatimTextOutput(ns("codigo_r_glmb"))
          )
        )
      )

    ) # fin navset_card_tab
  )
}

# ── SERVER ────────────────────────────────────────────────
mod_glm_bayes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos ─────────────────────────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_glmb
      req(!is.null(fuente) && nchar(fuente) > 0)
      tryCatch({
        e <- new.env()
        load(system.file("app/data", paste0(fuente, ".rda"),
                         package = "StatBayes"), envir = e)
        df <- get(ls(e)[1], envir = e)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(err) {
        showNotification(paste("Error:", err$message), type="error"); NULL
      })
    })

    datos_mod <- reactiveVal(NULL)
    observeEvent(datos(), { datos_mod(datos()) })

    # ── Manejo de NAs ────────────────────────────────────────────────────────
    datos_finales_glmb <- reactive({
      df <- datos_mod()
      req(df)
      if (isTRUE(input$manejo_na_glmb == "eliminar")) {
        df <- tidyr::drop_na(df)
      }
      df
    })

    output$na_info_glmb <- renderUI({
      df_orig  <- datos_mod()
      df_final <- datos_finales_glmb()
      req(df_orig)
      n_na <- sum(!stats::complete.cases(df_orig))
      if (n_na == 0) return(
        div(class = "alert alert-success small py-2 px-3 mb-0",
            bs_icon("check-circle", class = "me-1"), "Sin valores perdidos.")
      )
      n_elim <- nrow(df_orig) - nrow(df_final)
      if (input$manejo_na_glmb == "eliminar")
        div(class = "alert alert-warning small py-2 px-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            paste0(n_elim, " fila(s) eliminadas. Quedan ", nrow(df_final), " filas."))
      else
        div(class = "alert alert-info small py-2 px-3 mb-0",
            bs_icon("info-circle", class = "me-1"),
            paste0(n_na, " fila(s) con NA. El modelo puede fallar o excluirlas ",
                   "autom\u00e1ticamente \u2014 pod\u00e9s eliminarlas a la izquierda para mayor control."))
    })

    vars_num <- reactive({
      req(datos_finales_glmb())
      names(which(sapply(datos_finales_glmb(), is.numeric)))
    })
    vars_cat <- reactive({
      req(datos_finales_glmb())
      names(which(sapply(datos_finales_glmb(),
                         function(x) is.factor(x) || is.character(x))))
    })

    # ── Info dataset ──────────────────────────────────
    output$info_dataset_glmb <- renderUI({
      fuente <- input$fuente_datos_glmb
      if (is.null(fuente) || fuente == "propio") return(NULL)

      info <- list(
        cowles_glm = list(
          titulo = "Voluntariado (Cowles, carData)",
          texto  = tagList(
            "\u00bfParticipa la persona como voluntaria en investigaci\u00f3n? ",
            "(s\u00ed/no) en funci\u00f3n de rasgos de personalidad. ",
            strong("1421 participantes"), ". ",
            "Predictores: neuroticismo, extraversi\u00f3n y sexo. ",
            "Fuente: Cowles & Davis (1987)."
          )
        ),
        pima_glm = list(
          titulo = "Diabetes en mujeres Pima (PimaIndiansDiabetes, mlbench)",
          texto  = tagList(
            "Diagn\u00f3stico de diabetes (positivo/negativo) en ",
            strong("768 mujeres"), " de la tribu Pima, Arizona, EE.UU. ",
            "Predictores: glucosa, presi\u00f3n arterial, IMC, edad y otros. ",
            "Fuente: Smith et al. (1988)."
          )
        ),
        hcrabs_glm = list(
          titulo = "Cangrejos herradura (hcrabs, GLMsData)",
          texto  = tagList(
            "N\u00famero de machos sat\u00e9lite adheridos a hembras de ",
            em("Limulus polyphemus"), " en ",
            strong("173 hembras"), ". ",
            "Predictores: color, estado de la espina, ancho y peso. ",
            "Fuente: Brockmann (1996)."
          )
        ),
        insurance_glm = list(
          titulo = "Reclamaciones de seguro (Insurance, MASS)",
          texto  = tagList(
            "N\u00famero de reclamaciones de seguro de autom\u00f3vil en ",
            strong("64 grupos"), " clasificados por distrito, ",
            "grupo de motor y edad del conductor. ",
            "Offset: n\u00famero de asegurados. ",
            "Fuente: Baxter et al. (1980)."
          )
        ),
        ants_glm = list(
          titulo = "Riqueza de hormigas (ants, GLMsData)",
          texto  = tagList(
            "N\u00famero de especies de hormigas en ",
            strong("44 sitios"), " en el noreste de EE.UU. ",
            "Predictores: tipo de h\u00e1bitat (turbera/bosque), ",
            "latitud y elevaci\u00f3n. ",
            "Fuente: GLMsData (Dunn & Smyth, 2018)."
          )
        ),
        danish_glm = list(
          titulo = "C\u00e1ncer de pulm\u00f3n en Dinamarca (danishlc, GLMsData)",
          texto  = tagList(
            "Casos de c\u00e1ncer de pulm\u00f3n en ",
            strong("24 grupos"), " por ciudad y grupo de edad ",
            "en Dinamarca (1968-1971). ",
            "Offset: tama\u00f1o de la poblaci\u00f3n expuesta. ",
            "Fuente: Breslow & Day (1987)."
          )
        ),
        mite_logistic = list(
          titulo = "Presencia/ausencia de \u00e1caros (mite, vegan)",
          texto  = tagList(
            "Presencia (1) o ausencia (0) del \u00e1caro ", em("NPRA"),
            " en ", strong("70 muestras"), " de musgo en Quebec, Canad\u00e1. ",
            "Predictores: densidad del sustrato, contenido de agua, ",
            "cobertura de arbustos y topograf\u00eda. ",
            "Fuente: Borcard & Legendre (1994)."
          )
        ),
        mite_counts = list(
          titulo = "Abundancia de \u00e1caros (mite, vegan)",
          texto  = tagList(
            "Abundancia del \u00e1caro ", em("Brachy"),
            " en ", strong("70 muestras"), " de musgo en Quebec, Canad\u00e1. ",
            "Predictores: densidad del sustrato, contenido de agua, ",
            "cobertura de arbustos y topograf\u00eda."
          )
        )
      )

      datos_info <- info[[fuente]]
      if (is.null(datos_info)) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong(datos_info$titulo), br(),
          datos_info$texto)
    })

    output$resumen_datos_glmb <- renderUI({
      req(datos())
      d <- datos()
      div(class="small text-muted",
          bs_icon("check-circle-fill",
                  style=paste0("color:",colores$exito), class="me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_glmb <- renderUI({
      req(datos())
      d <- datos()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(col_widths=c(4,4,4),
        card(class="text-center",
          card_body(class="p-2",
            h3(style=paste0("color:",colores$primario,"; font-weight:700;"), nrow(d)),
            p(class="small text-muted mb-0", "Observaciones"))),
        card(class="text-center",
          card_body(class="p-2",
            h3(style=paste0("color:",colores$acento,"; font-weight:700;"), nnum),
            p(class="small text-muted mb-0", "Num\u00e9ricas"))),
        card(class="text-center",
          card_body(class="p-2",
            h3(style=paste0("color:",colores$secundario,"; font-weight:700;"), ncat),
            p(class="small text-muted mb-0", "Categ\u00f3ricas")))
      )
    })

    output$tabla_preview_glmb <- renderDT({
      req(datos())
      datatable(datos(), rownames=FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class="table-sm table-striped")
    })

    # ── Datos propios ─────────────────────────────────
    datos_propio <- reactive({
      req(input$archivo_glmb)
      ext <- tools::file_ext(input$archivo_glmb$name)
      tryCatch({
        df <- if (ext %in% c("xlsx","xls"))
          readxl::read_excel(input$archivo_glmb$datapath)
        else
          readr::read_delim(input$archivo_glmb$datapath,
                            delim=input$separador_glmb %||% ",",
                            show_col_types=FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error=function(e) {
        showNotification(paste("Error:", e$message), type="error"); NULL
      })
    })

    observeEvent(datos_propio(), { datos_mod(datos_propio()) })

    output$resumen_datos_propio_glmb <- renderUI({
      req(datos_propio())
      d <- datos_propio()
      div(class="small text-muted",
          bs_icon("check-circle-fill",
                  style=paste0("color:",colores$exito), class="me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_glmb <- renderUI({
      req(datos_propio())
      d <- datos_propio()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(col_widths=c(4,4,4),
        card(class="text-center",
          card_body(class="p-2",
            h3(style=paste0("color:",colores$primario,"; font-weight:700;"), nrow(d)),
            p(class="small text-muted mb-0", "Observaciones"))),
        card(class="text-center",
          card_body(class="p-2",
            h3(style=paste0("color:",colores$acento,"; font-weight:700;"), nnum),
            p(class="small text-muted mb-0", "Num\u00e9ricas"))),
        card(class="text-center",
          card_body(class="p-2",
            h3(style=paste0("color:",colores$secundario,"; font-weight:700;"), ncat),
            p(class="small text-muted mb-0", "Categ\u00f3ricas")))
      )
    })

    output$tabla_preview_propio_glmb <- renderDT({
      req(datos_propio())
      datatable(datos_propio(), rownames=FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class="table-sm table-striped")
    })

    # ── Tipos de variables ────────────────────────────
    tipos_usuario_glmb <- reactiveVal(NULL)

    output$tabla_tipos_glmb <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_glmb()
      filas <- lapply(names(d), function(nm) {
        col    <- d[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor" else "numeric"
        icono  <- if (actual=="factor")
          bs_icon("tag-fill", style=paste0("color:",colores$acento))
        else bs_icon("123", style=paste0("color:",colores$primario))
        sel <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else actual
        tags$tr(
          tags$td(style="vertical-align:middle; padding:5px 8px;",
                  div(class="d-flex align-items-center gap-2", icono, strong(nm))),
          tags$td(style="vertical-align:middle; padding:5px 8px;",
                  tags$span(class="badge",
                    style=paste0("background:",
                      if (actual=="factor") colores$acento else colores$primario,
                      "; font-size:0.75rem;"),
                    if (actual=="factor") "Factor" else "Num\u00e9rico")),
          tags$td(style="padding:5px 8px;",
                  selectInput(ns(paste0("tipo_",nm)), label=NULL,
                    choices=c("Num\u00e9rico"="numeric",
                              "Factor (categ\u00f3rico)"="factor",
                              "Excluir"="excluir"),
                    selected=sel, width="180px")),
          tags$td(style="vertical-align:middle; padding:5px 8px;",
                  if (!is.null(tu) && !is.null(tu[[nm]]) && tu[[nm]] != actual)
                    tags$span(class="badge",
                              style=paste0("background:",colores$exito),
                              "Modificado")
                  else tags$span(class="text-muted small", "Sin cambios"))
        )
      })
      tags$table(
        class="table table-sm table-hover small mb-0",
        tags$thead(
          style=paste0("background:",colores$primario," !important; color:#fff !important;"),
          tags$tr(tags$th(style="padding:7px 8px;","Variable"),
                  tags$th(style="padding:7px 8px;","Tipo detectado"),
                  tags$th(style="padding:7px 8px;","Tipo a usar"),
                  tags$th(style="padding:7px 8px;","Estado"))
        ),
        tags$tbody(filas)
      )
    })

    observeEvent(input$aplicar_tipos_glmb, {
      req(datos_mod())
      d  <- datos_mod()
      tu <- setNames(lapply(names(d), function(nm) input[[paste0("tipo_",nm)]]), names(d))
      tipos_usuario_glmb(tu)
      for (nm in names(d)) {
        nuevo <- tu[[nm]]
        if (!is.null(nuevo) && nuevo != "excluir")
          d[[nm]] <- switch(nuevo, numeric=as.numeric(d[[nm]]),
                            factor=as.factor(d[[nm]]))
      }
      excluir <- names(tu)[sapply(tu, function(t) !is.null(t) && t=="excluir")]
      if (length(excluir) > 0) d <- d[, !names(d) %in% excluir, drop=FALSE]
      datos_mod(d)
      showNotification("Tipos aplicados.", type="message", duration=2)
    })

    output$tipos_aplicados_msg_glmb <- renderUI({
      tu <- tipos_usuario_glmb(); if (is.null(tu)) return(NULL)
      n_excl <- sum(sapply(tu, function(t) !is.null(t) && t=="excluir"))
      if (n_excl == 0) return(NULL)
      div(class="alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle-fill", class="me-1",
                  style=paste0("color:",colores$exito)),
          paste0(n_excl, " variable(s) excluida(s)."))
    })

    observeEvent(input$resetear_tipos_glmb, {
      tipos_usuario_glmb(NULL); datos_mod(datos())
    })

    # ── Exploración ───────────────────────────────────
    output$sel_var_x_glmb <- renderUI({
      selectInput(ns("var_x_glmb"), "Variable X (predictor):",
                  choices=c(vars_num(), vars_cat()))
    })
    output$sel_color_glmb <- renderUI({
      selectInput(ns("var_color_glmb"), "Color por grupo (opcional):",
                  choices=c("Ninguno"="ninguno", vars_cat()))
    })

    output$plot_scatter_glmb <- renderPlot({
      req(datos_finales_glmb(), input$var_x_glmb, input$var_y_glmb)
      d <- datos_finales_glmb()
      x <- input$var_x_glmb; y <- input$var_y_glmb
      req(x %in% names(d), y %in% names(d))
      p <- ggplot(d, aes(.data[[x]], as.numeric(as.factor(.data[[y]]))-1)) +
        geom_jitter(color=colores$primario, alpha=0.4,
                    width=0.05, height=0.02, size=2) +
        labs(x=x, y=y) +
        theme_minimal(base_size=13) +
        theme(panel.grid.minor=element_blank(),
              plot.background=element_rect(fill=colores$fondo, color=NA))
      if (!is.null(input$var_color_glmb) && input$var_color_glmb != "ninguno")
        p <- p + aes(color=.data[[input$var_color_glmb]]) + scale_color_tableau_cb()
      if (isTRUE(input$mostrar_linea_glmb))
        p <- p + geom_smooth(method="loess", se=TRUE,
                             color=colores$acento, linewidth=0.8)
      p
    })

    output$info_y_glmb <- renderUI({
      req(datos_finales_glmb(), input$var_y_glmb)
      d <- datos_finales_glmb(); y <- input$var_y_glmb
      if (!y %in% names(d)) return(NULL)
      col <- d[[y]]
      if (is.factor(col) || is.character(col))
        div(class="alert alert-info small py-2 px-3 mb-0",
            bs_icon("info-circle", class="me-1"),
            "Y categ\u00f3rica \u2014 sugerida: familia ", strong("Binomial"))
      else if (all(col == floor(col), na.rm=TRUE) && min(col, na.rm=TRUE) >= 0)
        div(class="alert alert-info small py-2 px-3 mb-0",
            bs_icon("info-circle", class="me-1"),
            "Y de conteos \u2014 sugerida: ", strong("Poisson"), " o ",
            strong("Binomial negativa"))
      else if (max(col, na.rm=TRUE) <= 1 && min(col, na.rm=TRUE) >= 0)
        div(class="alert alert-info small py-2 px-3 mb-0",
            bs_icon("info-circle", class="me-1"),
            "Y en (0,1) \u2014 sugerida: familia ", strong("Beta"))
      else NULL
    })

    # ── Priors ────────────────────────────────────────
    output$codigo_priors_glmb <- renderText({
      dist_int <- switch(input$prior_intercept_dist_glmb,
        normal    = paste0("normal(",input$prior_intercept_mu_glmb,", ",
                           input$prior_intercept_sd_glmb,")"),
        student_t = paste0("student_t(3,",input$prior_intercept_mu_glmb,", ",
                           input$prior_intercept_sd_glmb,")"),
        cauchy    = paste0("cauchy(",input$prior_intercept_mu_glmb,", ",
                           input$prior_intercept_sd_glmb,")")
      )
      dist_b <- switch(input$prior_b_dist_glmb,
        normal    = paste0("normal(",input$prior_b_mu_glmb,", ",
                           input$prior_b_sd_glmb,")"),
        student_t = paste0("student_t(3,",input$prior_b_mu_glmb,", ",
                           input$prior_b_sd_glmb,")"),
        cauchy    = paste0("cauchy(",input$prior_b_mu_glmb,", ",
                           input$prior_b_sd_glmb,")")
      )
      paste0("c(\n  prior(",dist_int,", class = Intercept),\n",
             "  prior(",dist_b,", class = b)\n)")
    })

    observeEvent(input$ver_ppc_glmb, {
      req(datos_finales_glmb())
      n <- 200; x <- rnorm(100)
      familia <- input$familia_glmb %||% "binomial"
      mat <- replicate(n, {
        b0  <- rnorm(1, input$prior_intercept_mu_glmb,
                     input$prior_intercept_sd_glmb)
        b1  <- rnorm(1, input$prior_b_mu_glmb, input$prior_b_sd_glmb)
        eta <- b0 + b1 * x
        switch(familia,
          binomial                  = rbinom(100, 1, plogis(eta)),
          poisson                   = rpois(100, exp(pmin(eta, 10))),
          negbinomial               = rnbinom(100, mu=exp(pmin(eta,10)), size=1),
          beta                      = rbeta(100, plogis(eta)*5+0.1,
                                            (1-plogis(eta))*5+0.1),
          zero_inflated_poisson     = rpois(100, exp(pmin(eta,10))) *
                                      rbinom(100, 1, 0.7),
          zero_inflated_negbinomial = rnbinom(100, mu=exp(pmin(eta,10)),
                                              size=1) * rbinom(100,1,0.7),
          rnorm(100, eta, 1)
        )
      })
      output$plot_ppc_prior_glmb <- renderPlot({
        df <- data.frame(y=as.vector(mat),
                         sim=rep(seq_len(ncol(mat)), each=nrow(mat)))
        ggplot(df, aes(x=y, group=sim)) +
          geom_density(color=colores$primario, alpha=0.06, linewidth=0.3) +
          labs(x="Valores simulados de Y", y="Densidad de probabilidad",
               subtitle=paste0(n," simulaciones desde el prior \u2014 familia: ",familia)) +
          theme_minimal(base_size=13) +
          theme(panel.grid.minor=element_blank(),
                plot.background=element_rect(fill=colores$fondo, color=NA))
      })
      rango <- range(mat, na.rm=TRUE)
      output$msg_ppc_prior_glmb <- renderUI({
        clase <- if (abs(rango[1])>1e4 || abs(rango[2])>1e4) "sem-bad"
                 else if (abs(rango[1])>100 || abs(rango[2])>100) "sem-warn"
                 else "sem-ok"
        icono <- if (clase=="sem-ok") "check-circle-fill"
                 else if (clase=="sem-warn") "exclamation-triangle-fill"
                 else "x-circle-fill"
        div(class=paste("small p-2 mt-2 rounded",clase),
            bs_icon(icono, class="me-1"),
            if (clase=="sem-ok") "Rangos razonables. Prior adecuado."
            else if (clase=="sem-warn") "Rangos amplios. Considera priors m\u00e1s restrictivos."
            else "Rangos extremos. Prior demasiado difuso.")
      })
    })

    # ── Info familia ──────────────────────────────────
    output$info_familia_glmb <- renderUI({
      req(input$familia_glmb)
      switch(input$familia_glmb,
        binomial = div(class="alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("info-circle", class="me-1"),
          "Y debe ser 0/1 o factor de dos niveles. exp(\u03b2) = odds ratio (OR)."),
        poisson = div(class="alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("info-circle", class="me-1"),
          "Y = conteos no negativos. exp(\u03b2) = IRR. ",
          "Si hay sobredispersi\u00f3n, considera binomial negativa."),
        negbinomial = div(class="alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("info-circle", class="me-1"),
          "Conteos con sobredispersi\u00f3n. exp(\u03b2) = IRR. ",
          "Par\u00e1metro \u03c6 estimado autom\u00e1ticamente."),
        beta = div(class="alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("info-circle", class="me-1"),
          "Y debe estar en (0,1) \u2014 sin ceros ni unos exactos. ",
          "Ideal para proporciones y coberturas."),
        zero_inflated_poisson = div(class="alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("info-circle", class="me-1"),
          "Conteos con exceso de ceros. Modelo de dos componentes: ",
          "proceso de cero-inflaci\u00f3n + Poisson."),
        zero_inflated_negbinomial = div(class="alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("info-circle", class="me-1"),
          "Conteos con exceso de ceros y sobredispersi\u00f3n. ZIP + BN.")
      )
    })

    # ── Ajuste ────────────────────────────────────────
    modelo_actual_glmb    <- reactiveVal(NULL)
    modelos_guardados_glmb <- reactiveVal(list())

    output$sel_var_y_glmb <- renderUI({
      selectInput(ns("var_y_glmb"), "Variable respuesta (Y):",
                  choices=c(vars_num(), vars_cat()))
    })
    output$checks_numericos_glmb <- renderUI({
      ys  <- input$var_y_glmb %||% ""
      ops <- setdiff(vars_num(), ys)
      if (length(ops)==0) return(p(class="small text-muted","No hay variables num\u00e9ricas."))
      checkboxGroupInput(ns("preds_num_glmb"), label=NULL, choices=ops)
    })
    output$checks_categoricos_glmb <- renderUI({
      ys  <- input$var_y_glmb %||% ""
      ops <- setdiff(vars_cat(), ys)
      if (length(ops)==0) return(p(class="small text-muted","No hay variables categ\u00f3ricas."))
      checkboxGroupInput(ns("preds_cat_glmb"), label=NULL, choices=ops)
    })
    output$checks_interacciones_glmb <- renderUI({
      preds <- c(input$preds_num_glmb, input$preds_cat_glmb)
      if (length(preds)<2) return(NULL)
      pares <- combn(preds, 2, function(x) paste(x, collapse=" \u00d7 "), simplify=TRUE)
      checkboxGroupInput(ns("interacciones_glmb"), label=NULL,
                         choices=setNames(gsub(" \u00d7 ",":",pares), pares))
    })

    familia_brms_glmb <- reactive({
      switch(input$familia_glmb,
        binomial                  = binomial(),
        poisson                   = poisson(),
        negbinomial               = brms::negbinomial(),
        beta                      = brms::Beta(),
        zero_inflated_poisson     = brms::zero_inflated_poisson(),
        zero_inflated_negbinomial = brms::zero_inflated_negbinomial()
      )
    })

    priors_glmb <- reactive({
      dist_int <- switch(input$prior_intercept_dist_glmb,
        normal    = brms::prior(normal(0,2.5), class=Intercept),
        student_t = brms::prior(student_t(3,0,2.5), class=Intercept),
        cauchy    = brms::prior(cauchy(0,2.5), class=Intercept)
      )
      preds <- c(input$preds_num_glmb, input$preds_cat_glmb)
      if (length(preds)>0) {
        dist_b <- switch(input$prior_b_dist_glmb,
          normal    = brms::prior(normal(0,1), class=b),
          student_t = brms::prior(student_t(3,0,1), class=b),
          cauchy    = brms::prior(cauchy(0,1), class=b)
        )
        c(dist_int, dist_b)
      } else c(dist_int)
    })

    observeEvent(input$ajustar_glmb, {
      req(datos_finales_glmb(), input$var_y_glmb)
      d     <- datos_finales_glmb()
      preds <- c(input$preds_num_glmb, input$preds_cat_glmb,
                 input$interacciones_glmb)
      if (length(preds)==0) preds <- "1"
      frm <- as.formula(paste(input$var_y_glmb, "~",
                              paste(preds, collapse=" + ")))
      if (isTRUE(input$estandarizar_glmb))
        for (v in input$preds_num_glmb)
          if (v %in% names(d)) d[[v]] <- scale(d[[v]])[,1]

      withProgress(message="Ajustando GLM bayesiano (MCMC)\u2026",
                   detail="Esto puede tardar unos minutos.", value=0.1, {
        tryCatch({
          fit <- brms::brm(
            formula = frm, data = d,
            family  = familia_brms_glmb(),
            prior   = priors_glmb(),
            chains  = input$mcmc_chains_glmb,
            iter    = input$mcmc_iter_glmb,
            cores   = parallel::detectCores(),
            refresh = 0, silent = 2
          )
          modelo_actual_glmb(fit)
          setProgress(1)
        }, error=function(e) {
          showNotification(paste("Error al ajustar:", e$message),
                           type="error", duration=10)
        })
      })
    })

    # ── Cards métricas ────────────────────────────────
    output$cards_metricas_glmb <- renderUI({
      req(modelo_actual_glmb())
      fit <- modelo_actual_glmb()
      loo_val <- tryCatch({
        l <- loo::loo(fit)
        round(l$estimates["elpd_loo","Estimate"], 1)
      }, error=function(e) "\u2014")
      waic_val <- tryCatch({
        w <- loo::waic(fit)
        round(w$estimates["elpd_waic","Estimate"], 1)
      }, error=function(e) "\u2014")
      rmse <- tryCatch({
        pe <- brms::predictive_error(fit)
        round(sqrt(mean(pe^2)), 3)
      }, error=function(e) "\u2014")
      layout_columns(col_widths=c(4,4,4),
        card(class="text-center",
          card_body(class="p-2",
            h4(style=paste0("color:",colores$primario,"; font-weight:700;"), loo_val),
            p(class="small text-muted mb-0", "ELPD-LOO"))),
        card(class="text-center",
          card_body(class="p-2",
            h4(style=paste0("color:",colores$secundario,"; font-weight:700;"), waic_val),
            p(class="small text-muted mb-0", "ELPD-WAIC"))),
        card(class="text-center",
          card_body(class="p-2",
            h4(style=paste0("color:",colores$acento,"; font-weight:700;"), rmse),
            p(class="small text-muted mb-0", "RMSE posterior")))
      )
    })

    output$plot_predobs_glmb <- renderPlot({
      req(modelo_actual_glmb())
      fit   <- modelo_actual_glmb()
      preds <- fitted(fit)[,"Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      df    <- data.frame(obs=obs, pred=preds)
      ggplot(df, aes(obs, pred)) +
        geom_abline(slope=1, intercept=0, linetype="dashed", color=colores$texto) +
        geom_point(color=colores$primario, alpha=0.6, size=2) +
        labs(x="Observado", y="Predicho (media posterior)") +
        theme_minimal(base_size=12) +
        theme(plot.background=element_rect(fill=colores$fondo, color=NA))
    })

    output$texto_modelo_glmb <- renderUI({
      req(modelo_actual_glmb())
      fit    <- modelo_actual_glmb()
      familia <- input$familia_glmb
      escala  <- if (familia=="binomial") "log-odds \u2192 exp = OR"
                 else if (familia %in% c("poisson","negbinomial",
                   "zero_inflated_poisson","zero_inflated_negbinomial"))
                   "log \u2192 exp = IRR"
                 else "logit"
      pars <- posterior::summarise_draws(fit, mean, ~quantile(.x, c(0.025, 0.975)))
      pars <- pars[grep("^b_", pars$variable), ]
      items <- lapply(seq_len(nrow(pars)), function(i) {
        nm  <- gsub("^b_","", pars$variable[i])
        est <- round(pars$mean[i], 3)
        lo  <- round(pars$`2.5%`[i], 3)
        hi  <- round(pars$`97.5%`[i], 3)
        or  <- round(exp(pars$mean[i]), 3)
        dir <- if (lo>0) "positivo" else if (hi<0) "negativo" else "incierto"
        tags$li(class="small mb-1",
                strong(nm), ": \u03b2=", est,
                " [IC 95%: ",lo,", ",hi,"] \u2014 exp(\u03b2)=",strong(or),
                " \u2014 efecto ",strong(dir))
      })
      tagList(p(class="small text-muted mb-1",
                "Escala del enlace (", escala, "):"),
              tags$ul(items))
    })

    # ── Diagnóstico MCMC ──────────────────────────────
    output$semaforo_mcmc_glmb <- renderUI({
      req(modelo_actual_glmb())
      fit   <- modelo_actual_glmb()
      draws <- posterior::as_draws_df(fit)
      sumas <- posterior::summarise_draws(draws,
                 rhat=posterior::rhat, ess_bulk=posterior::ess_bulk)
      max_rhat <- max(as.numeric(sumas$rhat), na.rm=TRUE)
      min_ess  <- min(as.numeric(sumas$ess_bulk), na.rm=TRUE)
      clase_rhat <- if (max_rhat<1.01) "sem-ok"
                   else if (max_rhat<1.05) "sem-warn" else "sem-bad"
      clase_ess  <- if (min_ess>400) "sem-ok"
                   else if (min_ess>100) "sem-warn" else "sem-bad"
      ppd_card <- tryCatch({
        pp       <- posterior_predict(fit)
        mean_ppd <- round(mean(pp), 2)
        y_mean   <- round(mean(as.numeric(
          model.response(model.frame(fit))), na.rm=TRUE), 2)
        pct_diff <- abs(mean_ppd - y_mean) / abs(y_mean) * 100
        clase_ppd <- if (pct_diff<5) "sem-ok"
                     else if (pct_diff<15) "sem-warn" else "sem-bad"
        div(class=paste("p-2 rounded mb-2", clase_ppd),
            bs_icon(if (clase_ppd=="sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class="me-1"),
            strong("mean_PPD: "), mean_ppd, " \u2014 Y obs: ", y_mean, br(),
            span(class="text-muted small",
                 paste0("Diferencia: ", round(pct_diff,1), "% \u2014 ",
                   if (clase_ppd=="sem-ok") "el modelo reproduce bien la media"
                   else if (clase_ppd=="sem-warn") "diferencia moderada"
                   else "diferencia grande, revisa la familia")))
      }, error=function(e) NULL)
      tagList(
        div(class=paste("p-2 rounded mb-2", clase_rhat),
            bs_icon(if (clase_rhat=="sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class="me-1"),
            strong("R\u0302 m\u00e1ximo: "), round(max_rhat,4), br(),
            span(class="text-muted small",
                 if (clase_rhat=="sem-ok") "Convergencia correcta (< 1.01)"
                 else if (clase_rhat=="sem-warn") "Convergencia marginal"
                 else "Sin convergencia (\u2265 1.05)")),
        div(class=paste("p-2 rounded mb-2", clase_ess),
            bs_icon(if (clase_ess=="sem-ok") "check-circle-fill"
                    else "exclamation-triangle-fill", class="me-1"),
            strong("ESS m\u00ednimo: "), round(min_ess,0), br(),
            span(class="text-muted small",
                 if (clase_ess=="sem-ok") "ESS adecuado (> 400)"
                 else if (clase_ess=="sem-warn") "ESS marginal (> 100)"
                 else "ESS insuficiente (\u2264 100)")),
        ppd_card
      )
    })

    observe({
      req(modelo_actual_glmb())
      pars <- brms::variables(modelo_actual_glmb())
      pars <- pars[grep("^b_", pars)]
      updateSelectInput(session, "param_trace_glmb", choices=pars)
    })

    output$plot_trace_glmb <- renderPlot({
      req(modelo_actual_glmb(), input$param_trace_glmb)
      bayesplot::mcmc_trace(modelo_actual_glmb(),
                            pars=input$param_trace_glmb,
                            facet_args=list(ncol=1)) +
        theme_minimal(base_size=12)
    })

    output$plot_dens_mcmc_glmb <- renderPlot({
      req(modelo_actual_glmb())
      pars <- brms::variables(modelo_actual_glmb())
      pars <- pars[grep("^b_", pars)]
      bayesplot::mcmc_dens_overlay(modelo_actual_glmb(), pars=pars) +
        theme_minimal(base_size=12)
    })

    output$plot_ppc_post_glmb <- renderPlot({
      req(modelo_actual_glmb())
      bayesplot::pp_check(modelo_actual_glmb(), ndraws=100) +
        theme_minimal(base_size=12)
    })

    output$tabla_rhat_glmb <- renderDT({
      req(modelo_actual_glmb())
      fit   <- modelo_actual_glmb()
      draws <- posterior::as_draws_df(fit)
      sumas <- posterior::summarise_draws(draws,
                 rhat=posterior::rhat,
                 ess_bulk=posterior::ess_bulk,
                 ess_tail=posterior::ess_tail)
      df <- data.frame(
        Parametro = sumas$variable,
        Rhat      = round(as.numeric(sumas$rhat), 4),
        ESS_bulk  = round(as.numeric(sumas$ess_bulk), 0),
        ESS_tail  = round(as.numeric(sumas$ess_tail), 0),
        check.names=FALSE
      )
      df <- df[grep("^b_|^sigma|^shape|^phi", df$Parametro),]
      names(df) <- c("Par\u00e1metro","R\u0302","ESS bulk","ESS tail")
      datatable(df, options=list(pageLength=10), rownames=FALSE) |>
        DT::formatStyle("R\u0302",
          backgroundColor=DT::styleInterval(c(1.01,1.05),
            c("#f0f9f5","#fffbf0","#fff0f2")))
    })

    # ── Performance ───────────────────────────────────
    output$tabla_performance_glmb <- renderUI({
      req(modelo_actual_glmb())
      fit <- modelo_actual_glmb()
      loo_res <- tryCatch({
        l <- loo::loo(fit)
        paste0(round(l$estimates["elpd_loo","Estimate"],1),
               " (SE=",round(l$estimates["elpd_loo","SE"],1),")")
      }, error=function(e) "\u2014")
      waic_res <- tryCatch({
        w <- loo::waic(fit)
        paste0(round(w$estimates["elpd_waic","Estimate"],1),
               " (SE=",round(w$estimates["elpd_waic","SE"],1),")")
      }, error=function(e) "\u2014")
      p_waic <- tryCatch({
        w <- loo::waic(fit)
        round(w$estimates["p_waic","Estimate"],1)
      }, error=function(e) "\u2014")
      rmse <- tryCatch({
        pe <- brms::predictive_error(fit)
        round(sqrt(mean(pe^2)),3)
      }, error=function(e) "\u2014")
      mean_ppd <- tryCatch(round(mean(posterior_predict(fit)),2),
                           error=function(e) "\u2014")
      y_obs_mean <- tryCatch(
        round(mean(as.numeric(model.response(model.frame(fit))),na.rm=TRUE),2),
        error=function(e) "\u2014")
      ppd_diff <- tryCatch({
        d <- abs(as.numeric(mean_ppd)-as.numeric(y_obs_mean))
        p <- round(d/abs(as.numeric(y_obs_mean))*100,1)
        paste0(round(d,2)," (",p,"%)")
      }, error=function(e) "\u2014")
      tags$table(
        class="table table-sm small",
        tags$thead(style=paste0("background:",colores$primario,"; color:#fff;"),
                   tags$tr(tags$th("M\u00e9trica"), tags$th("Valor"))),
        tags$tbody(
          tags$tr(tags$td(strong("ELPD-LOO \u00b1 SE")), tags$td(loo_res)),
          tags$tr(style=paste0("background:",colores$fondo),
                  tags$td(strong("ELPD-WAIC \u00b1 SE")), tags$td(waic_res)),
          tags$tr(tags$td(strong("p_waic")), tags$td(p_waic)),
          tags$tr(style=paste0("background:",colores$fondo),
                  tags$td(strong("RMSE posterior")), tags$td(rmse)),
          tags$tr(tags$td(strong("Media Y observada")), tags$td(y_obs_mean)),
          tags$tr(style=paste0("background:",colores$fondo),
                  tags$td(strong("mean_PPD")), tags$td(mean_ppd)),
          tags$tr(tags$td(strong("Diferencia PPD vs. Y")), tags$td(ppd_diff))
        )
      )
    })

    output$plot_predobs_perf_glmb <- renderPlot({
      req(modelo_actual_glmb())
      fit   <- modelo_actual_glmb()
      preds <- fitted(fit)[,"Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      df    <- data.frame(obs=obs, pred=preds)
      ggplot(df, aes(obs, pred)) +
        geom_abline(slope=1, intercept=0, linetype="dashed", color=colores$texto) +
        geom_point(color=colores$primario, alpha=0.6, size=2.5) +
        geom_smooth(method="lm", se=FALSE, color=colores$acento, linewidth=0.8) +
        labs(x="Observado", y="Predicho (media posterior)") +
        theme_minimal(base_size=13) +
        theme(panel.grid.minor=element_blank(),
              plot.background=element_rect(fill=colores$fondo, color=NA))
    })

    # ── Parámetros ────────────────────────────────────
    output$tabla_params_ui_glmb <- renderUI({
      req(modelo_actual_glmb())
      DTOutput(ns("tabla_params_glmb"))
    })

    output$tabla_params_glmb <- renderDT({
      req(modelo_actual_glmb())
      pars <- parameters::model_parameters(modelo_actual_glmb(), ci=0.95,
                                            exponentiate=FALSE)
      df   <- as.data.frame(pars)
      df   <- df[, intersect(c("Parameter","Median","Mean","SD",
                                "CI_low","CI_high","pd","Rhat"), names(df))]
      if ("Mean" %in% names(df)) df[["exp(beta)"]] <- round(exp(df$Mean),3)
      df[sapply(df,is.numeric)] <- lapply(df[sapply(df,is.numeric)], round, 3)
      datatable(df, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE)
    })

    output$plot_forest_glmb <- renderPlot({
      req(modelo_actual_glmb())
      pars <- posterior::summarise_draws(modelo_actual_glmb(),
                mean, ~quantile(.x, c(0.025,0.975)))
      pars <- pars[grep("^b_", pars$variable),]
      pars$variable <- gsub("^b_","", pars$variable)
      names(pars)[3:4] <- c("lo","hi")
      ggplot(pars, aes(x=mean, y=reorder(variable,mean),
                       xmin=lo, xmax=hi, color=(lo>0|hi<0))) +
        geom_vline(xintercept=0, linetype="dashed", color=colores$texto) +
        geom_errorbarh(height=0.25, linewidth=0.6) +
        geom_point(size=2.5) +
        scale_color_manual(values=c("FALSE"=colores$acento,"TRUE"=colores$primario),
                           guide="none") +
        labs(x="Media posterior (IC 95%) \u2014 escala del enlace", y=NULL) +
        theme_minimal(base_size=13) +
        theme(plot.background=element_rect(fill=colores$fondo, color=NA))
    })

    output$plot_pd_glmb <- renderPlot({
      req(modelo_actual_glmb())
      pars <- parameters::model_parameters(modelo_actual_glmb(), ci=0.95)
      df   <- as.data.frame(pars)
      df   <- df[grep("^b_", df$Parameter),]
      if (!"pd" %in% names(df)) return(NULL)
      df$Parameter <- gsub("^b_","", df$Parameter)
      if (max(df$pd, na.rm=TRUE) <= 1) df$pd <- df$pd * 100
      ggplot(df, aes(x=pd, y=reorder(Parameter,pd), color=pd>95)) +
        geom_vline(xintercept=95, linetype="dashed", color=colores$texto) +
        geom_segment(aes(x=0, xend=pd, yend=reorder(Parameter,pd)),
                     linewidth=0.8) +
        geom_point(size=4) +
        scale_color_manual(values=c("FALSE"=colores$acento,"TRUE"=colores$primario),
                           guide="none") +
        scale_x_continuous(limits=c(0,105), breaks=c(0,25,50,75,95,100),
                           labels=function(x) paste0(x,"%")) +
        labs(x="Probabilidad de direcci\u00f3n (%)", y=NULL,
             caption="L\u00ednea punteada = 95% (\u2248 p < 0.05)") +
        theme_minimal(base_size=13) +
        theme(panel.grid.minor=element_blank(),
              plot.background=element_rect(fill=colores$fondo, color=NA))
    })

    # ── Gráficos ──────────────────────────────────────
    output$plot_areas_glmb <- renderPlot({
      req(modelo_actual_glmb())
      pars <- brms::variables(modelo_actual_glmb())
      pars <- pars[grep("^b_", pars)]
      bayesplot::mcmc_areas(modelo_actual_glmb(), pars=pars, prob=0.95) +
        theme_minimal(base_size=13)
    })

    output$plot_predobs_graf_glmb <- renderPlot({
      req(modelo_actual_glmb())
      fit   <- modelo_actual_glmb()
      preds <- fitted(fit)[,"Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      ggplot(data.frame(obs=obs, pred=preds), aes(obs,pred)) +
        geom_abline(slope=1, intercept=0, linetype="dashed", color=colores$texto) +
        geom_point(color=colores$primario, alpha=0.6, size=2.5) +
        labs(x="Observado", y="Predicho") +
        theme_minimal(base_size=13) +
        theme(plot.background=element_rect(fill=colores$fondo, color=NA))
    })

    output$plot_resid_glmb <- renderPlot({
      req(modelo_actual_glmb())
      fit   <- modelo_actual_glmb()
      preds <- fitted(fit)[,"Estimate"]
      obs   <- as.numeric(model.response(model.frame(fit)))
      df    <- data.frame(ajustado=preds, residuo=obs-preds)
      ggplot(df, aes(ajustado, residuo)) +
        geom_hline(yintercept=0, linetype="dashed", color=colores$texto) +
        geom_point(color=colores$primario, alpha=0.6, size=2) +
        geom_smooth(method="loess", se=FALSE, color=colores$acento, linewidth=0.8) +
        labs(x="Valores ajustados", y="Residuos") +
        theme_minimal(base_size=13) +
        theme(plot.background=element_rect(fill=colores$fondo, color=NA))
    })

    # ── Efectos marginales ────────────────────────────
    output$sel_pred_marginal_glmb <- renderUI({
      req(modelo_actual_glmb())
      preds <- c(input$preds_num_glmb, input$preds_cat_glmb)
      selectInput(ns("pred_marginal_glmb"), "Predictor focal:", choices=preds)
    })

    output$plot_marginal_glmb <- renderPlot({
      req(modelo_actual_glmb(), input$pred_marginal_glmb)
      tryCatch({
        ef <- brms::conditional_effects(modelo_actual_glmb(),
                                         effects=input$pred_marginal_glmb)
        plot(ef, plot=FALSE)[[1]] +
          theme_minimal(base_size=13) +
          theme(plot.background=element_rect(fill=colores$fondo, color=NA))
      }, error=function(e) {
        ggplot() + annotate("text", x=0.5, y=0.5,
                            label=paste("Error:",e$message)) + theme_void()
      })
    })

    output$marginal_interpretacion_glmb <- renderUI({
      req(modelo_actual_glmb(), input$pred_marginal_glmb)
      fit     <- modelo_actual_glmb()
      pred    <- input$pred_marginal_glmb
      familia <- input$familia_glmb
      pars    <- posterior::summarise_draws(fit, mean, ~quantile(.x, c(0.025,0.975)))
      row     <- pars[pars$variable == paste0("b_",pred),]
      if (nrow(row)==0) return(NULL)
      est <- round(row$mean, 3)
      lo  <- round(row$`2.5%`, 3)
      hi  <- round(row$`97.5%`, 3)
      or  <- round(exp(est), 3)
      escala_txt <- if (familia=="binomial")
        paste0("exp(\u03b2) = OR = ",or," \u2014 el odds se multiplica por ",or,
               " por cada unidad adicional de ",pred,".")
      else if (familia %in% c("poisson","negbinomial",
                               "zero_inflated_poisson","zero_inflated_negbinomial"))
        paste0("exp(\u03b2) = IRR = ",or," \u2014 el conteo esperado se multiplica por ",
               or," por cada unidad adicional de ",pred,".")
      else paste0("\u03b2 = ",est," (escala del enlace).")
      div(class="alert alert-info small",
          bs_icon("lightbulb", class="me-1"),
          strong("Interpretaci\u00f3n: "), "\u03b2 = ",est,
          " [IC 95%: ",lo,", ",hi,"]. ", escala_txt)
    })

    output$inputs_prediccion_glmb <- renderUI({
      req(modelo_actual_glmb())
      preds <- c(input$preds_num_glmb, input$preds_cat_glmb)
      d     <- datos_finales_glmb()
      lapply(preds, function(p) {
        if (is.numeric(d[[p]]))
          numericInput(ns(paste0("pred_val_",p)), p,
                       value=round(mean(d[[p]], na.rm=TRUE),2))
        else
          selectInput(ns(paste0("pred_val_",p)), p,
                      choices=levels(as.factor(d[[p]])))
      })
    })

    observeEvent(input$calcular_prediccion_glmb, {
      req(modelo_actual_glmb())
      preds    <- c(input$preds_num_glmb, input$preds_cat_glmb)
      nuevos   <- setNames(lapply(preds, function(p) input[[paste0("pred_val_",p)]]),
                           preds)
      nuevos_df <- as.data.frame(lapply(nuevos, function(x) {
        v <- suppressWarnings(as.numeric(x)); if (is.na(v)) x else v
      }))
      tryCatch({
        pred <- fitted(modelo_actual_glmb(), newdata=nuevos_df,
                       probs=c(0.025,0.975))
        output$resultado_prediccion_glmb <- renderUI({
          div(class="alert alert-success",
              h5(class="mb-1", round(pred[,"Estimate"],3)),
              p(class="small mb-0",
                "IC credible 95%: [",round(pred[,"Q2.5"],3),", ",
                round(pred[,"Q97.5"],3),"]"))
        })
      }, error=function(e) {
        output$resultado_prediccion_glmb <- renderUI({
          div(class="alert alert-danger small", e$message)
        })
      })
    })

    # ── Comparar modelos ──────────────────────────────
    observeEvent(input$guardar_modelo_glmb, {
      req(modelo_actual_glmb(), input$nombre_modelo_glmb != "")
      nm    <- input$nombre_modelo_glmb
      lista <- modelos_guardados_glmb()
      lista[[nm]] <- modelo_actual_glmb()
      modelos_guardados_glmb(lista)
      showNotification(paste("Modelo",nm,"guardado."), type="message")
    })

    output$lista_modelos_guardados_glmb <- renderUI({
      lista <- modelos_guardados_glmb()
      if (length(lista)==0)
        return(p(class="small text-muted","A\u00fan no hay modelos guardados."))
      tags$ul(class="small",
              lapply(names(lista), function(nm)
                tags$li(bs_icon("check2", class="me-1"), nm)))
    })

    observeEvent(input$limpiar_modelos_glmb, { modelos_guardados_glmb(list()) })

    output$tabla_comparacion_glmb <- renderUI({
      lista <- modelos_guardados_glmb()
      if (length(lista)<2)
        return(div(class="alert alert-info small",
                   "Guarda al menos 2 modelos para compararlos."))
      DTOutput(ns("dt_comparacion_glmb"))
    })

    output$dt_comparacion_glmb <- renderDT({
      lista <- modelos_guardados_glmb()
      req(length(lista)>=2)
      tryCatch({
        loos <- lapply(lista, loo::loo)
        comp <- loo::loo_compare(loos)
        datatable(as.data.frame(round(comp,2)),
                  options=list(pageLength=10, scrollX=TRUE))
      }, error=function(e) data.frame(Error=e$message))
    })

    output$plot_comparacion_glmb <- renderPlot({
      lista <- modelos_guardados_glmb()
      req(length(lista)>=2)
      tryCatch({
        loos <- lapply(lista, loo::loo)
        comp <- loo::loo_compare(loos)
        df   <- as.data.frame(comp); df$modelo <- rownames(df)
        ggplot(df, aes(x=elpd_diff, y=reorder(modelo,elpd_diff),
                       xmin=elpd_diff-se_diff, xmax=elpd_diff+se_diff)) +
          geom_vline(xintercept=0, linetype="dashed", color=colores$texto) +
          geom_errorbarh(height=0.25, color=colores$primario) +
          geom_point(size=3, color=colores$primario) +
          labs(x="Diferencia ELPD-LOO (\u00b1 SE)", y=NULL,
               caption="Mayor ELPD = mejor predicci\u00f3n") +
          theme_minimal(base_size=13) +
          theme(plot.background=element_rect(fill=colores$fondo, color=NA))
      }, error=function(e) {
        ggplot() + annotate("text", x=0.5, y=0.5,
                            label=paste("Error:",e$message)) + theme_void()
      })
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_glmb <- renderText({
      req(input$var_y_glmb)
      preds <- c(input$preds_num_glmb, input$preds_cat_glmb,
                 input$interacciones_glmb)
      if (length(preds)==0) preds <- "1"
      formula_str <- paste(input$var_y_glmb, "~",
                           paste(preds, collapse=" + "))
      nm_datos <- if (input$fuente_datos_glmb != "propio")
        input$fuente_datos_glmb else "mis_datos"
      familia_str <- switch(input$familia_glmb,
        binomial                  = "binomial()",
        poisson                   = "poisson()",
        negbinomial               = "negbinomial()",
        beta                      = "Beta()",
        zero_inflated_poisson     = "zero_inflated_poisson()",
        zero_inflated_negbinomial = "zero_inflated_negbinomial()"
      )
      dist_int <- switch(input$prior_intercept_dist_glmb,
        normal    = paste0("normal(",input$prior_intercept_mu_glmb,", ",
                           input$prior_intercept_sd_glmb,")"),
        student_t = paste0("student_t(3,",input$prior_intercept_mu_glmb,", ",
                           input$prior_intercept_sd_glmb,")"),
        cauchy    = paste0("cauchy(",input$prior_intercept_mu_glmb,", ",
                           input$prior_intercept_sd_glmb,")")
      )
      dist_b <- switch(input$prior_b_dist_glmb,
        normal    = paste0("normal(",input$prior_b_mu_glmb,", ",
                           input$prior_b_sd_glmb,")"),
        student_t = paste0("student_t(3,",input$prior_b_mu_glmb,", ",
                           input$prior_b_sd_glmb,")"),
        cauchy    = paste0("cauchy(",input$prior_b_mu_glmb,", ",
                           input$prior_b_sd_glmb,")")
      )
      paste0(
        "# ── GLM bayesiano con brms ───────────────────────────\n",
        "library(brms)\nlibrary(bayesplot)\nlibrary(posterior)\nlibrary(loo)\n\n",
        "# Datos\ndata('",nm_datos,"', package='StatBayes')\n\n",
        "# Priors\nmis_priors <- c(\n",
        "  prior(",dist_int,", class = Intercept),\n",
        "  prior(",dist_b,", class = b)\n)\n\n",
        "# Ajustar modelo\nfit <- brm(\n",
        "  formula = ",formula_str,",\n",
        "  data    = ",nm_datos,",\n",
        "  family  = ",familia_str,",\n",
        "  prior   = mis_priors,\n",
        "  chains  = ",input$mcmc_chains_glmb,",\n",
        "  iter    = ",input$mcmc_iter_glmb,",\n",
        "  cores   = parallel::detectCores()\n)\n\n",
        "# Resumen\nsummary(fit)\n\n",
        "# OR o IRR\nexp(fixef(fit))\n\n",
        "# Diagnóstico\nmcmc_trace(fit)\npp_check(fit, ndraws=100)\n\n",
        "# Efectos marginales\nconditional_effects(fit)\n\n",
        "# LOO\nloo(fit)\n"
      )
    })

    output$descargar_script_glmb <- downloadHandler(
      filename = function() paste0("StatBayes_glm_bayes_",Sys.Date(),".R"),
      content  = function(file) writeLines(output$codigo_r_glmb(), file)
    )

  })
}
