#' Helper: pantalla próximamente
#'
#' @param icono Bootstrap icon name.
#' @param titulo Module title.
#' @param subtitulo Module subtitle.
#' @param datasets Expected datasets string.
#' @noRd
proximamente_ui <- function(icono, titulo, subtitulo, datasets) {
  div(
    class = "py-5 px-3",
    style = "max-width: 620px; margin: 0 auto;",
    div(
      class = "text-center mb-4",
      bs_icon(icono, size = "3em",
              style = paste0("color:", colores$secundario)),
      h4(
        class = "mt-3 mb-2",
        style = paste0("color:", colores$primario, "; font-weight:700;"),
        titulo
      ),
      p(class = "text-muted", subtitulo)
    ),
    div(
      class = "alert mb-3",
      style = paste0(
        "background:", colores$fondo, ";",
        "border-left: 4px solid ", colores$acento, ";"
      ),
      bs_icon("hourglass-split", class = "me-2",
              style = paste0("color:", colores$acento)),
      strong("En desarrollo."),
      " Este m\u00f3dulo estar\u00e1 disponible en una pr\u00f3xima versi\u00f3n de StatModels."
    ),
    card(
      card_header(
        bs_icon("database", class = "me-1"),
        "Datasets previstos"
      ),
      card_body(
        p(class = "small text-muted mb-0",
          bs_icon("circle-fill", size = "0.5em", class = "me-1"),
          datasets)
      )
    )
  )
}
