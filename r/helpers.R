# =============================================================================
# helpers.R
# Funciones de formato y utilidades generales
# Proyecto: Fichas económicas IMSS
# =============================================================================

# -----------------------------------------------------------------------------
# 1. FORMATO DE CIFRAS
# -----------------------------------------------------------------------------

#' Formatea un número entero con comas (miles)
#' Ejemplo: 22748603 -> "22,748,603"
fmt_entero <- function(x) {
  scales::comma(x, accuracy = 1, big.mark = ",")
}

#' Formatea un porcentaje con n decimales
#' Ejemplo: 0.1057 -> "0.1%" (si ya viene en escala de porcentaje: 1.057 -> "1.1%")
#' @param x Valor ya en escala de porcentaje (ej: 1.5 para 1.5%)
#' @param decimales Número de decimales (default 1)
fmt_pct <- function(x, decimales = 1) {
  scales::number(x, accuracy = 10^(-decimales), suffix = "%", big.mark = ",")
}

#' Formatea un número decimal general
#' @param x Valor numérico
#' @param decimales Número de decimales (default 2)
fmt_num <- function(x, decimales = 2) {
  scales::number(x, accuracy = 10^(-decimales), big.mark = ",")
}

#' Formatea salario base de cotización (pesos)
#' Ejemplo: 809.18 -> "$809.18"
fmt_sbc <- function(x, decimales = 2) {
  scales::dollar(x, accuracy = 10^(-decimales), prefix = "$", big.mark = ",")
}

#' Agrega signo "+" a valores positivos (para variaciones)
#' Ejemplo: 1.5 -> "+1.5%"; -2.3 -> "-2.3%"
fmt_pct_signo <- function(x, decimales = 1) {
  val <- fmt_pct(abs(x), decimales)
  dplyr::if_else(x >= 0, paste0("+", val), paste0("-", val))
}


# -----------------------------------------------------------------------------
# 2. MANEJO DE FECHAS Y PERIODOS
# -----------------------------------------------------------------------------

#' Convierte número de mes a nombre en español
#' Ejemplo: 4 -> "abril"
mes_nombre <- function(m, abreviado = FALSE) {
  meses_completos <- c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio",
    "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
  )
  meses_cortos <- c(
    "ene", "feb", "mar", "abr", "may", "jun",
    "jul", "ago", "sep", "oct", "nov", "dic"
  )
  if (abreviado) meses_cortos[m] else meses_completos[m]
}

#' Devuelve el mes anterior como texto
#' @param m Mes actual (entero 1-12)
#' @param y Año actual (entero)
mes_anterior_texto <- function(m, y) {
  fecha_ant <- lubridate::make_date(y, m, 1) - months(1)
  mes_nombre(lubridate::month(fecha_ant))
}

#' Construye etiqueta de periodo "mes año"
#' Ejemplo: mes=4, year=2026 -> "abril de 2026"
etiqueta_periodo <- function(m, y) {
  paste0(mes_nombre(m), " de ", y)
}

#' Extrae el último periodo disponible de un data frame con columnas year y month
#' Retorna una lista con: year, month, date, label
ultimo_periodo <- function(df) {
  ultimo <- df |>
    dplyr::filter(date == max(date, na.rm = TRUE)) |>
    dplyr::slice(1)
  list(
    year  = ultimo$year,
    month = ultimo$month,
    date  = ultimo$date,
    label = etiqueta_periodo(ultimo$month, ultimo$year)
  )
}


# -----------------------------------------------------------------------------
# 3. CONSTRUCCIÓN DE RANKINGS TEXTUALES
# -----------------------------------------------------------------------------

#' Genera texto de ranking "i) Estado (X%); ii) Estado (X%); iii) Estado (X%)"
#' @param df      Data frame con columnas: entidad_federativa, tc_anual
#' @param n       Número de posiciones a mostrar (default 3)
#' @param tipo    "mayor" o "menor" (ordena desc o asc)
#' @param decimales Decimales en el porcentaje
texto_ranking <- function(df, n = 3, tipo = "mayor", decimales = 1) {
  prefijos <- c("(i)", "(ii)", "(iii)", "(iv)", "(v)")
  
  if (tipo == "mayor") {
    top <- df |> dplyr::slice_max(order_by = tc_anual, n = n, with_ties = FALSE)
  } else {
    top <- df |> dplyr::slice_min(order_by = tc_anual, n = n, with_ties = FALSE)
  }
  
  purrr::map2_chr(
    seq_len(nrow(top)),
    top$entidad_federativa,
    ~ {
      pct <- fmt_pct(top$tc_anual[.x], decimales)
      paste0(prefijos[.x], " ", .y, " (", pct, ")")
    }
  ) |>
    paste(collapse = "; ")
}


# -----------------------------------------------------------------------------
# 4. UTILIDADES MISCELÁNEAS
# -----------------------------------------------------------------------------

#' Comparación vs promedio histórico: "mayor" / "menor"
#' @param valor_actual  Valor del periodo actual
#' @param promedio      Promedio histórico de referencia
comp_vs_promedio <- function(valor_actual, promedio) {
  dplyr::if_else(valor_actual >= promedio, "mayor", "menor")
}

#' Silencia advertencias de lectura de RDS y retorna el objeto
leer_rds <- function(ruta) {
  tryCatch(
    readRDS(ruta),
    error = function(e) stop(paste("No se pudo leer:", ruta, "\n", e$message))
  )
}