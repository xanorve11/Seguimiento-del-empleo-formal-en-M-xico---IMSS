# =============================================================================
# narrativa.R
# Construcción de TODAS las variables narrativas dinámicas
# NO se imprime nada aquí — solo se generan objetos de R
# Proyecto: Fichas económicas IMSS
# =============================================================================

# =============================================================================
# BLOQUE 1 · NACIONAL — TRABAJADORES ASEGURADOS (imss_nacional_indicadores.rds)
# =============================================================================

construir_narrativa_nacional <- function(df_nac) {
  
  ult <- df_nac |>
    dplyr::filter(date == max(date, na.rm = TRUE)) |>
    dplyr::slice(1)
  
  per <- ultimo_periodo(df_nac)
  
  ta_actual        <- ult$ta
  tp_actual        <- ult$tp
  te_actual        <- ult$te
  diff_mensual_ta  <- ult$diff_mensual_ta
  diff_anual_ta    <- ult$diff_anual_ta
  tc_mensual_ta    <- ult$tc_mensual_ta
  tc_anual_ta      <- ult$tc_anual_ta
  tc_mensual_tp    <- ult$tc_mensual_tp
  tc_anual_tp      <- ult$tc_anual_tp
  tc_mensual_te    <- ult$tc_mensual_te
  tc_anual_te      <- ult$tc_anual_te
  
  pct_tp <- (tp_actual / ta_actual) * 100
  pct_te <- (te_actual / ta_actual) * 100
  
  # Acumulado en lo que va del año
  acumulado_anio <- df_nac |>
    dplyr::filter(year == ult$year, !is.na(diff_mensual_ta)) |>
    dplyr::summarise(acum = sum(diff_mensual_ta, na.rm = TRUE)) |>
    dplyr::pull(acum)
  
  # Promedios históricos del mismo mes
  prom_tc_mensual <- df_nac |>
    dplyr::filter(month == ult$month, !is.na(tc_mensual_ta)) |>
    dplyr::summarise(prom = mean(tc_mensual_ta, na.rm = TRUE)) |>
    dplyr::pull(prom)
  
  prom_tc_anual <- df_nac |>
    dplyr::filter(month == ult$month, !is.na(tc_anual_ta)) |>
    dplyr::summarise(prom = mean(tc_anual_ta, na.rm = TRUE)) |>
    dplyr::pull(prom)
  
  # Variables narrativas de dirección
  verbo_mensual       <- dplyr::if_else(diff_mensual_ta >= 0, "crearon", "perdieron")
  verbo_anual         <- dplyr::if_else(diff_anual_ta   >= 0, "generaron", "perdieron")
  sustantivo_mensual  <- dplyr::if_else(diff_mensual_ta >= 0, "aumento", "disminución")
  tipo_cambio_mensual <- dplyr::if_else(diff_mensual_ta >= 0, "crecimiento", "decremento")
  tipo_cambio_anual   <- dplyr::if_else(diff_anual_ta   >= 0, "crecimiento", "decremento")
  comp_mensual        <- comp_vs_promedio(tc_mensual_ta, prom_tc_mensual)
  comp_anual          <- comp_vs_promedio(tc_anual_ta,   prom_tc_anual)
  
  # Dirección por tipo de empleo
  dir_mensual_tp  <- dplyr::if_else(tc_mensual_tp >= 0, "aumento", "disminución")
  dir_anual_tp    <- dplyr::if_else(tc_anual_tp   >= 0, "crecimiento", "caída")
  dir_mensual_te  <- dplyr::if_else(tc_mensual_te >= 0, "aumento", "disminución")
  dir_anual_te    <- dplyr::if_else(tc_anual_te   >= 0, "crecimiento", "caída")
  
  list(
    # Periodo
    mes_label           = per$label,
    mes_nombre          = mes_nombre(per$month),
    anio                = per$year,
    anio_anterior       = per$year - 1,
    
    # Totales formateados
    ta_fmt              = fmt_entero(ta_actual),
    tp_fmt              = fmt_entero(tp_actual),
    te_fmt              = fmt_entero(te_actual),
    pct_tp_fmt          = fmt_pct(pct_tp),
    pct_te_fmt          = fmt_pct(pct_te),
    
    # Variación mensual
    verbo_mensual       = verbo_mensual,
    sustantivo_mensual  = sustantivo_mensual,
    tipo_cambio_mensual = tipo_cambio_mensual,
    abs_mensual_fmt     = fmt_entero(abs(diff_mensual_ta)),
    tc_mensual_fmt      = fmt_pct(abs(tc_mensual_ta)),
    prom_tc_mensual_fmt = fmt_pct(prom_tc_mensual),
    comp_mensual        = comp_mensual,
    
    # Variación anual
    verbo_anual         = verbo_anual,
    tipo_cambio_anual   = tipo_cambio_anual,
    abs_anual_fmt       = fmt_entero(abs(diff_anual_ta)),
    tc_anual_fmt        = fmt_pct(abs(tc_anual_ta)),
    prom_tc_anual_fmt   = fmt_pct(prom_tc_anual),
    comp_anual          = comp_anual,
    
    # Por tipo de empleo
    tc_mensual_tp_fmt   = fmt_pct(abs(tc_mensual_tp)),
    tc_anual_tp_fmt     = fmt_pct(abs(tc_anual_tp)),
    tc_mensual_te_fmt   = fmt_pct(abs(tc_mensual_te)),
    tc_anual_te_fmt     = fmt_pct(abs(tc_anual_te)),
    dir_mensual_tp      = dir_mensual_tp,
    dir_anual_tp        = dir_anual_tp,
    dir_mensual_te      = dir_mensual_te,
    dir_anual_te        = dir_anual_te,
    
    # Acumulado en el año
    acumulado_anio_fmt  = fmt_entero(abs(acumulado_anio)),
    dir_acumulado       = dplyr::if_else(acumulado_anio >= 0, "generación", "pérdida")
  )
}


# =============================================================================
# BLOQUE 2 · ENTIDADES — RANKING ANUAL
# =============================================================================

construir_narrativa_entidades <- function(df_ent) {
  
  ult_date <- df_ent |>
    dplyr::filter(!is.na(tc_anual)) |>
    dplyr::pull(date) |>
    max(na.rm = TRUE)
  
  df_ult <- df_ent |> dplyr::filter(date == ult_date)
  
  top3_mayor <- df_ult |> dplyr::slice_max(order_by = tc_anual, n = 3, with_ties = FALSE)
  top3_menor <- df_ult |> dplyr::slice_min(order_by = tc_anual, n = 3, with_ties = FALSE)
  
  entidades_nulas <- df_ult |>
    dplyr::filter(abs(tc_anual) < 0.05) |>
    dplyr::pull(entidad_federativa)
  
  entidades_nulas_txt <- if (length(entidades_nulas) > 0) {
    paste(entidades_nulas, collapse = " y ")
  } else {
    "ninguna entidad"
  }
  
  prefijos <- c("(i)", "(ii)", "(iii)")
  
  ranking_mayor_txt <- purrr::map2_chr(
    seq_len(nrow(top3_mayor)), top3_mayor$entidad_federativa,
    ~ paste0(prefijos[.x], " ", .y, " (", fmt_pct(top3_mayor$tc_anual[.x]), ")")
  ) |> paste(collapse = "; ")
  
  ranking_menor_txt <- purrr::map2_chr(
    seq_len(nrow(top3_menor)), top3_menor$entidad_federativa,
    ~ paste0(prefijos[.x], " ", .y, " (", fmt_pct(top3_menor$tc_anual[.x]), ")")
  ) |> paste(collapse = "; ")
  
  list(
    ranking_mayor_txt   = ranking_mayor_txt,
    ranking_menor_txt   = ranking_menor_txt,
    entidades_nulas_txt = entidades_nulas_txt,
    # Top 3 mayor — individuales
    entidad_1 = top3_mayor$entidad_federativa[1], tc_entidad_1 = fmt_pct(top3_mayor$tc_anual[1]),
    entidad_2 = top3_mayor$entidad_federativa[2], tc_entidad_2 = fmt_pct(top3_mayor$tc_anual[2]),
    entidad_3 = top3_mayor$entidad_federativa[3], tc_entidad_3 = fmt_pct(top3_mayor$tc_anual[3]),
    # Top 3 menor — individuales
    entidad_m1 = top3_menor$entidad_federativa[1], tc_entidad_m1 = fmt_pct(top3_menor$tc_anual[1]),
    entidad_m2 = top3_menor$entidad_federativa[2], tc_entidad_m2 = fmt_pct(top3_menor$tc_anual[2]),
    entidad_m3 = top3_menor$entidad_federativa[3], tc_entidad_m3 = fmt_pct(top3_menor$tc_anual[3])
  )
}


# =============================================================================
# BLOQUE 3 · SEXO (imss_nacional_sexo.rds)
# =============================================================================

construir_narrativa_sexo <- function(df_sexo) {
  
  ult_date <- max(df_sexo$date, na.rm = TRUE)
  df_ult   <- df_sexo |> dplyr::filter(date == ult_date)
  
  hombres  <- df_ult |> dplyr::filter(sexo == 1) |> dplyr::slice(1)
  mujeres  <- df_ult |> dplyr::filter(sexo == 2) |> dplyr::slice(1)
  
  total      <- hombres$ta + mujeres$ta
  pct_h      <- (hombres$ta / total) * 100
  pct_m      <- (mujeres$ta / total) * 100
  brecha_sbc <- hombres$sbc_prom - mujeres$sbc_prom
  pct_brecha <- (brecha_sbc / hombres$sbc_prom) * 100
  
  list(
    mes_label   = etiqueta_periodo(lubridate::month(ult_date), lubridate::year(ult_date)),
    ta_hombres  = fmt_entero(hombres$ta),
    ta_mujeres  = fmt_entero(mujeres$ta),
    pct_h_fmt   = fmt_pct(pct_h),
    pct_m_fmt   = fmt_pct(pct_m),
    sbc_h_fmt   = fmt_sbc(hombres$sbc_prom),
    sbc_m_fmt   = fmt_sbc(mujeres$sbc_prom),
    brecha_fmt  = fmt_sbc(brecha_sbc),
    brecha_pct  = fmt_pct(pct_brecha),
    sexo_mayor  = dplyr::if_else(hombres$ta >= mujeres$ta, "hombres", "mujeres")
  )
}


# =============================================================================
# BLOQUE 4 · SECTOR ECONÓMICO NACIONAL (imss_estado_sector.rds)
# =============================================================================

construir_narrativa_sector_nacional <- function(df_sector) {
  
  ult_date <- max(df_sector$date, na.rm = TRUE)
  
  df_nac_sec <- df_sector |>
    dplyr::filter(date == ult_date) |>
    dplyr::group_by(sector) |>
    dplyr::summarise(ta = sum(ta, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(!stringr::str_to_lower(sector) %in% c("total", "")) |>
    dplyr::arrange(dplyr::desc(ta)) |>
    dplyr::mutate(
      total_nac = sum(ta),
      pct       = (ta / total_nac) * 100
    )
  
  s1 <- df_nac_sec |> dplyr::slice(1)
  s2 <- df_nac_sec |> dplyr::slice(2)
  s3 <- df_nac_sec |> dplyr::slice(3)
  pct_top3 <- sum(df_nac_sec$pct[1:3])
  
  # Tabla completa para kable
  tabla <- df_nac_sec |>
    dplyr::mutate(
      Sector         = sector,
      `Trabajadores` = fmt_entero(ta),
      `% del total`  = fmt_pct(pct)
    ) |>
    dplyr::select(Sector, Trabajadores, `% del total`)
  
  list(
    mes_label      = etiqueta_periodo(
      lubridate::month(ult_date), lubridate::year(ult_date)
    ),
    sector_1       = s1$sector, ta_s1 = fmt_entero(s1$ta), pct_s1 = fmt_pct(s1$pct),
    sector_2       = s2$sector, ta_s2 = fmt_entero(s2$ta), pct_s2 = fmt_pct(s2$pct),
    sector_3       = s3$sector, ta_s3 = fmt_entero(s3$ta), pct_s3 = fmt_pct(s3$pct),
    pct_top3_fmt   = fmt_pct(pct_top3),
    tabla_sectores = tabla
  )
}


# =============================================================================
# BLOQUE 5 · PATRONES (imss_patrones.rds)
# Estructura: una fila por periodo, columnas anchas por tamaño de empresa.
# Columnas clave: fecha, Año, mes_num, Total, Total_t_mensual, Total_t_anual
# =============================================================================

construir_narrativa_patrones <- function(df_pat) {
  
  # Último periodo disponible
  ult <- df_pat |>
    dplyr::filter(fecha == max(fecha, na.rm = TRUE)) |>
    dplyr::slice(1)
  
  pat_actual    <- ult$Total
  tc_mensual    <- ult$Total_t_mensual
  tc_anual      <- ult$Total_t_anual
  
  # Referencia: noviembre 2023 (punto de quiebre mencionado en la narrativa)
  ref_row  <- df_pat |> dplyr::filter(fecha == as.Date("2023-11-01")) |> dplyr::slice(1)
  pat_ref  <- if (nrow(ref_row) > 0) ref_row$Total else NA_real_
  reduccion <- if (!is.na(pat_ref)) pat_ref - pat_actual else NA_real_
  
  # Promedios históricos del mismo mes para comparación
  prom_tc_mensual <- df_pat |>
    dplyr::filter(mes_num == ult$mes_num, !is.na(Total_t_mensual)) |>
    dplyr::summarise(prom = mean(Total_t_mensual, na.rm = TRUE)) |>
    dplyr::pull(prom)
  
  prom_tc_anual <- df_pat |>
    dplyr::filter(mes_num == ult$mes_num, !is.na(Total_t_anual)) |>
    dplyr::summarise(prom = mean(Total_t_anual, na.rm = TRUE)) |>
    dplyr::pull(prom)
  
  # Variables narrativas de dirección
  dir_mensual  <- dplyr::if_else(tc_mensual >= 0, "aumento", "disminución")
  dir_anual    <- dplyr::if_else(tc_anual   >= 0, "aumento", "disminución")
  comp_mensual <- comp_vs_promedio(tc_mensual, prom_tc_mensual)
  comp_anual   <- comp_vs_promedio(tc_anual,   prom_tc_anual)
  
  # Tabla de tamaños para el último periodo
  # Los tamaños están en columnas 800, 801, 802, 803, 804, 805, 808
  nombres_tamano <- c(
    "800 - Patrones con un puesto de trabajo",
    "801 - Patrones con 2 y hasta 5 PT",
    "802 - Patrones con 6 y hasta 50 PT",
    "803 - Patrones con 51 y hasta 250 PT",
    "804 - Patrones con 251 y hasta 500 PT",
    "805 - Patrones con 501 y hasta 1,000 PT",
    "808 - Patrones con más de 1,000 PT"
  )
  
  # Etiquetas cortas para la tabla
  etiquetas_cortas <- c(
    "1 puesto de trabajo",
    "2 a 5 puestos",
    "6 a 50 puestos",
    "51 a 250 puestos",
    "251 a 500 puestos",
    "501 a 1,000 puestos",
    "Más de 1,000 puestos"
  )
  
  valores_tamano <- purrr::map_dbl(nombres_tamano, ~ {
    col <- ult[[.x]]
    if (is.null(col) || length(col) == 0) NA_real_ else as.numeric(col)
  })
  
  tabla_tamano <- tibble::tibble(
    `Tamaño de empresa`  = etiquetas_cortas,
    `Patrones`           = fmt_entero(valores_tamano),
    `% del total`        = fmt_pct((valores_tamano / pat_actual) * 100)
  )
  
  list(
    mes_label           = etiqueta_periodo(ult$mes_num, ult$Año),
    mes_nombre          = mes_nombre(ult$mes_num),
    anio                = ult$Año,
    pat_actual_fmt      = fmt_entero(pat_actual),
    # Mensual
    tc_mensual_fmt      = fmt_pct(abs(tc_mensual)),
    dir_mensual         = dir_mensual,
    prom_tc_mensual_fmt = fmt_pct(prom_tc_mensual),
    comp_mensual        = comp_mensual,
    # Anual
    tc_anual_fmt        = fmt_pct(abs(tc_anual)),
    signo_anual         = dplyr::if_else(tc_anual >= 0, "+", "-"),
    dir_anual           = dir_anual,
    prom_tc_anual_fmt   = fmt_pct(prom_tc_anual),
    comp_anual          = comp_anual,
    # Referencia nov-2023
    pat_ref_fmt         = if (!is.na(pat_ref)) fmt_entero(pat_ref) else "N/D",
    reduccion_fmt       = if (!is.na(reduccion)) fmt_entero(abs(reduccion)) else "N/D",
    # Tabla por tamaño
    tabla_tamano        = tabla_tamano
  )
}


# =============================================================================
# BLOQUE 6 · TABLA TIPO DE EMPLEO POR SEXO
# =============================================================================

construir_tabla_tipo_empleo <- function(df_nac, df_sexo) {
  
  ult_nac  <- df_nac  |> dplyr::filter(date == max(date, na.rm = TRUE)) |> dplyr::slice(1)
  ult_sexo <- df_sexo |> dplyr::filter(date == max(date, na.rm = TRUE))
  
  hombres <- ult_sexo |> dplyr::filter(sexo == 1) |> dplyr::slice(1)
  mujeres <- ult_sexo |> dplyr::filter(sexo == 2) |> dplyr::slice(1)
  
  ta_total <- ult_nac$ta
  tp_total <- ult_nac$tp
  te_total <- ult_nac$te
  pct_tp   <- (tp_total / ta_total) * 100
  pct_te   <- (te_total / ta_total) * 100
  
  tibble::tibble(
    `Tipo de empleo` = c("Permanente", "Eventual", "Total"),
    `Total`          = c(fmt_entero(tp_total), fmt_entero(te_total), fmt_entero(ta_total)),
    `% del total`    = c(fmt_pct(pct_tp), fmt_pct(pct_te), "100.0%"),
    `Mujeres`        = c(fmt_entero(mujeres$tp), fmt_entero(mujeres$te), fmt_entero(mujeres$ta)),
    `Hombres`        = c(fmt_entero(hombres$tp), fmt_entero(hombres$te), fmt_entero(hombres$ta)),
    `No binario`     = c("N/D", "N/D", "N/D")
  )
}