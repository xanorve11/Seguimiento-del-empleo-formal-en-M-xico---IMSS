# 04_tasas de crecimiento.R
# Objetivo: calcular las tasas de crecimiento de puestos de trabajo por

# Por tamaño de patrón 
# Por sector económico 
# Por entidad 

# librerias ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  lubridate,
  openxlsx,
  beepr
)

# preparacion -------------------------------------------------------------
rm(list = ls())
gc()

# cargar datos ORIGINAL (sin modificar) -----------------------------------
datos_original <- readRDS("outputs/imss_desagregado.rds")

# funcion para calcular tasas de crecimiento y diferencias ---------------
calcular_indicadores <- function(df, grupo) {
  
  df_trabajo <- df %>%
    arrange(date, {{grupo}})
  
  df_indicadores <- df_trabajo %>%
    group_by({{grupo}}) %>%
    arrange(date) %>%
    mutate(
      diff_mensual = ta - lag(ta),
      diff_anual = ta - lag(ta, 12),
      tc_mensual = (ta / lag(ta) - 1) * 100,
      tc_anual = ifelse(!is.na(lag(ta, 12)), 
                        (ta / lag(ta, 12) - 1) * 100, 
                        NA)
    ) %>%
    ungroup() %>%
    mutate(
      ta = round(ta, 2),
      diff_mensual = round(diff_mensual, 2),
      diff_anual = round(diff_anual, 2),
      tc_mensual = round(tc_mensual, 4),
      tc_anual = round(tc_anual, 4)
    )
  
  return(df_indicadores)
}
# procesar cada variable (creando nuevos dataframes) ----------------------

# 1. SECTOR ECONOMICO - excluir "Sin dato (sector)" con valores 0
sector_tc <- datos_original %>%
  filter(variable == "sector_economico_1") %>%
  filter(!(categoria == "Sin dato (sector)" & ta == 0)) %>%
  calcular_indicadores(categoria) %>%
  select(year, month, date, categoria, ta, diff_mensual, diff_anual, 
         tc_mensual, tc_anual) %>%
  rename(sector_economico = categoria)

# 2. TAMAÑO DEL PATRON
patron_tc <- datos_original %>%
  filter(variable == "patron") %>%
  calcular_indicadores(categoria) %>%
  select(year, month, date, categoria, ta, diff_mensual, diff_anual, 
         tc_mensual, tc_anual) %>%
  rename(tamano_patron = categoria)

# 3. AGRUPACIÓN DE TAMAÑO DE PATRÓN (para hoja "Otros")
patron_otros <- datos_original %>%
  filter(variable == "patron") %>%
  
  mutate(tamano_grupo = case_when(
    categoria %in% c("1 PT", "2 a 5 PT") ~ "Micro (1-5)",
    categoria %in% c("6 a 50 PT") ~ "Pequeña (6-50)",
    categoria %in% c("51 a 250 PT") ~ "Mediana (51-250)",
    categoria %in% c("251 a 500 PT", "501 a 1,000 PT", "Más de 1,000 PT") ~ "Grande (251+)",
    TRUE ~ "Sin dato"
  )) %>%
  
  group_by(year, month, date, tamano_grupo) %>%
  summarise(ta = sum(ta, na.rm = TRUE), .groups = "drop") %>%
  
  calcular_indicadores(tamano_grupo) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
    mutate(tamano_grupo = factor(
    tamano_grupo,
    levels = c(
      "Micro (1-5)",
      "Pequeña (6-50)",
      "Mediana (51-250)",
      "Grande (251+)",
      "Sin dato"
    )
  )) %>%
  arrange(tamano_grupo) %>%
  
  select(
    tamano_grupo,
    ta,
    diff_mensual,
    tc_anual
  ) %>%
  
  rename(
    `Tamaño de patron` = tamano_grupo,
    `Total` = ta,
    `Diferencia mensual` = diff_mensual,
    `Cambio % anual` = tc_anual
  )


# 4. ENTIDAD FEDERATIVA
entidad_tc <- datos_original %>%
  
  filter(variable == "cve_entidad") %>%
  
  calcular_indicadores(categoria) %>%
  
  select(
    year,
    month,
    date,
    categoria,
    ta,
    diff_mensual,
    diff_anual,
    tc_mensual,
    tc_anual
  ) %>%
  
  rename(
    entidad_federativa = categoria
  ) %>%
  
  group_by(date) %>%
  
  mutate(
    ranking_tc_anual = min_rank(desc(tc_anual))
  ) %>%
  
  ungroup()

# guardar RDS -------------------------------------------------------------

saveRDS(
  entidad_tc,
  "outputs/imss_entidad_tasas.rds"
)

# crear libro de excel ----------------------------------------------------

# Crear un nuevo libro de trabajo
wb <- createWorkbook()

# Función para agregar hojas con formato
agregar_hoja_con_formato <- function(wb, datos, nombre_hoja) {
  addWorksheet(wb, nombre_hoja)
  writeData(wb, sheet = nombre_hoja, datos)
  
  # Agregar formato condicional para tasas de crecimiento y diferencias
  negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  
  # Aplicar a columnas de diferencias y tasas
  n_filas <- nrow(datos) + 1  # +1 para el header
  
  # Columnas a formatear
  columnas_formato <- c("tc_mensual", "tc_anual")
  
  for(col in columnas_formato) {
    if(col %in% names(datos)) {
      conditionalFormatting(wb, nombre_hoja, 
                            cols = which(names(datos) == col),
                            rows = 1:n_filas, 
                            type = "expression", 
                            rule = "<0", 
                            style = negStyle)
      conditionalFormatting(wb, nombre_hoja, 
                            cols = which(names(datos) == col),
                            rows = 1:n_filas, 
                            type = "expression", 
                            rule = ">0", 
                            style = posStyle)
    }
  }
  
  # Congelar paneles
  freezePane(wb, nombre_hoja, firstRow = TRUE)
  
  # Ajustar ancho de columnas automáticamente
  setColWidths(wb, nombre_hoja, cols = 1:ncol(datos), widths = "auto")
}

# Agregar las hojas
agregar_hoja_con_formato(wb, sector_tc, "Sector_Economico")
agregar_hoja_con_formato(wb, patron_tc, "Tamaño_Patron")
agregar_hoja_con_formato(wb, entidad_tc, "Entidad_Federativa")
agregar_hoja_con_formato(wb, patron_otros, "Otros")


# guardar el archivo excel ------------------------------------------------
archivo_excel <- "excel/tasas_crecimiento_imss.xlsx"
saveWorkbook(wb, archivo_excel, overwrite = TRUE)


beepr::beep()
