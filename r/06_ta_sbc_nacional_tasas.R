#06_tabla con las TC por sexo: 
#Objetivo: formar una tabla con los cambios % para ya tenerla lista cada mes

# librerias ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  lubridate,
  openxlsx
)

# preparacion -------------------------------------------------------------
rm(list = ls())
gc()

# cargar datos nacional ---------------------------------------------------
imss_nacional <- readRDS("outputs/imss_nacional.rds")

# calcular indicadores ----------------------------------------------------
imss_nacional_indicadores <- imss_nacional %>%
  arrange(date) %>%
  mutate(
    # DIFERENCIAS EN NIVELES (números absolutos)
    
    # Diferencia mensual (empleos creados vs mes anterior)
    diff_mensual_ta = ta - lag(ta),
    diff_mensual_te = te - lag(te),
    diff_mensual_tp = tp - lag(tp),
    diff_mensual_sbc = sbc_prom - lag(sbc_prom),
    
    # Diferencia anual (empleos creados vs mismo mes año anterior)
    diff_anual_ta = ta - lag(ta, 12),
    diff_anual_te = te - lag(te, 12),
    diff_anual_tp = tp - lag(tp, 12),
    diff_anual_sbc = sbc_prom - lag(sbc_prom, 12),
    
    # TASAS DE CRECIMIENTO (porcentajes)
    
    # Tasa de crecimiento mensual
    tc_mensual_ta = (ta / lag(ta) - 1) * 100,
    tc_mensual_te = (te / lag(te) - 1) * 100,
    tc_mensual_tp = (tp / lag(tp) - 1) * 100,
    tc_mensual_sbc = (sbc_prom / lag(sbc_prom) - 1) * 100,
    
    # Tasa de crecimiento anual
    tc_anual_ta = (ta / lag(ta, 12) - 1) * 100,
    tc_anual_te = (te / lag(te, 12) - 1) * 100,
    tc_anual_tp = (tp / lag(tp, 12) - 1) * 100,
    tc_anual_sbc = (sbc_prom / lag(sbc_prom, 12) - 1) * 100,
    
    # Formatear números
    across(c(ta, te, tp, sbc_prom), ~round(., 2)),
    across(starts_with("diff_"), ~round(., 2)),
    across(starts_with("tc_"), ~round(., 4))
  )

# guardar RDS -------------------------------------------------------------

saveRDS(
  imss_nacional_indicadores,
  "outputs/imss_nacional_indicadores.rds"
)


# crear libro de excel con formato ----------------------------------------
wb <- createWorkbook()

# Función para agregar formato condicional
agregar_hoja_con_formato <- function(wb, datos, nombre_hoja) {
  addWorksheet(wb, nombre_hoja)
  writeData(wb, sheet = nombre_hoja, datos)
  
  # Formato condicional para tasas y diferencias
  negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  
  n_filas <- nrow(datos) + 1  # +1 para el header
  
  # Aplicar formato a columnas de diferencias y tasas
  columnas_formato <- names(datos)[str_detect(names(datos), "diff_|tc_")]
  
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
  
  # Congelar paneles y ajustar ancho
  freezePane(wb, nombre_hoja, firstRow = TRUE)
  setColWidths(wb, nombre_hoja, cols = 1:ncol(datos), widths = "auto")
}

# Hoja 1: Todos los indicadores completos
agregar_hoja_con_formato(wb, imss_nacional_indicadores, "Todos_Indicadores")

# Hoja 2: Resumen solo de Empleo Total (TA)
ta_resumen <- imss_nacional_indicadores %>%
  select(year, month, date, 
         ta, diff_mensual_ta, tc_mensual_ta, 
         diff_anual_ta, tc_anual_ta)

agregar_hoja_con_formato(wb, ta_resumen, "Resumen_Empleo_Total")

# Hoja 3: Resumen de Salarios (SBC)
sbc_resumen <- imss_nacional_indicadores %>%
  select(year, month, date, 
         sbc_prom, diff_mensual_sbc, tc_mensual_sbc, 
         diff_anual_sbc, tc_anual_sbc)

agregar_hoja_con_formato(wb, sbc_resumen, "Resumen_Salarios")

# Hoja 4: Comparativa de tipos de empleo
empleo_comparativo <- imss_nacional_indicadores %>%
  select(year, month, date, 
         # Empleo Total
         ta, diff_mensual_ta, tc_mensual_ta,
         # Empleo Eventual
         te, diff_mensual_te, tc_mensual_te,
         # Empleo Permanente
         tp, diff_mensual_tp, tc_mensual_tp)

agregar_hoja_con_formato(wb, empleo_comparativo, "Comparativa_Empleos")

# guardar el archivo excel ------------------------------------------------
archivo_salida <- "excel/imss_nacional_indicadores.xlsx"
saveWorkbook(wb, archivo_salida, overwrite = TRUE)

# Mostrar preview de los últimos meses
cat("\nÚLTIMOS 3 MESES - EMPLEO TOTAL:\n")
imss_nacional_indicadores %>%
  tail(3) %>%
  select(date, ta, diff_mensual_ta, tc_mensual_ta, diff_anual_ta, tc_anual_ta) %>%
  mutate(
    across(where(is.numeric), ~round(., 2)),
    tc_mensual_ta = paste0(round(tc_mensual_ta, 3), "%"),
    tc_anual_ta = paste0(round(tc_anual_ta, 3), "%")
  ) %>%
  print()
beepr::beep()
