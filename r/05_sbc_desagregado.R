# 05_imss_sbc_desagregado.R
# Objetivo: conformar las bases del salario base de cotización (SBC)
# del IMSS desagregado por tamaño de patrón, sector económico y entidad federativa.
# Proceso incremental: si existe histórico, solo agrega meses nuevos

# librerias ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  parallel,
  tidyverse,
  data.table,
  purrr,
  stringr,
  lubridate,
  beepr,
  glue
)

rm(list = ls()); gc()

# rutas -------------------------------------------------------------------

ruta_inputs  <- "inputs/"
ruta_outputs <- "outputs/imss_sbc_desagregado.rds"

# -------------------------------------------------------------------------
# 1. Cargar histórico si existe
# -------------------------------------------------------------------------

if (file.exists(ruta_outputs)) {
  
  imss_historico <- readRDS(ruta_outputs)
  
  fechas_historico <- unique(imss_historico$date)
  
  message("Histórico SBC cargado correctamente.")
  
} else {
  
  imss_historico <- NULL
  fechas_historico <- as.Date(character(0))
  
  message("No existe histórico. Se procesarán todos los archivos.")
}

# -------------------------------------------------------------------------
# 2. Listar archivos disponibles
# -------------------------------------------------------------------------

archivos <- list.files(
  path = ruta_inputs,
  pattern = "asg-\\d{4}-\\d{2}-\\d{2}\\.csv",
  full.names = TRUE
)

if (length(archivos) == 0) stop("No hay archivos en carpeta inputs/")

fechas_archivo <- as.Date(
  str_extract(basename(archivos), "\\d{4}-\\d{2}-\\d{2}")
)

# normalizar a primer día del mes
fechas_archivo <- floor_date(fechas_archivo, unit = "month")

validos <- !is.na(fechas_archivo)

archivos <- archivos[validos]
fechas_archivo <- fechas_archivo[validos]

# -------------------------------------------------------------------------
# 3. Detectar meses nuevos
# -------------------------------------------------------------------------

# 3. Detectar mes más reciente (modo rápido)
# -------------------------------------------------------------------------

idx_nuevos <- which(!fechas_archivo %in% fechas_historico)

if (length(idx_nuevos) == 0) {
  
  message("No hay meses nuevos por procesar.")
  
  beepr::beep()
  
  quit(save = "no")
}

# Ordenar por fecha
idx_nuevos <- idx_nuevos[order(fechas_archivo[idx_nuevos])]

# 🚀 Solo el más reciente
archivos_nuevos <- archivos[idx_nuevos[length(idx_nuevos)]]

message("Modo rápido activado: solo el archivo más reciente.")

# -------------------------------------------------------------------------
# 4. Cluster
# -------------------------------------------------------------------------

ncores <- detectCores()

cl <- makeCluster(min(4, ncores))

clusterEvalQ(cl, {
  library(tidyverse)
  library(data.table)
  library(stringr)
  library(lubridate)
})

# -------------------------------------------------------------------------
# 5. Función de procesamiento
# -------------------------------------------------------------------------

cargar_datos <- function(archivo) {
  
  datos <- fread(archivo, sep = "|", encoding = "Latin-1")
  
  datos <- datos %>% 
    rename(patron = contains("_patron"))
  
  nombre_archivo <- basename(archivo)
  
  year  <- as.integer(str_extract(nombre_archivo, "(?<=asg-)\\d{4}"))
  month <- as.integer(str_extract(nombre_archivo, "(?<=asg-\\d{4}-)\\d{2}"))
  
  datos <- datos %>%
    mutate(
      year  = year,
      month = month,
      date  = make_date(year, month)
    )
  
  datos <- datos %>%
    mutate(
      sbc = masa_sal_ta / ta_sal,
      sbc = ifelse(sbc == 0, NA, sbc)
    ) %>%
    select(year, month, date, patron, sector_economico_1, cve_entidad, sbc, ta_sal)
  
  datos <- datos %>%
    mutate(
      patron = as.character(patron),
      sector_economico_1 = as.character(sector_economico_1),
      cve_entidad = as.character(cve_entidad)
    )
  
  datos <- datos %>%
    pivot_longer(
      cols = c(patron, sector_economico_1, cve_entidad),
      names_to = "variable",
      values_to = "categoria"
    ) %>%
    mutate(
      categoria = case_when(
        
        variable == "sector_economico_1" & categoria == "0" ~ "Agricultura, ganadería, silvicultura, pesca y caza",
        variable == "sector_economico_1" & categoria == "1" ~ "Industrias extractivas",
        variable == "sector_economico_1" & categoria == "3" ~ "Industrias de la transformación",
        variable == "sector_economico_1" & categoria == "4" ~ "Industria de la construcción",
        variable == "sector_economico_1" & categoria == "5" ~ "Industria eléctrica, captación y suministro de agua potable",
        variable == "sector_economico_1" & categoria == "6" ~ "Comercio",
        variable == "sector_economico_1" & categoria == "7" ~ "Transportes y comunicaciones",
        variable == "sector_economico_1" & categoria == "8" ~ "Servicios para empresas, personas y el hogar",
        variable == "sector_economico_1" & categoria == "9" ~ "Servicios sociales y comunales",
        variable == "sector_economico_1" & is.na(categoria) ~ "Sin dato (sector)",
        
        variable == "patron" & categoria == "S1" ~ "1 PT",
        variable == "patron" & categoria == "S2" ~ "2 a 5 PT",
        variable == "patron" & categoria == "S3" ~ "6 a 50 PT",
        variable == "patron" & categoria == "S4" ~ "51 a 250 PT",
        variable == "patron" & categoria == "S5" ~ "251 a 500 PT",
        variable == "patron" & categoria == "S6" ~ "501 a 1,000 PT",
        variable == "patron" & categoria == "S7" ~ "Más de 1,000 PT",
        variable == "patron" & is.na(categoria) ~ "Sin dato (tamaño patrón)",
        
        variable == "cve_entidad" & categoria == "1"  ~ "Aguascalientes",
        variable == "cve_entidad" & categoria == "2"  ~ "Baja California",
        variable == "cve_entidad" & categoria == "3"  ~ "Baja California Sur",
        variable == "cve_entidad" & categoria == "4"  ~ "Campeche",
        variable == "cve_entidad" & categoria == "5"  ~ "Coahuila de Zaragoza",
        variable == "cve_entidad" & categoria == "6"  ~ "Colima",
        variable == "cve_entidad" & categoria == "7"  ~ "Chiapas",
        variable == "cve_entidad" & categoria == "8"  ~ "Chihuahua",
        variable == "cve_entidad" & categoria == "9"  ~ "Ciudad de México",
        variable == "cve_entidad" & categoria == "10" ~ "Durango",
        variable == "cve_entidad" & categoria == "11" ~ "Guanajuato",
        variable == "cve_entidad" & categoria == "12" ~ "Guerrero",
        variable == "cve_entidad" & categoria == "13" ~ "Hidalgo",
        variable == "cve_entidad" & categoria == "14" ~ "Jalisco",
        variable == "cve_entidad" & categoria == "15" ~ "Estado de México",
        variable == "cve_entidad" & categoria == "16" ~ "Michoacán de Ocampo",
        variable == "cve_entidad" & categoria == "17" ~ "Morelos",
        variable == "cve_entidad" & categoria == "18" ~ "Nayarit",
        variable == "cve_entidad" & categoria == "19" ~ "Nuevo León",
        variable == "cve_entidad" & categoria == "20" ~ "Oaxaca",
        variable == "cve_entidad" & categoria == "21" ~ "Puebla",
        variable == "cve_entidad" & categoria == "22" ~ "Querétaro",
        variable == "cve_entidad" & categoria == "23" ~ "Quintana Roo",
        variable == "cve_entidad" & categoria == "24" ~ "San Luis Potosí",
        variable == "cve_entidad" & categoria == "25" ~ "Sinaloa",
        variable == "cve_entidad" & categoria == "26" ~ "Sonora",
        variable == "cve_entidad" & categoria == "27" ~ "Tabasco",
        variable == "cve_entidad" & categoria == "28" ~ "Tamaulipas",
        variable == "cve_entidad" & categoria == "29" ~ "Tlaxcala",
        variable == "cve_entidad" & categoria == "30" ~ "Veracruz de Ignacio de la Llave",
        variable == "cve_entidad" & categoria == "31" ~ "Yucatán",
        variable == "cve_entidad" & categoria == "32" ~ "Zacatecas",
        variable == "cve_entidad" & is.na(categoria) ~ "Sin dato (entidad)",
        
        TRUE ~ categoria
      )
    ) %>%
    group_by(year, month, date, variable, categoria) %>%
    summarise(
      sbc_prom = weighted.mean(sbc, w = ta_sal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date, variable, categoria)
  
  return(datos)
}

clusterExport(cl, varlist = c("cargar_datos"))

# -------------------------------------------------------------------------
# 6. Procesar solo meses nuevos
# -------------------------------------------------------------------------

datos_nuevos <- rbindlist(parLapply(cl, archivos_nuevos, cargar_datos))

stopCluster(cl)

# -------------------------------------------------------------------------
# 7. Unir con histórico
# -------------------------------------------------------------------------

if (!is.null(imss_historico)) {
  
  datos_final <- bind_rows(imss_historico, datos_nuevos) %>%
    distinct(year, month, date, variable, categoria, .keep_all = TRUE) %>%
    arrange(date, variable, categoria)
  
} else {
  
  datos_final <- datos_nuevos %>%
    arrange(date, variable, categoria)
}

# -------------------------------------------------------------------------
# 8. Guardar actualizado
# -------------------------------------------------------------------------

saveRDS(datos_final, ruta_outputs)

message("Histórico SBC actualizado correctamente.")

beepr::beep()