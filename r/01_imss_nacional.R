# 02_conformar_bases.R
# Objetivo: actualizar base agregada nacional IMSS (proceso incremental)

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
ruta_inputs   <- "inputs/"
ruta_outputs  <- "outputs/imss_nacional.rds"

# -------------------------------------------------------------------------
# 1. Cargar histórico si existe
# -------------------------------------------------------------------------

if (file.exists(ruta_outputs)) {
  
  imss_historico <- readRDS(ruta_outputs)
  
  fechas_historico <- unique(imss_historico$date)
  
  message("Histórico cargado correctamente.")
  
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

# Extraer fechas de archivos
fechas_archivo <- as.Date(
  str_extract(basename(archivos), "\\d{4}-\\d{2}-\\d{2}")
)

# Normalizar a primer día del mes
fechas_archivo <- floor_date(fechas_archivo, unit = "month")

# Filtrar válidos
validos <- !is.na(fechas_archivo)
archivos <- archivos[validos]
fechas_archivo <- fechas_archivo[validos]

# -------------------------------------------------------------------------
# 3. Detectar meses nuevos
# -------------------------------------------------------------------------

archivos_nuevos <- archivos[!fechas_archivo %in% fechas_historico]

if (length(archivos_nuevos) == 0) {
  message("No hay meses nuevos por procesar.")
  beepr::beep()
  quit(save = "no")
}

message(glue("Se procesarán {length(archivos_nuevos)} archivo(s) nuevo(s)."))

idx_nuevos <- which(!fechas_archivo %in% fechas_historico)

if (length(idx_nuevos) > 0) {
  idx_nuevos <- idx_nuevos[order(fechas_archivo[idx_nuevos])]
  archivos_nuevos <- archivos[idx_nuevos[length(idx_nuevos)]]
  
  message("Modo rápido activado: solo el archivo más reciente.")
}

# Validación normal
if (length(archivos_nuevos) == 0) {
  
  message("No hay meses nuevos por procesar.")
  
  beepr::beep()
  
  quit(save = "no")
}

idx_nuevos <- which(!fechas_archivo %in% fechas_historico)

if (length(idx_nuevos) > 0) {
  idx_nuevos <- idx_nuevos[order(fechas_archivo[idx_nuevos])]
  archivos_nuevos <- archivos[idx_nuevos[length(idx_nuevos)]]
  
  message("Modo rápido activado: solo el archivo más reciente.")
}

# Validación normal
if (length(archivos_nuevos) == 0) {
  
  message("No hay meses nuevos por procesar.")
  
  beepr::beep()
  
  quit(save = "no")
}


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
# 5. Función de procesamiento (misma lógica original)
# -------------------------------------------------------------------------

cargar_datos <- function(archivo) {
  
  datos <- fread(archivo, sep = "|", encoding = "Latin-1")
  
  nombre_archivo <- basename(archivo)
  
  year  <- as.integer(str_extract(nombre_archivo, "(?<=asg-)\\d{4}"))
  month <- as.integer(str_extract(nombre_archivo, "(?<=asg-\\d{4}-)\\d{2}"))
  
  datos <- datos %>%
    mutate(
      year  = year,
      month = month
    )
  
  datos <- datos %>% 
    mutate(
      te  = rowSums(across(c(teu, tec)), na.rm = TRUE),
      tp  = rowSums(across(c(tpu, tpc)), na.rm = TRUE),
      sbc = masa_sal_ta / ta_sal,
      sbc = ifelse(sbc == 0, NA, sbc)
    ) %>% 
    summarise(
      year     = mean(year),
      month    = mean(month),
      ta       = sum(ta, na.rm = TRUE),
      te       = sum(te, na.rm = TRUE),
      tp       = sum(tp, na.rm = TRUE),
      sbc_prom = weighted.mean(sbc, w = ta_sal, na.rm = TRUE)
    ) %>% 
    mutate(
      date = make_date(year, month)
    ) %>% 
    ungroup()
  
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
    arrange(date)
  
} else {
  
  datos_final <- datos_nuevos %>%
    arrange(date)
}

# -------------------------------------------------------------------------
# 8. Guardar actualizado
# -------------------------------------------------------------------------

saveRDS(datos_final, ruta_outputs)

message("Histórico actualizado correctamente.")

beepr::beep()
