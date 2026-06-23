# 04_imss_estado_sector.R
# Objetivo: conformar la base de empleo IMSS
# por entidad federativa y sector económico (mensual)

# librerias ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  parallel,
  tidyverse,
  data.table,
  stringr,
  lubridate,
  beepr,
  glue,
  treemapify, 
  scales
)

# preparacion -------------------------------------------------------------
rm(list = ls()); gc()

# rutas -------------------------------------------------------------------

ruta_inputs  <- "inputs/"
ruta_outputs <- "outputs/imss_estado_sector.rds"

## 1. Cargar histórico si existe-------------------------------------------------

if (file.exists(ruta_outputs)) {
  
  imss_historico <- readRDS(ruta_outputs)
  
  fechas_historico <- unique(imss_historico$date)
  
  message("Histórico estado-sector cargado correctamente.")
  
} else {
  
  imss_historico <- NULL
  fechas_historico <- as.Date(character(0))
  
  message("No existe histórico. Se procesarán todos los archivos.")
}


## 2. Listar archivos disponibles----------------------------------------------

archivos <- list.files(
  path = ruta_inputs,
  pattern = "asg-\\d{4}-\\d{2}-\\d{2}\\.csv",
  full.names = TRUE
)

if (length(archivos) == 0) stop("No hay archivos en carpeta inputs/")

fechas_archivo <- as.Date(
  str_extract(basename(archivos), "\\d{4}-\\d{2}-\\d{2}")
)

# Normalizar a mes
fechas_archivo <- floor_date(fechas_archivo, unit = "month")

validos <- !is.na(fechas_archivo)

archivos <- archivos[validos]
fechas_archivo <- fechas_archivo[validos]


## 3. Detectar meses nuevos-----------------------------------------------------

archivos_nuevos <- archivos[!fechas_archivo %in% fechas_historico]

if (length(archivos_nuevos) == 0) {
  
  message("No hay meses nuevos por procesar.")
  
  beepr::beep()
  
  quit(save = "no")
}

message(glue("Se procesarán {length(archivos_nuevos)} archivo(s) nuevo(s)."))


# 🔥 MODO RÁPIDO (solo el archivo más reciente)
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


## 4. Cluster----------------------------------------------------------------


ncores <- detectCores()
cl <- makeCluster(min(4, ncores))

clusterEvalQ(cl, {
  library(tidyverse)
  library(data.table)
  library(stringr)
  library(lubridate)
})

## 5. Función de procesamiento-----------------------------------------------

cargar_datos_estado_sector <- function(archivo) {
  
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
    select(
      year, month, date,
      cve_entidad,
      sector_economico_1,
      ta
    )
  
  datos <- datos %>%
    mutate(
      cve_entidad = as.character(cve_entidad),
      sector_economico_1 = as.character(sector_economico_1)
    )
  
  datos <- datos %>%
    mutate(
      entidad = case_when(
        cve_entidad == "1"  ~ "Aguascalientes",
        cve_entidad == "2"  ~ "Baja California",
        cve_entidad == "3"  ~ "Baja California Sur",
        cve_entidad == "4"  ~ "Campeche",
        cve_entidad == "5"  ~ "Coahuila de Zaragoza",
        cve_entidad == "6"  ~ "Colima",
        cve_entidad == "7"  ~ "Chiapas",
        cve_entidad == "8"  ~ "Chihuahua",
        cve_entidad == "9"  ~ "Ciudad de México",
        cve_entidad == "10" ~ "Durango",
        cve_entidad == "11" ~ "Guanajuato",
        cve_entidad == "12" ~ "Guerrero",
        cve_entidad == "13" ~ "Hidalgo",
        cve_entidad == "14" ~ "Jalisco",
        cve_entidad == "15" ~ "Estado de México",
        cve_entidad == "16" ~ "Michoacán de Ocampo",
        cve_entidad == "17" ~ "Morelos",
        cve_entidad == "18" ~ "Nayarit",
        cve_entidad == "19" ~ "Nuevo León",
        cve_entidad == "20" ~ "Oaxaca",
        cve_entidad == "21" ~ "Puebla",
        cve_entidad == "22" ~ "Querétaro",
        cve_entidad == "23" ~ "Quintana Roo",
        cve_entidad == "24" ~ "San Luis Potosí",
        cve_entidad == "25" ~ "Sinaloa",
        cve_entidad == "26" ~ "Sonora",
        cve_entidad == "27" ~ "Tabasco",
        cve_entidad == "28" ~ "Tamaulipas",
        cve_entidad == "29" ~ "Tlaxcala",
        cve_entidad == "30" ~ "Veracruz de Ignacio de la Llave",
        cve_entidad == "31" ~ "Yucatán",
        cve_entidad == "32" ~ "Zacatecas",
        TRUE ~ "Sin dato (entidad)"
      ),
      sector = case_when(
        sector_economico_1 == "0" ~ "Agricultura, ganadería, silvicultura, pesca y caza",
        sector_economico_1 == "1" ~ "Industrias extractivas",
        sector_economico_1 == "3" ~ "Industrias de la transformación",
        sector_economico_1 == "4" ~ "Industria de la construcción",
        sector_economico_1 == "5" ~ "Industria eléctrica, captación y suministro de agua potable",
        sector_economico_1 == "6" ~ "Comercio",
        sector_economico_1 == "7" ~ "Transportes y comunicaciones",
        sector_economico_1 == "8" ~ "Servicios para empresas, personas y el hogar",
        sector_economico_1 == "9" ~ "Servicios sociales y comunales",
        TRUE ~ "Sin dato (sector)"
      )
    )
  
  datos <- datos %>%
    group_by(year, month, date, entidad, sector) %>%
    summarise(
      ta = sum(ta, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date, entidad, sector)
  
  return(datos)
}

clusterExport(cl, varlist = c("cargar_datos_estado_sector"))


## 6. Procesar solo meses nuevos--------------------------------------------

datos_nuevos <- rbindlist(
  parLapply(cl, archivos_nuevos, cargar_datos_estado_sector)
)

stopCluster(cl)


## 7. Unir con histórico---------------------------------------------------

if (!is.null(imss_historico)) {
  
  datos_final <- bind_rows(imss_historico, datos_nuevos) %>%
    distinct(year, month, date, entidad, sector, .keep_all = TRUE) %>%
    arrange(date, entidad, sector)
  
} else {
  
  datos_final <- datos_nuevos %>%
    arrange(date, entidad, sector)
}

# 8. Guardar actualizado ------------------------------------------------------

saveRDS(datos_final, ruta_outputs)

message("Histórico estado-sector actualizado correctamente.")

beepr::beep()


