# 02_conformar_bases.R
# Objetivo: conformar las bases agregadas de empleo y salario formal IMSS

# empleo total
# eventual
# permanente
# sbc

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

# preparacion -------------------------------------------------------------

rm(list = ls());gc()

# cluster -----------------------------------------------------------------

# Detectar núcleos y crear el clúster
ncores <- detectCores()
cl <- makeCluster(min(4, ncores))

system.time({
  # Cargar librerías en los workers
  clusterEvalQ(cl, {
    library(tidyverse)
    library(data.table)
    library(stringr)
  })
  
  
  
  ## Seleccion de archivos por fechas ----------------------------------------
  
  # Definir rango de procesamiento
  inicio <- as.Date("2023-08-31")
  fin    <- as.Date("2026-01-31")
  
  # Listar todos los archivos en la carpeta
  archivos <- list.files(
    path = "inputs/", 
    pattern = "asg-\\d{4}-\\d{2}-\\d{2}\\.csv",
    full.names = TRUE
  )
  
  # Extraer fecha de cada archivo
  fechas_archivo <- as.Date(str_extract(basename(archivos), "\\d{4}-\\d{2}-\\d{2}"))
  
  # Filtrar solo fechas válidas
  validos <- !is.na(fechas_archivo)
  archivos <- archivos[validos]
  fechas_archivo <- fechas_archivo[validos]
  
  # Filtrar archivos dentro del rango deseado
  archivos <- archivos[fechas_archivo >= inicio & fechas_archivo <= fin]
  
  # Exportar funciones y variables al clúster
  clusterExport(cl, varlist = c("archivos"))
  
  # Función para cargar y procesar un archivo
  cargar_datos <- function(archivo) {
    datos <- fread(archivo, sep = "|", encoding = "Latin-1")
    
    # Renombrar columna de patrón
    # datos <- datos %>% 
    #   rename(patron = contains("_patron"))
    
    # Extraer año y mes del nombre del archivo
    nombre_archivo <- basename(archivo)
    year <- as.integer(str_extract(nombre_archivo, "(?<=asg-)\\d{4}"))
    month <- as.integer(str_extract(nombre_archivo, "(?<=asg-\\d{4}-)\\d{2}"))
    
    # Agregar año y mes
    datos <- datos %>%
      mutate(
        year = year,
        month = month
      )
    
    # Empleo y Salario base de cotizacion
    datos <- datos %>% 
      mutate(
        te = rowSums(across(c(teu, tec)), na.rm = TRUE),
        tp = rowSums(across(c(tpu, tpc)), na.rm = TRUE),
        sbc = masa_sal_ta/ta_sal, 
        sbc = ifelse(sbc == 0, NA, sbc) 
      ) %>% 
      group_by(sexo) %>% 
      summarise(
        year = mean(year),
        month = mean(month),
        ta = sum(ta, na.rm = TRUE),
        te = sum(te, na.rm = TRUE),
        tp = sum(tp, na.rm = TRUE),
        sbc_prom = weighted.mean(sbc, w = ta_sal, na.rm = TRUE)
      ) %>% 
      mutate(
        date = make_date(year, month)
      ) %>% 
      ungroup()
    
    return(datos)
  }
  
  
  # Procesar los archivos en paralelo
  datos <- rbindlist(parLapply(cl, archivos, cargar_datos))
  
  # Guardar el resultado en un archivo RDS
  archivo_salida <- glue("outputs/imss_nacional_sexo.rds")
  saveRDS(datos, archivo_salida)
  
  
  # Liberar el clúster
  stopCluster(cl)
  
  beepr::beep()
})
