# 05_imss_sbc_desagregado.R
# Objetivo: conformar las bases del salario base de cotización (SBC)
# del IMSS desagregado por tamaño de patrón, sector económico y entidad federativa.

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

rm(list = ls()); gc()

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
  
  ## Selección de archivos por fechas ----------------------------------------
  
  # Definir rango de procesamiento
  inicio <- as.Date("2025-12-31")
  fin    <- as.Date("2026-01-31")
  
  archivos <- list.files(
    path = "inputs/", 
    pattern = "asg-\\d{4}-\\d{2}-\\d{2}\\.csv",
    full.names = TRUE
  )
  
  fechas_archivo <- as.Date(str_extract(basename(archivos), "\\d{4}-\\d{2}-\\d{2}"))
  
  validos <- !is.na(fechas_archivo)
  archivos <- archivos[validos]
  fechas_archivo <- fechas_archivo[validos]
  
  archivos <- archivos[fechas_archivo >= inicio & fechas_archivo <= fin]
  
  clusterExport(cl, varlist = c("archivos"))
  
  # Función para cargar y procesar un archivo ------------------------------
  cargar_datos <- function(archivo) {
    datos <- fread(archivo, sep = "|", encoding = "Latin-1")
    
    # Renombrar columna de patrón si aplica
    datos <- datos %>% 
      rename(patron = contains("_patron"))
    
    # Extraer año y mes del nombre del archivo
    nombre_archivo <- basename(archivo)
    year <- as.integer(str_extract(nombre_archivo, "(?<=asg-)\\d{4}"))
    month <- as.integer(str_extract(nombre_archivo, "(?<=asg-\\d{4}-)\\d{2}"))
    
    # Agregar año y mes
    datos <- datos %>%
      mutate(
        year = year,
        month = month,
        date = make_date(year, month)
      )
    
    # Calcular el SBC promedio ponderado por patrón, sector y entidad
    datos <- datos %>%
      mutate(
        sbc = masa_sal_ta / ta_sal,
        sbc = ifelse(sbc == 0, NA, sbc)
      ) %>%
      select(year, month, date, patron, sector_economico_1, cve_entidad, sbc, ta_sal, sexo)
    
    # Asegurar tipo de variables
    datos <- datos %>%
      mutate(
        patron = as.character(patron),
        sector_economico_1 = as.character(sector_economico_1),
        cve_entidad = as.character(cve_entidad)
      )
    
    # Pasar a formato largo (sector, patrón, entidad)
    datos_long <- datos %>%
      pivot_longer(
        cols = c(patron, sector_economico_1, cve_entidad),
        names_to = "variable",
        values_to = "categoria"
      ) %>%
      mutate(
        categoria = case_when(
          # ---- Sector económico ----
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
          
          # ---- Tamaño de patrón ----
          variable == "patron" & categoria == "S1" ~ "1 puesto de trabajo",
          variable == "patron" & categoria == "S2" ~ "2 a 5 puestos de trabajo",
          variable == "patron" & categoria == "S3" ~ "6 a 50 puestos de trabajo",
          variable == "patron" & categoria == "S4" ~ "51 a 250 puestos de trabajo",
          variable == "patron" & categoria == "S5" ~ "251 a 500 puestos de trabajo",
          variable == "patron" & categoria == "S6" ~ "501 a 1,000 puestos de trabajo",
          variable == "patron" & categoria == "S7" ~ "Más de 1,000 puestos de trabajo",
          variable == "patron" & is.na(categoria) ~ "Sin dato (tamaño patrón)",
          
          # ---- Entidad federativa ----
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
      group_by(year, month, date, variable, categoria, sexo) %>%
      summarise(
        sbc_prom = weighted.mean(sbc, w = ta_sal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(date, variable, categoria)
    
    return(datos_long)
  }
  
  # Procesar en paralelo ----------------------------------------------------
  datos <- rbindlist(parLapply(cl, archivos, cargar_datos))
  
  # Guardar el resultado ----------------------------------------------------
  archivo_salida <- glue("outputs/imss_sbc_desagregado_sexo.rds")
  saveRDS(datos, archivo_salida)
  
  # Liberar clúster ---------------------------------------------------------
  stopCluster(cl)
  beepr::beep()
})
