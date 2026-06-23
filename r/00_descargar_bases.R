# 01_conformar_bases.R
# Objetivo: descarga y almacenamiento de bases IMSS

# librerias ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  glue,
  lubridate,
  dplyr,
  purrr,
  beepr
)

# preparacion -------------------------------------------------------------
rm(list = ls())

options(timeout = 10000)

# Crear carpeta inputs si no existe
if (!dir.exists("inputs")) dir.create("inputs")

# Rango temporal de descargas ---------------------------------------------

# Definir rango
inicio <- as.Date("2000-01-01")
fin    <- as.Date("2005-01-01")

# Secuencia de meses
meses <- seq.Date(from = inicio, to = fin, by = "month")

# Último día de cada mes
index_asg <- ceiling_date(meses, "month") - days(1)

# Convertir a carácter en formato YYYY-MM-DD
index_asg <- format(index_asg, "%Y-%m-%d")

# Funcion para descargar --------------------------------------------------
url <- "http://datos.imss.gob.mx/sites/default/files/asg-"

descarga_imss <- function(fecha, dest_folder = "inputs/") {
  
  # Construir URL y path local
  archivo <- glue("asg-{fecha}.csv")
  url_completa <- glue("{url}{fecha}.csv")
  destfile <- file.path(dest_folder, archivo)
  
  # Si el archivo ya existe, no volver a descargar
  if (file.exists(destfile)) {
    message(glue("{archivo} ya existe, se omite la descarga."))
    return(invisible(destfile))
  }
  
  # Intentar descargar con manejo de errores
  tryCatch(
    {
      download.file(url_completa, destfile, mode = "wb")
      message(glue("{archivo} descargado correctamente."))
    },
    error = function(e) {
      warning(glue("Error al descargar {archivo}: {e$message}"))
      beep(sound = 1)  # beep al finalizar
    }
  )
  
  return(invisible(destfile))
}

# Descargar todos los archivos --------------------------------------------
map(index_asg, descarga_imss)

message("Descarga completada para ", length(index_asg), " archivos.")
beepr::beep(3)
