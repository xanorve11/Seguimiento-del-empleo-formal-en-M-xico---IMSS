# Monitor de Empleo Formal — IMSS

Pipeline automatizado en R Studio para el procesamiento, análisis y reporte mensual del empleo formal en México a partir de los microdatos abiertos del IMSS.

---

## Objetivo

Construir un sistema de actualización incremental que, cada mes, descargue los microdatos del IMSS, genere indicadores de empleo y salarios desagregados por entidad, sector económico y tamaño de patrón para producir visualizaciones listas para publicar y alimente fichas resumen automatizadas en R Markdown.

---

## 🗂️ Estructura del repositorio

```
imss/
├── R/
│   ├── 00_descargar_bases.R         # Descarga automática de microdatos IMSS
│   ├── 01_imss_nacional.R           # Agregado nacional: TA, TP, TE, SBC
│   ├── 02_imss_nacional_sexo.R      # Desagregado por sexo
│   ├── 03_desagregado.R             # Por entidad, sector y tamaño de patrón
│   ├── 04_estado_sector.R           # Cruce entidad × sector económico
│   ├── 04_tasas_de_crecimiento.R    # Tasas mensuales y anuales + Excel
│   ├── 05_sbc_desagregado.R         # Salario base de cotización desagregado
│   ├── 06_ta_sbc_nacional_tasas.R   # Indicadores nacionales + Excel
│   ├── 07_plots_mensuales.R         # Mapa, barras, treemaps y series históricas
│   ├── Patrones.R                   # Análisis de patrones por tamaño y sector
│   ├── helpers.R                    # Funciones de formato y utilidades
│   └── narrativa.R                  # Construcción de variables narrativas dinámicas
├── outputs/                         # Bases procesadas (.rds) — generadas localmente
├── excel/                           # Reportes Excel con formato condicional
├── plots_mensuales/                 # Visualizaciones listas para publicar
├── plots_patrones_tam/              # Gráficas por tamaño de patrón
├── plots_patrones_sec/              # Gráficas por sector económico
├── shapes/                          # Shapefiles INEGI para mapas
└── inputs/                          # Microdatos crudos IMSS (.csv) — no versionados
```

---

## Pipeline de procesamiento

El sistema sigue un flujo incremental: solo procesa los meses que aún no existen en el histórico.

```
00_descargar_bases.R
        │
        ▼
┌───────────────────────────────────────┐
│  01 Nacional  │  02 Por sexo          │
│  03 Desagregado (entidad/sector/tam)  │
│  04 Estado × Sector                   │
│  05 SBC desagregado                   │
└───────────────────────────────────────┘
        │
        ▼
 06 Indicadores + Excel
 04 Tasas de crecimiento + Excel
        │
        ▼
 07 Visualizaciones   →  Patrones.R
        │
        ▼
 narrativa.R  →  ficha_imss.Rmd  →  .docx
```

---

## Indicadores que produce

**Empleo formal (Trabajadores Asegurados)**
- Total, permanente y eventual — nivel nacional y por entidad federativa
- Tasas de crecimiento mensual y anual
- Desagregación por sexo, sector económico y tamaño de patrón

**Salario Base de Cotización (SBC)**
- Promedio ponderado nacional
- Desagregado por entidad, sector y tamaño de empresa
- Brecha salarial por sexo

**Patrones registrados**
- Total y por tamaño de empresa (desde 1 PT hasta más de 1,000)
- Tasas de crecimiento y comparación vs. promedio histórico

---

## Visualizaciones

| Archivo | Descripción |
|---|---|
| `mapa_ent.png` | Mapa coroplético de tasa de crecimiento anual por entidad |
| `estados.png` | Barras horizontales con tasa de crecimiento anual por entidad federativa |
| `treemap_sector_nacional_YYYYMM.png` | Composición del empleo por sector económico |
| `treemap_sector_<estado>.png` | Composición sectorial por entidad (32 mapas) |
| `tasa_anual_total_ta.png` | Serie histórica de TC anual — empleo total |
| `tasa_anual_total_tp.png` | Serie histórica — empleo permanente |
| `tasa_anual_total_barras.png` | tasa de crecimiento anual del mes actual para cada año |
| `tasa_anual_total.png` | TC anual de patrones registrados |


---

## Principales paqueterias

| Paquete | Uso |
|---|---|
| `data.table` | Lectura eficiente de microdatos (sep `\|`, encoding Latin-1) |
| `parallel` | Procesamiento en cluster para múltiples archivos |
| `tidyverse` | Transformación, pivoteo y manipulación de datos |
| `lubridate` | Manejo de fechas y periodos |
| `ggplot2` | Series de tiempo, barras y mapas |
| `sf` | Mapas con shapefiles INEGI |
| `treemapify` | Treemaps de composición sectorial |
| `openxlsx` | Reportes Excel con formato condicional |
| `scales` | Formato de ejes y etiquetas |
| `showtext` / `sysfonts` | Tipografía Montserrat en gráficas |
| `glue` | Construcción de rutas y mensajes |

---

## Cómo reproducir

### 1. Requisitos

```r
install.packages("pacman")
```

Todos los demás paquetes se instalan automáticamente con `pacman::p_load()` al correr cada script.

### 2. Shapefiles

Descarga el Marco Geoestadístico INEGI 2024 y coloca los archivos en `shapes/mg_2024_integrado/`.

### 3. Ejecutar el pipeline

```r
# Paso 1 — Descargar microdatos IMSS
source("R/00_descargar_bases.R")

# Paso 2 — Construir bases procesadas
source("R/01_imss_nacional.R")
source("R/02_imss_nacional_sexo.R")
source("R/03_desagregado.R")
source("R/04_estado_sector.R")
source("R/05_sbc_desagregado.R")

# Paso 3 — Calcular indicadores y exportar Excel
source("R/04_tasas_de_crecimiento.R")
source("R/06_ta_sbc_nacional_tasas.R")

# Paso 4 — Generar visualizaciones
source("R/07_plots_mensuales.R")
source("R/Patrones.R")
```

> El pipeline es incremental: si los `.rds` ya existen, solo agrega el mes más reciente.

### 4. Fuente de datos

Los microdatos se descargan directamente de:
```
http://datos.imss.gob.mx/sites/default/files/asg-YYYY-MM-DD.csv
```
Datos abiertos del IMSS, disponibles desde 2000.

---

## Notas técnicas

- El procesamiento usa `parallel::makeCluster()` con hasta 4 núcleos para acelerar la lectura de microdatos.
- Los archivos `.csv` del IMSS usan separador `|` y encoding Latin-1.
- El SBC se calcula como `masa_sal_ta / ta_sal` (promedio ponderado por trabajadores con salario registrado).

---

## 📄 Fuente

Instituto Mexicano del Seguro Social (IMSS) — Datos Abiertos  
[datos.imss.gob.mx](http://datos.imss.gob.mx)
