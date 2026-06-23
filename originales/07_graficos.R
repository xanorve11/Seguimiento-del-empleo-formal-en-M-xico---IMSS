#07_Graficas
#Objetivo: hacer las graficas de las variables de interes
#SBC
#empleo por sector 

# librerias ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  lubridate,
  scales
)

# preparacion -------------------------------------------------------------
rm(list = ls())
gc()

# cargar datos de sectores economicos -------------------------------------
datos_sectores <- readRDS("outputs/imss_desagregado.rds")

# filtrar solo sector economico y calcular tasas --------------------------
sectores_tc <- datos_sectores %>%
  filter(variable == "sector_economico_1") %>%
  # Excluir "Sin dato (sector)" si existe
  filter(!(categoria == "Sin dato (sector)" & ta == 0)) %>%
  # Ordenar por sector y fecha
  arrange(categoria, date) %>%
  group_by(categoria) %>%
  mutate(
    # Calcular tasa de crecimiento mensual
    tc_mensual = (ta / lag(ta) - 1) * 100
  ) %>%
  ungroup() %>%
  # Renombrar para mejor presentación en el gráfico
  rename(sector_economico = categoria)

# crear el gráfico con tendencia LOESS ------------------------------------
grafico_con_tendencia <- sectores_tc %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = tc_mensual, color = "Serie original"), 
            linewidth = 1, na.rm = TRUE) +
  geom_smooth(aes(y = tc_mensual, color = "Tendencia (LOESS)"), 
              method = "loess", se = FALSE, linewidth = 0.8, na.rm = TRUE) +
  # Línea en cero como referencia
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_y_continuous(
    labels = scales::comma_format(suffix = "%")
  ) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = c(0.05, 0)
  ) +
  scale_color_manual(
    name = "",
    values = c("Serie original" = "#002060", 
               "Tendencia (LOESS)" = "#C00000")
  ) +
  facet_wrap(~ sector_economico, scales = "free_y") +
  labs(
    title = "Crecimiento Mensual por Sector Económico",
    subtitle = "Serie original y tendencia",
    x = "Fecha",
    y = "Porcentaje (%)",
    caption = "Fuente: Elaboración porpia con aatos Abiertos IMSS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),    # Izquierda
    plot.subtitle = element_text(size = 11, hjust = 0),                # Izquierda
    axis.text.x = element_text(angle = 0, hjust = 0.5),               # Totalmente horizontales
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

print(grafico_con_tendencia)

# guardar el gráfico en SVG ------------------------------------------------------
ggsave("plots/tasas_crecimiento_sectores_tendencia.svg", 
       plot = grafico_con_tendencia, 
       width = 14, 
       height = 10, 
       dpi = 300)
beepr::beep()

