#Mapa trabajadores asegurados ------------------------------------------------

#Preparación--------------------------------------------------------------------
rm(list = ls())
options(scipen=999)
gc()

sysfonts::font_add_google(name = "Montserrat", family = "Montserrat")
showtext::showtext_auto()

#Librerias ---------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  forcats,
  sf,
  patchwork, 
  showtext, 
  scales,
  treemapify
)

# Shapes INEGI -----------------------------------------------------------------

shape_ent <- st_read("shapes/mg_2024_integrado/conjunto_de_datos/00ent.shp") 


# Manejo de la BD ---------------------------------------------------------

bd <- readRDS("outputs/imss_desagregado.rds")

## Tasa de crecimiento - Trabajadores asegurados -------------------------

imss_entidad <- bd %>% 
  filter(variable == "cve_entidad") %>% 
  mutate(
    categoria = if_else(
      categoria == "Estado de México",
      "México",
      categoria)
  ) %>% 
  arrange(categoria, date) 

imss_entidad <- imss_entidad %>% 
  group_by(year, month, date, categoria) %>% 
  summarise(
    ta = sum(ta, na.rm = TRUE)
  ) %>% 
  ungroup()

imss_entidad <- imss_entidad %>% 
  group_by(categoria) %>% 
  mutate(
    tc = (ta / lag(ta, 12) - 1) * 100
  ) %>% 
  ungroup() 

#Nos quedamos con la ultima observación mensual 
ultima_fecha <- max(imss_entidad$date, na.rm = TRUE)

imss_plot <- imss_entidad %>% 
  filter(date == ultima_fecha)

##Unimos la bd al shape----------------------------------------------------------

shape_plot <- shape_ent %>% 
  left_join(imss_plot, by = c("NOMGEO" = "categoria"))


# Plots -------------------------------------------------------------------
sysfonts::font_add_google(name = "Montserrat", family = "Montserrat")
showtext::showtext_auto()

## Mapa --------------------------------------------------------------------

mapa_tc <- ggplot() +
  geom_sf(
    data = shape_plot,
    aes(fill = tc),
    color = "black",
    linewidth = 0.1
  ) +
  scale_fill_gradient(
    name = "Tasa interanual (%)",
    low = "#0071ce",
    high = "#C8C8C8",
    na.value = "grey80",
    labels = comma_format(accuracy = NULL),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      barwidth = unit(4, "cm"),
      barheight = unit(0.3, "cm"),
      frame.colour = "black",
      frame.linewidth = 0.1,
      ticks.colour = "black"
    )
  ) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(family = "Montserrat", face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(family = "Montserrat", face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(t = 5, r = 0, b = 0, l = 0)
  ) 

mapa_tc

ggsave("plots_mensuales/mapa_ent.png",
       plot = mapa_tc,
       width = 9, height = 6,
       units = "cm")

## bart plots ------------------------------------------------------------

barras_tc <- imss_plot %>% 
  mutate(
    categoria = fct_reorder(categoria, tc),
    signo = if_else(tc >= 0, "Positivo", "Negativo"),
    etiqueta = paste0(round(tc, 1), "%")
  ) %>% 
  ggplot(aes(x = categoria, y = tc, fill = signo)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(
      family = "Montserrat",
      label = etiqueta,
      hjust = if_else(tc >= 0, -0.1, 1.1)
    ),
    size = 6
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Positivo" = "#001B71",
      "Negativo" = "#C00000"
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.1, 0.25))
  ) +
  labs(title = "Tasa de crecimiento anual por entidad", 
       subtitle = format(ultima_fecha, "%B %Y"), caption = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(family = "Montserrat", face = "bold", size = 40, hjust = 0.5),
    plot.subtitle = element_text(family = "Montserrat", size = 30, hjust = 0.5),
    plot.caption = element_text(family = "Montserrat", size = 14, hjust = 0),
    panel.grid = element_blank(),
    axis.text.y = element_text(family = "Montserrat", size = 25, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

barras_tc

ggsave(
  filename = "plots_mensuales/estados.png",
  plot = barras_tc,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "transparent"
)


# Sectores nacional -------------------------------------------------------

rm(list = ls())

# cargar datos
df <- readRDS("outputs/imss_estado_sector.rds")

# obtener ultima fecha disponible
fecha_ref <- max(df$date, na.rm = TRUE)

# agregar a nivel nacional (sumando todos los estados)
df_nacional <- df %>%
  filter(date == fecha_ref) %>%
  group_by(sector) %>%
  summarise(
    ta = sum(ta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_nacional = sum(ta),
    share = ta / total_nacional
  ) %>%
  arrange(desc(share))

# crear treemap nacional (solo porcentajes)
# crear treemap nacional (sin grow = TRUE)
p_nacional <- ggplot(
  df_nacional,
  aes(
    area = share,
    fill = sector,
    label = paste0(
      sector, "\n",
      percent(share, accuracy = 0.1)
    )
  )
) +
  geom_treemap() +
  geom_treemap_text(
    family = "Montserrat",
    colour = "white",
    place = "centre",
    size = 14,  # tamaño fijo para todos
    grow = FALSE  # cambiado a FALSE
  ) +
  scale_fill_viridis_d() +
  labs(
    title = "Composición del empleo formal por sector económico a nivel nacional",
    subtitle = paste0(format(fecha_ref, "%B %Y")),
    caption = "Elaboración propia con datos abiertos del IMSS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Montserrat", face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(family = "Montserrat", size = 14, hjust = 0, color = "gray40"),
    plot.caption = element_text(family = "Montserrat", size = 9, color = "gray50")
  )

# mostrar el gráfico
print(p_nacional)

# guardar ---------------------------------------------------------------
nombre_archivo <- paste0(
  "plots_mensuales/treemap_sector_nacional_",
  format(fecha_ref, "%Y%m"),
  ".png"
)

ggsave(
  filename = nombre_archivo,
  plot = p_nacional,
  width = 5,
  height = 3,
  dpi = 300
)


# sectores estatal  -------------------------------------------------------

rm(list = ls())

# cargar datos
df <- readRDS("outputs/imss_estado_sector.rds")

# obtener ultima fecha disponible
fecha_ref <- max(df$date, na.rm = TRUE)

df_ref <- df %>%
  filter(date == fecha_ref) %>%
  group_by(entidad) %>%
  mutate(
    total_estado = sum(ta, na.rm = TRUE),
    share = ta / total_estado
  ) %>%
  ungroup()

# loop por estado
estados <- sort(unique(df_ref$entidad))

for (edo in estados) {
  
  df_edo <- df_ref %>%
    filter(entidad == edo)
  
  p <- ggplot(
    df_edo,
    aes(
      area = share,
      fill = sector,
      label = paste0(
        sector, "\n",
        percent(share, accuracy = 0.1)
      )
    )
  ) +
    geom_treemap() +
    geom_treemap_text(
      family = "Montserrat",
      colour = "white",
      place = "centre",
      size = 14
    ) +
    labs(
      title = "Composición del empleo por sector económico",
      subtitle = paste(edo, format(fecha_ref, "%B %Y")),
      caption = "Elaboración propia con datos abiertos del IMSS"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(family = "Montserrat", face = "bold", size = 16),
      plot.subtitle = element_text(family = "Montserrat", size = 14),
      plot.caption = element_text(family = "Montserrat", size = 9)
    )
  
  # guardar
  nombre_archivo <- paste0(
    "plots sector/treemap_sector_",
    str_replace_all(tolower(edo), " ", "_"),
    ".png"
  )
  
  ggsave(
    filename = nombre_archivo,
    plot = p,
    width = 5,
    height = 3,
    dpi = 300
  )
}
beep(5)


# TA - TC -----------------------------------------------------------------

rm(list = ls())

bd <- readRDS("outputs/imss_nacional.rds")

library(dplyr)

bd <- bd %>%
  arrange(date) %>%
  mutate(
    ta_tasa_anual = (ta / lag(ta, 12) - 1) * 100
  )
df_ta <- bd %>%
  select(date, ta_tasa_anual) %>%
  filter(!is.na(ta_tasa_anual))

ultimo_ta <- df_ta %>%
  filter(date == max(date))

p_ta <- ggplot(df_ta, aes(x = date, y = ta_tasa_anual)) +
  
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey90") +
  # Línea principal
  geom_line(color = "#001B71", linewidth = 1.1) +
  # Etiqueta último dato
  geom_text(
    family = "Montserrat", 
    data = ultimo_ta,
    aes(label = paste0(round(ta_tasa_anual, 2), "%")),
    hjust = -0.2,
    size = 6
  ) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  scale_y_continuous(
    labels = percent_format(scale = 1)
  ) +
  
  labs(
    title = "Trabajadores asegurados ante el IMSS",
    subtitle = "Tasas de crecimiento anual para cada mes",
    x = NULL,
    y = NULL,
    caption = "Incluye a trabajadores de plataforma."
  ) +
  
  theme_bw(base_size = 15) +
  theme(
    text = element_text(family = "Montserrat"),
    
    plot.title = element_text(face = "bold", size = 30),
    plot.subtitle = element_text(size = 20),
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20, color = "black"),
    plot.caption = element_text(size = 16, color = "black"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

p_ta

ggsave(
  "plots_mensuales/tasa_anual_total_ta.png",
  p_ta,
  width = 8,
  height = 4,
  dpi = 300
)

## Serie historica --------------------------------------------------------

df_tp <- bd %>%
  arrange(date) %>%
  select(date, tp)

ultimo_tp <- df_tp %>%
  filter(date == max(date))

p_tp <- ggplot(df_tp, aes(x = date, y = tp)) +
  
  # Línea principal
  geom_line(color = "#001B71", linewidth = 1.1) +
  
  # Etiqueta último dato
  geom_text(
    family = "Montserrat",
    data = ultimo_tp,
    aes(label = comma(tp)),
    hjust = -0.1,
    size = 5
  ) +
  
  scale_x_date(
    date_breaks = "4 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  scale_y_continuous(
    labels = label_number(scale = 1e-6)
  ) +
  
  labs(
    title = "Serie histórica de trabajadores asegurados ante el IMSS",
    subtitle = "Serie histórica mensual",
    x = NULL,
    y = "Millones",
    caption = "Elaboración propia con datos abiertos del IMSS."
  ) +
  
  theme_bw(base_size = 15) +
  theme(
    text = element_text(family = "Montserrat"),
    
    plot.title = element_text(face = "bold", size = 30),
    plot.subtitle = element_text(size = 20),
    axis.text = element_text(size = 20, color = "black"),
    plot.caption = element_text(size = 16, color = "black"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

p_tp

ggsave(
  "plots_mensuales/tasa_anual_total_tp.png",
  p_tp,
  width = 8,
  height = 4,
  dpi = 300
)


# Barras anual mes --------------------------------------------------------

mes_actual <- max(bd$date)
mes_num <- as.numeric(format(mes_actual, "%m"))

df_barras <- bd %>%
  filter(month == mes_num) %>%
  filter(!is.na(ta_tasa_anual)) %>%
  mutate(year = as.factor(year))

p_barras <- ggplot(df_barras, aes(x = year, y = ta_tasa_anual)) +
  
  geom_col(aes(fill = ta_tasa_anual > 0)) +
  
  # Línea en 0
  geom_hline(yintercept = 0, color = "black", linewidth = 0.1) +
  
  # Etiquetas
  geom_text(
    aes(label = paste0(round(ta_tasa_anual, 1), "%")),
    vjust = ifelse(df_barras$ta_tasa_anual > 0, -0.3, 1.3),
    size = 3.0,
    family = "Montserrat"
  ) +
  
  scale_fill_manual(
    values = c("TRUE" = "#001B71", "FALSE" = "#B22222"),
    guide = "none"
  ) +
  
  scale_x_discrete(
    breaks = levels(df_barras$year)[seq(1, length(levels(df_barras$year)), by = 3)]
  ) +
  
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  labs(
    title = paste0(
      "Tasa de crecimiento anual para ",
      format(mes_actual, "%B"),
      " de cada año"
    ),
    subtitle = "Trabajadores asegurados (%)",
    x = NULL,
    y = NULL,
    caption = "Elaboración propia con datos abiertos del IMSS."
  ) +
  
  theme_bw(base_size = 15) +
  theme(
    text = element_text(family = "Montserrat"),
    
    plot.title = element_text(face = "bold", size = 24),
    plot.subtitle = element_text(size = 16),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(size = 0.5),  # grosor de las rayitas
    plot.caption = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3)
  )

p_barras

ggsave(
  "plots_mensuales/tasa_anual_total_barras.png",
  p_barras,
  width = 1015,
  height = 384,
  units = "px"
)
