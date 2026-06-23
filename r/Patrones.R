#Patrones------------------------------------------
#Por sector 
#Por tamaño 

# librerias ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  lubridate,
  beepr,
  readxl,
  scales,
  sysfonts,
  showtext
)

# preparacion -------------------------------------------------------------
rm(list = ls())
gc()

sysfonts::font_add_google(name = "Montserrat", family = "Montserrat")
showtext::showtext_auto()

# carpetas ---------------------------------------------------------------
# dir.create("plots_patrones_tam", showWarnings = FALSE)
# dir.create("plots_patrones_sec", showWarnings = FALSE)
# dir.create("plots_mensuales", showWarnings = FALSE)

# carga de la base --------------------------------------------------------
patrones_tam <- read_excel("patrones.xlsx")
patrones_sec <- read_excel("patrones.xlsx", sheet = "sector")

# funciones ---------------------------------------------------------------

# limpieza
limpiar_base <- function(df){
  df %>% 
    mutate(
      mes_num = match(Mes, c(
        "Enero","Febrero","Marzo","Abril","Mayo","Junio",
        "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
      )),
      fecha = make_date(Año, mes_num, 1)
    ) %>% 
    arrange(fecha)
}

# tasas (solo sobre columnas originales)
calcular_tasas <- function(df){
  
  cols_base <- df %>%
    select(where(is.numeric)) %>%
    select(-Año, -mes_num) %>%
    names()
  
  df %>%
    arrange(fecha) %>%
    mutate(
      across(
        all_of(cols_base),
        list(
          t_mensual = ~ (./lag(.) - 1) * 100,
          t_anual   = ~ (./lag(., 12) - 1) * 100
        ),
        .names = "{.col}_{.fn}"
      )
    )
}

# formato largo con identificador
to_long_anual <- function(df, tipo){
  df %>%
    select(fecha, ends_with("t_anual")) %>%
    pivot_longer(
      cols = -fecha,
      names_to = "variable",
      values_to = "tasa_anual"
    ) %>%
    mutate(
      variable = gsub("_t_anual", "", variable),
      tipo = tipo
    )
}

# graficas individuales
plot_individual <- function(df_long, carpeta){
  
  for(var in unique(df_long$variable)){
    
    df_plot <- df_long %>% filter(variable == var)
    ultimo <- df_plot %>% filter(fecha == max(fecha))
    
    titulo <- paste0(var, " (", unique(df_plot$tipo), ")")
    
    p <- ggplot(df_plot, aes(x = fecha, y = tasa_anual)) +
      
      geom_line(linewidth = 1, color = "#1f4e79") +
      geom_hline(yintercept = 0, linewidth = 0.8, color = "grey90") +
      
      geom_text(
        data = ultimo,
        aes(label = paste0(round(tasa_anual, 2), "%")),
        hjust = -0.2,
        size = 4
      ) +
      
      scale_x_date(
        date_breaks = "4 years",
        date_labels = "%Y",
        expand = expansion(mult = c(0.01, 0.08))
      ) +
      
      labs(
        title = titulo,
        subtitle = "Variación porcentual respecto al mismo mes del año anterior",
        x = NULL,
        y = "Tasa anual (%)",
        caption = "Elaboración propia con datos abiertos del IMSS"
      ) +
      
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
      )
    
    ggsave(
      filename = paste0(carpeta, "/", var, "_", unique(df_plot$tipo), ".png"),
      plot = p,
      width = 6,
      height = 3,
      dpi = 300
    )
  }
}

# limpieza ----------------------------------------------------------------
patrones_tam <- limpiar_base(patrones_tam)
patrones_sec <- limpiar_base(patrones_sec)

# tasas -------------------------------------------------------------------
patrones_tam <- calcular_tasas(patrones_tam)
patrones_sec <- calcular_tasas(patrones_sec)

# formato largo -----------------------------------------------------------
patrones_long_tam <- to_long_anual(patrones_tam, "Tamaño")
patrones_long_sec <- to_long_anual(patrones_sec, "Sector")

# graficas individuales ---------------------------------------------------
plot_individual(patrones_long_tam, "plots_patrones_tam")
plot_individual(patrones_long_sec, "plots_patrones_sec")

# panel grid (solo tamaño) -----------------------------------------------
patrones_long_tam <- patrones_long_tam %>%
  mutate(variable = factor(variable, levels = unique(variable)))

ultimos <- patrones_long_tam %>%
  group_by(variable) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

p_facets <- ggplot(patrones_long_tam, aes(x = fecha, y = tasa_anual)) +
  
  geom_line(color = "#1f4e79", linewidth = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "black") +
  
  geom_text(
    data = ultimos,
    aes(label = paste0(round(tasa_anual, 2), "%")),
    hjust = -0.2,
    size = 3
  ) +
  
  scale_x_date(
    date_breaks = "4 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  
  labs(
    title = "Tasa de crecimiento anual por tamaño de patrón",
    subtitle = "Variación porcentual respecto al mismo mes del año anterior",
    x = NULL,
    y = "Tasa anual (%)",
    caption = "Elaboración propia con datos abiertos del IMSS"
  ) +
  
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

ggsave(
  "plots_patrones_tam/tasa_anual_facets.png",
  p_facets,
  width = 9,
  height = 6,
  dpi = 300
)

# TOTAL -------------------------------------------------------------------
df_total <- patrones_long_tam %>%
  filter(variable == "Total")

ultimo_total <- df_total %>%
  filter(fecha == max(fecha))

p_total <- ggplot(df_total, aes(x = fecha, y = tasa_anual)) +
  
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey90") +
  geom_line(color = "#001B71", linewidth = 1.1) +
  
  geom_text(
    family = "Montserrat",
    data = ultimo_total,
    aes(label = paste0(round(tasa_anual, 2), "%")),
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
    title = "Patrones registrados ante el IMSS",
    subtitle = "Tasas de crecimiento anual",
    x = NULL,
    y = NULL,
    caption = "Elaboración propia con datos abiertos del IMSS."
  ) +
  
  theme_bw(base_size = 15) +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold", size = 30),
    plot.subtitle = element_text(size = 20),
    axis.text = element_text(size = 20, color = "black"),
    plot.caption = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave(
  "plots_mensuales/tasa_anual_total.png",
  p_total,
  width = 8,
  height = 4,
  dpi = 300
)

saveRDS(
  patrones_tam,
  "outputs/imss_patrones.rds"
)


beepr::beep()