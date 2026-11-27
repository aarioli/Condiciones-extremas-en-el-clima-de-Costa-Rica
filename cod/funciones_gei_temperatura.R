library(dplyr)
library(tidyverse)
total_gei_anual <- function(datos){
  datos %>% 
    group_by(anio) %>% 
    summarise(
      emisiones_total = sum(emisiones_totales),
      .groups = "drop"
    )
  
}



emisiones_por_anio <- function(datos){
  datos %>% 
    group_by(anio,Substance) %>% 
    summarise(emisiones_totales= if (any(is.na(emisiones))) {
        NA
      } else {
        sum(emisiones)
      },
      .groups = "drop"
    )
}



tabla_por_anio <-  function(datos){
  datos %>% 
    group_by(anio) %>% 
    summarise(promedio_temp = mean(temperatura_c),
              .groups = "drop")
}


unir_gei_temperatura <- function(gei,temperatura){
  gei %>% 
    inner_join(temperatura,by= "anio")
}


correlacion_gei_temp <- function(datos){
  cor(
    datos$emisiones_total,
    datos$promedio_temp,
    method = "pearson"
  )
}

importar_gei <- function(ruta) {
  readxl::read_excel(ruta, skip = 1)
}

importar_temperatura <- function(ruta) {
  readxl::read_excel(ruta)
}



limpiar_gei <- function(datos) {
  datos %>%
    filter(Country == "Costa Rica") %>%
    select(
      Substance,
      Sector,
      "EDGAR Country Code",
      Country,
      `2001`:`2024`
    ) %>%
    pivot_longer(
      cols = `2001`:`2024`,
      names_to = "anio",
      values_to = "emisiones",
      names_transform = list(anio = as.integer)
    )
}

resumen_gei <- function(datos){
  datos %>% 
    group_by(Substance) %>% 
    summarise(
      promedio = mean(emisiones_totales,na.rm= TRUE),
      mínimo = min(emisiones_totales,na.rm= TRUE),
      máximo = max(emisiones_totales,na.rm= TRUE),
      faltantes = sum(is.na(emisiones_totales))
    )
}


resumen_temp <- function(datos){
  datos %>% 
    summarise(
      promedio = mean(promedio_temp),
      desviacion = sd(promedio_temp),
      mínimo = min(promedio_temp),
      máximo = max(promedio_temp),
    
          )
      }
    

correlacion_kendall <- function(datos) {
  cor(
    datos$emisiones_total,
    datos$promedio_temp,
    method = "kendall"
  )
}

library(readxl)
temperatura.cr <- read_excel("data/temperatura_Costa_Rica_tidy_2001_2024.xlsx")
gei <- read_excel("data/efecto_invernadero.xlsx")

gei_cr <- limpiar_gei(gei)

gei_anual <- emisiones_por_anio(gei_cr)

tabla_temperatura <- tabla_por_anio(temperatura.cr)

datos_gei <- unir_gei_temperatura(
  gei_anual,
  tabla_temperatura
)

gei_total <- total_gei_anual(gei_anual)

datos_total <- unir_gei_temperatura(
  gei_total,
  tabla_temperatura
)


datos_total_validos <- datos_total %>% 
  filter(!is.na(emisiones_total),
         !is.na(promedio_temp))

correlacion_total_Pearson <-  correlacion_gei_temp(datos_total_validos)


kendall_total <- correlacion_kendall(datos_total_validos)


grafico.gei <- ggplot(gei_anual, aes(x = anio, y = emisiones_totales)) +
  geom_line(color = "#287D8E", linewidth = 0.9) +
  geom_point(color = "#287D8E", size = 1.8) +
  facet_wrap(
    ~ Substance,
    scales = "free_y",
    labeller = as_labeller(c(
      "CO2" = "Dióxido de carbono",
      "GWP_100_AR5_CH4" = "Metano",
      "GWP_100_AR5_N2O" = "Óxido nitroso",
      "GWP_100_AR5_F-gases" = "Gases fluorados"
    ))
  ) +
  labs(
    x = "Año",
    y = "Emisiones (Mt CO2 eq/año)"
  ) +
  theme_minimal()

grafico.gei

ggsave(
  "res/grafico_gei.png",
  plot = grafico.gei,
  width = 10,
  height = 6,
  dpi = 300
)

prom_temp <- mean(tabla_temperatura$promedio_temp)

temp_min <- tabla_temperatura %>% 
  filter(promedio_temp == min(promedio_temp))

temp_max <- tabla_temperatura %>% 
  filter(promedio_temp == max(promedio_temp))

grafico.temperatura <- ggplot(tabla_temperatura, aes(x = anio, y = promedio_temp)) +
  geom_line(color = "#C0504D", linewidth = 0.9) +
  geom_point(color = "#C0504D", size = 1.8) +
  geom_hline(
    yintercept = prom_temp,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.8
  ) +
  geom_point(
    data = temp_min,
    color = "#287D8E",
    size = 3.2
  ) +
  geom_point(
    data = temp_max,
    color = "#7A1F1B",
    size = 3.2
  ) +
  annotate(
    "text",
    x = min(tabla_temperatura$anio) + 0.8,
    y = prom_temp + 0.03,
    label = paste0( "     " , round(prom_temp, 3), " °C"),
    color = "gray30",
    size = 3.5,
    hjust = 0
  ) +
  labs(
    x = "Año",
    y = "Temperatura promedio (°C)"
  ) +
  scale_x_continuous(
    breaks = c(2001, 2005, 2010, 2015, 2018, 2023),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  theme_minimal()

grafico.temperatura

ggsave(
  "res/grafico_temperatura.png",
  plot = grafico.temperatura,
  width = 10,
  height = 6,
  dpi = 300
)

grafica.comp <-  ggplot(
  datos_total_validos,
  aes(x = emisiones_total, y = promedio_temp)
) +
  geom_point(aes(color = anio), size = 3) +
  scale_color_viridis_c() +
  labs(
    x = "Emisiones totales (Mt CO2 eq/año)",
    y = "Temperatura promedio (°C)",
    color = "Año"
  ) +
  theme_minimal()

grafica.comp

ggsave(
  "res/comparacion_temperatura_emisiones.png",
  plot = grafica.comp,
  width = 10,
  height = 6,
  dpi = 300
)
