library(tidyverse)
library(ggplot2)

source("cod/analisis.precipitacion.R")
source("cod/analisis_perdida_arboles.R")

comparacion <- datos.precipitacion %>%
  select(anio, categoria.anio, anomalia.pct) %>%
  inner_join(
    CRI %>%
      select(year, driver, tc_loss_ha),
    by = c("anio" = "year")
  )

resumen.perdidas <- comparacion %>%
  group_by(driver, categoria.anio) %>%
  summarise(
    perdida.mediana = median(tc_loss_ha, na.rm = TRUE),
    perdida.promedio = mean(tc_loss_ha, na.rm = TRUE),
    cantidad = n(),
    .groups = "drop"
  )

grafico.incendios <- resumen.perdidas %>%
  filter(driver == "Wildfire") %>%
  ggplot(aes(x = categoria.anio,
             y = perdida.mediana,
             fill = categoria.anio)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(perdida.mediana, 1)),
    vjust = -0.5
  ) +
  scale_fill_manual(
    values = c(
      "Seco" = "#E8D038",
      "Normal" = "gray70",
      "Humedo" = "steelblue"
    )
  ) +
  labs(
    title = "Pérdida arbórea por incendios según precipitación",
    subtitle = "Costa Rica, 2001-2024",
    x = "Clasificación anual",
    y = "Mediana de hectáreas perdidas"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

grafico.incendios

grafico.perturbaciones <- resumen.perdidas %>%
  filter(driver == "Other natural disturbances") %>%
  ggplot(aes(x = categoria.anio,
             y = perdida.mediana,
             fill = categoria.anio)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(perdida.mediana, 1)),
    vjust = -0.5
  ) +
  scale_fill_manual(
    values = c(
      "Seco" = "#E8D038",
      "Normal" = "gray70",
      "Humedo" = "steelblue"
    )
  ) +
  labs(
    title = "Otras perturbaciones naturales según precipitación",
    subtitle = "Costa Rica, 2001-2024",
    x = "Clasificación anual",
    y = "Mediana de hectáreas perdidas"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

grafico.perturbaciones

ggsave(
  "res/comparacion_clasificación_incendios.png",
  plot = grafico.incendios,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  "res/comparacion_clasificación_perturbaciones_naturales.png",
  plot = grafico.perturbaciones,
  width = 10,
  height = 6,
  dpi = 300
)
