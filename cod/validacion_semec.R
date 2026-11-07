# Validación externa: incendios oficiales del SINAC vs. precipitación y vs. Global Forest Watch
# Fuente SINAC: Informe anual Estadísticas SEMEC 2021, Figura 26 (área afectada por incendios dentro de ASP, ha).
# Requiere: datos.precipitacion ya homogenizada (cod/analisis_precipitacion.R) y data/CRI.xlsx.
library(tidyverse)
library(readxl)

semec <- tibble(
  anio     = 2011:2021,
  area.asp = c(1992, 3373, 5319, 2182, 4368, 6260, 1171, 1552, 2518, 1395, 855)
)

fuego.gfw <- read_excel("data/CRI.xlsx") %>%
  filter(driver == "Wildfire", year >= 2001, year <= 2024) %>%
  select(anio = year, fuego.gfw = tc_loss_ha)

validacion <- datos.precipitacion %>%
  select(anio, anomalia.pct) %>%
  inner_join(semec, by = "anio") %>%
  inner_join(fuego.gfw, by = "anio")

# Resultados esperados (n = 11): tau = -0.42 (p = 0.09) y tau = -0.02 (p = 1.00)
cor.test(validacion$anomalia.pct, validacion$area.asp,  method = "kendall")  # lluvia vs. SINAC
cor.test(validacion$area.asp,     validacion$fuego.gfw, method = "kendall")  # SINAC vs. GFW

ggplot(validacion, aes(x = anomalia.pct, y = area.asp, label = anio)) +
  geom_point(size = 3, color = "#C0504D") +
  geom_text(vjust = -0.8, size = 3.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Anomalía de precipitación anual (%)",
    y = "Área afectada por incendios en ASP (ha)"
  ) +
  theme_minimal()
