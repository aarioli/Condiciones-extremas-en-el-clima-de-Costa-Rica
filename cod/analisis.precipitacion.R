library(readxl)
library(tidyverse)

datos.precipitacion <- read_excel("data/Precipitación.xlsx") %>% 
  rename(
    anio = date,
    prec.total = `prec_total(mm)`,
    prec.esperada = prec_esperada,
    dif.esperado.ocurrido = dif_esperado_ocurrido,
    anomalia.pct = anomalia_porcentual,
    prec.anual.max = prec_anual_max,
    prec.anual.min = prec_anual_min
  )

q1.anomalia <- quantile(datos.precipitacion$anomalia.pct, probs = 0.25)
q3.anomalia <- quantile(datos.precipitacion$anomalia.pct, probs = 0.75)

datos.precipitacion <- datos.precipitacion %>% 
  mutate(
    categoria.anio = case_when(
      anomalia.pct < q1.anomalia ~ "Seco",
      anomalia.pct > q3.anomalia ~ "Humedo",
      .default = "Normal"
    ),
    categoria.anio = factor(
      categoria.anio, levels = c("Seco", "Normal", "Humedo"))
  )

resumen.categorias <- datos.precipitacion %>% 
  group_by(categoria.anio) %>% 
  summarise(
    cant.anios = n(),
    prom.prec.total = mean(prec.total),
    prom.anomalia.pct = mean(anomalia.pct),
    prom.max = mean(prec.anual.max),
    prom.min = mean(prec.anual.min),
    pico.max = max(prec.anual.max),
    piso.min = min(prec.anual.min)
  )


grafico.anomalias <- datos.precipitacion %>% 
  ggplot(aes(x = anio, y = anomalia.pct, fill = categoria.anio)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(2001, 2024, by = 2)) +
  scale_fill_manual(
    values = c("Seco" = "#E06D53", "Normal" = "#BDC3C7", "Humedo" = "#2980B9"),
    labels = c("Seco" = "Seco", "Normal" = "Normal", "Humedo" = "Húmedo")
  ) +
  labs(
    title = "Anomalía porcentual de la precipitación anual (2001–2024)",
    x = "Año",
    y = "Anomalía (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

grafico.anomalias