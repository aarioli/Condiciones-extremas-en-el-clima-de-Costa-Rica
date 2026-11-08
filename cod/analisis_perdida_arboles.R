library(tidyverse)
library(readxl)
CRI <- read_excel("../data/CRI.xlsx")

CRI <- CRI %>% 
  filter(
    driver %in% c("Other natural disturbances", "Wildfire"),
    year >= 2001 & year <= 2024
  )

grafico.lineas <- ggplot(
  CRI,
  aes(x = year, y = tc_loss_ha, color = driver)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2001, 2024, 2)) +
  labs(
    title = "Pérdida de cobertura arbórea en Costa Rica",
    subtitle = "Incendios y otras perturbaciones naturales",
    x = "Año",
    y = "Hectáreas perdidas",
    color = "Causa"
  ) +
  theme_minimal()

grafico.lineas

ggsave(
  "res/perdida_cobertura_naturales.png",
  plot = grafico.lineas,
  width = 10,
  height = 6,
  dpi = 300
)

