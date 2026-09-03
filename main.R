cat("=== Iniciando análisis del clima de Costa Rica ===\n")

library(tidyverse)

datos_clima <- tibble(
  region = c("Pacífico Central", "Caribe Norte", "Valle Central", "Zona Norte"),
  temperatura_max = c(34.5, 31.0, 28.2, 33.8),
  precipitacion_mm = c(120.5, 250.0, 45.0, 180.2)
)

cat("\nResumen de datos cargados:\n")
print(datos_clima)

resumen <- datos_clima %>%
  summarise(
    temp_promedio = mean(temperatura_max),
    lluvia_total = sum(precipitacion_mm)
  )

cat("\nResultados procesados:\n")
print(resumen)

cat("\n=== Ejecución finalizada con éxito ===\n")