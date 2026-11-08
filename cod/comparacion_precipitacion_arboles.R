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