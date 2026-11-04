library(tidyverse)
library(readxl)
CRI <- read_excel("CRI.xlsx")

CRI <- CRI %>% 
  filter(
    driver %in% c("Other natural disturbances", "Wildfire"),
    year >= 2001 & year <= 2024
  )