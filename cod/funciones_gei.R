
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