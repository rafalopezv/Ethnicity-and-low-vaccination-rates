library(tidyverse)
library(broom)
library(reactable)

source("codigo/limpieza.R")

# test estadisticos

#---------------
# correlaciones
#---------------

# correlacion vacunacion primera dosis vs etnicidad
cor.test(df$porcentaje_primera_dosis, df$ind)

# correlaacion vacunacion segunda dosis vs etnicidad
cor.test(df$porcentaje_segunda_dosis, df$ind)

# correlacion vacunacion primera dosis vs votación del mas
cor.test(df$porcentaje_primera_dosis, df$mas)

# correlacion vacunacion segund dosis vs votación del mas
cor.test(df$porcentaje_segunda_dosis, df$mas)

# correlacion vacunacion primera dosis vs pobreza
cor.test(df$porcentaje_primera_dosis, df$pobre_porcentaje)

# correlacion vacunacion segund dosis vs pobreza
cor.test(df$porcentaje_segunda_dosis, df$pobre_porcentaje)


#--------------------------------
# regresiones múltiples lineales
#--------------------------------

# regresión principal: vacunacion, etnicidad + mas (primera dosis)  
df %>% 
  lm(formula = porcentaje_primera_dosis ~ ind + mas + pobre_porcentaje) %>% 
  tidy() %>% 
  rename(
    "Término" = term,
    Coeficiente = estimate,
    "Error estándar" = std.error, 
    "Valor 't'" = statistic,
    "Valor 'p'" = p.value
  ) %>% 
  mutate(
    "Término" = case_when(
      `Término` == "(Intercept)" ~ "Intercepto",
      `Término` == "ind" ~ "Etnicidad (%)",
      `Término` == "mas"~ "Voto al MAS-IPSP (%)",
      `Término` == "pobre_porcentaje"~ "Pobreza (%)"
    ),
    "Significancia estadística mayor al 90%" = case_when(
      `Valor 'p'` < 10/100 ~ "Si",
      T ~ "No" 
    )
  ) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  reactable(style = list(fontFamily = "IBM Plex Mono", fontSize = "12px"))-> resultado_es1_d1

# regresión principal: vacunacion, etnicidad + mas (segunda dosis)  
df %>% 
  lm(formula = porcentaje_segunda_dosis ~ ind + mas + pobre_porcentaje) %>% 
  tidy() %>% 
  rename(
    "Término" = term,
    Coeficiente = estimate,
    "Error estándar" = std.error, 
    "Valor 't'" = statistic,
    "Valor 'p'" = p.value
  ) %>% 
  mutate(
    "Término" = case_when(
      `Término` == "(Intercept)" ~ "Intercepto",
      `Término` == "ind" ~ "Etnicidad (%)",
      `Término` == "mas"~ "Voto al MAS-IPSP (%)",
      `Término` == "pobre_porcentaje"~ "Pobreza (%)"
    ),
    "Significancia estadística mayor al 90%" = case_when(
      `Valor 'p'` < 10/100 ~ "Si",
      T ~ "No" 
    ),
  ) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  reactable(style = list(fontFamily = "IBM Plex Mono", fontSize = "12px"))-> resultado_es1_d2

# regresión secundaria: La Paz primera dosis  
df %>% 
  filter(departamento == "La Paz") %>% 
  lm(formula = porcentaje_primera_dosis ~ ind + mas + pobre_porcentaje) %>% 
  tidy() %>% 
  rename(
    "Término" = term,
    Coeficiente = estimate,
    "Error estándar" = std.error, 
    "Valor 't'" = statistic,
    "Valor 'p'" = p.value
  ) %>% 
  mutate(
    "Término" = case_when(
      `Término` == "(Intercept)" ~ "Intercepto",
      `Término` == "ind" ~ "Etnicidad (%)",
      `Término` == "mas"~ "Voto al MAS-IPSP (%)",
      `Término` == "pobre_porcentaje"~ "Pobreza (%)"
    ),
    "Significancia estadística mayor al 90%" = case_when(
      `Valor 'p'` < 10/100 ~ "Si",
      T ~ "No" 
    ),
  ) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  reactable(style = list(fontFamily = "IBM Plex Mono", fontSize = "12px")) -> resultado_es1_d1_lpz

# regresión secundaria: La Paz segunda dosis  
df %>% 
  filter(departamento == "La Paz") %>% 
  lm(formula = porcentaje_segunda_dosis ~ ind + mas + pobre_porcentaje) %>% 
  tidy() %>% 
  rename(
    "Término" = term,
    Coeficiente = estimate,
    "Error estándar" = std.error, 
    "Valor 't'" = statistic,
    "Valor 'p'" = p.value
  ) %>% 
  mutate(
    "Término" = case_when(
      `Término` == "(Intercept)" ~ "Intercepto",
      `Término` == "ind" ~ "Etnicidad (%)",
      `Término` == "mas"~ "Voto al MAS-IPSP (%)",
      `Término` == "pobre_porcentaje"~ "Pobreza (%)"
    ),
    "Significancia estadística mayor al 90%" = case_when(
      `Valor 'p'` < 10/100 ~ "Si",
      T ~ "No" 
    ),
  ) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  reactable(style = list(fontFamily = "IBM Plex Mono", fontSize = "12px")) -> resultado_es1_d2_lpz












