library(tidyverse)

# datos vacunación Bolivia por municipio
url <- "https://raw.githubusercontent.com/dquintani/vacunacion-municipios-bo/master/datos/vacunacion_municipios_bo.csv"
df <- read_csv(url) %>% 
  select(-1)

# arreglo base vacunación
df %>% 
  mutate(cod_ine = paste0("0", cod_ine)) %>% 
  filter(dosis == 1) %>% 
  rename(porcentaje_primera_dosis = porcentaje) %>%
  select(-dosis, -municipio, -departamento) %>% 
  group_split(cod_ine) %>% 
  map(., ~arrange(., desc(fecha)) %>% 
        slice(1) %>% 
        rename(fecha_primera_dosis = fecha)) %>% 
  bind_rows() -> temp

df %>% 
  mutate(cod_ine = paste0("0", cod_ine)) %>% 
  filter(dosis == 2) %>% 
  rename(porcentaje_segunda_dosis = porcentaje) %>%
  select(-dosis, -municipio, -departamento) %>% 
  group_split(cod_ine) %>% 
  map(., ~arrange(., desc(fecha)) %>% 
        slice(1) %>% 
        rename(fecha_segunda_dosis = fecha)) %>% 
  bind_rows() %>% 
  left_join(temp, .) -> df

# importar base ine
ine <- rio::import("datos/codigos.ine.xlsx")

# arreglo de codigo ine
ine %>% 
  select(cod_ine = CODIGO, DEPARTAMENTO, MUNICIPIO) %>% 
  left_join(df, .) -> df

# importar base etnicidad
ind <-read_csv("datos/etnicidad_2001_2012.csv")

# arreglo base etnicidad
ind %>% 
  filter(año == 2012) %>% 
  group_split(cod_ine = codigo) %>% 
  map(., ~arrange(., desc(porcentaje_indigena_poblacion_total)) %>% 
        slice(1) %>% 
        select(cod_ine, nacion_pueblo, porcentaje_pueblo = porcentaje_indigena_poblacion_total)) %>% 
  bind_rows() %>% 
  mutate(nacion_pueblo = str_to_title(nacion_pueblo)) -> temp

ind %>% 
  filter(año == 2012) %>%
  group_by(cod_ine = codigo) %>% 
  summarise(
    ind = sum(porcentaje_indigena_poblacion_total),
    n_pueblos = n()
  ) %>% 
  left_join(., temp) %>% 
  left_join(., df) %>% 
  mutate(
    MUNICIPIO = str_to_title(MUNICIPIO),
    DEPARTAMENTO = str_to_title(DEPARTAMENTO)
  ) %>% 
  rename(
    municipio = MUNICIPIO,
    departamento = DEPARTAMENTO
  ) -> df

# importar base resultados elecciones 2020
elec <- read_csv("datos/2020.nacionales.presidente.csv")

# armado base de trabajo
elec %>% 
  group_by(cod_ine = CODIGO) %>% 
  summarise(
    mas_ipsp = sum(mas_ipsp),
    validos = sum(validos)
  ) %>% 
  mutate(mas = mas_ipsp/validos * 100) %>% 
  select(cod_ine, mas) %>% 
  left_join(df, .) -> df

# importar bases sobre pobreza
pobreza <- read_csv("datos/pobreza_estrato_2001_2012.csv")

pobreza %>% 
  filter(año == 2012) %>% 
  select(cod_ine = CODIGO, pobre_porcentaje) %>% 
  left_join(df, .) -> df

# importar datos vacunación Bolivia a nivel país
url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
vacunas <- read_csv(url)

vacunas %>% 
  filter(location == "Bolivia") %>% 
  mutate(
    corte = case_when(
      date > "2021-12-03" ~ "Porcentaje de vacunación [%]\n cubierto por el estudio",
      T ~ "Porcentaje de vacunación [%]\n actualizado"
    ),
    vacunas = people_vaccinated_per_hundred/100
  ) -> vacunas

# quitar objetos innecesarios
rm(elec, ind, ine, temp, url)
