library(ggbump)
library(highcharter)
library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(sf)
library(jsonlite)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


source("codigo/limpieza.R")

# cambio de locale aa español
Sys.setlocale(locale = "es_ES.UTF-8")

# grafico tiempo covid Bolivia vs estudio
vacunas %>% 
  ggplot(aes(date, vacunas, color = corte)) +
  geom_bump(smooth = 2, size = 2) +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "XY") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(
    date_labels = "%B\n%Y", 
    date_breaks = "2 months"
  ) +
  labs(
    title = "En el tiempo largo: los datos que faltan no son significativos",
    subtitle = "El 91.9% del tiempo de vacunación está cubierto por el análisis",
    y = "Porcentaje [%] de personas con al menos una dosis (Bolivia)",
    x = "", 
    caption = "Realizado el 2 de enero de 2022\nrafalopezv"
  ) +
  scale_color_manual(values = c("#60A160", "#402440")) +
  geom_vline(xintercept = as.Date("2021-12-04"), linetype = "dotted", size = .7) +
  annotate(
    "text", 
    label = "4 de diciembre:\nútima fecha de\ndisponibilidad de datos\na nivel municipal", 
    x = as.Date("2021-10-20"), 
    y = 0.2, 
    family = "IBM Plex Sans",
    size = 3.5
  ) +
  geom_curve(
    aes(x = as.Date("2021-10-20"), y = .28, xend = as.Date("2021-12-04"), yend = .425),
    arrow = arrow(length = unit(0.03, "npc")), colour = "black",
    angle = 45, curvature = .1
  ) +
  annotate(
    "text", 
    label = "30 de diciembre:\nútima actualización\nOur World in Data", 
    x = as.Date("2021-12-19"), 
    y = 0.33, 
    family = "IBM Plex Sans",
    size = 3.5
  ) +
  geom_curve(
    aes(x = as.Date("2021-12-30"), y = .46, xend = as.Date("2021-12-30"), yend = .4),
    arrow = arrow(length = unit(0.03, "npc")), colour = "black",
    angle = 45, curvature = -.1
  )
ggsave("graficos/tiempo_estudio.jpg", width = 10, height = 6)

# grafico distncia porcentual tiempo VS estudio
vacunas %>% 
  group_split(corte) %>% 
  map(., ~arrange(., desc(people_vaccinated_per_hundred)) %>% 
        slice(1)) %>% 
  bind_rows() %>% 
  mutate(date = format(date, "%d de %B\n%Y")) %>% 
  ggplot(aes(date, vacunas, fill = corte)) +
  geom_col() +
  coord_flip() +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "XY") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "bottom", 
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = c("#60A160", "#402440")) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(
    aes(label = paste0(vacunas*100, "%"), hjust = 1),
    family = "IBM Plex Sans", size = 4, fontface = "bold", color = "white"
  ) +
  geom_hline(yintercept = 0.4332) +
  labs(
    x = "",
    y = "Porcentaje [%] de personas con al menos una dosis",
    title = "El estudio no se invalida por las nuevas vacunaciones",
    subtitle = "No se contempla a 4.12% de personas vacunadas"
  ) +
  guides(fill = guide_legend(reverse = T)) 
ggsave("graficos/tiempo_estudio1.jpg", width = 10, height = 7)
  
# municipios 1ra dosis
df %>%
  arrange(porcentaje_primera_dosis) %>% 
  mutate(
    num = 1:nrow(.),
    mide = case_when(
      porcentaje_primera_dosis < 25 ~ "[-] Menos del 25% de la población\ndel municipio vacunada",
      porcentaje_primera_dosis >= 25 & porcentaje_primera_dosis < 50 ~ "[-] Menos del 50% de la población del\nmunicipio vacunada",
      porcentaje_primera_dosis >= 50 & porcentaje_primera_dosis < 75 ~ "[+] 50% o más de la población del\nmunicipio vacunada",
      T ~ "[+] 75% o más de la población\n del municipio vacunada"
    ),
    porcentaje_primera_dosis = porcentaje_primera_dosis/100
  ) %>%
  ggplot(aes(fct_reorder(cod_ine, num), porcentaje_primera_dosis, fill = mide)) +
  geom_col() +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "Y") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = c("#60A160", "#457345", "#946194", "#402440")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "",
    y = "Porcentaje [%] de personas con al menos una dosis",
    title = "53% de los municipios tienen tasas de vacunación menores a la nacional",
    subtitle = "Primera dosis",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv"
  ) +
  annotate(
    "text", 
    label = "30 municipios\n[8.8%]", 
    x = 20, 
    y = 0.33, 
    family = "IBM Plex Sans",
    size = 3.2,
    fontface = "bold",
    color = "#60A160"
  ) +
  annotate(
    "text", 
    label = "163 municipios\n[48%]", 
    x = 100, 
    y = 0.5, 
    family = "IBM Plex Sans",
    size = 3.2,
    fontface = "bold",
    color = "#457345"
  ) +
  annotate(
    "text", 
    label = "94 municipios\n[27.7%]", 
    x = 230, 
    y = 0.7, 
    family = "IBM Plex Sans",
    size = 3.2,
    fontface = "bold",
    color = "#946194"
  ) +
  annotate(
    "text", 
    label = "52 municipios\n[15.3%]", 
    x = 310, 
    y = 0.95, 
    family = "IBM Plex Sans",
    size = 3.2,
    fontface = "bold",
    color = "#402440"
  ) +
  geom_curve(
    aes(x = 170, y = .75, xend = 180, yend = .5),
    arrow = arrow(length = unit(0.02, "npc")), colour = "black",
    angle = 45, curvature = -.1
) +
  annotate(
    "text", 
    label = "Cada barra representa\na 1 de los 339 municipios", 
    x = 170, 
    y = 0.82, 
    family = "IBM Plex Sans",
    size = 3.2
  ) 
  
ggsave("graficos/municipios_barras_d1.jpg", width = 11, height = 8)
  

# municipios 2da dosis
df %>%
  arrange(porcentaje_segunda_dosis) %>% 
  mutate(
    num = 1:nrow(.),
    mide = case_when(
      porcentaje_segunda_dosis < 25 ~ "[-] Menos del 25% de la población\ndel municipio vacunada",
      porcentaje_segunda_dosis >= 25 & porcentaje_segunda_dosis < 50 ~ "[-] Menos del 50% de la población del\nmunicipio vacunada",
      porcentaje_segunda_dosis >= 50 & porcentaje_segunda_dosis < 75 ~ "[+] 50% o más de la población del\nmunicipio vacunada",
      T ~ "[+] 75% o más de la población\n del municipio vacunada"
    ),
    porcentaje_segunda_dosis = porcentaje_segunda_dosis/100
  ) %>%
  ggplot(aes(fct_reorder(cod_ine, num), porcentaje_segunda_dosis, fill = mide)) +
  geom_col() +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "Y") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = c("#60A160", "#457345", "#946194", "#402440")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "",
    y = "Porcentaje [%] de personas con al menos una dosis",
    title = "63% de los municipios tienen tasas de vacunación menores a la nacional",
    subtitle = "Segunda dosis",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv"
  ) +
  annotate(
    "text", 
    label = "45 municipios\n[13.3%]", 
    x = 20, 
    y = 0.33, 
    family = "IBM Plex Sans",
    size = 3.2,
    fontface = "bold",
    color = "#60A160"
  ) +
  annotate(
    "text", 
    label = "180 municipios\n[53.3%]", 
    x = 120, 
    y = 0.52, 
    family = "IBM Plex Sans",
    size = 3.2,
    fontface = "bold",
    color = "#457345"
  ) +
  annotate(
    "text", 
    label = "81 municipios\n[23.9%]", 
    x = 260, 
    y = 0.8, 
    family = "IBM Plex Sans",
    size = 3.2,
    fontface = "bold",
    color = "#946194"
  ) +
  annotate(
    "text", 
    label = "33 municipios\n[9.7%]", 
    x = 320, 
    y = 0.95, 
    family = "IBM Plex Sans",
    size = 3.2,
    fontface = "bold",
    color = "#402440"
  ) +
  geom_curve(
    aes(x = 170, y = .75, xend = 180, yend = .5),
    arrow = arrow(length = unit(0.02, "npc")), colour = "black",
    angle = 45, curvature = -.1
  ) +
  annotate(
    "text", 
    label = "Cada barra representa\na 1 de los 339 municipios", 
    x = 170, 
    y = 0.82, 
    family = "IBM Plex Sans",
    size = 3.2
  ) 

ggsave("graficos/municipios_barras_d2.jpg", width = 11, height = 8)

# gráfico municipios primera dosis highcharter
df %>%
  arrange(porcentaje_primera_dosis) %>% 
  mutate(
    num = 1:nrow(.),
    mide = case_when(
      porcentaje_primera_dosis < 25 ~ "[-] Menos del 25% de la población\ndel municipio vacunada",
      porcentaje_primera_dosis >= 25 & porcentaje_primera_dosis < 50 ~ "[-] Menos del 50% de la población del\nmunicipio vacunada",
      porcentaje_primera_dosis >= 50 & porcentaje_primera_dosis < 75 ~ "[+] 50% o más de la población del\nmunicipio vacunada",
      T ~ "[+] 75% o más de la población\n del municipio vacunada"
    )
  ) %>% 
  hchart("column", hcaes(x = cod_ine, y = porcentaje_primera_dosis, group = mide)) %>% 
  hc_chart(style = list(fontFamily = "IBM PLex Sans")) %>% 
  hc_colors(colors = c("#60A160", "#457345", "#946194", "#402440")) %>% 
  hc_tooltip(
    borderWidth = 1/20,
    pointFormat = paste("Municipio: <b>{point.municipio}</b><br>
                        Departamento: <b>{point.departamento}</b><br>
                        Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b>"),
    headerFormat = "") %>% 
  hc_title(text = "181 municipios [53%] tienen tasas de vacunación menores a la nacional") %>% 
  hc_subtitle(text = "Primera dosis<br>Cada barra representa un municipio<br>Desactive las categorías presionando sobre la leyenda y/o pase su mouse sobre cada barra para mayor información") %>% 
  hc_yAxis(
    title = list(text = "Porcentaje [%] de personas con al menos una dosis"), 
    labels = list(format = "{value}%"),
    max = 125
  ) %>% 
  hc_xAxis(visible = F) %>% 
  hc_credits(
    enabled = TRUE,
    text = "Realizado el 2 de enero de 2022 [rafalopezv]",
    href = "https://rafalopezv.io"
  ) -> mun_1d

# gráfico municipios segunda dosis highcharter
df %>%
  arrange(porcentaje_segunda_dosis) %>% 
  mutate(
    num = 1:nrow(.),
    mide = case_when(
      porcentaje_segunda_dosis < 25 ~ "[-] Menos del 25% de la población\ndel municipio vacunada",
      porcentaje_segunda_dosis >= 25 & porcentaje_segunda_dosis < 50 ~ "[-] Menos del 50% de la población del\nmunicipio vacunada",
      porcentaje_segunda_dosis >= 50 & porcentaje_segunda_dosis < 75 ~ "[+] 50% o más de la población del\nmunicipio vacunada",
      T ~ "[+] 75% o más de la población\n del municipio vacunada"
    )
  ) %>% 
  hchart("column", hcaes(x = cod_ine, y = porcentaje_segunda_dosis, group = mide)) %>% 
  hc_chart(style = list(fontFamily = "IBM PLex Sans")) %>% 
  hc_colors(colors = c("#60A160", "#457345", "#946194", "#402440")) %>% 
  hc_tooltip(
    borderWidth = 1/20,
    pointFormat = paste("Municipio: <b>{point.municipio}</b><br>
                        Departamento: <b>{point.departamento}</b><br>
                        Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b>"),
    headerFormat = "") %>% 
  hc_title(text = "144 municipios [42%] tienen tasas de vacunación menores a la nacional") %>% 
  hc_subtitle(text = "Segunda dosis<br>Cada barra representa un municipio<br>Desactive las categorías presionando sobre la leyenda y/o pase su mouse sobre cada barra para mayor información") %>% 
  hc_yAxis(
    title = list(text = "Porcentaje [%] de personas con dos dosis"), 
    labels = list(format = "{value}%")
  ) %>% 
  hc_xAxis(visible = F) %>% 
  hc_credits(
    enabled = TRUE,
    text = "Realizado el 2 de enero de 2022 [rafalopezv]",
    href = "https://rafalopezv.io"
  ) -> mun_2d

# mapa de municipios primera dosis
mapa <- jsonlite::fromJSON("geo/municipios.339.geojson", simplifyVector = F)
df %>% 
  mutate(
    value = case_when(
      porcentaje_primera_dosis > 100 ~ 100,
      T ~ porcentaje_primera_dosis
    )
  ) -> df
df$CODIGO <- df$cod_ine

highchart(type = "map") %>%
  hc_add_series(mapData = mapa, data = df, value = "value", joinBy = "CODIGO", 
                borderColor = "transparent") %>%  
  hc_colorAxis(minColor = "#D3B1D3", maxColor = "#402440") %>% 
  hc_legend(layout = "horizontal", align = "right",
            floating = T) %>% 
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01,
             pointFormat=paste("<br>Municipio: <b>{point.name}</b><br>
                               Departamento: <b>{point.departamento}</b><br>
                               Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b>"),
             headerFormat = "") %>% 
  hc_chart(style = list(fontFamily = "IBM Plex Sans")) %>% 
  hc_title(text = "Porcentaje de vacunación por municipio") %>% 
  hc_subtitle(text = "Primera dosis<br>Pase su mouse sobre cada municipio para mayor información") %>% 
  hc_credits(
    enabled = TRUE,
    text = "Realizado el 2 de enero de 2022 [rafalopezv]",
    href = "https://rafalopezv.io"
  ) -> mapa_1d

# mapa de municipios segunda dosis
mapa <- jsonlite::fromJSON("geo/municipios.339.geojson", simplifyVector = F)
df %>% 
  mutate(
    value = case_when(
      porcentaje_segunda_dosis > 100 ~ 100,
      T ~ porcentaje_segunda_dosis
    )
  ) -> df
df$CODIGO <- df$cod_ine

highchart(type = "map") %>%
  hc_add_series(mapData = mapa, data = df, value = "value", joinBy = "CODIGO", 
                borderColor = "transparent") %>%  
  hc_colorAxis(minColor = "#D3B1D3", maxColor = "#402440") %>% 
  hc_legend(layout = "horizontal", align = "right",
            floating = T) %>% 
  hc_tooltip(enabled = T, valueDecimals = 0, borderWidth = 0.01,
             pointFormat=paste("<br>Municipio: <b>{point.name}</b><br>
                               Departamento: <b>{point.departamento}</b><br>
                               Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b>"),
             headerFormat = "") %>% 
  hc_chart(style = list(fontFamily = "IBM Plex Sans")) %>% 
  hc_title(text = "Porcentaje de vacunación por municipio") %>% 
  hc_subtitle(text = "Segunda dosis<br>Pase su mouse sobre cada municipio para mayor información") %>% 
  hc_credits(
    enabled = TRUE,
    text = "Realizado el 2 de enero de 2022 [rafalopezv]",
    href = "https://rafalopezv.io"
  ) -> mapa_2d

# mapa primera dosis ggplot
mapa <- sf::st_read("geo/municipios.339.geojson", stringsAsFactors = F) %>% 
  rename(cod_ine = CODIGO) %>% 
  select(-MUNICIPIO)

# juntar datos con mapa
mapa_datos <- merge(mapa, df, all.x = T, by = "cod_ine")

# mapa primera dosis
ggplot(mapa_datos) +
  geom_sf(aes(fill = porcentaje_primera_dosis), 
          color = "#402440", size = 0.05) + 
  scale_fill_gradient(low = "#D3B1D3", high = "#402440", na.value = "#FFFFFF") +
  theme_map(base_family = "IBM Plex Sans") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "bottom"
  ) +
  labs(
    fill = "Porcentaje de población [%] con al menos una dosis",
    title = "Porcentaje de vacunación por municipio",
    subtitle = "Primera dosis",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv"
  ) 
ggsave("graficos/mapa_1d.jpg", width = 6, height = 6)  

# mapa segunda dosis ggplot
ggplot(mapa_datos) +
  geom_sf(aes(fill = porcentaje_segunda_dosis), 
          color = "#402440", size = 0.05) + 
  scale_fill_gradient(low = "#D3B1D3", high = "#402440", na.value = "#FFFFFF") +
  theme_map(base_family = "IBM Plex Sans") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "bottom"
  ) +
  labs(
    fill = "Porcentaje de población [%] con dos dosis",
    title = "Porcentaje de vacunación por municipio",
    subtitle = "Segunda dosis",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv"
  )
ggsave("graficos/mapa_2d.jpg", width = 6, height = 6)  

#------------------
# correlaciones
#------------------
# mas vs  vacunación: primera dosis gglot
df %>% 
  filter(porcentaje_primera_dosis < 100) %>% 
  mutate(
    mas = mas/100,
    porcentaje_primera_dosis = porcentaje_primera_dosis/100
  ) %>% 
  ggplot(aes(mas, porcentaje_primera_dosis)) +
  geom_jitter(size = 2, shape = 21, stroke = .7, color = "#402440",
  alpha = .9) +
  geom_smooth(method = "lm", colour = "red") +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "XY") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, .25)) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relación entre el nivel de vacunación y la votación al MAS-IPSP",
    subtitle = "¿En cuánto se parece la votación al MAS-IPSP y el nivel de vacunación?: en 50%\nCada círculo azul es un municipio\nLa lína roja muestra la tendencia, el margen plomo sus límites",
    x = "Porcentaje [%] de votación a favor del MAS-IPS, elecciones 2020",
    y = "Porcentaje [%] de personas con al menos una dosis (Bolivia)",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv\nLa línea de tendencia es producto de una regresión lineal"
  )
ggsave("graficos/scatter_mas_1d.jpg", width = 8, height = 8)

# mas vs  vacunación: segunda dosis
df %>% 
  mutate(
    mas = mas/100,
    porcentaje_segunda_dosis = porcentaje_segunda_dosis/100
  ) %>% 
  ggplot(aes(mas, porcentaje_segunda_dosis)) +
  geom_jitter(size = 2, shape = 21, stroke = .7, color = "#402440",
              alpha = .9) +
  geom_smooth(method = "lm", colour = "#E15759") +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "XY") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, .25)) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relación entre el nivel de vacunación y la votación al MAS-IPSP",
    subtitle = "¿En cuánto se parece la votación al MAS-IPSP y el nivel de vacunación?: en 45%\nCada círculo azul es un municipio\nLa lína roja muestra la tendencia, el margen plomo sus límites",
    x = "Porcentaje [%] de votación a favor del MAS-IPS, elecciones 2020",
    y = "Porcentaje [%] de personas con dos dosis (Bolivia)",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv"
  )
ggsave("graficos/scatter_mas_2d.jpg", width = 8, height = 8)

# etnicidad vs  vacunación: primera dosis gglot
df %>% 
  mutate(
    ind = ind/100,
    porcentaje_primera_dosis = porcentaje_primera_dosis/100
  ) %>% 
  ggplot(aes(ind, porcentaje_primera_dosis)) +
  geom_jitter(size = 2, shape = 21, stroke = .7, color = "#402440",
              alpha = .9) +
  geom_smooth(method = "lm", colour = "red") +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "XY") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, .25)) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relación entre el nivel de vacunación y el nivel de etnicidad",
    subtitle = "¿En cuánto se parece la votación al MAS-IPSP y el nivel de etnicidad?: en 53%\nCada círculo azul es un municipio\nLa lína roja muestra la tendencia, el margen plomo sus límites",
    x = "Porcentaje [%] de auto-identificación étnica (Censo 2012)",
    y = "Porcentaje [%] de personas con al menos una dosis (Bolivia)",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv\nLa línea de tendencia es producto de una regresión lineal"
  )
ggsave("graficos/scatter_ind_1d.jpg", width = 8, height = 8)

# etnicidad vs  vacunación: segundaa dosis gglot
df %>% 
  mutate(
    ind = ind/100,
    porcentaje_segunda_dosis = porcentaje_segunda_dosis/100
  ) %>% 
  ggplot(aes(ind, porcentaje_segunda_dosis)) +
  geom_jitter(size = 2, shape = 21, stroke = .7, color = "#402440",
              alpha = .9) +
  geom_smooth(method = "lm", colour = "red") +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "XY") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, .25)) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relación entre el nivel de vacunación y el nivel de etnicidad",
    subtitle = "¿En cuánto se parece la votación al MAS-IPSP y el nivel de etnicidad?: en 47%\nCada círculo azul es un municipio\nLa lína roja muestra la tendencia, el margen plomo sus límites",
    x = "Porcentaje [%] de auto-identificación étnica (Censo 2012)",
    y = "Porcentaje [%] de personas con dos dosis (Bolivia)",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv\nLa línea de tendencia es producto de una regresión lineal"
  )
ggsave("graficos/scatter_ind_2d.jpg", width = 8, height = 8)

# pobreza vs  vacunación: primera dosis gglot
df %>% 
  mutate(pobre_porcentaje = pobre_porcentaje/100) %>%
  mutate(porcentaje_primera_dosis = porcentaje_primera_dosis/100) %>% 
  ggplot(aes(pobre_porcentaje, porcentaje_primera_dosis)) +
  geom_jitter(size = 2, shape = 21, stroke = .7, color = "#402440",
              alpha = .9) +
  geom_smooth(method = "lm", colour = "red") +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "XY") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relación entre el nivel de vacunación y el nivel de pobrezaa",
    subtitle = "¿En cuánto se parece la votación al MAS-IPSP y el nivel de pobreza?: en 60%\nCada círculo azul es un municipio\nLa lína roja muestra la tendencia, el margen plomo sus límites",
    x = "Porcentaje [%] de población pobre (Censo 2012)",
    y = "Porcentaje [%] de personas con al menos una dosis (Bolivia)",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv\nLa línea de tendencia es producto de una regresión lineal"
  )
ggsave("graficos/scatter_pobre_1d.jpg", width = 8, height = 8)

# pobreza vs  vacunación: segunda dosis gglot
df %>% 
  mutate(pobre_porcentaje = pobre_porcentaje/100) %>%
  mutate(porcentaje_segunda_dosis = porcentaje_segunda_dosis/100) %>% 
  ggplot(aes(pobre_porcentaje, porcentaje_segunda_dosis)) +
  geom_jitter(size = 2, shape = 21, stroke = .7, color = "#402440",
              alpha = .9) +
  geom_smooth(method = "lm", colour = "red") +
  theme_ipsum_rc(base_family = "IBM Plex Sans", grid = "XY") +
  theme(
    plot.subtitle = element_text(family = "IBM Plex Sans", hjust = .5),
    plot.title = element_text(hjust = .5), 
    plot.caption = element_text(family = "IBM Plex Sans"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relación entre el nivel de vacunación y el nivel de pobrezaa",
    subtitle = "¿En cuánto se parece la votación al MAS-IPSP y el nivel de pobreza?: en 60%\nCada círculo azul es un municipio\nLa lína roja muestra la tendencia, el margen plomo sus límites",
    x = "Porcentaje [%] de población pobre (Censo 2012)",
    y = "Porcentaje [%] de personas con al dos dosis (Bolivia)",
    caption = "Realizado el 2 de enero de 2022\nrafalopezv\nLa línea de tendencia es producto de una regresión lineal"
  )
ggsave("graficos/scatter_pobre_2d.jpg", width = 8, height = 8)

# mas vs  vacunación: primera dosis highcharter
df %>%
  filter(porcentaje_primera_dosis < 100) %>% 
  mutate(mas = round(mas, 0)) %>%
  hchart(
    "scatter",
    hcaes(x = mas, y = porcentaje_primera_dosis),
    regression = TRUE,
    regressionSettings = list(
      dashStyle = "ShortDash",
      color = "red",
      order = 3,
      lineWidth = 2,
      name = "%eq | r2: %r",
      hideInLegend = FALSE)
  ) %>%
  hc_add_dependency("plugins/highcharts-regression.js") %>% 
  hc_plotOptions(series = list(
    marker = list(
      fillColor = "#402440",
      borderColor = "blue",
      lineWidth = .5
    )
  )) %>%
  hc_tooltip(
    borderWidth = 1 / 20,
    pointFormat = paste(
      "Municipio: <b>{point.municipio}</b><br>
                        Departamento: <b>{point.departamento}</b><br>
                        Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b><br>
                        Votación MAS-IPSP elecciones 2020: <b>{point.mas}%</b>"
    ),
    headerFormat = ""
  ) %>%
  hc_title(text = "Relación entre el nivel de vacunación y la votación al MAS-IPSP") %>%
  hc_subtitle(text = "Correlación: 53%") %>%
  hc_yAxis(
    title = list(text = "Porcentaje [%] de personas con al menos una dosis (Bolivia)"),
    labels = list(format = "{value}%"),
    max = 100
  ) %>%
  hc_xAxis(
    title = list(text = "Porcentaje [%] de votación a favor del MAS-IPS, elecciones 2020"),
    labels = list(format = "{value}%"),
    visivle = T
  ) %>%
  hc_credits(enabled = TRUE,
             text = "Realizado el 2 de enero de 2022|rafalopezv|La línea de tendencia es producto de una regresión lineal",
             href = "https://rafalopezv.io") %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(
    zoomType = "xy",
    style = list(
      backgroundColor = "#FFFFFF",
      fontFamily = "IBM Plex Sans"
    )
  ) -> scatter_mas_d1

# mas vs  vacunación: segunda dosis highcharter
df %>%
  filter(porcentaje_segunda_dosis < 101) %>% 
  mutate(mas = round(mas, 0)) %>%
  hchart(
    "scatter",
    hcaes(x = mas, y = porcentaje_segunda_dosis),
    regression = TRUE,
    regressionSettings = list(
      dashStyle = "ShortDash",
      color = "red",
      order = 3,
      lineWidth = 2,
      name = "%eq | r2: %r",
      hideInLegend = FALSE)
  ) %>%
  hc_add_dependency("plugins/highcharts-regression.js") %>% 
  hc_plotOptions(series = list(
    marker = list(
      fillColor = "#402440",
      borderColor = "blue",
      lineWidth = .5
    )
  )) %>%
  hc_tooltip(
    borderWidth = 1 / 20,
    pointFormat = paste(
      "Municipio: <b>{point.municipio}</b><br>
                        Departamento: <b>{point.departamento}</b><br>
                        Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b><br>
                        Votación MAS-IPSP elecciones 2020: <b>{point.mas}%</b>"
    ),
    headerFormat = ""
  ) %>%
  hc_title(text = "Relación entre el nivel de vacunación y la votación al MAS-IPSP") %>%
  hc_subtitle(text = "Correlación: 47%") %>%
  hc_yAxis(
    title = list(text = "Porcentaje [%] de personas con dos dosis (Bolivia)"),
    labels = list(format = "{value}%"),
    max = 100
  ) %>%
  hc_xAxis(
    title = list(text = "Porcentaje [%] de votación a favor del MAS-IPS, elecciones 2020"),
    labels = list(format = "{value}%"),
    visivle = T
  ) %>%
  hc_credits(enabled = TRUE,
             text = "Realizado el 2 de enero de 2022|rafalopezv|La línea de tendencia es producto de una regresión lineal",
             href = "https://rafalopezv.io") %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(
    zoomType = "xy",
    style = list(
      backgroundColor = "#FFFFFF",
      fontFamily = "IBM Plex Sans"
    )
  ) -> scatter_mas_d2

# etnicidaad vs  vacunación: primera dosis highcharter
df %>%
  filter(porcentaje_primera_dosis < 101) %>% 
  mutate(mas = round(mas, 0)) %>%
  hchart(
    "scatter",
    hcaes(x = ind, y = porcentaje_primera_dosis),
    regression = TRUE,
    regressionSettings = list(
      dashStyle = "ShortDash",
      color = "red",
      order = 3,
      lineWidth = 2,
      name = "%eq | r2: %r",
      hideInLegend = FALSE)
  ) %>%
  hc_add_dependency("plugins/highcharts-regression.js") %>% 
  hc_plotOptions(series = list(
    marker = list(
      fillColor = "#402440",
      borderColor = "blue",
      lineWidth = .5
    )
  )) %>%
  hc_tooltip(
    borderWidth = 1 / 20,
    pointFormat = paste(
      "Municipio: <b>{point.municipio}</b><br>
                        Departamento: <b>{point.departamento}</b><br>
                        Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b>
                        Votación de población indígena: <b>{point.mas}%</b><br>
                        Pueblo o nación mayoritaria: <b>{point.nacion_pueblo}"
    ),
    headerFormat = ""
  ) %>%
  hc_title(text = "Relación entre el nivel de vacunación y el nivel de etnicidad") %>%
  hc_subtitle(text = "Correlación: 55%") %>%
  hc_yAxis(
    title = list(text = "Porcentaje [%] de personas con al menos una dosis (Bolivia)"),
    labels = list(format = "{value}%"),
    max = 125
  ) %>%
  hc_xAxis(
    title = list(text = "Porcentaje [%] de auto-identificación étnica (Censo 2012)"),
    labels = list(format = "{value}%"),
    visivle = T
  ) %>%
  hc_credits(enabled = TRUE,
             text = "Realizado el 2 de enero de 2022|rafalopezv|La línea de tendencia es producto de una regresión lineal",
             href = "https://rafalopezv.io") %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(
    zoomType = "xy",
    style = list(
      backgroundColor = "#FFFFFF",
      fontFamily = "IBM Plex Sans"
    )
  ) -> scatter_in_d1

# etnicidad vs  vacunación: segunda dosis highcharter
df %>%
  filter(porcentaje_segunda_dosis < 101) %>% 
  mutate(mas = round(mas, 0)) %>%
  hchart(
    "scatter",
    hcaes(x = ind, y = porcentaje_segunda_dosis),
    regression = TRUE,
    regressionSettings = list(
      dashStyle = "ShortDash",
      color = "red",
      order = 3,
      lineWidth = 2,
      name = "%eq | r2: %r",
      hideInLegend = FALSE)
  ) %>%
  hc_add_dependency("plugins/highcharts-regression.js") %>% 
  hc_plotOptions(series = list(
    marker = list(
      fillColor = "#402440",
      borderColor = "blue",
      lineWidth = .5
    )
  )) %>%
  hc_tooltip(
    borderWidth = 1 / 20,
    pointFormat = paste(
      "Municipio: <b>{point.municipio}</b><br>
                        Departamento: <b>{point.departamento}</b><br>
                        Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b>
                        Porcentaje de población indígena: <b>{point.ind}%</b><br>
                        Pueblo o nación mayoritaario: <b>{point.nacion_pueblo}%</b>"
    ),
    headerFormat = ""
  ) %>%
  hc_title(text = "Relación entre el nivel de vacunación y el nivel de etnicidad") %>%
  hc_subtitle(text = "Correlación: 49%") %>%
  hc_yAxis(
    title = list(text = "Porcentaje [%] de personas con dos dosis (Bolivia)"),
    labels = list(format = "{value}%"),
    max = 125
  ) %>%
  hc_xAxis(
    title = list(text = "Porcentaje [%] de auto-identificación étnica (Censo 2012)"),
    labels = list(format = "{value}%"),
    visivle = T
  ) %>%
  hc_credits(enabled = TRUE,
             text = "Realizado el 2 de enero de 2022|rafalopezv|La línea de tendencia es producto de una regresión lineal",
             href = "https://rafalopezv.io") %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(
    zoomType = "xy",
    style = list(
      backgroundColor = "#FFFFFF",
      fontFamily = "IBM Plex Sans"
    )
  ) -> scatter_in_d2

# pobreza vs  vacunación: primera dosis highcharter
df %>%
  filter(porcentaje_primera_dosis < 101) %>% 
  mutate(pobre_porcentaje = round(pobre_porcentaje, 0)) %>% 
  hchart(
    "scatter",
    hcaes(x = pobre_porcentaje, y = porcentaje_primera_dosis),
    regression = TRUE,
    regressionSettings = list(
      dashStyle = "ShortDash",
      color = "red",
      order = 3,
      lineWidth = 2,
      name = "%eq | r2: %r",
      hideInLegend = FALSE)
  ) %>%
  hc_add_dependency("plugins/highcharts-regression.js") %>% 
  hc_plotOptions(series = list(
    marker = list(
      fillColor = "#402440",
      borderColor = "blue",
      lineWidth = .5
    )
  )) %>%
  hc_tooltip(
    borderWidth = 1 / 20,
    pointFormat = paste(
      "Municipio: <b>{point.municipio}</b><br>
                        Departamento: <b>{point.departamento}</b><br>
                        Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b><br>
                        Porcentaje de población pobre: <b>{point.pobre_porcentaje}%</b>"
    ),
    headerFormat = ""
  ) %>%
  hc_title(text = "Relación entre el nivel de vacunación y el nivel de pobreza") %>%
  hc_subtitle(text = "Correlación: 60%") %>%
  hc_yAxis(
    title = list(text = "Porcentaje [%] de personas con al menos una dosis (Bolivia)"),
    labels = list(format = "{value}%"),
    max = 125
  ) %>%
  hc_xAxis(
    title = list(text = "Porcentaje [%] de pobreza (Censo 2012)"),
    labels = list(format = "{value}%"),
    visivle = T
  ) %>%
  hc_credits(enabled = TRUE,
             text = "Realizado el 2 de enero de 2022|rafalopezv|La línea de tendencia es producto de una regresión lineal",
             href = "https://rafalopezv.io") %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(
    zoomType = "xy",
    style = list(
      backgroundColor = "#FFFFFF",
      fontFamily = "IBM Plex Sans"
    )
  ) -> scatter_pobre_d1

# pobreza vs  vacunación: segunda dosis highcharter
df %>%
  filter(porcentaje_segunda_dosis < 101) %>% 
  mutate(pobre_porcentaje = round(pobre_porcentaje, 0)) %>% 
  hchart(
    "scatter",
    hcaes(x = pobre_porcentaje, y = porcentaje_segunda_dosis),
    regression = TRUE,
    regressionSettings = list(
      dashStyle = "ShortDash",
      color = "red",
      order = 3,
      lineWidth = 2,
      name = "%eq | r2: %r",
      hideInLegend = FALSE)
  ) %>%
  hc_add_dependency("plugins/highcharts-regression.js") %>% 
  hc_plotOptions(series = list(
    marker = list(
      fillColor = "#402440",
      borderColor = "blue",
      lineWidth = .5
    )
  )) %>%
  hc_tooltip(
    borderWidth = 1 / 20,
    pointFormat = paste(
      "Municipio: <b>{point.municipio}</b><br>
                        Departamento: <b>{point.departamento}</b><br>
                        Porcentaje de población vacunada: <b>{point.porcentaje_primera_dosis}%</b><br>
                        Porcentaje de población pobre: <b>{point.pobre_porcentaje}%</b>"
    ),
    headerFormat = ""
  ) %>%
  hc_title(text = "Relación entre el nivel de vacunación y el nivel de pobreza") %>%
  hc_subtitle(text = "Correlación: 58%") %>%
  hc_yAxis(
    title = list(text = "Porcentaje [%] de personas con dos dosis (Bolivia)"),
    labels = list(format = "{value}%"),
    max = 125
  ) %>%
  hc_xAxis(
    title = list(text = "Porcentaje [%] de pobreza (Censo 2012)"),
    labels = list(format = "{value}%"),
    visivle = T
  ) %>%
  hc_credits(enabled = TRUE,
             text = "Realizado el 2 de enero de 2022|rafalopezv|La línea de tendencia es producto de una regresión lineal",
             href = "https://rafalopezv.io") %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(
    zoomType = "xy",
    style = list(
      backgroundColor = "#FFFFFF",
      fontFamily = "IBM Plex Sans"
    )
  ) -> scatter_pobre_d2

