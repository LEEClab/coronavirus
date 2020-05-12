#' ---
#' title: covid19 municipios e estados do brasil
#' author: mauricio vancine
#' date: 2021-05-08
#' ---

# packages
#library(ggrepel)
library(geobr)
library(lubridate)
library(sf)
library(spatialEco)
library(tmap)
library(tidyverse)

# packages in linux (ubuntu e linux mint)
# sudo apt install imagemagick

# import data -------------------------------------------------------------
# coronavirus no mundo
download.file(url = "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx",
              destfile = "COVID-19-geographic-disbtribution-worldwide.xlsx")
wd_cases <- readxl::read_xlsx("COVID-19-geographic-disbtribution-worldwide.xlsx") %>%
  dplyr::mutate(date = dateRep %>% lubridate::as_date() %>% lubridate::ymd(),
                country_name = stringr::str_replace_all(countriesAndTerritories, "_", " "),
                country_code = countryterritoryCode,
                country_id = geoId,
                country_pop2018 = popData2018,
                cases_pop = cases/(popData2018/1e6),
                deaths_pop = deaths/(popData2018/1e6)) %>%
  dplyr::select(date, country_name, country_code, country_id, country_pop2018,
                cases, deaths, cases_pop, deaths_pop)
wd_cases

# state
sta_cases <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv") %>% 
  dplyr::rename(abbrev_state = state)
tibble::glimpse(sta_cases)

# state time
sta_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>% 
  dplyr::rename(abbrev_state = state)
tibble::glimpse(sta_cases_time)

# municipality
mun_cases <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv") %>%
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))
tibble::glimpse(mun_cases)

# municipality time
mun_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv") %>% 
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))
tibble::glimpse(mun_cases_time)

# import geodata ----------------------------------------------------------
# state geodata
sta_geo <- geobr::read_state(code_state = "all", year = 2018) %>%
  sf::as_Spatial() %>%
  spatialEco::explode() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(area = as.numeric(sf::st_area(.)/1e6)) %>%
  dplyr::arrange(area) %>%
  dplyr::filter(area > 5e3)
sta_geo

# states centroids
sta_geo_cen <- sf::st_centroid(sta_geo)
sta_geo_cen

# munipality geodata
mun_geo <- geobr::read_municipality(code_muni = "all", year = 2018) %>%
  sf::st_crop(sta_geo)
mun_geo

# municipality centroids
mun_geo_cen <- geobr::read_municipal_seat(year = 2010)
mun_geo_cen

# join data ---------------------------------------------------------------
# state data
 sta_cases_geo <- sta_geo %>%
  dplyr::mutate(abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(sta_cases, "abbrev_state")
tibble::glimpse(sta_cases_geo)

# state data time
sta_cases_time_geo <- sta_geo_cen %>%
  dplyr::mutate(abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(sta_cases, "abbrev_state")
tibble::glimpse(sta_cases_time_geo)

# municipality data
mun_cases_geo <- mun_geo %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni),
                abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(mun_cases, by = "name_muni")
tibble::glimpse(mun_cases_geo)

# municipality data time
mun_cases_time_geo <- mun_geo_cen %>%
  dplyr::mutate(name_muni = as.character(name_muni)) %>%
  dplyr::left_join(mun_cases_time, by = "name_muni") %>%
  tidyr::drop_na(date)
tibble::glimpse(mun_cases_time_geo)

# world -------------------------------------------------------------------
# total cases world
cou_cases <- wd_cases %>%
  dplyr::group_by(country_name) %>%
  dplyr::summarise(cases_sum = sum(cases)) %>%
  dplyr::arrange(-cases_sum) %>%
  dplyr::slice(1:5) %>%
  dplyr::select(country_name) %>%
  rbind("Brazil") %>%
  dplyr::pull()
cou_cases

fig_cases_wd <- wd_cases %>%
  dplyr::filter(country_name %in% cou_cases, date >= "2020-02-20") %>%
  ggplot() +
  aes(x = date, y = cases_pop, col = country_name, fill = country_name) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  # geom_label_repel(data =  wd_cases %>% dplyr::filter(country_name %in% cou_cases) %>%
  #                    dplyr::filter(date == max(date)),
  #                  aes(label = country_name), fill = "white", hjust = 1, alpha = .9) +
  labs(x = "Data",
       y = "Número de casos confirmados (por milhões de hab.)",
       title = "Número de casos confirmados de coronavírus no Mundo (cinco maiores e Brasil)",
       color = "Países",
       fill = "Países") +
  scale_x_date(date_breaks = "5 day",
               date_labels = "%d/%m") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

fig_cases_wd
# ggsave(filename = "graficos/covid19_cases_wd.png", plot = fig_cases_wd, width = 30, height = 20, units = "cm", dpi = 300)

cou_deaths <- wd_cases %>%
  dplyr::group_by(country_name) %>%
  dplyr::summarise(deaths_sum = sum(deaths)) %>%
  dplyr::arrange(-deaths_sum) %>%
  dplyr::slice(1:5) %>%
  dplyr::select(country_name) %>%
  rbind("Brazil") %>%
  dplyr::pull()
cou_deaths

fig_deaths_wd <- wd_cases %>%
  dplyr::filter(country_name %in% cou_deaths, date >= "2020-02-20") %>%
  ggplot() +
  aes(x = date, y = deaths_pop + 1, color = country_name, fill = country_name) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  # geom_label_repel(data = wd_cases %>% dplyr::filter(country_name %in% cou_deaths) %>%
  #                    dplyr::filter(date == max(date)),
  #                  aes(label = country_name), fill = "white", hjust = 1, alpha = .9) +
  labs(x = "Data",
       y = "Número de mortes (por milhões de hab.)",
       title = "Número de mortes por coronavírus no Mundo (cinco maiores e Brasil)",
       color = "Países",
       fill = "Países") +
  scale_x_date(date_breaks = "10 day",
               date_labels = "%d/%m") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
fig_deaths_wd
# ggsave(filename = "graficos/covid19_deaths_wd.png", plot = fig_deaths_wd, width = 30, height = 20, units = "cm", dpi = 300)

# graphics ----------------------------------------------------------------
# total cases brazil
fig_cases_br <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  ggplot() +
  aes(x = date, y = totalCases, label = totalCases) +
  geom_line(size = 1, color = "steelblue") +
  geom_bar(stat = "identity", size = 2, fill = "steelblue", alpha = .5) +
  geom_point(size = 1.5, color = "white", fill = "steelblue", shape = 21, stroke = .3, alpha = .95) +
  #  geom_text(vjust = -1.2) +
  labs(x = "Data",
       y = "Número de casos confirmados",
       title = "Número de casos confirmados de coronavírus no Brasil") +
  scale_x_date(date_breaks = "2 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
fig_cases_br
#ggsave(filename = "graficos/covid19_cases_br.png", plot = fig_cases_br, width = 30, height = 20, units = "cm", dpi = 300)

# novos casos
fig_new_cases_br <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  ggplot() +
  aes(x = date, y = newCases, label = newCases) +
  geom_line(size = 1, color = "red") +
  geom_bar(stat = "identity", size = 2, fill = "red", alpha = .5) +
  geom_point(size = 1.5, color = "white", fill = "red", shape = 21, stroke = .3, alpha = .95) +
  #geom_text(vjust = -1.2, size = 3) +
  labs(x = "Data",
       y = "Número de novos casos confirmados",
       title = "Número de novos casos confirmados de coronavírus no Brasil") +
  scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
fig_new_cases_br
#ggsave(filename = "graficos/covid19_new_cases_br.png", plot = fig_new_cases_br, width = 30, height = 20, units = "cm", dpi = 300)

# recuperados
fig_recovery_br <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  ggplot() +
  aes(x = date, y = recovered, label = recovered) +
  geom_line(size = 1, color = "forestgreen") +
  geom_bar(stat = "identity", size = 2, alpha = .5, fill = "forestgreen") +
  geom_point(size = 1.5, color = "white", fill = "forestgreen", shape = 21, stroke = .3, alpha = .95) +
  #geom_text(vjust = -1.2) +
  labs(x = "Data",
       y = "Número de recuperados",
       title = "Número de recuperados de coronavírus no Brasil") +
  scale_x_date(date_breaks = "2 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
fig_recovery_br
# ggsave(filename = "graficos/covid19_deaths_br.png", plot = fig_deaths_br, width = 30, height = 20, units = "cm", dpi = 300)

# mortes
fig_deaths_br <- sta_cases_time %>%
  dplyr::filter(abbrev_state == "TOTAL") %>%
  ggplot() +
  aes(x = date, y = deaths, label = deaths) +
  geom_line(size = 1, color = "gray30") +
  geom_bar(stat = "identity", size = 2, alpha = .5) +
  geom_point(size = 1.5, color = "white", fill = "gray30", shape = 21, stroke = .3, alpha = .95) +
  #geom_text(vjust = -1.2) +
  labs(x = "Data",
       y = "Número de mortes",
       title = "Número de mortes de coronavírus no Brasil") +
  scale_x_date(date_breaks = "2 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
fig_deaths_br

# figura states
# casos
sta_cases_min <- sta_cases_time %>%
  dplyr::filter(date == max(date), totalCases > 4000, abbrev_state != "TOTAL") %>%
  dplyr::select(abbrev_state)
sta_cases_min

fig_cases_sta <- sta_cases_time %>%
  dplyr::filter(abbrev_state %in% sta_cases_min$abbrev_state, totalCases > 0000) %>%
  dplyr::mutate(nome = reorder(abbrev_state, -totalCases)) %>%
  ggplot() +
  aes(x = date, y = totalCases, col = abbrev_state, fill = abbrev_state) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  # geom_label_repel(data = dplyr::filter(sta_cases_time, date == max(date),
  #                                       totalCases > 1000, abbrev_state != "TOTAL"),
  #                  aes(label = abbrev_state), fill = "white", hjust = 1) +
  labs(x = "Data",
       y = "Número de casos confirmados (log10)",
       #title = "Estados com mais de 2000 casos",
       fill = "UF") +
  guides(color = guide_legend("UF")) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_date(date_breaks = "5 day",
               date_labels = "%d/%m") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
fig_cases_sta
# ggsave(filename = "graficos/covid19_cases_sta.png", plot = fig_cases_sta, width = 25, height = 20, units = "cm", dpi = 300)

fig_cases_sta_pop <- sta_cases_time %>%
  dplyr::filter(abbrev_state %in% sta_cases_min$abbrev_state) %>%
  dplyr::mutate(nome = reorder(abbrev_state, -totalCases)) %>%
  ggplot() +
  aes(x = date, y = totalCases_per_100k_inhabitants, col = abbrev_state, fill = abbrev_state) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  # geom_label_repel(data = dplyr::filter(sta_cases_time, date == max(date),
  #                                       totalCases > 1000, abbrev_state != "TOTAL"),
  #                  aes(label = abbrev_state), fill = "white", hjust = 1) +
  labs(x = "Data",
       y = "Número de casos confirmados (por 100 mil hab.)",
       #title = "Estados com mais de 1000 casos (por 100 mil hab.)",
       fill = "UF") +
  guides(color = guide_legend("UF")) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_date(date_breaks = "5 day",
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
fig_cases_sta_pop
# ggsave(filename = "graficos/covid19_cases_sta_pop.png", plot = fig_cases_sta_pop, width = 25, height = 20, units = "cm", dpi = 300)

# mortes
sta_deaths_min <- sta_cases_time %>%
  dplyr::filter(date == max(date), deaths > 100, abbrev_state != "TOTAL") %>%
  dplyr::select(abbrev_state)
sta_deaths_min

fig_deaths_sta <- sta_cases_time %>%
  dplyr::filter(abbrev_state %in% sta_deaths_min$abbrev_state) %>%
  ggplot() +
  aes(x = date, y = deaths, col = abbrev_state, fill = abbrev_state) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  # geom_label_repel(data = dplyr::filter(sta_cases_time, date == max(date)-1,
  #                                       deaths > 50, abbrev_state != "TOTAL"),
  #                  aes(label = abbrev_state), fill = "white", hjust = 1) +
  labs(x = "Data",
       y = "Número de mortes (log10)",
       # title = "Estados com mais de 50 mortes",
       fill = "UF") +
  guides(color = guide_legend("UF")) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
fig_deaths_sta
# ggsave(filename = "graficos/covid19_deaths_sta.png", plot = fig_deaths_sta, width = 25, height = 20, units = "cm", dpi = 300)

fig_deaths_sta_pop <- sta_cases_time %>%
  dplyr::filter(abbrev_state %in% sta_deaths_min$abbrev_state) %>%
  ggplot() +
  aes(x = date, y = deaths_per_100k_inhabitants, col = abbrev_state, fill = abbrev_state) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  # geom_label_repel(data = dplyr::filter(sta_cases_time, date == max(date),
  #                                       deaths > 50, abbrev_state != "TOTAL"),
  #                  aes(label = abbrev_state), fill = "white", hjust = 1) +
  labs(x = "Data",
       y = "Número de mortes (por 100 mil hab.)",
      # title = "Estados com mais de 50 mortes (por 100 mil hab.)",
       fill = "UF") +
  guides(color = guide_legend("UF")) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
fig_deaths_sta_pop
# ggsave(filename = "graficos/covid19_deaths_sta_pop.png", plot = fig_deaths_sta_pop, width = 25, height = 20, units = "cm", dpi = 300)

# cases muninipality time
mun_cases_min <- mun_cases_time %>%
  dplyr::filter(date == max(date), totalCases > 2000, abbrev_state != "TOTAL", name_muni != "Caso Sem Localização Definida") %>%
  dplyr::select(name_muni)
mun_cases_min

fig_cases_mun_pop <- mun_cases_time %>%
  dplyr::filter(name_muni %in% mun_cases_min$name_muni) %>%
  dplyr::mutate(name_muni = paste0(name_muni, " (", abbrev_state, ")")) %>% 
  tidyr::drop_na() %>%
  ggplot() +
  aes(x = date, y = totalCases_per_100k_inhabitants, col = name_muni, fill = name_muni) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  # geom_label_repel(data = dplyr::filter(mun_cases_time,
  #                                       date == max(date),
  #                                       totalCases > 1000,
  #                                       abbrev_state != "TOTAL",
  #                                       name_muni != "Caso Sem Localização Definida"),
  #                  aes(label = name_muni), fill = "white", hjust = 1) +
  labs(x = "Data",
       y = "Número de casos (por 100 mil hab.)",
#       title = "Municípios com mais de 1000 casos (por 100 mil hab.)",
       fill = "name_muni") +
  scale_color_viridis_d(name = "Municípios") +
  scale_fill_viridis_d(name = "Municípios") +
  scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
fig_cases_mun_pop
# ggsave(filename = "graficos/covid19_cases_mun_pop.png", plot = fig_cases_mun_pop, width = 25, height = 20, units = "cm", dpi = 300)

# cases muninipality time
mun_deaths_min <- mun_cases_time %>%
  dplyr::filter(date == max(date), deaths > 100, abbrev_state != "TOTAL", name_muni != "Caso Sem Localização Definida") %>%
  dplyr::select(name_muni)
mun_deaths_min

fig_deaths_mun_pop <- mun_cases_time %>%
  dplyr::filter(name_muni %in% mun_deaths_min$name_muni) %>%
  dplyr::mutate(name_muni = paste0(name_muni, " (", abbrev_state, ")")) %>% 
  tidyr::drop_na() %>%
  ggplot() +
  aes(x = date, y = totalCases_per_100k_inhabitants, col = name_muni, fill = name_muni) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  # geom_label_repel(data = dplyr::filter(mun_cases_time,
  #                                       date == max(date),
  #                                       totalCases > 1000,
  #                                       abbrev_state != "TOTAL",
  #                                       name_muni != "Caso Sem Localização Definida"),
  #                  aes(label = name_muni), fill = "white", hjust = 1) +
  labs(x = "Data",
       y = "Número de casos (por 100 mil hab.)",
       #       title = "Municípios com mais de 1000 casos (por 100 mil hab.)",
       fill = "name_muni") +
  scale_color_viridis_d(name = "Municípios") +
  scale_fill_viridis_d(name = "Municípios") +
  scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
fig_deaths_mun_pop
# ggsave(filename = "graficos/covid19_cases_mun_pop.png", plot = fig_cases_mun_pop, width = 25, height = 20, units = "cm", dpi = 300)

# state -------------------------------------------------------------------
# state total
map_sta_total <-  tm_shape(sta_cases_geo) +
  tm_polygons(border.col = "gray50", col = "totalCases", palette = "Reds", textNA = "Sem registros",
              title = "Casos confirmados (total)", n = 5, style = "jenks") +
  tm_text(text = "totalCases", shadow = TRUE) +
  # tm_graticules(lines = FALSE) +
  # tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .21),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_sta_total
# tmap::tmap_save(map_sta_total, "mapas/covid19_brasil_estados_total.png", dpi = 300)

map_sta_total_pop <- sta_cases_geo %>% 
  dplyr::mutate(totalCases_per_100k_inhabitants = round(totalCases_per_100k_inhabitants, 1)) %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "totalCases_per_100k_inhabitants", palette = "Reds", textNA = "Sem registros",
              title = "Casos confirmados (por 100 mil hab.)", n = 5, style = "pretty") +
  tm_text(text = "totalCases_per_100k_inhabitants", shadow = TRUE) +
  # tm_graticules(lines = FALSE) +
  # tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .21),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_sta_total_pop
# tmap::tmap_save(map_sta_total_pop, "mapas/covid19_brasil_estados_total_pop.png", dpi = 300)

# state deaths
map_sta_dea <- sta_cases_geo %>%
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "deaths", palette = "Greys",
              textNA = "Sem registros",
              title = "Mortes confirmadas", n = 5, style = "jenks") +
  tm_text(text = "deaths", shadow = TRUE) +
  # tm_graticules(lines = FALSE) +
  # tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .21),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_sta_dea
# tmap::tmap_save(map_sta_dea, "mapas/covid19_brasil_estados_mortes.png", dpi = 300)

map_sta_dea_pop <- sta_cases_geo %>%
  dplyr::mutate(deaths_per_100k_inhabitants = round(deaths_per_100k_inhabitants, 1)) %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "deaths_per_100k_inhabitants", palette = "Greys",
              textNA = "Sem registros",
              title = "Mortes confirmadas (por 100 mil hab.)", n = 5, style = "pretty") +
  tm_text(text = "deaths_per_100k_inhabitants", shadow = TRUE) +
  # tm_graticules(lines = FALSE) +
  # tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .25),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_sta_dea_pop
# tmap::tmap_save(map_sta_dea_pop, "mapas/covid19_brasil_estados_mortes_pop.png", dpi = 300)

# municipality ------------------------------------------------------------
# municipality for brazil
map_br_mun <- tm_shape(mun_cases_geo, bbox = sf::st_bbox(mun_cases_geo)) +
  tm_fill(col = "totalCases", palette = "Reds",
          textNA = "Sem registros", colorNA = "gray70",
          title = "Casos confirmados", n = 5, style = "jenks") +
  tm_borders(col = "gray30", lwd = .1) +
  tm_shape(sta_geo) +
  tm_borders(lwd = .5, col = "gray20") +
  # tm_graticules(lines = FALSE) + 
  # tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .23),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_br_mun
# tmap::tmap_save(map_br_mun, "mapas/covid19_brasil_municipios.png", dpi = 300)

# municipality for brazil pop
map_br_mun_pop <- mun_cases_geo %>% 
  dplyr::mutate(totalCases_per_100k_inhabitants = round(totalCases_per_100k_inhabitants, 1)) %>% 
  tm_shape(., bbox = sf::st_bbox(mun_cases_geo)) +
  tm_fill(col = "totalCases_per_100k_inhabitants", palette = "Reds",
          textNA = "Sem registros", colorNA = "gray70",
          title = "Casos confirmados (por 100 mil hab.)", n = 5, style = "pretty") +
  tm_borders(col = "gray30", lwd = .1) +
  tm_shape(sta_geo) +
  tm_borders(lwd = .5, col = "gray20") +
  # tm_graticules(lines = FALSE) +
  # tm_compass(position = c(.75, .08)) +
  tm_scale_bar(text.size = .8, position = c(.65, .02)) +
  tm_layout(title = lubridate::today() %>% format(format="%d/%m/%Y"),
            title.position = c(.02, .25),
            title.size = 2) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
map_br_mun_pop
# tmap::tmap_save(map_br_mun_pop, "mapas/covid19_brasil_municipios_pop.png", dpi = 300)

# # map for each state
# da_state <- data.frame(
#   abbrev_state = mun_geo$abbrev_state %>% unique %>% sort,
#   legend_h = c("left", "left", "left", "left", "left", "right", "left", "left", "left", "right",
#                "left", "left", "right", "right", "right", "left", "left", "right", "left", "left",
#                "left", "left", "left", "left", "right", "left", "left"),
#   legend_v = c("bottom", "bottom", "top", "bottom", "bottom", "bottom", "bottom", "top", "top", "bottom",
#                "top", "bottom", "top", "bottom", "bottom", "bottom", "top", "top", "top", "top",
#                "bottom", "bottom", "bottom", "bottom", "top", "bottom", "top"),
#   compass_h = c("right", "right", "right", "right", "right", "right", "right", "right", "right", "right",
#                 "right", "right", "right", "right", "right", "right", "right", "right", "right", "right",
#                 "right", "right", "right", "left", "right", "right", "right"),
#   compass_v = c("top", "bottom", "top", "top", "top", "top", "top", "bottom", "bottom", "top",
#                 "top", "top", "top", "top", "top", "top", "bottom", "bottom", "top", "top",
#                 "top", "top", "top", "bottom", "bottom", "top", "top"),
#   scalebar_h = c("left", "right", "right", "right", "left", "left", "left", "right", "right", "left",
#                  "left", "right", "left", "left", "left", "right", "right", "right", "right", "right",
#                  "right", "right", "right", "left", "right", "right", "right"),
#   scalebar_v = c("bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom",
#                  "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom",
#                  "bottom", "bottom", "bottom", "bottom", "bottom", "bottom", "bottom"))
# da_state <- as.matrix(da_state)
# da_state
# 
# for(i in mun_geo$abbrev_state %>% unique %>% seq){
# 
#   # information
#   print(da_state[i, 1])
# 
#   # filter data
#   mun_cases_geo_st <- mun_cases_geo %>%
#     dplyr::filter(abbrev_state.x == da_state[i, 1]) %>%
#     sf::st_crop(sta_geo %>%
#                   dplyr::filter(abbrev_state == da_state[i, 1]))
# 
#   # map
#   map_st <- tm_shape(mun_cases_geo_st) +
#     tm_polygons(border.col = "gray40", col = "totalCases_per_100k_inhabitants", palette = "Reds", textNA = "Sem registros",
#                 title = "Casos confirmados (por 100 mil hab.)", n = 5, style = "pretty") +
#     tm_graticules(lines = FALSE) +
#     # tm_compass(size = 2.5, position = c(da_state[i, 4], da_state[i, 5])) +
#     tm_scale_bar(text.size = .8, position = c(da_state[i, 6], da_state[i, 7])) +
#     tm_layout(title = paste0(da_state[i, 1]),
#               legend.position = c(da_state[i, 2], da_state[i, 3])) +
#     tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br")
#   map_st
#   tmap::tmap_save(map_st, paste0("mapas/covid19_municipios_", stringr::str_to_lower(da_state[i, 1]), ".png"), dpi = 300)
# 
# }

# models ------------------------------------------------------------------
# state model
fig_casos_mortes_sta <- sta_cases %>%
  dplyr::filter(abbrev_state != "TOTAL") %>%
  ggplot() +
  aes(x = totalCases, y = deaths) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point(size = 1.5, fill = "black", col = "black", shape = 21, alpha = .7) +
  geom_text(aes(label = abbrev_state), nudge_x = 0, nudge_y = .05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  labs(x = "Total de casos (log10)", y = "Mortes (log10)") +
  theme(axis.title = element_text(size = 15))
fig_casos_mortes_sta
# ggsave(filename = "modelos/covid19_total_casos_mortes_sta_gam.png", plot = fig_casos_mortes_sta, width = 30, height = 20, units = "cm", dpi = 300)

# state model pop
fig_casos_mortes_sta_pop <- sta_cases %>%
  dplyr::filter(abbrev_state != "TOTAL") %>%
  ggplot() +
  aes(x = totalCases_per_100k_inhabitants, y = deaths_per_100k_inhabitants) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point(size = 1.5, fill = "black", col = "black", shape = 21, alpha = .7) +
  geom_text(aes(label = abbrev_state), nudge_x = 0, nudge_y = .5) +
  theme_bw() +
  labs(x = "Total de casos (por 100 mil hab.)", y = "Mortes (por 100 mil hab.)") +
  theme(axis.title = element_text(size = 15))
fig_casos_mortes_sta_pop
# ggsave(filename = "modelos/covid19_total_casos_mortes_sta_gam_pop.png", plot = fig_casos_mortes_sta_pop, width = 30, height = 20, units = "cm", dpi = 300)

# municipality model
fig_casos_mortes_mun <- mun_cases %>%
  dplyr::filter(deaths > 50, name_muni != "Caso Sem Localização Definida") %>%
  ggplot() +
  aes(x = totalCases, y = deaths) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point(size = 1.5, fill = "black", col = "black", shape = 21, alpha = .7) +
  geom_text(data = mun_cases %>% 
                    dplyr::filter(deaths > 50, name_muni != "Caso Sem Localização Definida"),
                  aes(label = paste0(name_muni, " (", abbrev_state, ")")), nudge_y = .05) +
  labs(x = "Total de casos (log10)", 
       #title = "Relação do número de mortes (log10) e de casos (log10) acima de 50 mortes",
       y = "Mortes (log10)") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  labs(x = "Total de casos (log10)", y = "Mortes (log10)") +
  theme(axis.title = element_text(size = 15))
fig_casos_mortes_mun
# ggsave(filename = "modelos/covid19_total_casos_mortes_mun_gam.png", plot = fig_casos_mortes_mun, width = 30, height = 20, units = "cm", dpi = 300)

# municipality model pop
fig_casos_mortes_mun_pop <- mun_cases %>%
  dplyr::filter(deaths > 70, 
                name_muni != "Caso Sem Localização Definida") %>%
  ggplot() +
  aes(x = totalCases_per_100k_inhabitants, y = deaths_per_100k_inhabitants) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_point(size = 1.5, fill = "black", col = "black", shape = 20, alpha = .7) +
  geom_text(data = mun_cases %>% 
                    dplyr::filter(deaths > 70, name_muni != "Caso Sem Localização Definida"), 
                  aes(label = paste0(name_muni, " (", abbrev_state, ")")), nudge_y = .7) +
  labs(x = "Total de casos (por 100 mil hab.) (log10)", 
       #title = "Relação do número de mortes (por 100 mil hab.) e de casos (por 100 mil hab.) acima de 25 mortes por 100 mil hab.",
       y = "Mortes (por 100 mil hab.)") +
  theme_bw() +
  labs(x = "Total de casos (por 100 mil hab.)", y = "Mortes (por 100 mil hab.)") +
  theme(axis.title = element_text(size = 15))
fig_casos_mortes_mun_pop
# ggsave(filename = "modelos/covid19_total_casos_mortes_mun_gam_pop.png", plot = fig_casos_mortes_mun_pop, width = 30, height = 20, units = "cm", dpi = 300)

  # # sus data ----------------------------------------------------------------
# # sus
# sus <- readr::read_csv("https://raw.githubusercontent.com/JoaoCarabetta/SimulaCovid/master/data/raw/covid19_SUS_database.csv")
# tibble::glimpse(sus)
# 
# sta_sus <- sus %>%
#   dplyr::select(-(1:4)) %>%
#   dplyr::rename(abbrev_state = uf) %>%
#   dplyr::group_by(abbrev_state) %>%
#   dplyr::summarise_all(~sum(., na.rm = TRUE))
# tibble::glimpse(sta_sus)
# 
# mun_sus <- sus %>%
#   dplyr::rename(name_muni = municipio,
#                 abbrev_state = uf) %>%
#   dplyr::select(-(1:3)) %>%
#   dplyr::group_by(name_muni, abbrev_state) %>%
#   dplyr::summarise_all(~sum(., na.rm = TRUE))
# tibble::glimpse(mun_sus)

# # state leitos
# map_sta_leitos <- sta_cases_sus_geo %>%
#   tm_shape() +
#   tm_polygons(border.col = "gray50", col = "quantidade_leitos", palette = "Blues",
#               textNA = "Sem registros",
#               title = "Quantidade de leitos", n = 5, style = "pretty") +
#   tm_text(text = "quantidade_leitos", shadow = TRUE) +
#   tm_graticules(lines = FALSE) +
#   # tm_compass(position = c(.75, .08)) +
#   tm_scale_bar(text.size = .8, position = c(.65, .02)) +
#   tm_layout(title = paste0("Quantidade leitos no \n Brasil por Estado"),
#             title.position = c(.7, .9),
#             title.size = 1) +
#   tm_credits("Fonte: https://github.com/JoaoCarabetta/SimulaCovid", position = c(.57, 0))
# map_sta_leitos
# tmap::tmap_save(map_sta_dea, "mapas/leitos_brasil_estados.png", dpi = 300)
#
# # state ventiladores
# map_sta_vent <- sta_cases_sus_geo %>%
#   tm_shape() +
#   tm_polygons(border.col = "gray50", col = "ventiladores_existentes", palette = "Greens",
#               textNA = "Sem registros",
#               title = "Ventiladores existentes", n = 5, style = "pretty") +
#   tm_text(text = "ventiladores_existentes", shadow = TRUE) +
#   tm_graticules(lines = FALSE) +
#   # tm_compass(position = c(.75, .08)) +
#   tm_scale_bar(text.size = .8, position = c(.65, .02)) +
#   tm_layout(title = paste0("Quantidade de ventinadores no \n Brasil por Estado"),
#             title.position = c(.7, .9),
#             title.size = 1) +
#   tm_credits("Fonte: https://github.com/JoaoCarabetta/SimulaCovid", position = c(.57, 0))
# map_sta_vent
# tmap::tmap_save(map_sta_vent, "mapas/vent_brasil_estados.png", dpi = 300)

# in time -----------------------------------------------------------------
# # state map time
# map_sta_total_time <- tm_shape(sta_geo, bbox = sf::st_bbox(sta_geo)) +
#   tm_polygons() +
#   tm_shape(sta_cases_sus_time_geo) +
#   tm_symbols(size = "totalCases_pop", scale = 2, title.size = "Casos por estado (por 100 mil hab.)",
#              alpha = .5, col = "red", border.col = "darkred") +
#   tm_facets(along = "date") +
#   tm_text(text = "totalCases_pop") +
#   tm_graticules(lines = FALSE) +
#   # tm_compass(position = c(.73, .08)) +
#   tm_scale_bar(text.size = .6, position = c(.6, .02)) +
#   tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n ao longo dos dias",
#             title.position = c(.7, .9),
#             title.size = 1,
#             legend.position = c("left", "bottom")) +
#   tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
# tmap::tmap_animation(tm = map_sta_total_time, filename = "gifs/covid19_brasil_estados_evolucao.gif",
#                      wi = 2000, he = 2000, delay = 30)
# # magick::image_read("gifs/covid19_brasil_estados_evolucao.gif")
#
#
# # municipality map time
# map_mun_total_time <- tm_shape(sta_geo, bbox = sf::st_bbox(sta_geo)) +
#   tm_polygons() +
#   tm_shape(mun_cases_sus_time_geo) +
#   tm_symbols(size = "totalCases_pop", scale = 2, title.size = "Casos por municípios (por 100 mil hab.)",
#              alpha = .5, col = "red", border.col = "darkred") +
#   tm_facets(along = "date") +
#   tm_graticules(lines = FALSE) +
#   # tm_compass(position = c(.73, .08)) +
#   tm_scale_bar(text.size = .6, position = c(.6, .02)) +
#   tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n ao longo dos dias",
#             title.position = c(.7, .9),
#             title.size = 1,
#             legend.position = c("left", "bottom")) +
#   tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.57, 0))
# tmap::tmap_animation(tm = map_mun_total_time, filename = "gifs/covid19_brasil_municipios_evolucao.gif",
#                      wi = 2000, he = 2000, delay = 50)
# # magick::image_read("gifs/covid19_brasil_municipios_evolucao.gif")

# end ---------------------------------------------------------------------
