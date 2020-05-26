#' ---
#' title: covid19 rio claro
#' author: mauricio vancine
#' date: 2021-05-26
#' ---

# packages
library(geobr)
library(gganimate)
library(ggrepel)
library(gifski)
library(lubridate)
library(sf)
library(tmap)
library(tidyverse)

# entorno -----------------------------------------------------------------
# munipality geodata
mun_geo <- geobr::read_municipality(code_muni = "all", year = 2018) %>% 
  dplyr::filter(abbrev_state == "SP")

# cases
mun_cases_time <- readr::read_csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv")
mun_cases_time

# join
mun_geo_join <- mun_geo %>%
  dplyr::left_join(mun_cases_time, by = c("code_muni" = "codigo_ibge"))
mun_geo_join

# rio claro
rc <- mun_geo %>% 
  dplyr::filter(name_muni == "Rio Claro", abbrev_state == "SP")

# buffer
bu <- rc %>% 
  sf::st_buffer(.4)

# entorno
en <- mun_geo_join %>% 
  dplyr::mutate(en = sf::st_intersects(., bu, sparse = FALSE)) %>% 
  dplyr::filter(en == TRUE)

# data entorno
mun_cases_time_en <- mun_cases_time %>% 
  dplyr::filter(nome_munic %in% en$nome_munic)
mun_cases_time_en

# verificar
dplyr::glimpse(mun_cases_time_en)

# graphics ----------------------------------------------------------------
# graphs
mun_cases_time_en_rc <- mun_cases_time_en %>%
  dplyr::filter(nome_munic == "Rio Claro")

mun_cases_time_en_sem_rc <- mun_cases_time_en %>%
  dplyr::filter(nome_munic != "Rio Claro")

fig_cases_mun_ani <- ggplot() +
  geom_line(data = mun_cases_time_en_sem_rc, aes(x = datahora, y = casos, col = nome_munic), size = 1.2) +
  geom_segment(data = mun_cases_time_en_sem_rc, aes(x = datahora, y = casos, xend = max(datahora), yend = casos, col = nome_munic, fill = nome_munic), linetype = 2, colour = "grey") +
  geom_point(data = mun_cases_time_en_sem_rc, aes(x = datahora, y = casos, col = nome_munic, fill = nome_munic), size = 2) +
  geom_text(data = mun_cases_time_en_sem_rc, aes(x = max(datahora), y = casos, col = nome_munic, label = nome_munic), hjust = -.2, vjust = -.2) +
  geom_line(data = mun_cases_time_en_rc, aes(x = datahora, y = casos, col = nome_munic), size = 1.2, color = "red") +
  geom_segment(data = mun_cases_time_en_rc, aes(x = datahora, y = casos, xend = max(datahora) + 1, yend = casos), linetype = 2, colour = "red") +
  geom_point(data = mun_cases_time_en_rc, aes(x = datahora, y = casos, col = nome_munic, fill = nome_munic), size = 2, color = "red") +
  geom_text(data = mun_cases_time_en_rc, aes(x = max(datahora), y = casos,label = nome_munic), color = "red", hjust = -.2, vjust = -.2) +
  labs(x = "Data", y = "Número de casos") +
  guides(color = guide_legend("nome_munic")) +
  scale_colour_grey(start = .75, end = .75) +
  scale_fill_grey(start = .75, end = .75) +
  scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.margin = margin(5.5, 60, 5.5, 5.5)) +
  transition_reveal(datahora)
# anim_save("fig_cases.gif", fig_cases_mun_ani, fps = 20, duration = 15, start_pause = 10, end_pause = 60, wi = 25, he = 15, un = "cm", res = 100)

fig_mortes_mun_ani <- ggplot() +
  geom_line(data = mun_cases_time_en_sem_rc, aes(x = datahora, y = obitos, col = nome_munic), size = 1.2) +
  geom_segment(data = mun_cases_time_en_sem_rc, aes(x = datahora, y = obitos, xend = max(datahora), yend = obitos, col = nome_munic, fill = nome_munic), linetype = 2, colour = "grey") +
  geom_point(data = mun_cases_time_en_sem_rc, aes(x = datahora, y = obitos, col = nome_munic, fill = nome_munic), size = 2) +
  geom_text(data = mun_cases_time_en_sem_rc, aes(x = max(datahora), y = obitos, col = nome_munic, label = nome_munic), hjust = -.2, vjust = -.2) +
  geom_line(data = mun_cases_time_en_rc, aes(x = datahora, y = obitos, col = nome_munic), size = 1.2, color = "blue") +
  geom_segment(data = mun_cases_time_en_rc, aes(x = datahora, y = obitos, xend = max(datahora) + 1, yend = obitos), linetype = 2, colour = "blue") +
  geom_point(data = mun_cases_time_en_rc, aes(x = datahora, y = obitos, col = nome_munic, fill = nome_munic), size = 2, color = "blue") +
  geom_text(data = mun_cases_time_en_rc, aes(x = max(datahora), y = obitos,label = nome_munic), color = "blue", hjust = -.2, vjust = -.2) +
  labs(x = "Data", y = "Número de mortes") +
  guides(color = guide_legend("nome_munic")) +
  scale_colour_grey(start = .75, end = .75) +
  scale_fill_grey(start = .75, end = .75) +
  scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.margin = margin(5.5, 60, 5.5, 5.5)) +
  transition_reveal(datahora)
# anim_save("fig_mortes.gif", fig_mortes_mun_ani, fps = 20, duration = 15, start_pause = 10, end_pause = 60, wi = 25, he = 15, un = "cm", res = 100)

# fixo
fig_cases_mun_rc <- mun_cases_time_en %>%
  ggplot() +
  aes(x = datahora, y = casos, col = nome_munic, fill = nome_munic) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Data", y = "Número de casos") +
  scale_colour_viridis_d(name = "Municípios") +
  scale_fill_viridis_d(name = "Municípios") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  coord_cartesian(clip = "off") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(.15, .65))
fig_cases_mun_rc
# ggsave("fig_cases.png", fig_cases_mun_rc, wi = 25, he = 15, un = "cm", dpi = 300)

fig_mortes_mun_rc <- mun_cases_time_en %>%
  ggplot() +
  aes(x = datahora, y = obitos, col = nome_munic, fill = nome_munic) +
  geom_line() +
  geom_point(size = 2) +
  # geom_text(data = mun_cases_time_en_sem_rc %>% dplyr::filter(date == max(date), nome_munic %in% mun_cases_min$nome_munic), 
  #           aes(x = date, label = nome_munic)) + 
  # geom_text(data = mun_cases_time_en_rc %>% dplyr::filter(nome_munic == "Rio Claro", date == max(date)), 
  #           aes(x = date, label = nome_munic), color = "blue") + 
  labs(x = "Data", y = "Número de mortes") +
  scale_colour_viridis_d(name = "Municípios") +
  scale_fill_viridis_d(name = "Municípios") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  coord_cartesian(clip = "off") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(.15, .65))
fig_mortes_mun_rc
# ggsave("fig_mortos.png", fig_mortes_mun_rc, wi = 25, he = 15, un = "cm", dpi = 300)

# map ---------------------------------------------------------------------
# map cases
map_casos <- en %>% 
  dplyr::select(casos, nome_munic) %>% 
  tm_shape() +
  tm_polygons("casos", palette = "Reds", title = "Total de casos", 
              textNA = "Sem mortos", style = "jenks") +
  tm_text("nome_munic", size = .8) +
  tm_shape(bu) +
  tm_borders(col = "red", lwd = 4, lty = 2) +
  tm_shape(rc) +
  tm_borders(col = "red", lwd = 4) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_layout(legend.position = c(.78, .03))
map_casos
# tmap_save(map_casos, "map_cases.png", wi = 20, he = 15, un = "cm", dpi = 300)

# maps obitos
map_mortes <- en %>%
  dplyr::select(obitos, nome_munic) %>% 
  tm_shape() +
  tm_polygons(col = "obitos", palette = "Blues", title = "Total de mortos", 
              textNA = "Sem mortos", style = "jenks") +
  tm_text("nome_munic", size = .8) +
  tm_shape(bu) +
  tm_borders(col = "red", lwd = 4, lty = 2) +
  tm_shape(rc) +
  tm_borders(col = "red", lwd = 4) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_layout(legend.position = c(.78, .03))
map_mortes
# tmap_save(map_mortes, "map_mortes.png", wi = 20, he = 15, un = "cm", dpi = 300)

# end ---------------------------------------------------------------------