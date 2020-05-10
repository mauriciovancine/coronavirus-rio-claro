#' ---
#' title: covid19 rio claro
#' author: mauricio vancine
#' date: 2021-05-04
#' ---

# packages
library(geobr)
library(gganimate)
library(gifski)
library(lubridate)
library(sf)
library(tmap)
library(tidyverse)

# import data -------------------------------------------------------------
# sus - https://github.com/JoaoCarabetta/SimulaCovid
rc_sus <- readr::read_csv("https://raw.githubusercontent.com/JoaoCarabetta/SimulaCovid/master/data/raw/covid19_SUS_database.csv") %>% 
  dplyr::filter(municipio == "Rio Claro", uf == "SP")

# municipality time - https://covid19br.wcota.me/
rc_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv") %>% 
  dplyr::filter(city  == "Rio Claro/SP") %>% 
  dplyr::rename(total_de_casos = totalCases,
                novos_casos = newCases,
                mortes = deaths)

# graphics ----------------------------------------------------------------
# total cases
fig_cases_rc <- ggplot(data = rc_cases_time) +
  aes(x = date, y = total_de_casos) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 3, color = "white", fill = "steelblue", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de casos confirmados") +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# new cases
fig_new_cases_rc <- ggplot(data = rc_cases_time) +
  aes(x = date, y = novos_casos) +
  geom_line(size = 1, color = "red") +
  geom_point(size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de novos casos confirmados") +
  scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# deaths
fig_deaths_rc <-  ggplot(data = rc_cases_time) +
  aes(x = date, y = mortes) +
  geom_line(size = 1, color = "gray30") +
  geom_point(size = 3, color = "white", fill = "gray30", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de mortes") +
  scale_x_date(date_breaks = "2 day", 
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# sus data
fig_sus_rc <- rc_sus %>%
  dplyr::select(municipio_sus, clinicos:leitos_nao_sus) %>%
  tidyr::pivot_longer(-municipio_sus, names_to = "informations", values_to = "frequency") %>%
  dplyr::mutate(informations = stringr::str_replace_all(informations, "_", " ") %>%
                  stringr::str_to_title()) %>%
  ggplot() +
  aes(x = informations, y = frequency) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Informações",
       y = "Frequência absoluta") +
  ylim(0, 430) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "none")


# entorno -----------------------------------------------------------------
# munipality geodata
mun_geo <- geobr::read_municipality(code_muni = "all", year = 2018) %>% 
  dplyr::filter(abbrev_state == "SP")

# cases
mun_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv") %>% 
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))

# join
mun_geo_join <- mun_geo %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni),
                abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(mun_cases_time, by = c("abbrev_state", "name_muni"))

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
  dplyr::filter(name_muni %in% en$name_muni, abbrev_state == "SP")

# graphics ----------------------------------------------------------------
# graphs
mun_cases_min <- mun_cases_time_en %>%
  dplyr::filter(date == max(date), totalCases > 18, abbrev_state != "TOTAL", name_muni != "Caso Sem Localização Definida") %>%
  dplyr::select(name_muni)

mun_cases_time_en_rc <- mun_cases_time_en %>%
  dplyr::filter(name_muni %in% mun_cases_min$name_muni) %>% 
  dplyr::filter(name_muni == "Rio Claro")

mun_cases_time_en_sem_rc <- mun_cases_time_en %>%
  dplyr::filter(name_muni %in% mun_cases_min$name_muni) %>% 
  dplyr::filter(name_muni != "Rio Claro")

fig_cases_mun_ani <- ggplot() +
  geom_line(data = mun_cases_time_en_sem_rc, aes(x = date, y = totalCases, col = name_muni), size = 1.2) +
  geom_segment(data = mun_cases_time_en_sem_rc, aes(x = date, y = totalCases, xend = max(date), yend = totalCases, col = name_muni, fill = name_muni), linetype = 2, colour = "grey") +
  geom_point(data = mun_cases_time_en_sem_rc, aes(x = date, y = totalCases, col = name_muni, fill = name_muni), size = 2) +
  geom_text(data = mun_cases_time_en_sem_rc, aes(x = max(date), y = totalCases, col = name_muni, label = name_muni), hjust = -.2, vjust = -.2) +
  geom_line(data = mun_cases_time_en_rc, aes(x = date, y = totalCases, col = name_muni), size = 1.2, color = "red") +
  geom_segment(data = mun_cases_time_en_rc, aes(x = date, y = totalCases, xend = max(date) + 1, yend = totalCases), linetype = 2, colour = "red") +
  geom_point(data = mun_cases_time_en_rc, aes(x = date, y = totalCases, col = name_muni, fill = name_muni), size = 2, color = "red") +
  geom_text(data = mun_cases_time_en_rc, aes(x = max(date), y = totalCases,label = name_muni), color = "red", hjust = -.2, vjust = -.2) +
  labs(x = "Data", y = "Número de casos") +
  guides(color = guide_legend("name_muni")) +
  scale_colour_grey(start = .75, end = .75) +
  scale_fill_grey(start = .75, end = .75) +
  scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.margin = margin(5.5, 60, 5.5, 5.5)) +
  transition_reveal(date)
# anim_save("fig_cases.gif", fig_cases_mun_ani, fps = 20, duration = 15, start_pause = 10, end_pause = 60, wi = 25, he = 15, un = "cm", res = 100)

fig_mortes_mun_ani <- ggplot() +
  geom_line(data = mun_cases_time_en_sem_rc, aes(x = date, y = deaths, col = name_muni), size = 1.2) +
  geom_segment(data = mun_cases_time_en_sem_rc, aes(x = date, y = deaths, xend = max(date), yend = deaths, col = name_muni, fill = name_muni), linetype = 2, colour = "grey") +
  geom_point(data = mun_cases_time_en_sem_rc, aes(x = date, y = deaths, col = name_muni, fill = name_muni), size = 2) +
  geom_text(data = mun_cases_time_en_sem_rc, aes(x = max(date), y = deaths, col = name_muni, label = name_muni), hjust = -.2, vjust = -.2) +
  geom_line(data = mun_cases_time_en_rc, aes(x = date, y = deaths, col = name_muni), size = 1.2, color = "blue") +
  geom_segment(data = mun_cases_time_en_rc, aes(x = date, y = deaths, xend = max(date) + 1, yend = deaths), linetype = 2, colour = "blue") +
  geom_point(data = mun_cases_time_en_rc, aes(x = date, y = deaths, col = name_muni, fill = name_muni), size = 2, color = "blue") +
  geom_text(data = mun_cases_time_en_rc, aes(x = max(date), y = deaths,label = name_muni), color = "blue", hjust = -.2, vjust = -.2) +
  labs(x = "Data", y = "Número de casos") +
  guides(color = guide_legend("name_muni")) +
  scale_colour_grey(start = .75, end = .75) +
  scale_fill_grey(start = .75, end = .75) +
  scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.margin = margin(5.5, 60, 5.5, 5.5)) +
  scale_y_continuous(limits = c(0, 13)) +
  transition_reveal(date)
# anim_save("fig_mortes.gif", fig_mortes_mun_ani, fps = 20, duration = 15, start_pause = 10, end_pause = 60, wi = 25, he = 15, un = "cm", res = 100)

# fixo
fig_cases_mun_rc <- mun_cases_time_en %>%
  dplyr::filter(name_muni %in% mun_cases_min$name_muni) %>%
  ggplot() +
  aes(x = date, y = totalCases, col = name_muni, fill = name_muni) +
  geom_line() +
  geom_point(size = 1) +
  geom_line(data = mun_cases_time_en %>% dplyr::filter(name_muni == "Rio Claro"), aes(x = date, y = totalCases), color = "red") +
  geom_point(data = mun_cases_time_en %>% dplyr::filter(name_muni == "Rio Claro"), aes(x = date, y = totalCases), 
             color = "red", size = 1) +
  geom_text(data = mun_cases_time_en %>% dplyr::filter(date == max(date), name_muni %in% mun_cases_min$name_muni), 
            aes(x = date, label = name_muni), hjust = -.2, vjust = -.2) + 
  geom_text(data = mun_cases_time_en %>% dplyr::filter(name_muni == "Rio Claro", date == max(date)), 
            aes(x = date, label = name_muni), color = "red", hjust = -.2, vjust = -.2) + 
  labs(x = "Data", y = "Número de casos") +
  scale_colour_grey(start = .75, end = .75) +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  coord_cartesian(clip = "off") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.margin = margin(5.5, 60, 5.5, 5.5))
# fig_cases_mun_rc
# ggsave("fig_cases.png", fig_cases_mun_rc, wi = 25, he = 15, un = "cm", dpi = 300)

fig_mortes_mun_rc <- mun_cases_time_en %>%
  dplyr::filter(name_muni %in% mun_cases_min$name_muni) %>%
  ggplot() +
  aes(x = date, y = deaths, col = name_muni, fill = name_muni) +
  geom_line() +
  geom_point(size = 2) +
  geom_line(data = mun_cases_time_en %>% dplyr::filter(name_muni == "Rio Claro"), aes(x = date, y = deaths), color = "blue") +
  geom_point(data = mun_cases_time_en %>% dplyr::filter(name_muni == "Rio Claro"), aes(x = date, y = deaths), color = "blue", size = 2) +
  geom_text(data = mun_cases_time_en %>% dplyr::filter(date == max(date), name_muni %in% mun_cases_min$name_muni), 
            aes(x = date, label = name_muni), hjust = -.2, vjust = -.2) + 
  geom_text(data = mun_cases_time_en %>% dplyr::filter(name_muni == "Rio Claro", date == max(date)), 
            aes(x = date, label = name_muni), color = "blue", hjust = -.2, vjust = -.2) + 
  labs(x = "Data", y = "Número de casos") +
  scale_colour_grey(start = .75, end = .75) +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  coord_cartesian(clip = "off") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.margin = margin(5.5, 60, 5.5, 5.5)) +
  scale_y_continuous(limits = c(0, 13))
# fig_mortes_mun_rc
# ggsave("fig_mortos.png", fig_mortes_mun_rc, wi = 25, he = 15, un = "cm", dpi = 300)

# map ---------------------------------------------------------------------
# map cases
map_casos <- tm_shape(en) +
  tm_polygons("totalCases", palette = "Reds", title = "Total de casos", textNA = "Sem casos", n = 10) +
  tm_text("name_muni", size = .8) +
  tm_shape(bu) +
  tm_borders(col = "black", lwd = 4) +
  tm_shape(rc) +
  tm_borders(col = "red", lwd = 4) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_layout(legend.position = c(.78, .03))
# tmap_save(map_casos, "map_cases.png", wi = 20, he = 15, un = "cm", dpi = 300)

# maps deaths
map_mortes <- en %>%
  dplyr::mutate(deaths = ifelse(deaths == 0, NA, deaths)) %>%
  tm_shape() +
  tm_polygons("deaths", palette = "Blues", title = "Total de mortos", textNA = "Sem mortos",
              labels = c("1", "2", "3", "7", "10", "12")) +
  tm_text("name_muni", size = .8) +
  tm_shape(bu) +
  tm_borders(col = "black", lwd = 4) +
  tm_shape(rc) +
  tm_borders(col = "red", lwd = 4) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_layout(legend.position = c(.78, .03))
# tmap_save(map_mortes, "map_mortes.png", wi = 20, he = 15, un = "cm", dpi = 300)

# end ---------------------------------------------------------------------