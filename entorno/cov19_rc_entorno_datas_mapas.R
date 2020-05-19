#' ---
#' title: covid19 rio claro
#' author: mauricio vancine
#' date: 2021-05-18
#' ---

# packages
library(geobr)
library(ggrepel)
library(lubridate)
library(sf)
library(tmap)
library(tidyverse)

# directory
setwd("/home/mude/data/github/coronavirus-rio-claro/entorno")

# entorno -----------------------------------------------------------------
# munipality geodata
mun_geo <- geobr::read_municipality(code_muni = "all", year = 2018) %>% 
  dplyr::filter(abbrev_state == "SP")

# cases
mun_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv") %>% 
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))

# filter
mun_cases_2020_05_17 <- mun_cases_time %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::filter(date <= "2020-05-17")

mun_cases_2020_05_02 <- mun_cases_time %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::filter(date <= "2020-05-02")

mun_cases_2020_04_17 <- mun_cases_time %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::filter(date <= "2020-04-17")

# join
mun_geo_join_2020_05_17 <- mun_geo %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni),
                abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(mun_cases_2020_05_17, by = c("abbrev_state", "name_muni"))
mun_geo_join_2020_05_17

mun_geo_join_2020_05_02 <- mun_geo %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni),
                abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(mun_cases_2020_05_02, by = c("abbrev_state", "name_muni"))
mun_geo_join_2020_05_02

mun_geo_join_2020_04_17 <- mun_geo %>%
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni),
                abbrev_state = as.character(abbrev_state)) %>%
  dplyr::left_join(mun_cases_2020_04_17, by = c("abbrev_state", "name_muni"))
mun_geo_join_2020_04_17

# rio claro
rc <- mun_geo %>% 
  dplyr::filter(name_muni == "Rio Claro", abbrev_state == "SP")

# buffer
bu <- rc %>% 
  sf::st_buffer(.4)

# entorno
en_2020_05_17 <- mun_geo_join_2020_05_17 %>% 
  dplyr::mutate(en = sf::st_intersects(., bu, sparse = FALSE)) %>% 
  dplyr::filter(en == TRUE) %>% 
  dplyr::group_by(name_muni) %>% 
  dplyr::summarise(totalCases = sum(newCases, rm.na = TRUE),
                   deaths = sum(newDeaths, rm.na = TRUE)) %>% 
  dplyr::mutate(totalCases = ifelse(is.na(totalCases) == TRUE, 0, totalCases)) %>%
  dplyr::mutate(deaths = ifelse(is.na(deaths) == TRUE, 0, deaths)) %>%
  dplyr::mutate(name_muni_cases = paste0(name_muni, " (", totalCases, ")")) %>% 
  dplyr::mutate(name_muni_deaths = paste0(name_muni, " (", deaths, ")"))
en_2020_05_17

en_2020_05_02 <- mun_geo_join_2020_05_02 %>% 
  dplyr::mutate(en = sf::st_intersects(., bu, sparse = FALSE)) %>% 
  dplyr::filter(en == TRUE) %>% 
  dplyr::group_by(name_muni) %>% 
  dplyr::summarise(totalCases = sum(newCases, rm.na = TRUE),
                   deaths = sum(newDeaths, rm.na = TRUE)) %>% 
  dplyr::mutate(totalCases = ifelse(is.na(totalCases) == TRUE, 0, totalCases)) %>%
  dplyr::mutate(deaths = ifelse(is.na(deaths) == TRUE, 0, deaths)) %>%
  dplyr::mutate(name_muni_cases = paste0(name_muni, " (", totalCases, ")")) %>% 
  dplyr::mutate(name_muni_deaths = paste0(name_muni, " (", deaths, ")"))
en_2020_05_02

en_2020_04_17 <- mun_geo_join_2020_04_17 %>% 
  dplyr::mutate(en = sf::st_intersects(., bu, sparse = FALSE)) %>% 
  dplyr::filter(en == TRUE) %>% 
  dplyr::group_by(name_muni) %>% 
  dplyr::summarise(totalCases = sum(newCases, rm.na = TRUE),
                   deaths = sum(newDeaths, rm.na = TRUE)) %>% 
  dplyr::mutate(totalCases = ifelse(is.na(totalCases) == TRUE, 0, totalCases)) %>%
  dplyr::mutate(deaths = ifelse(is.na(deaths) == TRUE, 0, deaths)) %>%
  dplyr::mutate(name_muni_cases = paste0(name_muni, " (", totalCases, ")")) %>% 
  dplyr::mutate(name_muni_deaths = paste0(name_muni, " (", deaths, ")"))
en_2020_04_17

# map ---------------------------------------------------------------------
# map cases
map_casos_2020_05_17 <- en_2020_05_17 %>% 
  tm_shape() +
  tm_polygons("totalCases", palette = "Reds", title = "Total de casos",
              breaks = c(0, 10, 25, 50, 100, 150, 200, 250)) +
  tm_text("name_muni_cases", size = .6) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_credits("Fonte: https://covid19br.wcota.me. Mapa: Maurício Vancine", 
             position = c(.44, 0), size = .6) +
  tm_layout(title = "17-05-2020",
            legend.position = c(.77, .03))
map_casos_2020_05_17
tmap_save(map_casos_2020_05_17, "map_cases_2020_05_17.png", wi = 20, he = 15, un = "cm", dpi = 300)

map_casos_2020_05_02 <- en_2020_05_02 %>% 
  tm_shape() +
  tm_polygons("totalCases", palette = "Reds", title = "Total de casos",
              breaks = c(0, 10, 25, 50, 100, 150, 200, 250)) +
  tm_text("name_muni_cases", size = .6) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_credits("Fonte: https://covid19br.wcota.me. Mapa: Maurício Vancine", 
             position = c(.44, 0), size = .6) +
  tm_layout(title = "02-05-2020",
            legend.position = c(.77, .03))
map_casos_2020_05_02
tmap_save(map_casos_2020_05_02, "map_cases_2020_05_02.png", wi = 20, he = 15, un = "cm", dpi = 300)

map_casos_2020_04_17 <- en_2020_04_17 %>% 
  tm_shape() +
  tm_polygons("totalCases", palette = "Reds", title = "Total de casos",
              breaks = c(0, 10, 25, 50, 100, 150, 200, 250)) +
  tm_text("name_muni_cases", size = .6) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_credits("Fonte: https://covid19br.wcota.me. Mapa: Maurício Vancine", 
             position = c(.44, 0), size = .6) +
  tm_layout(title = "17-04-2020",
            legend.position = c(.77, .03))
map_casos_2020_04_17
tmap_save(map_casos_2020_04_17, "map_cases_2020_04_17.png", wi = 20, he = 15, un = "cm", dpi = 300)

# -------------------------------------------------------------------------
# maps deaths
# map cases
map_mortes_2020_05_17 <- en_2020_05_17 %>% 
  tm_shape() +
  tm_polygons(col = "deaths", palette = "Blues", title = "Total de mortos", 
              breaks = c(0, 1, 2, 4, 7, 10, 15, 20, 25)) +
  tm_text("name_muni_deaths", size = .6) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_credits("Fonte: https://covid19br.wcota.me. Mapa: Maurício Vancine", 
             position = c(.44, 0), size = .6) +
  tm_layout(title = "17-05-2020",
            legend.position = c(.77, .03))
map_mortes_2020_05_17
tmap_save(map_mortes_2020_05_17, "map_deaths_2020_05_17.png", wi = 20, he = 15, un = "cm", dpi = 300)

map_mortes_2020_05_02 <- en_2020_05_02 %>% 
  tm_shape() +
  tm_polygons("deaths", palette = "Blues", title = "Total de mortos",
              breaks = c(0, 1, 2, 4, 7, 10, 15, 20, 25)) +
  tm_text("name_muni_deaths", size = .6) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_credits("Fonte: https://covid19br.wcota.me. Mapa: Maurício Vancine", 
             position = c(.44, 0), size = .6) +
  tm_layout(title = "02-05-2020",
            legend.position = c(.77, .03))
map_mortes_2020_05_02
tmap_save(map_mortes_2020_05_02, "map_deaths_2020_05_02.png", wi = 20, he = 15, un = "cm", dpi = 300)

map_mortes_2020_04_17 <- en_2020_04_17 %>% 
  tm_shape() +
  tm_polygons("deaths", palette = "Blues", title = "Total de mortos",
              breaks = c(0, 1, 2, 4, 7, 10, 15, 20, 25)) +
  tm_text("name_muni_deaths", size = .6) +
  tm_scale_bar(position = c(0, -.01), text.size = .6) +
  tm_credits("Fonte: https://covid19br.wcota.me. Mapa: Maurício Vancine", 
             position = c(.44, 0), size = .6) +
  tm_layout(title = "17-04-2020",
            legend.position = c(.77, .03))
map_mortes_2020_04_17
tmap_save(map_mortes_2020_04_17, "map_deaths_2020_04_17.png", wi = 20, he = 15, un = "cm", dpi = 300)

# end ---------------------------------------------------------------------