#' ---
#' title: covid19 rio claro
#' author: mauricio vancine
#' date: 2021-05-04
#' ---

# packages
library(ggrepel)
library(lubridate)
library(tidyverse)

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

# graphic
ggplot() +
  geom_line(data = mun_cases_time_en_rc, aes(x = date, y = totalCases), color = "red") +
  geom_point(data = mun_cases_time_en_rc, aes(x = date, y = totalCases), size = 2, color = "red", fill = "red") +
  geom_label_repel(data = mun_cases_time_en_rc %>% dplyr::filter(date == max(date)), hjust = 0, vjust = -2,
                   aes(x = date, y = totalCases, label = "Casos - Rio Claro"), col = "red") +
  geom_text(data = mun_cases_time_en_rc, aes(x = date, y = totalCases, label = totalCases), col = "red", vjust = -.5) +
  geom_line(data = mun_cases_time_en_sem_rc, aes(x = date, y = deaths, color = name_muni)) +
  geom_point(data = mun_cases_time_en_sem_rc, aes(x = date, y = deaths, color = name_muni), size = 1) +
  geom_label_repel(data = mun_cases_time_en_sem_rc %>% dplyr::filter(date == max(date)), 
            aes(x = date, y = deaths, label = paste0("Mortes - ", name_muni)), hjust = -.2, vjust = -.5) +
  geom_line(data = mun_cases_time_en_rc %>% dplyr::filter(name_muni %in% mun_cases_min$name_muni), 
            aes(x = date, y = deaths, color = name_muni), col = "blue") +
  geom_point(data = mun_cases_time_en_rc %>% dplyr::filter(name_muni %in% mun_cases_min$name_muni), 
             aes(x = date, y = deaths, color = name_muni), col = "blue", size = 2) +
  geom_label_repel(data = mun_cases_time_en_rc %>% dplyr::filter(date == max(date)), 
                   aes(x = date, y = deaths, label = paste0("Mortes - ", name_muni)), col = "blue", hjust = -.2, vjust = -2) +
  geom_text(data = mun_cases_time_en_rc, aes(x = date, y = deaths, label = deaths), col = "blue", hjust = .5, vjust = -.5) +
  scale_colour_grey(start = 0, end = 0) +
  scale_fill_grey(start = 0, end = 0) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d/%m") +
  coord_cartesian(clip = "off") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.margin = margin(5.5, 60, 5.5, 5.5)) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(x = "Datas", y = "Números", title = "Evolução do número de casos e de mortes em Rio Claro/SP e em seu entorno",
       caption = "Fonte: W. Cota, “Monitoring the number of COVID-19 cases and deaths in brazil at municipal and federative units level”, SciELOPreprints:362 (2020), 10.1590/scielopreprints.362 \n Gráfico: Maurício Vancine") 
ggsave("casos_mortes_rc_entorno.png", wi = 30, he = 20, un = "cm", dpi = 300)

# end ---------------------------------------------------------------------