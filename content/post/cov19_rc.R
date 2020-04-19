#' ---
#' title: covid19 rio claro
#' author: mauricio vancine
#' date: 2021-04-18
#' ---

# packages
library(lubridate)
library(tidyverse)
library(plotly)

# import data -------------------------------------------------------------
# sus - https://github.com/JoaoCarabetta/SimulaCovid
rc_sus <- readr::read_csv("https://raw.githubusercontent.com/JoaoCarabetta/SimulaCovid/master/data/raw/covid19_SUS_database.csv") %>% 
  dplyr::filter(municipio == "Rio Claro", uf == "SP")

# municipality time - https://covid19br.wcota.me/
rc_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv") %>% 
  dplyr::filter(city  == "Rio Claro/SP")

# graphics ----------------------------------------------------------------
# total cases
fig_cases_rc <- ggplot(data = rc_cases_time) +
  aes(x = date, y = totalCases, label = totalCases) +
  geom_line(size = 2, color = "steelblue") +
  geom_point(size = 4, color = "white", fill = "steelblue", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de casos confirmados") +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# new cases
fig_new_cases_rc <- ggplot(data = rc_cases_time) +
  aes(x = date, y = newCases, label = newCases) +
  geom_line(size = 2, color = "red") +
  geom_point(size = 4, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de novos casos confirmados") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# deaths
fig_deaths_rc <-  ggplot(data = rc_cases_time) +
  aes(x = date, y = deaths, label = deaths) +
  geom_line(color = "gray30", size = 2) +
  geom_point(size = 4, color = "white", fill = "gray30", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de mortes") +
  scale_x_date(date_breaks = "1 day", 
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

# end ---------------------------------------------------------------------