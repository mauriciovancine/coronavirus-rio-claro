#' ---
#' title: covid19 rio claro
#' author: mauricio vancine
#' date: 2020-05-26
#' ---

# packages
library(lubridate)
library(tidyverse)

# import data -------------------------------------------------------------
# sus - https://github.com/JoaoCarabetta/SimulaCovid
rc_sus <- readr::read_csv("https://raw.githubusercontent.com/JoaoCarabetta/SimulaCovid/master/data/raw/covid19_SUS_database.csv") %>% 
  dplyr::filter(municipio == "Rio Claro", uf == "SP")

# municipality time - https://covid19br.wcota.me/
rc_cases_time <- readr::read_csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv") %>% 
  dplyr::filter(nome_munic == "Rio Claro")
rc_cases_time

# isolamento - https://www.saopaulo.sp.gov.br/coronavirus/isolamento/
# iso <- readr::read_csv("https://public.tableau.com/vizql/w/IsolamentoSocial/v/Dashboard/tempfile/sessions/3B5787EA77C449AC8F17B2AAC9F5B9D9-0:0/?key=1559693726&keepfile=yes&attachment=yes") %>% 
#   dplyr::mutate(nome_munic = stringr::str_to_title(Município1),
#                 isolamento = stringr::str_replace(`Média de Índice De Isolamento`, "%", "") %>% as.numeric(),
#                 data = lubridate::dmy(Data)) %>% 
#   dplyr::select(data, nome_munic, isolamento) %>% 
#   dplyr::filter(nome_munic == "Rio Claro")
# iso

# join
# rc_cases_time_iso <- rc_cases_time %>% 
#   dplyr::left_join(iso, by = c("datahora" = "data")) %>% 
#   dplyr::select(datahora, casos_novos, isolamento) %>% 
#   tidyr::drop_na()
# rc_cases_time_iso

# graphics ----------------------------------------------------------------
# total cases
fig_cases_rc <- ggplot(data = rc_cases_time) +
  aes(x = datahora, y = casos) +
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
  aes(x = datahora, y = casos_novos) +
  geom_line(size = 1, color = "red") +
  geom_point(size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de novos casos confirmados") +
  scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# new cases and isolation
# scale_factor <- max(rc_cases_time_iso$casos_novos) / max(rc_cases_time_iso$isolamento)
# scale_factor
# 
# fig_new_cases_isolation_rc <- ggplot(data = rc_cases_time_iso) +
#   geom_line(aes(x = datahora, y = casos_novos), color = "red", size = 1) +
#   geom_point(aes(x = datahora, y = casos_novos), size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(x = datahora, y = isolamento), color = "blue", size = 1) +
#   geom_point(aes(x = datahora, y = isolamento * scale_factor), size = 3, color = "white", fill = "blue", shape = 21, stroke = .5, alpha = .95) +
#   labs(x = "Data") +
#   scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
#   scale_y_continuous(name = "Novos casos", sec.axis = sec_axis(~./scaleFactor, name = "Isolamento (%)")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90), 
#         legend.position = "none",
#           axis.title.y.left = element_text(color = "blue"),
#           axis.text.y.left = element_text(color = "blue"),
#           axis.title.y.right = element_text(color = "red"),
#           axis.text.y.right = element_text(color = "red")
#         )
# fig_new_cases_isolation_rc

# deaths
fig_deaths_rc <-  ggplot(data = rc_cases_time) +
  aes(x = datahora, y = obitos) +
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

# end ---------------------------------------------------------------------