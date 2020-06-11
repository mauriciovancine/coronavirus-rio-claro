#' ---
#' title: covid19 rio claro
#' author: mauricio vancine
#' date: 2020-05-26
#' ---

# packages
library(lubridate)
library(tidyverse)

# import data -------------------------------------------------------------
# sus - https://www.seade.gov.br/coronavirus/
leitos_internacoes <- readr::read_csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes.csv") %>% 
  dplyr::filter(nome_drs == "DRS 10 - Piracicaba") %>% 
  dplyr::select(datahora:internacoes_7v7) %>% 
  dplyr::mutate(datahora = lubridate::dmy(datahora))
leitos_internacoes

# municipality time - https://covid19br.wcota.me/
rc_cases_time <- readr::read_csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv") %>% 
  dplyr::filter(nome_munic == "Rio Claro")
rc_cases_time

# isolamento - https://www.saopaulo.sp.gov.br/coronavirus/isolamento/
# iso <- readr::read_csv("https://public.tableau.com/vizql/w/IsolamentoSocial/v/DADOS/vudcsv/sessions/3FA33652009142819EC147088782A822-0:0/views/1292535119418963567_2497449162794898547?summary=true") %>% 
#   dplyr::mutate(nome_munic = stringr::str_to_title(Município1),
#                 isolamento = stringr::str_replace(`Avg. Índice De Isolamento`, "%", "") %>% as.numeric(),
#                 data = Data %>% 
#                   stringr::str_split(", ", simplify = TRUE) %>% 
#                   tibble::as_tibble() %>% 
#                   dplyr::select(2) %>%
#                   dplyr::pull() %>%
#                   paste0(., "/2020") %>% 
#                   lubridate::dmy()) %>% 
#   dplyr::select(data, nome_munic, isolamento) %>%
#   dplyr::filter(nome_munic == "Rio Claro")
# iso

# join
# rc_cases_time_iso <- rc_cases_time %>%
#   dplyr::left_join(iso, by = c("datahora" = "data")) %>%
#   dplyr::select(datahora, casos, casos_novos, isolamento) %>%
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
  theme(axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

# total cases and isolation
# scale_factor <- max(rc_cases_time_iso$casos) / max(rc_cases_time_iso$isolamento)
# scale_factor
# 
# fig_cases_isolation_rc <- ggplot(data = rc_cases_time_iso, aes(x = datahora)) +
#   geom_line(aes(y = casos), color = "red", size = 1, alpha = .5) +
#   geom_point(aes(y = casos), size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
#   geom_smooth(aes(y = casos), method = "loess", col = "red") +
#   geom_line(aes(y = isolamento * scale_factor), color = "blue", size = 1, alpha = .5) +
#   geom_point(aes(y = isolamento * scale_factor), size = 3, color = "white", fill = "blue", shape = 21, stroke = .5, alpha = .95) +
#   geom_smooth(aes(y = isolamento * scale_factor), method = "loess", col = "blue") +
#   labs(x = "Data") +
#   scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
#   scale_y_continuous(name = "Total de casos", sec.axis = sec_axis(~./scale_factor, name = "Isolamento (%)")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5),
#         legend.position = "none",
#         axis.title.y.left = element_text(color = "red", size = 12),
#         axis.text.y.left = element_text(color = "red", size = 12),
#         axis.title.y.right = element_text(color = "blue", size = 12),
#         axis.text.y.right = element_text(color = "blue", size = 12))
# fig_cases_isolation_rc

# new cases
fig_new_cases_rc <- ggplot(data = rc_cases_time) +
  aes(x = datahora, y = casos_novos) +
  geom_line(size = 1, color = "red") +
  geom_point(size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de novos casos confirmados") +
  scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

# new cases and isolation
# scale_factor <- max(rc_cases_time_iso$casos_novos) / max(rc_cases_time_iso$isolamento)
# scale_factor
# 
# fig_new_cases_isolation_rc <- ggplot(data = rc_cases_time_iso, aes(x = datahora)) +
#   geom_line(aes(y = casos_novos), color = "red", size = 1, alpha = .5) +
#   geom_point(aes(y = casos_novos), size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
#   geom_smooth(aes(y = casos_novos), method = "loess", col = "red") +
#   geom_line(aes(y = isolamento* scale_factor), color = "blue", size = 1, alpha = .5) +
#   geom_point(aes(y = isolamento * scale_factor), size = 3, color = "white", fill = "blue", shape = 21, stroke = .5, alpha = .95) +
#   geom_smooth(aes(y = isolamento * scale_factor), method = "loess", col = "blue") +
#   labs(x = "Data") +
#   scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
#   scale_y_continuous(name = "Novos casos", sec.axis = sec_axis(~./scale_factor, name = "Isolamento (%)")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5),
#         legend.position = "none",
#           axis.title.y.left = element_text(color = "red", size = 12),
#           axis.text.y.left = element_text(color = "red", size = 12),
#           axis.title.y.right = element_text(color = "blue", size = 12),
#           axis.text.y.right = element_text(color = "blue", size = 12)
#         )
# fig_new_cases_isolation_rc

# deaths
fig_deaths_rc <-  ggplot(data = rc_cases_time) +
  aes(x = datahora, y = obitos) +
  geom_line(size = 1, color = "gray30") +
  geom_point(size = 3, color = "white", fill = "gray30", shape = 21, stroke = .5, alpha = .95) +
  labs(x = "Data", 
       y = "Número de mortes") +
  scale_x_date(date_breaks = "5 day", 
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")
fig_deaths_rc

# internacoes e leitos
# fig_int_lei <-  ggplot(data = leitos_internacoes) +
#   geom_bar(aes(x = datahora, y = internacoes_7d), stat = "identity", size = 2, fill = "blue") +
#   geom_bar(aes(x = datahora, y = total_covid_uti), stat = "identity", size = 2, fill = "steelblue") +
#   labs(x = "Data", y = "Número totais") +
#   scale_x_date(date_breaks = "2 day", 
#                date_labels = "%d/%m") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5))
# fig_int_lei

# end ---------------------------------------------------------------------