#' ---
#' title: covid19 rio claro
#' author: mauricio vancine
#' date: 2020-09-01
#' ---

# prepare r ---------------------------------------------------------------
# packages
library(lubridate)
library(tidyverse)
library(EpiEstim)

# import data -------------------------------------------------------------
# municipality time - https://covid19br.wcota.me/
rc_cases_time <- readr::read_csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv") %>% 
  dplyr::filter(nome_munic == "Rio Claro") %>% 
  dplyr::mutate(casos_total_mm7d = zoo::rollmean(casos, k = 7, fill = NA),
                obitos_total_mm7d = zoo::rollmean(obitos, k = 7, fill = NA))
rc_cases_time

# # isolamento - https://www.saopaulo.sp.gov.br/coronavirus/isolamento/
# download.file(url = paste0("https://www.saopaulo.sp.gov.br/wp-content/uploads/", 
#                            lubridate::today() %>% lubridate::year(), "/",
#                            lubridate::today() %>% lubridate::month() %>% ifelse(. < 10, paste0("0", .), .), "/",
#                            paste0(lubridate::today() %>% lubridate::year(),
#                                   lubridate::today() %>% lubridate::month() %>% ifelse(. < 10, paste0("0", .), .),
#                                   lubridate::today() %>% lubridate::day() %>% ifelse(. < 10, paste0("0", .), .)),
#                            "_isolamento.csv"),
#               destfile = "isolamento.txt")
# 
# iso <- read.table("isolamento.txt", sep = ";")[-1, c(2, 4, 5)] %>% 
#   tibble::as_tibble() %>% 
#   dplyr::mutate(data = stringr::str_split(V4, ", ", simplify = TRUE)[, 2] %>% 
#                   stringr::str_c("/2020") %>% 
#                   lubridate::dmy(),
#                 isolamento = stringr::str_replace(V5, "%", "") %>% as.numeric(),
#                 isolamento_mm7d = stringr::str_replace(V5, "%", "") %>% as.numeric() %>% zoo::rollmean(., k = 7, fill = NA),
#                 ibge = V2) %>% 
#   dplyr::select(ibge, data, isolamento, isolamento_mm7d) %>% 
#   dplyr::filter(ibge == 3543907)
# iso
# 
# # join
# rc_cases_time_iso <- rc_cases_time %>%
#   dplyr::left_join(iso, by = c("datahora" = "data"))
# rc_cases_time_iso

# graphics ----------------------------------------------------------------
# total cases ----
fig_cases_rc <- ggplot(data = rc_cases_time) +
  geom_line(aes(x = datahora, y = casos), size = .2, color = "steelblue4", linetype = 2) +
  geom_point(aes(x = datahora, y = casos), size = 3, color = "white", fill = "steelblue4", shape = 21, stroke = .5, alpha = .95) +
  geom_line(aes(x = datahora, y = casos_total_mm7d), size = 1.5, color = "gray30", alpha = .8) +
  labs(x = "Data", 
       y = "Número de casos confirmados") +
  scale_x_date(date_breaks = "10 day", 
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")
fig_cases_rc

# new cases ----
fig_new_cases_rc <- ggplot(data = rc_cases_time) +
  geom_line(aes(x = datahora, y = casos_novos), size = .2, color = "red", linetype = 2) +
  geom_point(aes(x = datahora, y = casos_novos), size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
  geom_line(aes(x = datahora, y = casos_mm7d), size = 1.5, color = "gray30", alpha = .8) +
  labs(x = "Data", 
       y = "Número de novos casos confirmados") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")
fig_new_cases_rc

# total cases and isolation ----
# scale_factor_cases <- max(rc_cases_time_iso$casos, na.rm = TRUE) / max(rc_cases_time_iso$isolamento, na.rm = TRUE)
# scale_factor_cases

# fig_cases_isolation_rc <- ggplot(data = rc_cases_time_iso, aes(x = datahora)) +
#   geom_line(aes(y = casos), color = "red", size = .2, linetype = 2) +
#   geom_point(aes(y = casos), size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(y = casos_total_mm7d), color = "red", size = 1.5, alpha = .6) +
#   geom_line(aes(y = isolamento * scale_factor_cases), size = .2, linetype = 2) +
#   geom_point(aes(y = isolamento * scale_factor_cases), size = 3, color = "white", fill = "blue", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(y = isolamento_mm7d * scale_factor_cases), color = "blue", size = 1.5, alpha = .6) +
#   labs(x = "Data") +
#   scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
#   scale_y_continuous(name = "Total de casos", 
#                      sec.axis = sec_axis(~./scale_factor_cases, name = "Isolamento (%)")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5),
#         legend.position = "none",
#         axis.title.y.left = element_text(color = "red", size = 12),
#         axis.text.y.left = element_text(color = "red", size = 12),
#         axis.title.y.right = element_text(color = "blue", size = 12),
#         axis.text.y.right = element_text(color = "blue", size = 12))
# fig_cases_isolation_rc
# 
# # new cases and isolation ----
# scale_factor_new_cases <- max(rc_cases_time_iso$casos_novos, na.rm = TRUE) / max(rc_cases_time_iso$isolamento, na.rm = TRUE)
# scale_factor_new_cases 
# 
# fig_new_cases_isolation_rc <- ggplot(data = rc_cases_time_iso, aes(x = datahora)) +
#   geom_line(aes(y = casos_novos), color = "red", size = .2, linetype = 2) +
#   geom_point(aes(y = casos_novos), size = 3, color = "white", fill = "red", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(y = casos_mm7d), color = "red", size = 1.5, alpha = .6) +
#   geom_line(aes(y = isolamento * scale_factor_new_cases), size = .2, linetype = 2) +
#   geom_point(aes(y = isolamento * scale_factor_new_cases), size = 3, color = "white", fill = "blue", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(y = isolamento_mm7d * scale_factor_new_cases), color = "blue", size = 1.5, alpha = .6) +
#   labs(x = "Data") +
#   scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
#   scale_y_continuous(name = "Novos casos diários", 
#                      sec.axis = sec_axis(~./scale_factor_new_cases, name = "Isolamento (%)")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5),
#         legend.position = "none",
#         axis.title.y.left = element_text(color = "red", size = 12),
#         axis.text.y.left = element_text(color = "red", size = 12),
#         axis.title.y.right = element_text(color = "blue", size = 12),
#         axis.text.y.right = element_text(color = "blue", size = 12))
# fig_new_cases_isolation_rc

# deaths ----
fig_deaths_rc <- ggplot(data = rc_cases_time) +
  geom_line(aes(x = datahora, y = obitos), size = .2, color = "gray40", linetype = 2) +
  geom_point(aes(x = datahora, y = obitos), size = 3, color = "white", fill = "gray40", shape = 21, stroke = .5, alpha = .95) +
  geom_line(aes(x = datahora, y = obitos_total_mm7d), size = 1.5, color = "gray30", alpha = .8) +
  labs(x = "Data", 
       y = "Número de óbitos") +
  scale_x_date(date_breaks = "10 day", 
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")
fig_deaths_rc

# new deaths ----
fig_new_deaths_rc <- ggplot(data = rc_cases_time) +
  geom_line(aes(x = datahora, y = obitos_novos), size = .2, color = "gray40", linetype = 2) +
  geom_point(aes(x = datahora, y = obitos_novos), size = 3, color = "white", fill = "gray40", shape = 21, stroke = .5, alpha = .95) +
  geom_line(aes(x = datahora, y = obitos_mm7d), size = 1.5, color = "gray30", alpha = .8) +
  labs(x = "Data", 
       y = "Número de novos óbitos confirmados") +
  scale_x_date(date_breaks = "10 day", date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")
fig_new_deaths_rc

# # total deaths and isolation ----
# scale_factor_deaths <- max(rc_cases_time_iso$obitos, na.rm = TRUE) / max(rc_cases_time_iso$isolamento, na.rm = TRUE)
# scale_factor_deaths
# 
# fig_deaths_isolation_rc <- ggplot(data = rc_cases_time_iso, aes(x = datahora)) +
#   geom_line(aes(y = obitos), color = "gray40", size = .2, linetype = 2) +
#   geom_point(aes(y = obitos), size = 3, color = "white", fill = "gray40", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(y = obitos_total_mm7d), color = "gray40", size = 1.5, alpha = .6) +
#   geom_line(aes(y = isolamento * scale_factor_deaths), size = .2, linetype = 2) +
#   geom_point(aes(y = isolamento * scale_factor_deaths), size = 3, color = "white", fill = "blue", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(y = isolamento_mm7d * scale_factor_deaths), color = "blue", size = 1.5, alpha = .6) +
#   labs(x = "Data") +
#   scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
#   scale_y_continuous(name = "Total de obitos", 
#                      sec.axis = sec_axis(~./scale_factor_deaths, name = "Isolamento (%)")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5),
#         legend.position = "none",
#         axis.title.y.left = element_text(color = "gray40", size = 12),
#         axis.text.y.left = element_text(color = "gray40", size = 12),
#         axis.title.y.right = element_text(color = "blue", size = 12),
#         axis.text.y.right = element_text(color = "blue", size = 12))
# fig_deaths_isolation_rc
# 
# # new deaths and isolation ----
# scale_factor_new_deaths <- max(rc_cases_time_iso$obitos_novos, na.rm = TRUE) / max(rc_cases_time_iso$isolamento, na.rm = TRUE)
# scale_factor_new_deaths
# 
# fig_new_deaths_isolation_rc <- ggplot(data = rc_cases_time_iso, aes(x = datahora)) +
#   geom_line(aes(y = obitos_novos), color = "gray40", size = .2, linetype = 2) +
#   geom_point(aes(y = obitos_novos), size = 3, color = "white", fill = "gray40", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(y = obitos_mm7d), color = "gray40", size = 1.5, alpha = .6) +
#   geom_line(aes(y = isolamento * scale_factor_new_deaths), size = .2, linetype = 2) +
#   geom_point(aes(y = isolamento * scale_factor_new_deaths), size = 3, color = "white", fill = "blue", shape = 21, stroke = .5, alpha = .95) +
#   geom_line(aes(y = isolamento_mm7d * scale_factor_new_deaths), color = "blue", size = 1.5, alpha = .6) +
#   labs(x = "Data") +
#   scale_x_date(date_breaks = "5 day", date_labels = "%d/%m") +
#   scale_y_continuous(name = "Novos obitos diários", 
#                      sec.axis = sec_axis(~./scale_factor_new_deaths, name = "Isolamento (%)")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5),
#         legend.position = "none",
#         axis.title.y.left = element_text(color = "gray40", size = 12),
#         axis.text.y.left = element_text(color = "gray40", size = 12),
#         axis.title.y.right = element_text(color = "blue", size = 12),
#         axis.text.y.right = element_text(color = "blue", size = 12))
# fig_new_deaths_isolation_rc

# end ---------------------------------------------------------------------