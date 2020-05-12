ggplot() +
  geom_line(data = mun_cases_time_en_rc, aes(x = date, y = totalCases), color = "red") +
  geom_point(data = mun_cases_time_en_rc, aes(x = date, y = totalCases), size = 2, color = "red", fill = "red") +
  geom_label_repel(data = mun_cases_time_en_rc %>% dplyr::filter(date == max(date)), hjust = -.2, vjust = -.5,
                   aes(x = date, y = totalCases, label = "Casos - Rio Claro"), col = "red") +
  geom_line(data = mun_cases_time_en %>% dplyr::filter(name_muni %in% mun_cases_min$name_muni), 
            aes(x = date, y = deaths, color = name_muni)) +
  geom_point(data = mun_cases_time_en %>% dplyr::filter(name_muni %in% mun_cases_min$name_muni), 
             aes(x = date, y = deaths, color = name_muni), size = 1) +
  geom_label_repel(data = mun_cases_time_en %>% dplyr::filter(date == max(date), name_muni %in% mun_cases_min$name_muni), 
            aes(x = date, y = deaths, label = paste0("Mortes - ", name_muni)), hjust = -.2, vjust = -.2) +
  scale_colour_grey(start = 0, end = 0) +
  scale_fill_grey(start = 0, end = 0) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d/%m") +
  coord_cartesian(clip = "off") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        plot.margin = margin(5.5, 60, 5.5, 5.5)) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(x = "Datas", y = "Números") 

