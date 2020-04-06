

joined0 <- get_africa(level = 0)


borno <- 
  afrob_tidy %>% 
  filter(NAME_1 == "Borno" & temporal_window == "3 months")

borno1 <- afrob_sf_buffers[[1]] %>% 
  right_join(borno) %>% filter(spatial_window == "10km" & yearintr == 2012 & date > "2012-08-01")

borno2 <- afrob_sf_buffers[[2]] %>% 
  right_join(borno) %>% filter(spatial_window == "25km" & yearintr == 2012 & date > "2012-08-01")

borno3 <- afrob_sf_buffers[[3]] %>% 
  right_join(borno) %>% filter(spatial_window == "50km"& yearintr == 2012 & date > "2012-08-01")

borno4 <- afrob_sf_buffers[[4]] %>% 
  right_join(borno) %>% filter(spatial_window == "100km"& yearintr == 2012 & date > "2012-08-01")


borno1 <- st_transform(borno1, st_crs(joined0)) %>% mutate(attacks = case_when(attacks == 0 ~ NA_real_, TRUE ~ attacks))
borno2 <- st_transform(borno2, st_crs(joined0))  %>% mutate(attacks = case_when(attacks == 0 ~ NA_real_, TRUE ~ attacks))
borno3 <- st_transform(borno3, st_crs(joined0)) %>% mutate(attacks = case_when(attacks == 0 ~ NA_real_, TRUE ~ attacks))
borno4 <- st_transform(borno4, st_crs(joined0)) %>% mutate(attacks = case_when(attacks == 0 ~ NA_real_, TRUE ~ attacks))

temp <- rbind(borno1, borno2, borno3, borno4)

plot_borno <- 
  gtd_sf %>% st_transform(., st_crs(joined0)) %>% 
  filter(country_txt == "Nigeria" & iyear == 2012 & imonth >= 8) %>% 
  ggplot() +
  geom_sf(data = filter(joined0, NAME_0 == "Nigeria"), fill = "gray50") +
  geom_sf(data = temp, 
          aes(fill = attacks), color = "white") + 
  facet_wrap(~spatial_window, ncol = 4) +
  geom_sf(pch = 4, color = "red", alpha = 0.5, size = 4) +
  coord_sf(xlim = c(12, 15), ylim = c(10.25, 14.25)) +
  scale_fill_viridis_c(option = "magma", begin = 0.9, end = 0.1, 
                       # alpha = 0.8,
                       na.value = "gray75", trans = "log10",
                       breaks = c(3, 10, 30), 
                       guide = F) +
  # facet_wrap(~) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
        strip.text = element_text(color = "white"),
        panel.background = element_rect(color = "gray20"),
        legend.position = "right",
        panel.grid = element_line(color = "gray95"),
        axis.text = element_blank())

plot_leg <- 
  gtd_sf %>% st_transform(., st_crs(joined0)) %>% 
  filter(country_txt == "Nigeria" & iyear == 2012 & imonth >= 8) %>% 
  ggplot() +
  geom_sf(data = filter(joined0, NAME_0 == "Nigeria"), fill = "gray50") +
  geom_sf(data = temp, 
          aes(fill = attacks), color = "white") + 
  facet_wrap(~spatial_window, ncol = 4) +
  geom_sf(pch = 4, color = "red", alpha = 0.5, size = 4) +
  coord_sf(xlim = c(12, 15), ylim = c(10.25, 14.25)) +
  scale_fill_viridis_c(option = "magma", begin = 0.9, end = 0.1, 
                       # alpha = 0.8,
                       na.value = "gray75", trans = "log10",
                       breaks = c(3, 10, 30), 
                       guide = guide_colorbar(title = "Attacks in buffer",
                                              keywidth = 5, keyheight = 2)) +
  # facet_wrap(~) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
        strip.text = element_text(color = "white"),
        panel.background = element_rect(color = "gray20"),
        # legend.position = c(0.4,0.60),
        # legend.title.align = 0.5,
        panel.grid = element_line(color = "gray95"),
        axis.text = element_blank(),
        legend.margin = margin(0,0,0,0))


plot_nga <- 
  ggplot() +
  geom_sf(data = filter(joined1, NAME_0 == "Nigeria"), 
          size = 1,fill = "gray90", color = "white") +
  geom_sf(data = filter(joined1, NAME_1 == "Borno"), 
          size = 1, fill = "gray50", color = "white") +
  geom_sf_text(data = filter(joined1, NAME_1 == "Borno"), aes(label = NAME_1), 
               color = "white", check_overlap = T, fontface = "bold") +
  theme_void()


legend <- get_legend(plot_leg)
bottom <- plot_grid(NULL, plot_nga, legend, NULL, ncol = 4)
plot_grid(plot_borno, bottom, ncol = 1)


