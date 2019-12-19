### Goal: match GTD event locations to spatio-temporal afrob buffers ### 

library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
# library(furrr)
library(sf)
library(data.table)


# Load data ---------------------------------------------------------------

# Load buffered afrob buffered points
load(file = here::here("afrob", "afrob5_buffers.Rdata"))
load(file = here::here("afrob", "afrob6_buffers.Rdata"))

# These are lists of each unique point-date, by buffer size
head(afrob5_buffers[[1]])

# Load gtd points; add date and subset to shrink the data
load(file = here::here("attacks", "gtd_sf.Rdata"))
gtd_sf <- gtd_sf %>% 
  mutate(gtd_date = lubridate::ymd(paste(iyear, imonth, iday, sep = "-"))) %>% 
  filter(iyear > 2005)

# Match up projections (albers africa) for spatial join
gtd_sf <- st_transform(gtd_sf, crs = 102022)






# Round 5 -----------------------------------------------------------------

# Buffers to only unique obs per location to match with gtd
afrob5_buffers2 <- map(.x = afrob5_buffers, ~.x %>% group_by(geoid) %>% tally())

# Identify each buffer that intersects with each GTD event for each spatial window
gtd_buffers5 <- 
  map(.x = afrob5_buffers2, ~st_join(gtd_sf, .x) %>% 
        st_drop_geometry %>% 
        filter(!is.na(geoid)) %>% 
        setDT())

# Create data.table for with month lag indicators; only need 1
afrob5.dt <- afrob5_buffers[[1]] %>% 
  st_drop_geometry() %>%  
  mutate(lag_3 = date %m-% months(3),
         lag_6 = date %m-% months(6),
         lag_12 = date %m-% months(12),
         lag_24 = date %m-% months(24),
         lag_36 = date %m-% months(36),
         lag_48 = date %m-% months(48),
         lag_60 = date %m-% months(60)) %>% 
  setDT()



# Map a data.table join on geoid and conditional on a date window
# There are 7 temporal windows, each mapped through the spatial windows separately 
# So the only thing varying in the 7 functions below is the "lag_" value
# This should all be wrapped into a single function...
afrob5_lag3 <- imap(.x = gtd_buffers5, 
                    ~.x[afrob5.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_3)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob5_buffers[[1]], by = "geoid") %>% 
                      mutate(temporal_window = '3 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob5_lag6 <- imap(.x = gtd_buffers5, 
                    ~.x[afrob5.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_6)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob5_buffers[[1]], by = "geoid") %>% 
                      mutate(temporal_window = '6 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob5_lag12 <- imap(.x = gtd_buffers5, 
                     ~.x[afrob5.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_12)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob5_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '12 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))


afrob5_lag24 <- imap(.x = gtd_buffers5, 
                     ~.x[afrob5.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_24)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob5_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '24 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))

afrob5_lag36 <- imap(.x = gtd_buffers5, 
                     ~.x[afrob5.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_36)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob5_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '36 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))


afrob5_lag48 <- imap(.x = gtd_buffers5, 
                     ~.x[afrob5.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_48)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob5_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '48 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))

afrob5_lag60 <- imap(.x = gtd_buffers5, 
                     ~.x[afrob5.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_60)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob5_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '60 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))

# Bind each list into one tidy frame; this will throw warnings because of the geometry column
attacks_buff <- bind_rows(afrob5_lag3, afrob5_lag6, 
                          afrob5_lag12, afrob5_lag24,
                          afrob5_lag36, afrob5_lag48, 
                          afrob5_lag60) %>% 
  # Cleanup the naming of the spatial and temporal identifiers
  mutate(spatial_window = forcats::fct_inorder(case_when(spatial_window == 1 ~ "5km",
                                                         spatial_window == 2 ~ "10km",
                                                         spatial_window == 3 ~ "25km",
                                                         spatial_window == 4 ~ "50km",
                                                         spatial_window == 5 ~ "100km")),
         temporal_window = forcats::fct_inorder(temporal_window)) %>% 
  # Force back into an sf object
  st_as_sf()


ggplot(data = attacks_buff) +
  geom_histogram(aes(x = attacks), 
                 color = "white",
                 binwidth = 0.5) +
  facet_grid(temporal_window ~ spatial_window) +
  scale_x_continuous(trans = "log", labels = scales::breaks_log()) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95"),
        panel.grid = element_line(color = "gray95")) +
  labs(x = "Terrorist attacks",
       y = "Count",
       caption = "Includes only observations with > 0 attacks within spatio-temporal window (9.19% of total)")


ggplot(data = attacks_buff) +
 geom_boxplot(aes(y = log(attacks), x = forcats::fct_reorder(country_name, attacks), 
                  group = forcats::fct_reorder(country_name, attacks), 
                  color = forcats::fct_reorder(country_name, attacks))) +
  facet_grid(temporal_window ~ spatial_window, scales = "free_y") +
  scale_y_continuous() +
  scale_color_viridis_d(guide = F) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95"),
        panel.grid = element_line(color = "gray95")) +
  labs(x = "Terrorist attacks",
       y = "Count",
       caption = "Includes only observations with > 0 attacks within spatio-temporal window (9.19% of total)") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 4))
ggsave("temp.pdf", height = 12, width = 20, dpi = 500)



ggplot(data = filter(attacks_buff, country_name == "Nigeria")) +
  geom_boxplot(aes(y = attacks, x = temporal_window, 
                   group = temporal_window, 
                   color = temporal_window)) +
  facet_wrap(~spatial_window, nrow = 1) +
  scale_y_continuous(trans = "log", labels = scales::breaks_log()) +
  scale_color_viridis_d(guide = F) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95"),
        panel.grid = element_line(color = "gray95")) +
  labs(x = "",
       y = "Count") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8))
ggsave("temp.pdf", height = 5, width = 8, dpi = 500)
