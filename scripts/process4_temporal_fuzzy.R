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
load(file = here::here("processed", "afrob_sf_buffers.Rdata"))

# These are lists of each unique point-date, by buffer size
head(afrob_sf_buffers[[1]])

# Load gtd points; add date and subset to shrink the data
load(file = here::here("attacks", "gtd_sf.Rdata"))
gtd_sf <- gtd_sf %>% 
  mutate(gtd_date = lubridate::ymd(paste(iyear, imonth, iday, sep = "-"))) %>% 
  filter(iyear > 1997)

# Match up projections (albers africa) for spatial join
gtd_sf <- st_transform(gtd_sf, crs = 102022)



# GTD to buffers ----------------------------------------------------------

# Buffers to only unique obs per location to match with gtd
afrob_sf_buffers2 <- map(.x = afrob_sf_buffers, ~.x %>% 
                           group_by(geoid) %>% 
                           tally())

# Identify each buffer that intersects with each GTD event for each spatial window
# This includes any overlap between an event and a buffer, regardless of timing
gtd_buffers <- 
  map(.x = afrob_sf_buffers2, ~st_join(gtd_sf, .x) %>% 
        st_drop_geometry %>% 
        filter(!is.na(geoid)) %>% 
        setDT())

# Create data.table for month lag indicators; only need 1
# Not sure why this is so slow
afrob.dt <- afrob_sf_buffers[[1]] %>% 
  st_drop_geometry() %>%  
  mutate(lag_3 = date %m-% months(3),
         lag_6 = date %m-% months(6),
         lag_12 = date %m-% months(12),
         # lag_24 = date %m-% months(24),
         # lag_36 = date %m-% months(36),
         # lag_48 = date %m-% months(48),
         # lag_60 = date %m-% months(60),
         date2 = date) %>% 
  setDT()



# Map a data.table join on geoid and conditional on a date window
# There are 7 temporal windows, each mapped through the spatial windows separately 
# So the only thing varying in the 7 functions below is the "lag_" value
# This should all be wrapped into a single function...

afrob_lag3 <- imap(.x = gtd_buffers, 
                   ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_3)] %>% 
                     filter(!is.na(eventid)) %>% 
                     group_by(geoid, date) %>% 
                     tally(name = "attacks") %>%
                     full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                     mutate(temporal_window = '3 months',
                            spatial_window = .y,
                            attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag6 <- imap(.x = gtd_buffers, 
                    ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_6)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid, date) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                      mutate(temporal_window = '6 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob_lag12 <- imap(.x = gtd_buffers, 
                     ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_12)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid, date) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
                       mutate(temporal_window = '12 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))


# afrob_lag24 <- imap(.x = gtd_buffers, 
#                      ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_24)] %>% 
#                        filter(!is.na(eventid)) %>% 
#                        group_by(geoid, date) %>% 
#                        tally(name = "attacks") %>%
#                        full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
#                        mutate(temporal_window = '24 months',
#                               spatial_window = .y,
#                               attacks = ifelse(is.na(attacks), 0, attacks)))
# 
# afrob_lag36 <- imap(.x = gtd_buffers, 
#                      ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_36)] %>% 
#                        filter(!is.na(eventid)) %>% 
#                        group_by(geoid, date) %>% 
#                        tally(name = "attacks") %>%
#                        full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
#                        mutate(temporal_window = '36 months',
#                               spatial_window = .y,
#                               attacks = ifelse(is.na(attacks), 0, attacks)))
# 
# 
# afrob_lag48 <- imap(.x = gtd_buffers, 
#                      ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_48)] %>% 
#                        filter(!is.na(eventid)) %>% 
#                        group_by(geoid, date) %>% 
#                        tally(name = "attacks") %>%
#                        full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
#                        mutate(temporal_window = '48 months',
#                               spatial_window = .y,
#                               attacks = ifelse(is.na(attacks), 0, attacks)))
# 
# afrob_lag60 <- imap(.x = gtd_buffers, 
#                      ~.x[afrob.dt, on = .(geoid, gtd_date < date2, gtd_date >= lag_60)] %>% 
#                        filter(!is.na(eventid)) %>% 
#                        group_by(geoid, date) %>% 
#                        tally(name = "attacks") %>%
#                        full_join(afrob_sf_buffers[[1]], by = c("geoid", "date")) %>% 
#                        mutate(temporal_window = '60 months',
#                               spatial_window = .y,
#                               attacks = ifelse(is.na(attacks), 0, attacks)))

# Bind each list into one tidy frame; this will throw warnings because of the geometry column
attacks_buff <- bind_rows(afrob_lag3, 
                          afrob_lag6, 
                          afrob_lag12) %>% 
                          # afrob_lag24,
                          # afrob_lag36, 
                          # afrob_lag48, 
                          # afrob_lag60
                          # ) %>% 
  # Cleanup the naming of the spatial and temporal identifiers
  mutate(spatial_window = forcats::fct_inorder(case_when(spatial_window == 1 ~ "10km",
                                                         spatial_window == 2 ~ "25km",
                                                         spatial_window == 3 ~ "50km",
                                                         spatial_window == 4 ~ "100km")),
         temporal_window = forcats::fct_inorder(temporal_window)) %>% 
  # Force back into an sf object
  st_as_sf()


afrob_merge_sf$geometry <- NULL
attacks_buff$geometry <- NULL
afrob_tidy <- afrob_merge_sf %>% 
  # filter(!is.na(date) & date != "1582-10-13") %>% 
  # st_drop_geometry() %>% 
  left_join(., attacks_buff, by = c("geoid", "date")) %>% 
  select(country, date, temporal_window, spatial_window, attacks, everything())


save(afrob_tidy, file = here::here("processed", "afrob_tidy.Rdata"))
