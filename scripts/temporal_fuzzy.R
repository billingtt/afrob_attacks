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
attacks_buff5 <- bind_rows(afrob5_lag3, afrob5_lag6, 
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



# Round 6 -----------------------------------------------------------------

# Buffers to only unique obs per location to match with gtd
afrob6_buffers2 <- map(.x = afrob6_buffers, ~.x %>% group_by(geoid) %>% tally())

# Identify each buffer that intersects with each GTD event for each spatial window
gtd_buffers6 <- 
  map(.x = afrob6_buffers2, ~st_join(gtd_sf, .x) %>% 
        st_drop_geometry %>% 
        filter(!is.na(geoid)) %>% 
        setDT())

# Create data.table for with month lag indicators; only need 1
afrob6.dt <- afrob6_buffers[[1]] %>% 
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
afrob6_lag3 <- imap(.x = gtd_buffers6, 
                    ~.x[afrob6.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_3)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob6_buffers[[1]], by = "geoid") %>% 
                      mutate(temporal_window = '3 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob6_lag6 <- imap(.x = gtd_buffers6, 
                    ~.x[afrob6.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_6)] %>% 
                      filter(!is.na(eventid)) %>% 
                      group_by(geoid) %>% 
                      tally(name = "attacks") %>%
                      full_join(afrob6_buffers[[1]], by = "geoid") %>% 
                      mutate(temporal_window = '6 months',
                             spatial_window = .y,
                             attacks = ifelse(is.na(attacks), 0, attacks)))

afrob6_lag12 <- imap(.x = gtd_buffers6, 
                     ~.x[afrob6.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_12)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob6_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '12 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))


afrob6_lag24 <- imap(.x = gtd_buffers6, 
                     ~.x[afrob6.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_24)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob6_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '24 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))

afrob6_lag36 <- imap(.x = gtd_buffers6, 
                     ~.x[afrob6.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_36)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob6_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '36 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))


afrob6_lag48 <- imap(.x = gtd_buffers6, 
                     ~.x[afrob6.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_48)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob6_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '48 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))

afrob6_lag60 <- imap(.x = gtd_buffers6, 
                     ~.x[afrob6.dt, on = .(geoid, gtd_date < date, gtd_date >= lag_60)] %>% 
                       filter(!is.na(eventid)) %>% 
                       group_by(geoid) %>% 
                       tally(name = "attacks") %>%
                       full_join(afrob6_buffers[[1]], by = "geoid") %>% 
                       mutate(temporal_window = '60 months',
                              spatial_window = .y,
                              attacks = ifelse(is.na(attacks), 0, attacks)))

# Bind each list into one tidy frame; this will throw warnings because of the geometry column
attacks_buff6 <- bind_rows(afrob6_lag3, afrob6_lag6, 
                          afrob6_lag12, afrob6_lag24,
                          afrob6_lag36, afrob6_lag48, 
                          afrob6_lag60) %>% 
  # Cleanup the naming of the spatial and temporal identifiers
  mutate(spatial_window = forcats::fct_inorder(case_when(spatial_window == 1 ~ "5km",
                                                         spatial_window == 2 ~ "10km",
                                                         spatial_window == 3 ~ "25km",
                                                         spatial_window == 4 ~ "50km",
                                                         spatial_window == 5 ~ "100km")),
         temporal_window = forcats::fct_inorder(temporal_window)) %>% 
  # Force back into an sf object
  st_as_sf()



attacks_buff <- list(attacks_buff5, attacks_buff6)

save(attacks_buff, file = here::here("processed", "attacks_buff.Rdata"))
