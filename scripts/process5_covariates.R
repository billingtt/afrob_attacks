library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(sf)
library(readr)

# Load buffered afrob buffered points
load(file = here::here("processed", "afrob_sf_buffers.Rdata"))

# prio grid
grid <- read_sf(here::here("prio", "priogrid_cell.shp"))

# yearly data
yearly <- read_csv(here::here("prio", "yearly.csv"))
yearly <- yearly %>% group_by(gid) %>% arrange(gid, year) %>% 
  mutate(year = year + 1) %>% 
  rename_at(vars(agri_ih:water_ih), list(~paste0("lag_",.)))

yearly %>% dplyr::select(lag_pop_gpw_sum, year)



# Fill years forward
t15 <- yearly %>% filter(year == 2014) %>% 
  mutate(year = 2015)
t16 <- yearly %>% filter(year == 2014) %>% 
  mutate(year = 2016)
t17 <- yearly %>% filter(year == 2014) %>% 
  mutate(year = 2017)
t18 <- yearly %>% filter(year == 2014) %>% 
  mutate(year = 2018)
yearly <- bind_rows(yearly, t15, t16, t17, t18)

# fill gpw forward
yearly <- 
  yearly %>% group_by(gid) %>% 
  tidyr::fill(lag_pop_gpw_sum) %>% 
  ungroup()

# static
static <- read_csv(here::here("prio", "static.csv"))

# join afrob clusters to prio grid
grid <- st_transform(grid, st_crs(afrob_sf_buffers[[1]]))
buffers_grid <- st_join(afrob_sf_buffers[[1]], grid)

# join to prio data
buffers_grid2 <-
  buffers_grid %>% 
  mutate(year = lubridate::year(date)) %>% 
  left_join(static, by = c("gid", "xcoord", "ycoord", "col", "row")) %>% 
  left_join(yearly, by = c("gid", "year")) %>% 
  dplyr::select(-year)


buffers_grid3 <-
  buffers_grid2 %>% st_drop_geometry() %>% 
  group_by(geoid, date, n) %>% 
  summarise_all(., list(~mean(., na.rm = T)))


# join to afrob tidy
load(file = here::here("processed", "afrob_tidy.Rdata"))

afrob_tidy_full <-
  afrob_tidy %>% mutate(year = lubridate::year(date)) %>% 
  left_join(., buffers_grid3, by = c("geoid", "date"))

save(afrob_tidy_full, file = here::here("processed", "afrob_tidy_full.Rdata"))


