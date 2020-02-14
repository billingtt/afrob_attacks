library(tidyverse)
library(sf)
source(here::here("scripts", "funs.R"))



# Load merged geocoded data -----------------------------------------------
load(here::here("processed", "afrob_merge.Rdata"))

afrob_merge <-
  afrob_merge %>% 
  rename(date = ymd) %>% 
  select(country, date, everything())


### Location ###
joined1 <- get_africa(level = 1) %>% 
  select(NAME_0, NAME_1, HASC_1)

# Convert to sf
proj <- sf::st_crs(joined1)
afrob_merge <- afrob_merge %>% 
  mutate(geoid = paste(ylong, xlat, sep = ", ")) %>% 
  filter(!is.na(ylong)) %>% 
  st_as_sf(., coords = c("ylong", "xlat"),
           crs = proj)


# Match to ADM1s
afrob_merge_sf <- st_join(afrob_merge, joined1) %>% 
  select(country, date, NAME_0, NAME_1, HASC_1, everything())

save(afrob_merge_sf, file = here::here("processed", "afrob_merge_sf.Rdata"))



