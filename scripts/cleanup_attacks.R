library(tidyverse)
library(sf)
# library(meltt)
source(here::here("scripts", "funs.R"))


# ACLED
acled <- read_csv(here::here("attacks", "acled_africa.csv"))
table(acled$event_type)


# GTD ---------------------------------------------------------------------
gtd <- readxl::read_excel(here::here("attacks", "globalterrorismdb_0919dist.xlsx"),
                          sheet  = 1) %>% 
  filter(region_txt == "Sub-Saharan Africa" & iyear > 1996)

head(gtd)

### Location ###
joined1 <- get_africa(level = 1) %>% 
  select(NAME_0, NAME_1, HASC_1)

# Convert to sf
proj <- sf::st_crs(joined1)
gtd_sf <- gtd %>% 
  filter(!is.na(longitude)) %>% # cannot have missing coords
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = proj)
class(gtd_sf)

# Match to ADM1s
gtd_sf <- st_join(gtd_sf, joined1) %>% 
  select(country_txt, iyear, NAME_0, NAME_1, HASC_1, everything())

save(gtd_sf, file = here::here("attacks", "gtd_sf.Rdata"))






