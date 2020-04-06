library(tidyverse)
library(sf)


# Load data ---------------------------------------------------------------
joined1 <- get_africa(level = 1)
load(file = here::here("attacks", "gtd_sf.Rdata"))



# Spatial join and aggregate to adm1-month counts -------------------------
gtd_sf <- gtd_sf %>% 
  mutate(gtd_date = lubridate::ymd(paste(iyear, imonth, iday, sep = "-"))) %>% 
  filter(iyear > 1997)

gtd_agg <- 
  gtd_sf %>% 
  dplyr::select(country_txt, gtd_date) %>% 
  st_join(., joined1) %>% 
  st_drop_geometry() %>% 
  group_by(NAME_0, NAME_1, gtd_date) %>% 
  count()

gtd_agg <- 
  gtd_agg %>% 
  mutate(gtd_month = lubridate::round_date(gtd_date, "month")) %>% 
  group_by(NAME_0, NAME_1, gtd_month) %>% 
  summarise(attacks_agg = sum(n))

gtd_agg_full <-
  expand_grid(id = paste(joined1$NAME_0, joined1$NAME_1, sep = ";"),
            gtd_month = seq(lubridate::ymd("1998-01-01"),
                            lubridate::ymd("2018-12-01"),
                            by = "month")) %>% 
  mutate(NAME_0 = stringr::str_split(id, pattern = ";", simplify = T)[,1],
         NAME_1 = stringr::str_split(id, pattern = ";", simplify = T)[,2]) %>% 
  dplyr::select(-id) %>% 
  left_join(., gtd_agg) %>% 
  mutate(attacks_agg = case_when(is.na(attacks_agg) ~ as.numeric(0), TRUE ~ as.numeric(attacks_agg)))


# Create lags -------------------------------------------------------------

### From Romain Francois https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/ ###

lags <- function(var, n){
  library(rlang)
  var <- enquo(var)
  
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("lag_%s_%02d", quo_text(var), indices))
}            


gtd_agg_full <- 
  gtd_agg_full %>% 
  arrange(NAME_0, NAME_1, gtd_month) %>% 
  group_by(NAME_0, NAME_1) %>% 
  mutate(!!!lags(attacks_agg, 36))

gtd_agg_full$total_attacks_24mos <- 
  gtd_agg_full %>% 
  ungroup() %>% 
  dplyr::select(lag_attacks_agg_13:lag_attacks_agg_24) %>% 
  rowSums(na.rm = T)
  

save(gtd_agg_full, file = here::here("attacks", "gtd_agg_full.Rdata"))
  
