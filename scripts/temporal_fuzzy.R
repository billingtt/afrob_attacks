library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(fuzzyjoin)
source(here::here("scripts", "funs.R"))

load(file = here::here("afrob", "afrob5_buffers.Rdata"))
load(file = here::here("afrob", "afrob6_buffers.Rdata"))
load(file = here::here("attacks", "gtd_sf.Rdata"))
gtd_sf <- st_transform(gtd_sf, crs = 102022)

temp <- afrob5_buffers[[1]] %>% filter(country_name == "Nigeria") %>% 
  ungroup() %>% 
  group_by(geoid) %>% 
  tally()

gtd_temp <- filter(gtd_sf, country_txt == "Nigeria") %>% 
  select(iyear, imonth, iday, eventid)

tempjoin <-
  st_join(gtd_temp, temp) %>% 
  arrange(geoid, iyear, imonth, iday) %>% 
  mutate(gtd_date = lubridate::ymd(paste(iyear, imonth, iday, sep = "-")))
tempjoin$geometry <- NULL


temp2 <- 
  afrob5_buffers[[1]] %>% filter(country_name == "Nigeria") %>% 
  mutate(
    # lag_1 = date %m-% months(1),
    lag_3 = date %m-% months(3),
    # lag_6 = date %m-% months(6),
    # lag_12 = date %m-% months(12),
    # lag_18 = date %m-% months(18),
    # lag_24 = date %m-% months(24),
    # lag_36 = date %m-% months(36),
    # lag_48 = date %m-% months(48),
    # lag_60 = date %m-% months(60)
  ) %>%
  select(geoid, date, starts_with("lag"), everything())


temp3 <- fuzzy_full_join(temp2, tempjoin,
                by = c("geoid" = "geoid",
                       "date" = "gtd_date",
                       "lag_3" = "gtd_date"),
                match_fun = list(`==`, `>`, `<=`)) %>% 
  select(geoid.x, geoid.y, date, lag_3, gtd_date)


fuzzy_left_join(
  df1, df2,
  by = c(
    "category" = "category",
    "date" = "start",
    "date" = "end"
  ),
  match_fun = list(`==`, `>=`, `<=`)
) %>%
  select(id, category = category.x, other_info, date, start, end)



