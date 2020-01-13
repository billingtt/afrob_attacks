library(tidyverse)
library(sf)
source(here::here("scripts", "funs.R"))


# Round 5 -----------------------------------------------------------------

afrob5 <- haven::read_dta(here::here("afrob", "Merged R5.fulldata.dta"))

### Country identifiers ###

# From the codebook

# 1=Algeria, 2=Benin, 3=Botswana, 4=Burkina Faso, 5=Burundi, 6=Cameroon, 7=Cape Verde,
# 8=Cote d’Ivoire, 9=Egypt, 11=Ghana, 12=Guinea, 13=Kenya, 14=Lesotho, 15=Liberia, 16=Madagascar,
# 17=Malawi, 18=Mali, 19=Mauritius, 20=Morocco, 21=Mozambique, 22=Namibia, 23=Niger, 24=Nigeria,
# 25=Senegal, 26=Sierra Leone, 27=South Africa, 28=Sudan, 29=Swaziland, 30=Tanzania, 31=Togo, 32=Tunisia,
# 33=Uganda, 34=Zambia, 35=Zimbabwe

# Ethiopia is missing country ids in the data; add back in as #10
afrob5 <- afrob5 %>% 
  mutate(country_alpha = case_when(substr(respno, 1,3) == "ETH" ~ 10, TRUE ~ country_alpha))

namer <- tibble(country_name = c("Algeria", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", 
                                 "Côte d'Ivoire", "Egypt", "Ghana", "Guinea", "Kenya", "Lesotho", "Liberia", "Madagascar",
                                 "Malawi", "Mali", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", 
                                 "Senegal", "Sierra Leone", "South Africa", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", 
                                 "Uganda", "Zambia", "Zimbabwe", "Ethiopia"),
                country_alpha = c(1,2,3,4,5,6,7,
                                  8,9,11,12,13,14,15,16,
                                  17,18,19, 20,21,22,23,24,
                                  25,26,27,28,29,30,31,32,
                                  33,34,35,10))

afrob5 <-
  afrob5 %>% 
  left_join(namer, by = c("country_alpha")) %>% 
  select(country_name, everything())


# Check with front 3 of respno
unique(paste(afrob5$country_name, substr(afrob5$respno, 1, 3))) # all match

### Date ###
# 2011, 2012, 2013
table(stringr::str_sub(afrob5$dateintr, start = -2, -1))


# Round 5
afrob5 <-
  afrob5 %>% 
  mutate(newdate = stringr::str_pad(dateintr, 9, pad = "0"), # add leading zero
         year = as.numeric(paste0("20", stringr::str_sub(dateintr, -2, -1))),
         month = stringr::str_sub(newdate, 4, 6),
         day = stringr::str_sub(newdate, 1, 2),
         date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>% 
  select(country_name, date, day, month, year, dateintr, everything(), -newdate)



### Location ###
joined1 <- get_africa(level = 1) %>% 
  select(NAME_0, NAME_1, HASC_1)

# Convert to sf
proj <- sf::st_crs(joined1)
afrob5_sf <- afrob5 %>% 
  mutate(geoid = paste(longitude, latitude, sep = ", ")) %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = proj)

class(afrob5_sf)

# Match to ADM1s
afrob5_sf <- st_join(afrob5_sf, joined1) %>% 
  select(country_name, year, NAME_0, NAME_1, HASC_1, everything())

save(afrob5_sf, file = here::here("afrob", "afrob5_sf.Rdata"))




# Round 6 -----------------------------------------------------------------

afrob6 <- read_csv(here::here("afrob", "afrob6.csv"))


table(afrob6$country, afrob6$country_r5list)


# Figure out country names

# 1=Algeria, 2=Benin, 3=Botswana, 4=Burkina Faso, 5=Burundi, 6=Cameroon, 7=Cape Verde,
# 8=Cote d'Ivoire, 9=Egypt, 10=Gabon, 11=Ghana, 12=Guinea, 13=Kenya, 14=Lesotho, 15=Liberia,
# 16=Madagascar, 17=Malawi, 18=Mali, 19=Mauritius, 20=Morocco, 21=Mozambique, 22=Namibia, 23=Niger,
# 24=Nigeria, 25=São Tomé and Príncipe, 26=Senegal, 27=Sierra Leone, 28=South Africa, 29=Sudan,
# 30=Swaziland, 31=Tanzania, 32=Togo, 33=Tunisia, 34=Uganda, 35=Zambia, 36=Zimbabwe

namer6 <- tibble(country_name = c("Algeria", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde",
                                  "Cote d'Ivoire", "Egypt", "Gabon", "Ghana", "Guinea", "Kenya", "Lesotho", "Liberia",
                                  "Madagascar", "Malawi", "Mali", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger",
                                  "Nigeria", "São Tomé and Príncipe", "Senegal", "Sierra Leone", "South Africa", "Sudan",
                                  "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"),
                 country = seq(1,36,1))


afrob6 <-
  afrob6 %>% 
  left_join(namer6, by = c("country")) %>% 
  select(country_name, everything())


# Check with front 3 of respno
unique(paste(afrob6$country_name, substr(afrob6$respno, 1, 3))) # all match


### Date ###
# 2014, 2015
table(stringr::str_sub(afrob6$dateintr, start = -2, -1))

afrob6 <-
  afrob6 %>% 
  mutate(newdate = stringr::str_pad(dateintr, 9, pad = "0"), # add leading zero
         year = as.numeric(paste0("20", stringr::str_sub(dateintr, -2, -1))),
         month = stringr::str_sub(newdate, 4, 6),
         day = stringr::str_sub(newdate, 1, 2),
         date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>% 
  select(country_name, date, day, month, year, dateintr, everything(), -newdate)


### Location ###
joined1 <- get_africa(level = 1) %>% 
  select(NAME_0, NAME_1, HASC_1)

# Convert to sf
proj <- sf::st_crs(joined1)
afrob6_sf <- afrob6 %>%
  mutate(geoid = paste(longitude, latitude, sep = ", ")) %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = proj)

class(afrob6_sf)

# Match to ADM1s
afrob6_sf <- st_join(afrob6_sf, joined1) %>% 
  select(country_name, year, NAME_0, NAME_1, HASC_1, everything())

save(afrob6_sf, file = here::here("afrob", "afrob6_sf.Rdata"))