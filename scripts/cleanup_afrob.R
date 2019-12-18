library(tidyverse)
library(sf)
source(here::here("scripts", "funs.R"))


# Data --------------------------------------------------------------------

# Afrob round 5
afrob5 <- haven::read_dta(here::here("afrob", "Merged R5.fulldata.dta"))

# Afrob round 6
afrob6 <- read_csv(here::here("afrob", "afrob6.csv"))

# ADM1s in Africa
joined1 <- get_africa(level = 1)
joined0 <- get_africa(level = 0)


# Country identifiers -----------------------------------------------------

### Round 5 ###

# From codebook
# 1=Algeria, 2=Benin, 3=Botswana, 4=Burkina Faso, 5=Burundi, 6=Cameroon, 7=Cape Verde,
# 8=Cote d’Ivoire, 9=Egypt, 11=Ghana, 12=Guinea, 13=Kenya, 14=Lesotho, 15=Liberia, 16=Madagascar,
# 17=Malawi, 18=Mali, 19=Mauritius, 20=Morocco, 21=Mozambique, 22=Namibia, 23=Niger, 24=Nigeria,
# 25=Senegal, 26=Sierra Leone, 27=South Africa, 28=Sudan, 29=Swaziland, 30=Tanzania, 31=Togo, 32=Tunisia,
# 33=Uganda, 34=Zambia, 35=Zimbabwe

# Ethiopia is missing country ids in the data; add back in as #10 for now
afrob5 <- afrob5 %>% 
  mutate(country_alpha = case_when(substr(respno, 1,3) == "ETH" ~ 10, 
                                   TRUE ~ country_alpha))

namer5 <- tibble(country_name = c("Algeria", "Benin", "Botswana", 
                                  "Burkina Faso", "Burundi", "Cameroon", 
                                  "Cape Verde", "Côte d'Ivoire", "Egypt", 
                                  "Ghana", "Guinea", "Kenya", "Lesotho", 
                                  "Liberia", "Madagascar", "Malawi", "Mali", 
                                  "Mauritius", "Morocco", "Mozambique", 
                                  "Namibia", "Niger", "Nigeria", "Senegal", 
                                  "Sierra Leone", "South Africa", "Sudan", 
                                  "Swaziland", "Tanzania", "Togo", "Tunisia", 
                                 "Uganda", "Zambia", "Zimbabwe", "Ethiopia"),
                country_alpha = c(1,2,3,4,5,6,7,
                                  8,9,11,12,13,14,15,16,
                                  17,18,19, 20,21,22,23,24,
                                  25,26,27,28,29,30,31,32,
                                  33,34,35,10))



# Every observation in Ethiopia is coded to the same location
# Join in names and drop Ethiopia
afrob5 <-
  afrob5 %>% 
  left_join(namer5, by = c("country_alpha")) %>% 
  select(country_name, everything()) %>% 
  filter(country_name != "Ethiopia")

# Check with front 3 of respno
unique(paste(afrob5$country_name, substr(afrob5$respno, 1, 3))) # all match


### Round 6 ###

# From codebook
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


# Time --------------------------------------------------------------------

# Round 5
afrob5 <-
  afrob5 %>% 
  mutate(newdate = stringr::str_pad(dateintr, 9, pad = "0"), # add leading zero
         year = as.numeric(paste0("20", stringr::str_sub(dateintr, -2, -1))),
         month = stringr::str_sub(newdate, 4, 6),
         day = stringr::str_sub(newdate, 1, 2),
         date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>% 
  select(country_name, date, day, month, year, dateintr, everything(), -newdate)

afrob5 %>% 
  group_by(country_name, date) %>% 
  tally() %>% 
  ggplot() +
  geom_line(aes(y = n, x = date)) +
  facet_wrap(~country_name)

afrob5 %>% filter(country_name == "Zambia") %>% 
  arrange(date)


# Round 6
afrob6 <-
  afrob6 %>% 
  mutate(newdate = stringr::str_pad(dateintr, 9, pad = "0"), # add leading zero
         year = as.numeric(paste0("20", stringr::str_sub(dateintr, -2, -1))),
         month = stringr::str_sub(newdate, 4, 6),
         day = stringr::str_sub(newdate, 1, 2),
         date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>% 
  select(country_name, date, day, month, year, dateintr, everything(), -newdate)

afrob6 %>% 
  group_by(country_name, date) %>% 
  tally() %>% 
  ggplot() +
  geom_line(aes(y = n, x = date)) +
  facet_wrap(~country_name)



# Location ----------------------------------------------------------------

# To create buffers, need to use correct units for distance 
# Using Albers projection, which uses meters as unit
# For some reason, need to set projection as 4326 and the transform to albers instead of straight to albers

# ADM1s in Africa
joined1 <- get_africa(level = 1)
joined0 <- get_africa(level = 0)
baseproj <- st_crs(joined0)

afrob5_sf <- afrob5 %>% 
  mutate(geoid = paste(longitude, latitude, sep = ", ")) %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = baseproj)

afrob6_sf <- afrob6 %>%
  mutate(geoid = paste(longitude, latitude, sep = ", ")) %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = baseproj)

# Albers projection in Africa is 102022
joined1 <- st_transform(joined1, crs = 102022)
joined0 <- st_transform(joined0, crs = 102022)
afrob5_sf <- st_transform(afrob5_sf, crs = 102022)
afrob6_sf <- st_transform(afrob6_sf, crs = 102022)

# Map through 5km, 10km, 25km, 50,000km, 100,000km buffers

afrob5_sf_unique <- afrob5_sf %>% 
  group_by(geoid) %>% 
  tally() 

afrob5_buffers <- map(.x = c(5000, 10000, 25000, 50000, 100000), 
                      ~st_buffer(afrob5_sf_unique, dist = .x))

afrob6_sf_unique <- afrob6_sf %>% 
  group_by(geoid) %>% 
  tally() 

afrob6_buffers <- map(.x = c(5000, 10000, 25000, 50000, 100000), 
                      ~st_buffer(afrob6_sf_unique, dist = .x))



base <- expand_grid(geoid = afrob6_buffers[[1]]$geoid,
                    date = seq(as.Date("2005-01-01"), as.Date("2015-01-01"), by = "day"))



# Some examples

load(file = here::here("attacks", "gtd_sf.Rdata"))

ggplot() +
  geom_sf(data = filter(joined1, NAME_1 == "Abia"), 
          color = "white", fill = "gray90") +
  geom_sf(data = filter(afrob6_100km, NAME_1 == "Abia"),
          aes(color = "100km"), 
          show.legend = "point", 
          alpha = 0.5, 
          fill = "transparent") +
  geom_sf(data = filter(afrob6_50km, NAME_1 == "Abia"),
          aes(color = "50km"), 
          show.legend = "point", 
          alpha = 0.5, 
          fill = "transparent") +
  geom_sf(data = filter(afrob6_25km, NAME_1 == "Abia"),
          aes(color = "25km"), 
          show.legend = "point", 
          alpha = 0.5, 
          fill = "transparent") +
  geom_sf(data = filter(gtd_sf, NAME_1 == "Abia" & iyear > 2008 & iyear < 2013),
          color = "red",
          shape = 3,
          alpha = 1) +
  scale_color_viridis_d(guide = guide_legend(title = "Buffer size")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Abia State, Nigeria", sutitle = "Afrobarometer round 5 locations")

p1 <- ggplot() +
  geom_sf(data = filter(joined1, NAME_0 == "Nigeria"), 
          color = "white", fill = "gray90") +
  geom_sf(data = filter(afrob6_100km, NAME_0 == "Nigeria"),
          aes(color = "100km"),
          show.legend = "point",
          alpha = 0.5,
          fill = "transparent") +
  geom_sf(data = filter(afrob6_50km, NAME_0 == "Nigeria"),
          aes(color = "50km"),
          show.legend = "point",
          alpha = 0.5,
          fill = "transparent") +
  geom_sf(data = (gtd_sf %>% 
                    filter(NAME_0 == "Nigeria" & iyear > 2008 & iyear < 2013) %>% 
                    mutate(geoid = as.character(geometry)) %>% 
                    group_by(geoid) %>% 
                    tally()),
          aes(size = n),
          color = "darkred",
          shape = 16,
          alpha = 0.8) +
  scale_color_viridis_d(option = "magma", guide = guide_legend(title = "Buffer size"), end = 0.75) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_size(range = c(3,10), guide = F) +
  labs(title = "Nigeria", subtitle = "Afrobarometer round 6 locations and GTD events in previous 5 years")


p2 <- ggplot() +
  geom_sf(data = filter(joined1, NAME_1 == "Taraba"), 
          color = "white", fill = "gray90") +
  geom_sf(data = filter(afrob6_50km, NAME_1 == "Taraba"),
          aes(color = "50km"),
          show.legend = "point",
          alpha = 0.5,
          fill = "transparent") +
  geom_sf(data = filter(afrob6_100km, NAME_1 == "Taraba"),
          aes(color = "100km"),
          show.legend = "point",
          alpha = 0.5,
          fill = "transparent") +
  geom_sf(data = (gtd_sf %>% 
                    filter(NAME_1 == "Taraba"& iyear > 2008 & iyear < 2013) %>% 
                    mutate(geoid = as.character(geometry)) %>% 
                    group_by(geoid) %>% 
                    tally()),
          aes(size = n),
          color = "darkred",
          shape = 16,
          alpha = .8,
          show.legend = "point") +
  scale_size(range = c(3,10), guide = guide_legend(title = "# of GTD events")) +
  scale_color_viridis_d(option = "magma", guide = F, end = 0.75) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "", subtitle = "Taraba State") 


gridExtra::grid.arrange(p1, p2, ncol = 2)
ggsave(here::here("buffers_eg.png"), height = 7, width = 12, dpi= 800,
       gridExtra::arrangeGrob(p1, p2, ncol = 2))





