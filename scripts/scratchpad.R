library(tidyverse)
library(sf)



# Data --------------------------------------------------------------------

# Afrob round 5
afrob5 <- haven::read_dta(here::here("afrob", "Merged R5.fulldata.dta"))

# Match up name ids from codebook

# 1=Algeria, 2=Benin, 3=Botswana, 4=Burkina Faso, 5=Burundi, 6=Cameroon, 7=Cape Verde,
# 8=Cote d’Ivoire, 9=Egypt, 11=Ghana, 12=Guinea, 13=Kenya, 14=Lesotho, 15=Liberia, 16=Madagascar,
# 17=Malawi, 18=Mali, 19=Mauritius, 20=Morocco, 21=Mozambique, 22=Namibia, 23=Niger, 24=Nigeria,
# 25=Senegal, 26=Sierra Leone, 27=South Africa, 28=Sudan, 29=Swaziland, 30=Tanzania, 31=Togo, 32=Tunisia,
# 33=Uganda, 34=Zambia, 35=Zimbabwe

# Ethiopia is missing country ids in the data; add back in as # 10

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


# GTD
gtd <- readxl::read_excel(here::here("attacks", "globalterrorismdb_0919dist.xlsx"),
                        sheet  = 1) %>% 
  filter(region_txt == "Sub-Saharan Africa" & iyear > 2000)

# All countries in Africa
afnames <- raster::ccodes() %>% 
  filter(continent == "Africa") %>% 
  dplyr::select(NAME)
temp1 <- vector("list", 0)
for(i in afnames$NAME) {
  print(i)
  temp1[[i]] <- raster::getData(name = "GADM", 
                                country = i, 
                                download = T, 
                                level = 1, path = here::here("shapefiles"))
}

temp0 <- vector("list", 0)
for(i in afnames$NAME) {
  print(i)
  temp0[[i]] <- raster::getData(name = "GADM", 
                                country = i, 
                                download = T, 
                                level = 0, path = here::here("shapefiles"))
}


# Join into sf frame
joined1 <- list(temp1, makeUniqueIDs = T) %>% 
  purrr::flatten() %>% 
  do.call(rbind, .) %>% 
  st_as_sf()

joined0 <- list(temp0, makeUniqueIDs = T) %>% 
  purrr::flatten() %>% 
  do.call(rbind, .) %>% 
  st_as_sf()

ggplot(data = joined1) +
  geom_sf() +
  theme_bw()

ggplot(data = joined0) +
  geom_sf(color = "white") +
  geom_sf(data = filter(joined0, NAME_0 %in% namer$country_name),
          fill = "steelblue3", color = "white") +
  theme_bw()



head(afrob5$locationlevel1)
head(afrob5$locationlevel2)
head(afrob5$locationlevel3)

proj <- sf::st_crs(joined1)
nigeria <- afrob5 %>% filter(country_name == "Nigeria") %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
         crs = proj)


ggplot() +
  geom_sf(data = filter(joined1, NAME_0 == "Nigeria")) +
  geom_sf(data = nigeria, alpha = 0.1, size = 4) +
  theme_minimal()


afrob5 %>% filter(country_name == "Nigeria") %>% 
  group_by(longitude, latitude) %>% 
  tally() %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = proj) -> test

ggplot() +
  geom_sf(data = filter(joined1, NAME_0 == "Nigeria")) +
  geom_sf(data = test, aes(fill = n), pch = 21, 
          color = "white", size = 4, alpha = 0.5) +
  theme_minimal() +
  scale_fill_viridis_c()

# to buffer correctly, need to st_transform and use correct units
test2 <- st_buffer(test, dist = 0.5)

ggplot() +
  geom_sf(data = filter(joined1, NAME_0 == "Nigeria")) +
  geom_sf(data = test2, aes(fill = n), 
          pch = 21, alpha = 0.1) +
  theme_minimal() +
  scale_fill_viridis_c()


head(gtd$country_txt)

ngi_gtd <- gtd %>% 
  filter(country_txt == "Nigeria" & iyear > 2007 & iyear < 2013) %>% 
  group_by(longitude, latitude) %>%
  tally() %>% 
  filter(!is.na(longitude)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = proj)


ggplot() +
  geom_sf(data = filter(joined1, NAME_0 == "Nigeria"),
          fill = "transparent", color = "gray40", size = 1) +
  geom_sf(data = test2,  
          pch = 21, alpha = 0.1) +
  geom_sf(data = ngi_gtd,
          aes(size = n),
          color = "darkred", alpha = 0.5,
          show.legend = "point") +
  theme_minimal() +
  scale_fill_viridis_c() +
  scale_size_continuous(breaks = c(1,10,20,50,100))





# Test over with adm0 -----------------------------------------------------

# sapply(st_intersects(x,y), function(z) if (length(z)==0) NA_integer_ else z[1])
# st_join(pts, poly, join = st_intersects)

afrob5 %>% filter(country_name == "Nigeria") %>% 
  group_by(longitude, latitude) %>% 
  # tally() %>% 
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = proj) -> test

test3 <- st_join(test, joined1[joined1$NAME_0=="Nigeria",])
table(test3$region, test3$NAME_1)

