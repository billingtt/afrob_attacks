library(tidyverse)
library(sf)
source(here::here("scripts", "funs.R"))

# Afrob round 5
load(here::here("afrob", "afrob5_sf.Rdata"))

# Afrob round 6
load(here::here("afrob", "afrob6_sf.Rdata"))

# ADM1s in Africa
joined1 <- get_africa(level = 1)
joined0 <- get_africa(level = 0)



# Re-project and buffer ---------------------------------------------------

# Albers projection in Africa is 102022
joined1 <- st_transform(joined1, crs = 102022)
joined0 <- st_transform(joined0, crs = 102022)
afrob5_sf <- st_transform(afrob5_sf, crs = 102022)
afrob6_sf <- st_transform(afrob6_sf, crs = 102022)

# 25km buffers for every point
afrob5_25km <- st_buffer(afrob5_sf, 25000)
afrob6_25km <- st_buffer(afrob6_sf, 25000)

# 50km buffers for every point
afrob5_50km <- st_buffer(afrob5_sf, 50000)
afrob6_50km <- st_buffer(afrob6_sf, 50000)

# 100km buffers for every point
afrob5_100km <- st_buffer(afrob5_sf, 100000)
afrob6_100km <- st_buffer(afrob6_sf, 100000)


# Plot both rounds 50km
p1 <- ggplot(data = afrob5_50km) +
  geom_sf(data = joined0, color = "white", fill = "gray90") +
  geom_sf(aes(color = country_name), show.legend = "point", alpha = 0.5) +
  scale_color_viridis_d(guide = F) +
  # facet_wrap(~round) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95"))

p2 <- ggplot(data = afrob6_50km) +
  geom_sf(data = joined0, color = "white", fill = "gray90") +
  geom_sf(aes(color = country_name), show.legend = "point", alpha = 0.5) +
  scale_color_viridis_d(guide = F) +
  # facet_wrap(~round) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95"))

# gridExtra::grid.arrange(p1, p2) # slow to render

p3 <- ggplot(data = filter(afrob5_50km, country_name == "Senegal")) +
  geom_sf(data = filter(joined1, NAME_0 == "Senegal"), 
          color = "white", fill = "gray90") +
  geom_sf(aes(color = country_name), 
          show.legend = "point", alpha = 0.5, fill = "transparent") +
  scale_color_viridis_d(guide = F) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95")) +
  labs(title = "50km buffers")

p4 <- ggplot(data = filter(afrob5_25km, country_name == "Senegal")) +
  geom_sf(data = filter(joined1, NAME_0 == "Senegal"), 
          color = "white", fill = "gray90") +
  geom_sf(aes(color = country_name), 
          show.legend = "point", alpha = 0.5, fill = "transparent") +
  scale_color_viridis_d(guide = F) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95")) +
  labs(title = "25km buffers")


p5 <- ggplot(data = filter(afrob5_100km, country_name == "Senegal")) +
  geom_sf(data = filter(joined1, NAME_0 == "Senegal"), 
          color = "white", fill = "gray90") +
  geom_sf(aes(color = country_name), 
          show.legend = "point", alpha = 0.5, fill = "transparent") +
  scale_color_viridis_d(guide = F) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95")) +
  labs(title = "100km buffers")


gridExtra::grid.arrange(p4,p3,p5, ncol = 3)



