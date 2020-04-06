library(tidyverse)
library(sf)
library(lme4)
library(arm)
library(cowplot)
# library(furrr)
source(here::here("scripts", "funs.R"))

load(file = here::here("processed", "afrob_tidy_full.Rdata"))
load(here::here("processed", "afrob_sf_buffers.Rdata"))
load(here::here("attacks", "gtd_sf.Rdata"))
load(here::here("attacks", "gtd_agg_full.Rdata"))

# For subsetting to only countries with attacks
countries <- 
  gtd_sf %>% st_drop_geometry() %>% 
  filter(iyear > 2001) %>% 
  group_by(NAME_0) %>% 
  summarise(events = n()) %>% 
  arrange(events) %>% 
  filter(!is.na(NAME_0))

afrob_tidy_full <-
  afrob_tidy_full %>% 
  # filter(NAME_0 %in% countries$NAME_0) %>% 
  mutate(month = lubridate::round_date(date, "month")) %>% 
  left_join(., dplyr::select(gtd_agg_full, NAME_0, NAME_1, 
                             gtd_month, total_attacks_24mos), 
            by = c("NAME_0", "NAME_1", "month" = "gtd_month"))

scaler = function(x) {(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)}

nested <- afrob_tidy_full %>% 
  mutate(Lights = scaler(log1p(lag_nlights_mean)),
         Population = scaler(log1p(lag_pop_gpw_sum)),
         `Capital distance` = scaler(log1p(lag_capdist))) %>% 
  group_by(spatial_window, temporal_window) %>% 
  nest()



# Plot attacks over time at country-level ---------------------------------

basegrid <- expand_grid(NAME_0 = unique(afrob_tidy_full$NAME_0),
                        iyear = seq(1997, 2018),
                        attacks_place = 0)

gtd_sf %>% st_drop_geometry() %>% 
  group_by(NAME_0, iyear) %>% 
  tally(name = "attacks") %>% 
  filter(NAME_0 %in% unique(afrob_tidy_full$NAME_0)) %>% 
  full_join(basegrid) %>% 
  mutate(attacks = case_when(is.na(attacks) ~ attacks_place, TRUE ~ as.numeric(attacks))) %>% 
  filter(!is.na(NAME_0)) %>% 
  ggplot() +
  geom_line(aes(x = iyear, y = attacks, group = NAME_0),
            size = 0.75, color = "gray35") +
  geom_vline(data = (afrob_tidy_full %>% 
                       group_by(NAME_0, round) %>% 
                       summarise(year = min(year, na.rm = T)) %>% 
                       mutate(round = substr(round, 2,2)) %>% 
                       filter(!is.na(NAME_0))),
             aes(xintercept = year, color = factor(round)), 
             alpha = 1, size = 0.5) +
  facet_wrap(~NAME_0, ncol = 6) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(labels = substr(seq(2002, 2018, 4), 3, 4),
                     breaks = seq(2002, 2018, 4),
                     limits = c(2000, 2018)) +
  scale_color_viridis_d(option = "magma", end = 0.9, 
                        guide = guide_legend(title = "Afrobarometer round", 
                                             nrow = 1)) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
        strip.text = element_text(color = "white"),
        panel.background = element_rect(color = "gray20"),
        legend.position = "bottom",
        panel.grid = element_blank()) +
  labs(x = "Year", y = "Terrorist attacks (log10)")
ggsave(filename = here::here("figures", "attacks_surveys_facet.png"),
       dpi = 600, width = 10, height = 8)


# Plot attacks over time at country-level, only attack countries ----------


gtd_sf %>% st_drop_geometry() %>%
  group_by(NAME_0, iyear) %>% 
  tally(name = "attacks") %>% 
  filter(NAME_0 %in% unique(afrob_tidy_full$NAME_0)) %>% 
  full_join(basegrid) %>% 
  mutate(attacks = case_when(is.na(attacks) ~ attacks_place, TRUE ~ as.numeric(attacks))) %>% 
  filter(!is.na(NAME_0)) %>% 
  filter(NAME_0 %in% countries$NAME_0) %>% 
  ggplot() +
  geom_line(aes(x = iyear, y = attacks, group = NAME_0),
            size = 0.75, color = "gray35") +
  geom_vline(data = (afrob_tidy_full %>% 
                       filter(NAME_0 %in% countries$NAME_0) %>% 
                       group_by(NAME_0, round) %>% 
                       summarise(year = min(year, na.rm = T)) %>% 
                       mutate(round = substr(round, 2,2)) %>% 
                       filter(!is.na(NAME_0))),
             aes(xintercept = year, color = factor(round)), 
             alpha = 1, size = 0.5) +
  facet_wrap(~NAME_0, ncol = 6) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(labels = substr(seq(2002, 2018, 4), 3, 4),
                     breaks = seq(2002, 2018, 4),
                     limits = c(2000, 2018)) +
  scale_color_viridis_d(option = "magma", end = 0.9, 
                        guide = guide_legend(title = "Afrobarometer round", 
                                             nrow = 1)) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
        strip.text = element_text(color = "white"),
        panel.background = element_rect(color = "gray20"),
        legend.position = "bottom",
        panel.grid = element_blank()) +
  labs(x = "Year", y = "Terrorist attacks (log10)")
ggsave(filename = here::here("figures", "attacks_surveys_facet_attacksonly.png"),
       dpi = 600, width = 10, height = 8)


# Plot buffer matching example --------------------------------------------

joined0 <- get_africa(level = 0)
joined1 <- get_africa(level = 1)


borno <- 
  afrob_tidy_full %>% 
  filter(NAME_1 == "Borno" & temporal_window == "3 months")

borno1 <- afrob_sf_buffers[[1]] %>% 
  right_join(borno) %>% filter(spatial_window == "10km" & yearintr == 2012 & date > "2012-08-01")

borno2 <- afrob_sf_buffers[[2]] %>% 
  right_join(borno) %>% filter(spatial_window == "25km" & yearintr == 2012 & date > "2012-08-01")

borno3 <- afrob_sf_buffers[[3]] %>% 
  right_join(borno) %>% filter(spatial_window == "50km"& yearintr == 2012 & date > "2012-08-01")

borno4 <- afrob_sf_buffers[[4]] %>% 
  right_join(borno) %>% filter(spatial_window == "100km"& yearintr == 2012 & date > "2012-08-01")


borno1 <- st_transform(borno1, st_crs(joined0)) %>% 
  mutate(attacks = case_when(attacks == 0 ~ NA_real_, TRUE ~ attacks))
borno2 <- st_transform(borno2, st_crs(joined0))  %>% 
  mutate(attacks = case_when(attacks == 0 ~ NA_real_, TRUE ~ attacks))
borno3 <- st_transform(borno3, st_crs(joined0)) %>% 
  mutate(attacks = case_when(attacks == 0 ~ NA_real_, TRUE ~ attacks))
borno4 <- st_transform(borno4, st_crs(joined0)) %>% 
  mutate(attacks = case_when(attacks == 0 ~ NA_real_, TRUE ~ attacks))

temp <- rbind(borno1, borno2, borno3, borno4)

plot_borno <- 
  gtd_sf %>% st_transform(., st_crs(joined0)) %>% 
  filter(country_txt == "Nigeria" & iyear == 2012 & imonth >= 8) %>% 
  ggplot() +
  geom_sf(data = filter(joined0, NAME_0 == "Nigeria"), fill = "#BCAAA4", 
          color = "white") +
  geom_sf(data = temp, 
          aes(fill = attacks), color = "white") + 
  facet_wrap(~spatial_window, ncol = 4) +
  geom_sf(aes(color = "Attack"), pch = 4, alpha = 0.25, size = 4, show.legend = "point") +
  coord_sf(xlim = c(12, 15), ylim = c(10.25, 14.25)) +
  scale_fill_viridis_c(option = "magma", begin = 0.9, end = 0.1, 
                       # alpha = 0.8,
                       na.value = "gray90", trans = "log10",
                       breaks = c(3, 10, 30), 
                       guide = F) +
  scale_color_manual(values = c("red"), guide = F) +
  # facet_wrap(~) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
        strip.text = element_text(color = "white"),
        panel.background = element_rect(color = "gray20"),
        legend.position = "right",
        panel.grid = element_line(color = "gray95"),
        axis.text = element_blank())


plot_leg <- 
  gtd_sf %>% st_transform(., st_crs(joined0)) %>% 
  filter(country_txt == "Nigeria" & iyear == 2012 & imonth >= 8) %>% 
  ggplot() +
  geom_sf(data = filter(joined0, NAME_0 == "Nigeria"), fill = "#607D8B", 
          color = "white") +
  geom_sf(data = temp, 
          aes(fill = attacks), color = "white") + 
  facet_wrap(~spatial_window, ncol = 4) +
  geom_sf(aes(color = "Attack"), pch = 4, alpha = 0.25, size = 4, show.legend = "point") +
  coord_sf(xlim = c(12, 15), ylim = c(10.25, 14.25)) +
  scale_fill_viridis_c(option = "magma", begin = 0.9, end = 0.1, 
                       # alpha = 0.8,
                       na.value = "gray75", trans = "log10",
                       breaks = c(3, 10, 30), 
                       guide = guide_colorbar(title = "Total attacks in buffer",
                                              title.position = "top",
                                              order = 2)) +
  scale_color_manual(values = "red", guide = guide_legend(title = "Single attack",
                                                          label = F, order = 1,
                                                          override.aes = list(pch = 4, alpha = 1))) +
  # facet_wrap(~) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
        strip.text = element_text(color = "white"),
        panel.background = element_rect(color = "gray20"),
        legend.position="bottom", 
        legend.box="vertical", 
        legend.margin=margin(),
        panel.grid = element_line(color = "gray95"),
        axis.text = element_blank(),
        legend.title = element_text(size = 8))


plot_nga <- 
  ggplot() +
  geom_sf(data = filter(joined1, NAME_0 == "Nigeria"), 
          size = 0.75,fill = "#D7CCC8", color = "white") +
  geom_sf(data = filter(joined1, NAME_1 == "Borno"), 
          size = 0.75, fill = "#BCAAA4", color = "white") +
  geom_sf_text(data = filter(joined1, NAME_1 == "Borno"), aes(label = NAME_1), 
               color = "white", check_overlap = T, fontface = "bold", size = 2.5) +
  theme_void()


legend <- get_legend(plot_leg)
bottom <- plot_grid(NULL, plot_nga, legend, NULL, ncol = 4)
plot_grid(plot_borno, bottom, ncol = 1)

ggsave(filename = here::here("figures", "borno_example.png"),
       dpi = 600, width = 8, height = 5)




# Percentage exposed within windows ---------------------------------------

afrob_tidy_full <-
  afrob_tidy_full %>% 
  mutate(any_attack = case_when(attacks > 0 ~ 1,
                                attacks == 0 ~ 0,
                                TRUE ~ NA_real_))

# Full sample
afrob_tidy_full %>% 
  group_by(spatial_window, temporal_window) %>% 
  mutate(total = n()) %>% 
  group_by(spatial_window, temporal_window, any_attack, total) %>% 
  tally() %>% ungroup() %>% 
  filter(any_attack == 0) %>% 
  mutate(pct_none = n / total) %>% 
  ggplot() +
  geom_point(aes(x = 1 - pct_none, y = spatial_window),
             size = 2.5) +
  
  geom_text(aes(x = 1 - pct_none, y = spatial_window,
                label = paste0(round(100 * (1 - pct_none), 1), "%")),
            size = 3, check_overlap = T, vjust = 1.75) +
  facet_grid(rows = vars(temporal_window)) +
  scale_x_continuous(labels = scales::percent_format(),
                     limits = c(0, .25)) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
        strip.text = element_text(color = "white"),
        panel.background = element_rect(color = "gray20"),
        legend.position = "bottom",
        panel.grid = element_line(color = "gray95"), 
        axis.title.x = element_blank()) +
  labs(y = "Spatial window", 
       subtitle = "Percentage of respondents exposed to at least 1 terrorist attack")
ggsave(filename = here::here("figures", "percent_exposed.png"),
       dpi = 600, width = 5.5, height = 5)

afrob_tidy_full <-
  afrob_tidy_full %>% 
  mutate(any_attack = case_when(attacks > 0 ~ 1,
                                attacks == 0 ~ 0,
                                TRUE ~ NA_real_))

# Attack countries only
afrob_tidy_full %>% filter(NAME_0 %in% countries$NAME_0) %>% 
  group_by(spatial_window, temporal_window) %>% 
  mutate(total = n()) %>% 
  group_by(spatial_window, temporal_window, any_attack, total) %>% 
  tally() %>% ungroup() %>% 
  filter(any_attack == 0) %>% 
  mutate(pct_none = n / total) %>% 
  ggplot() +
  geom_point(aes(x = 1 - pct_none, y = spatial_window),
             size = 2.5) +
  
  geom_text(aes(x = 1 - pct_none, y = spatial_window,
                label = paste0(round(100 * (1 - pct_none), 1), "%")),
            size = 3, check_overlap = T, vjust = 1.75) +
  facet_grid(rows = vars(temporal_window)) +
  scale_x_continuous(labels = scales::percent_format(),
                     limits = c(0, .25)) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
        strip.text = element_text(color = "white"),
        panel.background = element_rect(color = "gray20"),
        legend.position = "bottom",
        panel.grid = element_line(color = "gray95"), 
        axis.title.x = element_blank()) +
  labs(y = "Spatial window", 
       subtitle = "Percentage of respondents exposed to at least 1 terrorist attack")
ggsave(filename = here::here("figures", "percent_exposed_attacksonly.png"),
       dpi = 600, width = 5.5, height = 5)

# Covariate descriptives --------------------------------------------------


# Need to update with trans
mycovs <- c("DM_fem", "DM_urban", "DM_age", "DM_edu", "WF_nofood", 
            "supragroup", "Lights", "Population")

covs <-
  nested %>% 
  filter(spatial_window == "10km" & temporal_window == "3 months") %>% 
  unnest(cols = c(data)) %>% ungroup() %>% 
  dplyr::select(mycovs)




covs <- covs %>%
  # categorical variables
  mutate(DM_fem = case_when(DM_fem == 1 ~ 0,
                            DM_fem == 2 ~ 1,
                            TRUE ~ NA_real_),
         DM_urban = case_when(DM_urban == 1 ~ 0,
                              DM_urban == 2 ~ 1,
                              TRUE ~ NA_real_),
         Christian = case_when(supragroup == "Christian" ~ 1,
                               TRUE ~ 0),
         Muslim = case_when(supragroup == "Muslim" ~ 1,
                            TRUE ~ 0),
         `Other/None` = case_when(supragroup == "Other/None" ~ 1,
                                  TRUE ~ 0)) %>% 
  dplyr::select(Female = DM_fem,
                Urban = DM_urban,
                Christian, Muslim, `Other/None`, -supragroup,
                Education = DM_edu,
                Lights, Population)


summarise_all(covs, list(~round(mean(., na.rm = T), 3))) %>% 
  pivot_longer(., Female:Population, 
               values_to = "Mean",
               names_to = "Variable") %>% 
  left_join((summarise_all(covs, list(~round(sd(., na.rm = T), 3))) %>% 
               pivot_longer(., Female:Population, 
                            values_to = "SD",
                            names_to = "Variable"))) %>% 
  mutate(Source = c("Afrobarometer", "Afrobarometer", "Afrobarometer", 
                    "Afrobarometer", "Afrobarometer", "Afrobarometer",
                    "DMSP/Prio Grid", "GPW/Prio Grid"),
         Details = c("--", "--", "--", 
                     "--", "--", "Range 0 to 9",
                     "logged, scaled, centered", "logged, scaled, centered")) %>%
  knitr::kable("latex", booktabs = T, 
               caption="\\label{tab:tabvb}Summary statistics for the covariates.",
               align = "c", linesep = "") %>% 
  kableExtra::kable_styling(full_width = F, 
                            latex_options = c("hold_position"), 
                            position = "center",
                            row_label_position = "c") %>% 
  cat(., file = here::here("tables", "cov_sumstats.tex"))
