library(tidyverse)
library(sf)
library(lme4)
library(arm)
library(cowplot)
# library(furrr)
source(here::here("scripts", "funs.R"))

load(file = here::here("processed", "afrob_tidy_full.Rdata"))
load(here::here("attacks", "gtd_agg_full.Rdata"))
load(here::here("attacks", "gtd_sf.Rdata"))

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



# Democracy base models ---------------------------------------------------

lmer_ester <- function(data, dv){
  
  myform <- formula(
    paste0(dv,"~", "log1p(attacks) + factor(DM_fem) + factor(DM_urban) + DM_age + DM_edu + WF_nofood + factor(supragroup) + factor(round) + Lights + Population + (1|NAME_1) + (1|country)")
  )
  
  
  data %>% 
    mutate(
      # Estimate models
      out = map(data, ~lmer(myform, data = .x)),
      
      
      # Pull out attack coefs and ses
      outtab = map(out, ~broom::tidy(.x)),
      mycoef = map(outtab, ~filter(.x, term == "log1p(attacks)") %>% dplyr::select(estimate)),
      myse = map(outtab, ~filter(.x, term == "log1p(attacks)") %>% dplyr::select(std.error)))
  
}

base_out <-
  tibble(outcome = c("DA_supdem2", "DA_demext", "SD_demhappy")) %>% 
  mutate(model = map(outcome, ~lmer_ester(data = nested, dv = .x)))


# Plot coefs
(p1 <- 
    base_out$model %>% 
    pluck(1) %>% 
    unnest(c(mycoef, myse)) %>% 
    plot_coefs(data = ., 
               dv = "DA_supdem2",
               # iv = "log(Attacks + 1)",
               title = "Support for democracy",
               subtitle = "1 (it doesn't matter), 2 (democracy sometimes preferable), 3 (democracy always preferable)"))


(p2 <- 
    base_out$model %>% 
    pluck(2) %>%  
    unnest(c(mycoef, myse)) %>% 
    plot_coefs(data = ., 
               dv = "DA_demext",
               # iv = "log(Attacks + 1)",
               title = "Extent of democracy",
               subtitle = "1 (not a democracy) to 4 (full democracy)"))


(p3 <- 
    base_out$model %>% 
    pluck(3) %>% 
    unnest(c(mycoef, myse)) %>% 
    plot_coefs(data = ., 
               dv = "SD_demhappy",
               # iv = "log(Attacks + 1)",
               title = "Satisfaction with democracy",
               subtitle = "0 (not a democracy) to 4 (very satisfied)"))


pgrid1 <- plot_grid(p1, p2, p3, ncol = 1, align = "hv")
ggsave(pgrid1, filename = here::here("figures", "democracy_panels_coefs.png"),
       dpi = 600, width = 7, height = 9)
