library(tidyverse)
library(sf)
source(here::here("scripts", "funs.R"))

joined0 <- get_africa(level = 0) %>% 
  select(NAME_0)


# Data --------------------------------------------------------------------

# Afrob round 5
load(here::here("afrob", "afrob5_sf.Rdata"))

# Afrob round 6
load(here::here("afrob", "afrob6_sf.Rdata"))

# GTD
load(here::here("attacks", "gtd_sf.Rdata"))



# Aggregate GTD to ADM1-year -----------------------------------------------

gtd_agg <-
  gtd_sf %>% rename(year = iyear) %>% 
  group_by(NAME_0, NAME_1, year) %>% 
  tally()


ggplot() +
  geom_sf(data = joined0, fill = "gray95", color = "white", size = 1) +
  geom_sf(data = filter(gtd_agg, year > 2009 & year < 2013), 
          aes(size = n), 
          color = "darkred", alpha = 0.5,
          show.legend = "point") +
  theme_bw() +
  facet_wrap(~year) +
  theme(strip.background = element_rect(fill = "gray95"))

# Sahel only

sahel <- c("Nigeria", "Niger", "Mali", "Burkina Faso", "Chad")

ggplot() +
  geom_sf(data = filter(joined0, NAME_0 %in% sahel), fill = "gray95", color = "white", size = 1) +
  geom_sf(data = filter(gtd_agg, year > 2006 & year < 2013 & NAME_0 %in% sahel), 
          aes(size = n, color = NAME_0), 
          alpha = 0.5,
          show.legend = "point") +
  theme_bw() +
  facet_wrap(~year) +
  theme(strip.background = element_rect(fill = "gray95")) +
  scale_color_viridis_d(option = "magma", end = 0.75, guide = F) +
  scale_size_continuous(guide = guide_legend(title = "Number of attacks"))


# Rolling counts

afrob5_sf <- 
  afrob5_sf %>% 
  mutate(myid = paste(NAME_0, NAME_1, sep = "_"))

base <- expand_grid(myid = unique(afrob5_sf$myid),
                    year = seq(1997, 2018))


gtd_agg_roll <- 
  gtd_agg %>% 
  mutate(myid = paste(NAME_0, NAME_1, sep = "_")) %>% 
  full_join(base) %>% 
  arrange(myid, year) %>% ungroup() %>% 
  mutate(n = case_when(is.na(n) ~ 0, TRUE ~ as.numeric(n)),
         NAME_0 = str_split(myid, pattern = "_")[[1]][1],
         NAME_1 = str_split(myid, pattern = "_")[[1]][2]) %>% 
  arrange(year) %>% 
  group_by(NAME_1, NAME_0) %>% 
  mutate(attack_sum1 = zoo::rollapply(n, 1, sum, align = "right", fill = NA),
         attack_sum2 = zoo::rollapply(n, 2, sum, align = "right", fill = NA),
         attack_sum3 = zoo::rollapply(n, 3, sum, align = "right", fill = NA),
         attack_sum4 = zoo::rollapply(n, 4, sum, align = "right", fill = NA),
         attack_sum5 = zoo::rollapply(n, 5, sum, align = "right", fill = NA)) %>% 
  arrange(NAME_1) %>% 
  rename(attacks = n) %>% 
  ungroup() %>% 
  select(myid, year, everything(), -NAME_1, -NAME_0)
gtd_agg_roll$geometry <- NULL

# Join to afrob
afrob5_attacks <-
  afrob5_sf %>% 
  full_join(gtd_agg_roll, by = c("myid", "year")) %>% 
  select(myid, NAME_0, country_name, NAME_1, HASC_1, year, starts_with("attack"), everything()) %>% 
  filter(!is.na(NAME_1))



# Religious questions -----------------------------------------------------

# Membership
table(afrob5_attacks$q25a)

# 0=Not a Member, 1=Inactive member, 2=Active member, 3=Official leader, 9=Don’t know,

afrob5_attacks <-
  afrob5_attacks %>% 
  mutate(rel_member_cat = case_when(q25a == -1 ~ "Missing",
                                q25a == 0 ~ "Not a member",
                                q25a == 1 ~ "Inactive member",
                                q25a == 2 ~ "Active member",
                                q25a == 3 ~ "Official leader",
                                q25a == 9 ~ "Don't know"),
         rel_member_dum = case_when(rel_member_cat %in% c("Active member", "Official leader") ~ 1,
                                    rel_member_cat %in% c("Not a member", "Inactive member") ~ 0,
                                    TRUE ~ NA_real_
                                    ))

member_reg <-
  afrob5_attacks %>% 
  group_by(NAME_0) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(rel_member_dum ~ log1p(attack_sum5), data = .x)),
         tidied = map(mod, ~broom::tidy(.x))) %>% 
  unnest(tidied)


ggplot(filter(member_reg, term != "(Intercept)")) +  
  geom_hline(yintercept = 0, color = "darkred", size = 1) +
  geom_pointrange(aes(y = estimate, x = forcats::fct_reorder(NAME_0, estimate),
                      ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  color = "gray25", fatten = 3, size = 1) +
  facet_wrap(~term) +
  coord_flip() +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray25", color = "gray25", size = 1.5),
        strip.text = element_text(color = "white", face = "bold"),
        # panel.background = element_rect(color = "gray25"),
        panel.grid = element_line(color = "gray97"),
        axis.title.y = element_blank())


  
# How important is religion in your life?
# Q98B
# 1=Not at all important, 2=Not very important, 3=Somewhat important, 4=Very important,
# 8=Refused to answer, 9=Don’t know, -1=Missing

afrob5_attacks <-
  afrob5_attacks %>% 
  mutate(rel_importance = case_when(q98b %in% c(8,9,-1) ~ NA_real_,
                                    TRUE ~ q98b))
imp_reg <-
  afrob5_attacks %>% 
  group_by(NAME_0) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(rel_importance ~ log1p(attack_sum5), data = .x)),
         tidied = map(mod, ~broom::tidy(.x))) %>% 
  unnest(tidied)


ggplot(filter(imp_reg, term != "(Intercept)")) +  
  geom_hline(yintercept = 0, color = "darkred", size = 1) +
  geom_pointrange(aes(y = estimate, x = forcats::fct_reorder(NAME_0, estimate),
                      ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error)) +
  facet_wrap(~term) +
  coord_flip() +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray25", color = "gray25", size = 1.5),
        strip.text = element_text(color = "white", face = "bold"),
        # panel.background = element_rect(color = "gray25"),
        panel.grid = element_line(color = "gray97"),
        axis.title.y = element_blank())



# How important is religion in your life?
# Q98B
# 1=Not at all important, 2=Not very important, 3=Somewhat important, 4=Very important,
# 8=Refused to answer, 9=Don’t know, -1=Missing

afrob5_attacks <-
  afrob5_attacks %>% 
  mutate(rel_importance = case_when(q98b %in% c(8,9,-1) ~ NA_real_,
                                    TRUE ~ q98b))
imp_reg <-
  afrob5_attacks %>% 
  group_by(NAME_0) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(rel_importance ~ log1p(attack_sum5), data = .x)),
         tidied = map(mod, ~broom::tidy(.x))) %>% 
  unnest(tidied)


ggplot(filter(imp_reg, term != "(Intercept)")) +  
  geom_hline(yintercept = 0, color = "darkred", size = 1) +
  geom_pointrange(aes(y = estimate, x = forcats::fct_reorder(NAME_0, estimate),
                      ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error)) +
  facet_wrap(~term) +
  coord_flip() +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray25", color = "gray25", size = 1.5),
        strip.text = element_text(color = "white", face = "bold"),
        # panel.background = element_rect(color = "gray25"),
        panel.grid = element_line(color = "gray97"),
        axis.title.y = element_blank())






# Round 6 -----------------------------------------------------------------


afrob6_sf <- 
  afrob6_sf %>% 
  mutate(myid = paste(NAME_0, NAME_1, sep = "_"))

base6 <- expand_grid(myid = unique(afrob6_sf$myid),
                    year = seq(1997, 2018))


gtd_agg_roll <- 
  gtd_agg %>% 
  mutate(myid = paste(NAME_0, NAME_1, sep = "_")) %>% 
  full_join(base6) %>% 
  arrange(myid, year) %>% ungroup() %>% 
  mutate(n = case_when(is.na(n) ~ 0, TRUE ~ as.numeric(n)),
         NAME_0 = str_split(myid, pattern = "_")[[1]][1],
         NAME_1 = str_split(myid, pattern = "_")[[1]][2]) %>% 
  arrange(year) %>% 
  group_by(NAME_1, NAME_0) %>% 
  mutate(attack_sum1 = zoo::rollapply(n, 1, sum, align = "right", fill = NA),
         attack_sum2 = zoo::rollapply(n, 2, sum, align = "right", fill = NA),
         attack_sum3 = zoo::rollapply(n, 3, sum, align = "right", fill = NA),
         attack_sum4 = zoo::rollapply(n, 4, sum, align = "right", fill = NA),
         attack_sum5 = zoo::rollapply(n, 5, sum, align = "right", fill = NA)) %>% 
  arrange(NAME_1) %>% 
  rename(attacks = n) %>% 
  ungroup() %>% 
  select(myid, year, everything(), -NAME_1, -NAME_0)
gtd_agg_roll$geometry <- NULL

# Join to afrob
afrob6_attacks <-
  afrob6_sf %>% 
  full_join(gtd_agg_roll, by = c("myid", "year")) %>% 
  select(myid, NAME_0, country_name, NAME_1, HASC_1, year, starts_with("attack"), everything()) %>% 
  filter(!is.na(NAME_1))


afrob6_sum <-
  afrob6_attacks %>% filter(!is.na(NAME_1)) %>% 
  group_by(place_name, country_name) %>% 
  summarise_at(vars(starts_with("attack"), 
                    direction_country:rel_practice), 
               .funs = ~mean(., na.rm = T))

ggplot(data = filter(afrob6_sum, !is.na(place_name) & country_name %in% sahel)) +
  # geom_hline(yintercept = 3, color = "darkred", size = 0.75, alpha = 0.8) +
  geom_point(aes(x = forcats::fct_reorder(place_name, rel_practice), 
                 y = rel_practice, group = place_name)) +
  coord_flip() +
  facet_wrap(~country_name, scales = "free_y") +
  labs(y = "Religious attendance\n0 = never/no religion | 6 = multiple per day") +
  theme_minimal() +
  theme(axis.title.y = element_blank()) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray25", color = "gray25", size = 1.5),
        strip.text = element_text(color = "white", face = "bold"),
        # panel.background = element_rect(color = "gray25"),
        panel.grid = element_line(color = "gray97"),
        axis.title.y = element_blank())



ggplot(data = filter(afrob6_sum, !is.na(NAME_1) & country_name %in% sahel)) +
  # geom_hline(yintercept = 3, color = "darkred", size = 0.75, alpha = 0.8) +
  geom_point(aes(x = attack_sum1, 
                 y = rel_practice, group = NAME_1)) +
  facet_wrap(~country_name, scales = "free_y") +
  labs(y = "Religious attendance\n0 = never/no religion | 6 = multiple per day") +
  theme_minimal() +
  theme() +
  theme(strip.background = element_rect(fill = "gray25", color = "gray25", size = 1.5),
        strip.text = element_text(color = "white", face = "bold"),
        # panel.background = element_rect(color = "gray25"),
        panel.grid = element_line(color = "gray97"),
        axis.title.y = element_blank())

