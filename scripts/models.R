library(tidyverse)
library(sf)
library(estimatr)
source(here::here("scripts", "funs.R"))

load(file = here::here("processed", "afrob_tidy.Rdata"))

nested <- afrob_tidy %>% 
  group_by(spatial_window, temporal_window) %>% 
  nest()



# Base model funs ------------------------------------------------------------

# Map models
est_mods <- function(dv){
  
  myform <- formula(paste0(dv,"~", "log1p(attacks) + factor(DM_fem) + factor(DM_urban) + 
               DM_age + DM_edu + WF_nofood + factor(round) + factor(country)"))
  
  
  nested %>% 
    mutate(out = map(data, ~lm(myform, 
                               data = .x)),
           outtab = map(out, ~ broom::tidy(.x)),
           mycoef = map(outtab, ~filter(.x, term == "log1p(attacks)") %>% select(estimate)),
           myse = map(outtab, ~filter(.x, term == "log1p(attacks)") %>% select(std.error)))
  }


plot_coefs <- function(data, dv, title = NULL, subtitle = NULL) {
  
  
  
  ggplot(data = data) +
    geom_hline(yintercept = 0, color = "darkred") +
    geom_pointrange(aes(x = spatial_window, y = estimate, ymin = estimate - 1.96 * std.error,
                        ymax = estimate + 1.96 * std.error),
                    size = 0.75, fatten = 2) +
    facet_wrap(~temporal_window, nrow = 1) +
    theme_minimal() +
    theme(strip.background = element_rect(fill = "gray95", color = "gray95"),
          panel.grid = element_line(color = "gray95")) +
    labs(x = "Spatial Window",
         y = "Coefficient",
         title = title,
         subtitle = subtitle, 
         caption = paste0("Round(s) ",
                          paste(substr(unique(data$data[[1]]$round[!is.na(data$data[[1]][dv])]), 2,2), 
                                collapse = ", ")))
  }




# Descriptives ------------------------------------------------------------

ggplot(data = afrob_tidy[afrob_tidy$attacks != 0,]) +
  geom_histogram(aes(x = log1p(attacks)), 
                 color = "white",
                 binwidth = 0.5) +
  facet_grid(temporal_window ~ spatial_window) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray95", color = "gray95"),
        panel.grid = element_line(color = "gray95")) +
  labs(x = "Terrorist attacks, logged + 1",
       y = "Count")



# Economic perceptions  -------------------------------------------------------

# Present living conditions
est_mods(dv = "WF_livcon") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "WF_livcon",
             title = "Own present living conditions",
             subtitle = "1 (very bad) to 5 (very good)") %>% 
  ggsave(., filename = here::here("figures", "living_conditions_coefs.png"),
         dpi = 600, width = 8, height = 5)



# Present living conditions
est_mods(dv = "WF_livconothr") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "WF_livconothr",
             title = "Own present living conditions compared to others",
             subtitle = "1 (very bad) to 5 (very good)") %>% 
  ggsave(., filename = here::here("figures", "living_conditions_vothers_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Perceptions economic conditions of country
est_mods(dv = "PSP_econ") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PSP_econ",
             title = "Country's current economic conditions",
             subtitle = "1 (very bad) to 5 (very good)") %>% 
  ggsave(., filename = here::here("figures", "country_econ_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Perceptions economic conditions of country versus 1 year ago
est_mods(dv = "PSP_econ1yrago") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PSP_econ1yrago",
             title = "Country's current economic conditions versus 1 year ago",
             subtitle = "1 (much worse) to 5 (much better)") %>% 
  ggsave(., filename = here::here("figures", "country_econ_lastyear_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Perceptions economic conditions of country versus 1 year into future
est_mods(dv = "PSP_econin1yr") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PSP_econin1yr",
             title = "Country's current economic conditions versus in 1 year",
             subtitle = "1 (much worse) to 5 (much better)") %>% 
  ggsave(., filename = here::here("figures", "country_econ_nextyear_coefs.png"),
         dpi = 600, width = 8, height = 5)




# Democracy ---------------------------------------------------------------

# Support democracy
est_mods(dv = "DA_supdem2") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "DA_supdem2",
             title = "Support for democracy",
             subtitle = "1 (it doesn't matter), 2 (democracy sometimes preferable), 3 (democracy always preferable)") %>% 
  ggsave(., filename = here::here("figures", "support_democracy_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Extent of democracy
est_mods(dv = "DA_demext") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "DA_demext",
             title = "Extent of democracy",
             subtitle = "1 (not a democracy) to 4 (full democracy)") %>% 
  ggsave(., filename = here::here("figures", "extent_democracy_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Satisfaction with democracy
est_mods(dv = "SD_demhappy") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "SD_demhappy",
             title = "Satisfaction with democracy",
             subtitle = "0 (not a democracy) to 4 (very satisfied)") %>% 
  ggsave(., filename = here::here("figures", "satisfaction_democracy_coefs.png"),
         dpi = 600, width = 8, height = 5)



# Trust -------------------------------------------------------------------

# President
est_mods(dv = "PT_trustpres") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustpres",
             title = "Trust president",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_president_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Parliament
est_mods(dv = "PT_trustparl") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustparl",
             title = "Trust parliament",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_parliament_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Electoral commission
est_mods(dv = "PT_trustelecom") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustelecom",
             title = "Trust electoral commission",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_electoral_commission_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Tax department
est_mods(dv = "PT_trusttax") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trusttax",
             title = "Trust tax department",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_tax_dep_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Municipal/District assemply
est_mods(dv = "PT_trustcouncil") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustcouncil",
             title = "Trust municipal/district assembly",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_assembly_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Ruling party
est_mods(dv = "PT_trustrlparty") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustrlparty",
             title = "Trust ruling party",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_ruling_party_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Opposition party
est_mods(dv = "PT_trustopprty") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustopprty",
             title = "Trust opposition ruling party",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_opposition_party_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Police
est_mods(dv = "PT_trustpolice") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustpolice",
             title = "Trust police",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_police_coefs.png"),
         dpi = 600, width = 8, height = 5)



# Army
est_mods(dv = "PT_trustarmy") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustarmy",
             title = "Trust army",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_army_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Courts
est_mods(dv = "PT_trustcourt") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustcourt",
             title = "Trust courts",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_courts_coefs.png"),
         dpi = 600, width = 8, height = 5)

# Religious leaders
est_mods(dv = "PT_trustrel") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_trustrel",
             title = "Trust religious leaders",
             subtitle = "0 (not at all) to 3 (somewhat)") %>% 
  ggsave(., filename = here::here("figures", "trust_rel_coefs.png"),
         dpi = 600, width = 8, height = 5)




# Corruption --------------------------------------------------------------

# President
est_mods(dv = "PT_crptpres") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_crptpres",
             title = "Corruption: president and officials in his office",
             subtitle = "0 (none) to 3 (all)") %>% 
  ggsave(., filename = here::here("figures", "corruption_president_coefs.png"),
         dpi = 600, width = 8, height = 5)

# Parliament
est_mods(dv = "PT_crptparl") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_crptparl",
             title = "Corruption: members of parliament",
             subtitle = "0 (none) to 3 (all)") %>% 
  ggsave(., filename = here::here("figures", "corruption_parliament_coefs.png"),
         dpi = 600, width = 8, height = 5)

# Government officials
est_mods(dv = "PT_crptgov") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_crptgov",
             title = "Corruption: government officials",
             subtitle = "0 (none) to 3 (all)") %>% 
  ggsave(., filename = here::here("figures", "corruption_govofficials_coefs.png"),
         dpi = 600, width = 8, height = 5)

# Local government councilors
est_mods(dv = "PT_crptcouncil") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_crptcouncil",
             title = "Corruption: local government councilors",
             subtitle = "0 (none) to 3 (all)") %>% 
  ggsave(., filename = here::here("figures", "corruption_govofficials_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Police
est_mods(dv = "PT_crptpolice") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_crptpolice",
             title = "Corruption: police",
             subtitle = "0 (none) to 3 (all)") %>% 
  ggsave(., filename = here::here("figures", "corruption_police_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Tax officials
est_mods(dv = "PT_crptax") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_crptax",
             title = "Corruption: tax officials",
             subtitle = "0 (none) to 3 (all)") %>% 
  ggsave(., filename = here::here("figures", "corruption_tax_coefs.png"),
         dpi = 600, width = 8, height = 5)

# Judges
est_mods(dv = "PT_crptjudge") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_crptjudge",
             title = "Corruption: judges and magistrates",
             subtitle = "0 (none) to 3 (all)") %>% 
  ggsave(., filename = here::here("figures", "corruption_judges_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Religious leaders
est_mods(dv = "PT_crptrel") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "PT_crptpres",
             title = "Corruption: religious leaders",
             subtitle = "0 (none) to 3 (all)") %>% 
  ggsave(., filename = here::here("figures", "corruption_religious_coefs.png"),
         dpi = 600, width = 8, height = 5)




# Religion ----------------------------------------------------------------

# Religious practice
est_mods(dv = "rel_attend") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "rel_attend",
             title = "Engage in religious practice",
             subtitle = "0 (never) to 6 (multiple times per day)") %>% 
  ggsave(., filename = here::here("figures", "religion_attend_coefs.png"),
         dpi = 600, width = 8, height = 5)

# Religious membership
est_mods(dv = "rel_member") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "rel_member",
             title = "Member of religious group",
             subtitle = "0 (no), 1 (inactive member), 2 (active member), 3 (leader)") %>% 
  ggsave(., filename = here::here("figures", "religion_membership_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Religious importance
est_mods(dv = "rel_important") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "rel_important",
             title = "Importance of religion in your life",
             subtitle = "1 (not at all) to 4 (very)") %>% 
  ggsave(., filename = here::here("figures", "religion_importance_coefs.png"),
         dpi = 600, width = 8, height = 5)



# Governance --------------------------------------------------------------

# One party rule
est_mods(dv = "rule_oneparty") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "rule_oneparty",
             title = "Approval of one-party rule",
             subtitle = "1 (strongly disapprove) to 5 (strongly approve)") %>% 
  ggsave(., filename = here::here("figures", "rule_oneparty_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Military rule
est_mods(dv = "rule_military") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "rule_military",
             title = "Approval of military rule",
             subtitle = "1 (strongly disapprove) to 5 (strongly approve)") %>% 
  ggsave(., filename = here::here("figures", "rule_military_coefs.png"),
         dpi = 600, width = 8, height = 5)


# One-man rule
est_mods(dv = "rule_oneman") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "rule_oneman",
             title = "Approval of one-man rule",
             subtitle = "1 (strongly disapprove) to 5 (strongly approve)") %>% 
  ggsave(., filename = here::here("figures", "rule_oneman_coefs.png"),
         dpi = 600, width = 8, height = 5)



# Ethnic versus national and neighbors ----------------------------------------

est_mods(dv = "eth_nat_num") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "eth_nat_num",
             title = "Ethnic versus national identity",
             subtitle = "1 (only ethnicity) to 5 (only national identity)") %>% 
  ggsave(., filename = here::here("figures", "ethnic_national_coefs.png"),
         dpi = 600, width = 8, height = 5)

est_mods(dv = "neighbor_religion") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "neighbor_religion",
             title = "Live next to neighbor of different religion",
             subtitle = "1 (strongly dislike) to 5 (strongly like)") %>% 
  ggsave(., filename = here::here("figures", "neighbor_religion_coefs.png"),
         dpi = 600, width = 8, height = 5)

est_mods(dv = "neighbor_ethnicity") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "neighbor_ethnicity",
             title = "Live next to neighbor of different ethnicity",
             subtitle = "1 (strongly dislike) to 5 (strongly like)") %>% 
  ggsave(., filename = here::here("figures", "neighbor_ethnicity_coefs.png"),
         dpi = 600, width = 8, height = 5)


est_mods(dv = "neighbor_homosexual") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "neighbor_homosexual",
             title = "Live next to homosexual neighbor",
             subtitle = "1 (strongly dislike) to 5 (strongly like)") %>% 
  ggsave(., filename = here::here("figures", "neighbor_homosexual_coefs.png"),
         dpi = 600, width = 8, height = 5)


# est_mods(dv = "neighbor_hiv") %>% 
#   unnest(c(mycoef, myse)) %>% 
#   plot_coefs(data = ., 
#              dv = "neighbor_hiv",
#              title = "Live next to neighbor with HIV/AIDs",
#              subtitle = "1 (strongly dislike) to 5 (strongly like)") %>% 
#   ggsave(., filename = here::here("figures", "neighbor_homosexual_coefs.png"),
#          dpi = 600, width = 8, height = 5)


est_mods(dv = "neighbor_foreign") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "neighbor_foreign",
             title = "Live next to immigrant/foreign worker neighbor",
             subtitle = "1 (strongly dislike) to 5 (strongly like)") %>% 
  ggsave(., filename = here::here("figures", "neighbor_foreign_coefs.png"),
         dpi = 600, width = 8, height = 5)


# Political participation -------------------------------------------------

# Voted in last election
est_mods(dv = "voted_last_election") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "voted_last_election",
             title = "Voted in most recent national elections",
             subtitle = "0 (no) to 1 (yes)") %>% 
  ggsave(., filename = here::here("figures", "voted_coefs.png"),
         dpi = 600, width = 8, height = 5)


# How likely raise issue with others
est_mods(dv = "raise_issue_community") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "raise_issue_community",
             title = "Join others to raise an issue",
             subtitle = "0 (no, and would never) to 4 (yes, often)") %>% 
  ggsave(., filename = here::here("figures", "raise_issue_coefs.png"),
         dpi = 600, width = 8, height = 5)


# How likely to protest
est_mods(dv = "protest") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "protest",
             title = "Protest",
             subtitle = "0 (no, and would never) to 4 (yes, often)") %>% 
  ggsave(., filename = here::here("figures", "protest.png"),
         dpi = 600, width = 8, height = 5)







# Security ----------------------------------------------------------------

# Fear crime in home
est_mods(dv = "fear_crime") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "fear_crime",
             title = "Fear crime in home",
             subtitle = "0 (never) to 4 (always)") %>% 
  ggsave(., filename = here::here("figures", "fear_crime.png"),
         dpi = 600, width = 8, height = 5)


# Feel unsafe in neighborhood
est_mods(dv = "feel_unsafe") %>% 
  unnest(c(mycoef, myse)) %>% 
  plot_coefs(data = ., 
             dv = "feel_unsafe",
             title = "Feel unsafe in neighborhood",
             subtitle = "0 (never) to 4 (always)") %>% 
  ggsave(., filename = here::here("figures", "feel_unsafe.png"),
         dpi = 600, width = 8, height = 5)







