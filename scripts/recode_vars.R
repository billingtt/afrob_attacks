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




# Round 5 -----------------------------------------------------------------


# Round 6 -----------------------------------------------------------------


afrob6_sf_recoded <- 
  afrob6_sf %>% 
  
  mutate(
    ### Economic conditions etc ###
    
    # Q3 - direct of the country (dummy, my 1 == right direction)
    direction_country = case_when(q3 == 1 ~ 0,
                                  q3 == 2 ~ 1,
                                  TRUE ~ NA_real_),
    
    # Q4A - country's present economic conditions (1-5, low bad high good)
    conditions_country = case_when(q4a %in% c(1:5) ~ q4a,
                                   TRUE ~ NA_real_),
    
    # Q4B - own economic conditions (1-5, low bad high good)
    conditions_own = case_when(q4b %in% c(1:5) ~ q4b,
                               TRUE ~ NA_real_),
    
    # Q5 - own conditions versus others in country
    conditions_own_vcountry = case_when(q5 %in% c(1:5) ~ q5,
                                        TRUE ~ NA_real_),
    
    # Q6 - own conditions versus past 12 months
    conditions_own_vpast = case_when(q6 %in% c(1:5) ~ q6,
                                     TRUE ~ NA_real_),
    
    # Q7 - own conditions versus future
    conditions_own_vfuture = case_when(q7 %in% c(1:5) ~ q7,
                                       TRUE ~ NA_real_),
    
    # Q8A - how often no food in last year (0 = never, 4 = always)
    often_no_food = case_when(q8a %in% c(0:4) ~ q8a,
                              TRUE ~ NA_real_),
    
    # Q8b - how often no clean water in last year (0 = never, 4 = always)
    often_no_water = case_when(q8b %in% c(0:4) ~ q8b,
                               TRUE ~ NA_real_),
    
    # Q8c - how often no medicine/treatment in last year (0 = never, 4 = always)
    often_no_med = case_when(q8c %in% c(0:4) ~ q8c,
                             TRUE ~ NA_real_),
    
    # Q8d - how often no fuel to cook in last year (0 = never, 4 = always)
    often_no_fuel = case_when(q8d %in% c(0:4) ~ q8d,
                              TRUE ~ NA_real_),
    
    # Q8e - how often no cash income in last year (0 = never, 4 = always)
    often_no_case = case_when(q8e %in% c(0:4) ~ q8e,
                              TRUE ~ NA_real_),
    
    # Q9 - how often receive remittances (0 = never, 5 = once per month)
    often_remittances = case_when(q9 %in% c(0:5) ~ q9,
                                  TRUE ~ NA_real_),
    
    # Q10a - unsafe walking in neighborhood (0 = never, 4 = always)
    unsafe_neighborbood = case_when(q10a %in% c(0:4) ~ q10a,
                                    TRUE ~ NA_real_),
    
    # Q10b - fear crime (0 = never, 4 = always)
    fear_crime = case_when(q10b %in% c(0:4) ~ q10b,
                           TRUE ~ NA_real_),
    
    ### Religion ###
    
    # Member of religious group that meets outside of church
    rel_outside_cat = case_when(q19a == -1 ~ "Missing",
                                q19a == 0 ~ "Not a member",
                                q19a == 1 ~ "Inactive member",
                                q19a == 2 ~ "Active member",
                                q19a == 3 ~ "Official leader",
                                q19a == 9 ~ "Don't know"),
    # rel_outside_dum = case_when(rel_member_cat %in% c("Active member", "Official leader") ~ 1,
    #                             rel_member_cat %in% c("Not a member", "Inactive member") ~ 0,
    #                             TRUE ~ NA_real_
    # ),
    
    # Trust religious leaders
    rel_trust_leaders = case_when(q52l %in% c(-1, 9) ~ NA_real_,
                                  TRUE ~ q52l),
    
    # Religious practice
    rel_practice = case_when(q98b %in% c(0,1,2,3,4,5,6) ~ q98b,
                             q98b == 7 ~ 0,
                             q98b %in% c(-1, 9) ~ NA_real_),
    
    # Have neighbors of different religion 1 = strong dislike 5 = strong like
    neighbors_rel = case_when(q89a %in% c(1:5) ~ q89a, TRUE ~ NA_real_),
    
    
    ### Identity ###
    
    # How often ethnic group treated unfairly
    eth_often_unfair = case_when(q88a %in% c(0:3) ~ q88a,
                                 TRUE ~ NA_real_),
    
    # Ethnic versus national (1 only ethnic, 5 only national)
    eth_nat = case_when(q88b %in% c(1:5) ~ q88b,
                        TRUE ~ NA_real_),
    
    # Have neighbors of different ethnicity 1 = strong dislike 5 = strong like
    neighbors_eth = case_when(q89b %in% c(1:5) ~ q89b, TRUE ~ NA_real_),
    
    # Have homosexual neighbors 1 = strong dislike 5 = strong like
    neighbors_homosexual = case_when(q89c %in% c(1:5) ~ q89c, TRUE ~ NA_real_),
    
    # Have neighbors with HIV/AIDs 1 = strong dislike 5 = strong like
    neighbors_hiv = case_when(q89d %in% c(1:5) ~ q89d, TRUE ~ NA_real_),
    
    # Have immigrant neighbors 1 = strong dislike 5 = strong like
    neighbors_immigrant = case_when(q89e %in% c(1:5) ~ q89e, TRUE ~ NA_real_),
    
    
    
    ### Political ###
    
    # Type of governance (1 strong dis, 5 strong approve)
    govern_oneparty = case_when(q28a %in% c(1:5) ~ q28a, TRUE ~ NA_real_),
    govern_military = case_when(q28b %in% c(1:5) ~ q28b, TRUE ~ NA_real_),
    govern_bigman = case_when(q28c %in% c(1:5) ~ q28c, TRUE ~ NA_real_),
    
    # Support for democracy (1 doesn't matter, 2 sometimes non-dem better, 3 democracy preferable)
    democracy_support = case_when(q30 %in% c(1:3) ~ q30, TRUE ~ NA_real_),
    
    # Perceptions of democracy
    democracy_perception = case_when(q40 %in% c(1:4) ~ q40, TRUE ~ NA_real_),
    # Satisfaction with democracy
    democracy_satisfaction = case_when(q41 %in% c(0:4) ~ q41, TRUE ~ NA_real_),
    
    # Party competition leads to conflict (0 never, 3 always)
    party_violence = case_when(q45b %in% c(0:3) ~ q45b, TRUE ~ NA_real_)
    # hold = 1
  )


save(afrob6_sf_recoded, file = here::here("afrob", "afrob6_sf_recoded.Rdata"))
