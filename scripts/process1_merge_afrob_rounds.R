library(dplyr)



# Load each round and drop in respno_r identifier -------------------------

# Round 1
### Has no time stamps ###
# r1 <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
#                                  "merged_r1_data.sav")) %>% 
#   mutate(respno_r = paste0(casenumb, "_r1"))

# Round 2
r2 <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                 "merged_r2_data.sav")) %>% 
  mutate(respno_r = paste0(respno, "_r2")) %>% 
  # remove senegal which has bad time stamps
  filter(substr(respno, 1, 3) != "SEN")

# Round 3
r3 <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                 "merged_r3_data.sav")) %>% 
  mutate(respno_r = paste0(respno, "_r3"))

# Round 4
r4 <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                 "merged_r4_data.sav")) %>% 
  mutate(respno_r = paste0(RESPNO, "_r4"))

# Round 5
r5 <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                 "merged_r5_data.sav")) %>% 
  mutate(respno_r = paste0(RESPNO, "_r5"))

# Round 6
r6 <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                 "merged_r6_data.sav")) %>% 
  mutate(respno_r = paste0(RESPNO, "_r6"))


# Round 7
r7 <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                 "merged_r7_data.sav")) %>% 
  mutate(respno_r = paste0(RESPNO, "_r7"))




# Grab and clean extra variables ------------------------------------------


# Date and time
r2 <- r2 %>% 
  mutate(ymd = lubridate::ymd(dateintr))
r3 <- r3 %>% 
  mutate(ymd = lubridate::ymd(dateintr))
r4 <- r4 %>% 
  mutate(ymd = lubridate::ymd(DATEINTR))
r5 <- r5 %>% 
  mutate(ymd = lubridate::ymd(DATEINTR))
r6 <- r6 %>% 
  mutate(ymd = lubridate::ymd(DATEINTR))
r7 <- r7 %>% 
  mutate(ymd = lubridate::ymd(DATEINTR))

# Religious ID

# r2 Q85
table(r2$q85)

# r3 Q91
table(r3$q91)

# r4 Q90 changed labels
table(r4$Q90)

# r5 Q98A 
table(r5$Q98A)

# r6 Q98A 
table(r6$Q98A)

# r7 Q98A 
table(r7$Q98A)



# How important is religion in your life
# 1-4 (not at all to very)

r4 <-
  r4 %>% 
  mutate(rel_important = Q91)

r5 <-
  r5 %>% 
  mutate(rel_important = Q98B)


# How often attend religious services
# 0 = never 6 = more than once per day
# 7 = not religious
# 7 not in rounds 2 and 3, so making that the same as never for round 6

# r2 q86
# r3 q92
# r6 q98b

table(r2$q86)
table(r3$q92)
table(r6$Q98B)

r2 <- r2 %>% 
  mutate(rel_attend = q86)

r3 <- r3 %>% 
  mutate(rel_attend = q92)

r6 <- r6 %>% 
  mutate(rel_attend = case_when(Q98B == 7 ~ 0,
                                TRUE ~ as.numeric(Q98B)))



# Member of religious group
#  0=Not a Member, 1=Inactive member, 2=Active member, 3=Official leader
# r2 q24a
# r3 q28a
# r4 q22a
# r5 q25a
# r6 q19a
# r7 q20a

table(r2$q24a)
table(r3$q28a)
table(r4$Q22A)
table(r5$Q25A)
table(r6$Q19A)
table(r7$Q20A)

r2 <- r2 %>% 
  mutate(rel_member = q24a)

r3 <- r3 %>% 
  mutate(rel_member = q28a)

r4 <- r4 %>% 
  mutate(rel_member = Q22A)

r5 <- r5 %>% 
  mutate(rel_member = Q25A)

r6 <- r6 %>% 
  mutate(rel_member = Q19A)

r7 <- r7 %>% 
  mutate(rel_member = Q20A)




# Ethnicity verus national

# r2 q57
# r3 q82
# r4 Q83
# r5 Q85B
# r6 Q88B
# r7 Q85B

r2 <- r2 %>% 
  mutate(eth_nat = q57)

r3 <- r3 %>% 
  mutate(eth_nat = q82)

r4 <- r4 %>% 
  mutate(eth_nat = Q83)

r5 <- r5 %>% 
  mutate(eth_nat = Q85B)

r6 <- r6 %>% 
  mutate(eth_nat = Q88B)

r7 <- r7 %>% 
  mutate(eth_nat = Q85B)



# Voted in last election

r3 <- r3 %>% 
  mutate(voted_last_election = case_when(q30 == 1 ~ 1, TRUE ~ 0))

r4 <- r4 %>% 
  mutate(voted_last_election = case_when(Q23D == 1 ~ 1, TRUE ~ 0))

r5 <- r5 %>% 
  mutate(voted_last_election = case_when(Q27 == 1 ~ 1, TRUE ~ 0))

r6 <- r6 %>% 
  mutate(voted_last_election = case_when(Q21 == 1 ~ 1, TRUE ~ 0))

r7 <- r7 %>% 
  mutate(voted_last_election = case_when(Q22 == 1 ~ 1, TRUE ~ 0))


# Protest

r2 <- r2 %>% 
  mutate(protest = q25d)

r3 <- r3 %>% 
  mutate(protest = q31c)

r4 <- r4 %>% 
  mutate(protest = Q23C)

r5 <- r5 %>% 
  mutate(protest = Q26D)

r6 <- r6 %>% 
  mutate(protest = Q27E)

r7 <- r7 %>% 
  mutate(protest = Q26E)

# Join others to raise issue

r2 <- r2 %>% 
  mutate(raise_issue_community = q25c)

r3 <- r3 %>% 
    mutate(raise_issue_community = q31b)

r4 <- r4 %>% 
  mutate(raise_issue_community = Q23B)

r5 <- r5 %>% 
  mutate(raise_issue_community = Q26B)

r6 <- r6 %>% 
  mutate(raise_issue_community = Q27A)

r7 <- r7 %>% 
  mutate(raise_issue_community = Q26A)


# One party rule
# strong dis to strong appr

r2 <- r2 %>% 
  mutate(rule_oneparty = q35a)

r3 <- r3 %>% 
  mutate(rule_oneparty = q36a)

r4 <- r4 %>% 
  mutate(rule_oneparty = Q29A)

r5 <- r5 %>% 
  mutate(rule_oneparty = Q31A)

r6 <- r6 %>% 
  mutate(rule_oneparty = Q28A)

r7 <- r7 %>% 
  mutate(rule_oneparty = Q27A)


# Military rule
# strong dis to strong appr

r2 <- r2 %>% 
  mutate(rule_military = q35c)

r3 <- r3 %>% 
  mutate(rule_military = q36b)

r4 <- r4 %>% 
  mutate(rule_military = Q29B)

r5 <- r5 %>% 
  mutate(rule_military = Q31B)

r6 <- r6 %>% 
  mutate(rule_military = Q28B)

r7 <- r7 %>% 
  mutate(rule_military = Q27B)

# One man rule
# strong dis to strong appr

r2 <- r2 %>% 
  mutate(rule_oneman = q35d)

r3 <- r3 %>% 
  mutate(rule_oneman = q36c)

r4 <- r4 %>% 
  mutate(rule_oneman = Q29C)

r5 <- r5 %>% 
  mutate(rule_oneman = Q31C)

r6 <- r6 %>% 
  mutate(rule_oneman = Q28C)

r7 <- r7 %>% 
  mutate(rule_oneman = Q27C)



# Fear crime

r2 <- r2 %>% 
  mutate(fear_crime = q11a)

r3 <- r3 %>% 
  mutate(fear_crime = q9a)

r4 <- r4 %>% 
  mutate(fear_crime = Q9A)

r5 <- r5 %>% 
  mutate(fear_crime = Q9B)

r6 <- r6 %>% 
  mutate(fear_crime = Q10B)

r7 <- r7 %>% 
  mutate(fear_crime = Q10B)


# Feel unsafe 

r5 <- r5 %>% 
  mutate(feel_unsafe = Q9A)

r6 <- r6 %>% 
  mutate(feel_unsafe = Q10A)

r7 <- r7 %>% 
  mutate(feel_unsafe = Q10A)

# Trust religious leaders

r6 <- r6 %>% 
  mutate(PT_trustrel = Q52L)

r7 <- r7 %>% 
  mutate(PT_trustrel = Q43K)

# Live next to...

r6 <- r6 %>% 
  mutate(neighbor_religion = Q89A)

r7 <- r7 %>% 
  mutate(neighbor_religion = Q87A)

r6 <- r6 %>% 
  mutate(neighbor_ethnicity = Q89B)

r7 <- r7 %>% 
  mutate(neighbor_ethnicity = Q87B)

r6 <- r6 %>% 
  mutate(neighbor_homosexual = Q89C)

r7 <- r7 %>% 
  mutate(neighbor_homosexual = Q87C)

r6 <- r6 %>% 
  mutate(neighbor_hiv = Q89D)

r6 <- r6 %>% 
  mutate(neighbor_foreign = Q89E)

r7 <- r7 %>% 
  mutate(neighbor_foreign = Q87D)

r6 <- r6 %>% 
  mutate(PT_crptrel = Q53I)

r7 <- r7 %>% 
  mutate(PT_crptrel = Q44H)


# Bind all together -------------------------------------------------------

keepers <-c("respno_r", "ymd", "rel_important", "rel_attend", "rel_member", 
            "eth_nat", "voted_last_election", "raise_issue_community", 
            "rule_oneparty", "rule_military", "rule_oneman", "protest",
            "fear_crime", "feel_unsafe", "PT_trustrel", "neighbor_religion", 
            "neighbor_ethnicity", "neighbor_homosexual", "neighbor_hiv", 
            "neighbor_foreign", "PT_crptrel")



afrob_bind <- 
  r2 %>% 
  select(one_of(keepers)) %>% 
  mutate(round = "r2") %>% 
  bind_rows(
    
    (r3 %>% 
       select(one_of(keepers)) %>% 
       mutate(round = "r3")),
    
    (r4 %>% 
       select(one_of(keepers)) %>% 
       mutate(round = "r4")),
    
    (r5 %>% 
       select(one_of(keepers)) %>% 
       mutate(round = "r5")),
    
    (r6 %>% 
       select(one_of(keepers)) %>% 
       mutate(round = "r6")),
    
    (r7 %>% 
       select(one_of(keepers)) %>% 
       mutate(round = "r7"))
    
  )




# Joined with merged geocoded data ----------------------------------------
readxl::read_excel(here::here("afrob", "afb_r1tor7.xlsx")) %>% 
  select(-"...1") %>%   
  mutate(country = substr(respno_r, 1, 3)) %>% 
  select(country, everything()) %>% 
  left_join(., afrob_bind) %>% 
  filter(!is.na(ymd)) -> afrob_merge



# Clean up IVs ------------------------------------------------------------
colnames(afrob_merge)


afrob_merge <-
  afrob_merge %>% 
  mutate(DM_fem = case_when(DM_fem == 99 ~ NA_real_, TRUE ~ DM_fem),
         DM_urban = case_when(DM_urban %in% c(2, 3, 460) ~ 2, TRUE ~ 1),
         DM_age = case_when(DM_age > 130 ~ NA_real_, TRUE ~ DM_age),
         DM_edu = case_when(DM_edu %in% c(-1, 98, 99) ~ NA_real_, TRUE ~ DM_edu),
         DM_emp = DM_emp,
         DM_relig = DM_relig,
         WF_nofood = case_when(WF_nofood %in% c(0:4) ~ WF_nofood, TRUE ~ NA_real_),
         WF_noinc = case_when(WF_noinc %in% c(0:4) ~ WF_noinc, TRUE ~ NA_real_),
         WF_livcon = case_when(WF_livcon %in% c(0:5) ~ as.numeric(WF_livcon), TRUE ~ NA_real_),
         WF_livconothr = case_when(WF_livconothr %in% c(0:5) ~ as.numeric(WF_livconothr), TRUE ~ NA_real_),
         PSP_econ = case_when(PSP_econ %in% c(1:5) ~ as.numeric(PSP_econ), TRUE ~ NA_real_),
         PSP_econ1yrago = case_when(PSP_econ1yrago %in% c(1:5) ~ as.numeric(PSP_econ1yrago), TRUE ~ NA_real_),
         PSP_econin1yr = case_when(PSP_econin1yr %in% c(1:5) ~ as.numeric(PSP_econin1yr), TRUE ~ NA_real_),
         SG_econ = SG_econ,
         SG_job = SG_job,
         DA_supdem = case_when(DA_supdem %in% c(1:3) ~ DA_supdem, TRUE ~ NA_real_),
         DA_supdem2 = case_when(DA_supdem == 1 ~ 3,
                                DA_supdem == 2 ~ 2, 
                                DA_supdem == 3 ~ 1,
                                TRUE ~ NA_real_),
         DA_demext = case_when(DA_demext %in% c(1:4) ~ DA_demext, TRUE ~ NA_real_),
         SD_demhappy = case_when(SD_demhappy %in% c(0:4) ~ SD_demhappy, TRUE ~ NA_real_),
         rel_attend = case_when(rel_attend %in% c(0:6) ~ rel_attend, TRUE ~ NA_real_),
         rel_member = case_when(rel_member %in% c(0:3) ~ rel_member, TRUE ~ NA_real_),
         eth_nat_num = case_when(round == "r2" ~ NA_real_, 
                                 eth_nat %in% c(1:5) ~ eth_nat, 
                                 TRUE ~ NA_real_),
         raise_issue_community = case_when(raise_issue_community %in% c(0:4) ~ raise_issue_community, TRUE ~ NA_real_),
         rule_oneparty = case_when(rule_oneparty %in% c(1:5) ~ rule_oneparty, TRUE ~ NA_real_),
         rule_military = case_when(rule_military %in% c(1:5) ~ rule_military, TRUE ~ NA_real_),
         rule_oneman = case_when(rule_oneman %in% c(1:5) ~ rule_oneman, TRUE ~ NA_real_),
         voted_last_election = voted_last_election,
         protest = case_when(protest %in% c(0:4) ~ protest, TRUE ~ NA_real_),
         rel_important = case_when(rel_important %in% c(1:4) ~ rel_important, TRUE ~ NA_real_),
         fear_crime = case_when(fear_crime %in% c(0:4) ~ fear_crime, TRUE ~ NA_real_),
         feel_unsafe = case_when(feel_unsafe %in% c(0:4) ~ feel_unsafe, TRUE ~ NA_real_)) %>% 
  mutate_at(vars(starts_with("neighbor")), ~case_when(. %in% c(1:5) ~ as.numeric(.), TRUE ~ NA_real_)) %>% 
  mutate_at(vars(starts_with("PT_trust")), ~case_when(. %in% c(0:3) ~ as.numeric(.), TRUE ~ NA_real_)) %>% 
  mutate_at(vars(starts_with("PT_crpt")), ~case_when(. %in% c(0:3) ~ as.numeric(.), TRUE ~ NA_real_)) 
  


save(afrob_merge, file = here::here("processed", "afrob_merge.Rdata"))








