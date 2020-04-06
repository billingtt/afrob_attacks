library(tidyverse)

# Load data ---------------------------------------------------------------

# Codebook, reshaped to tidy
codetbl <- 
  readr::read_csv(here::here("afrob", "AB_fixes_tidy.csv")) %>% 
  pivot_longer(r1:r7,
               names_to = "round", values_to = "original") %>% 
  rename(label = Label)


# Afrob rounds

r2 <- readxl::read_excel(here::here("afrob", "afb_r1tor7.xlsx")) %>% 
  filter(round == "r2")

r2_nogeo <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                       "merged_r2_data.sav"))
r2 <-
  r2 %>% mutate(respno = substr(respno_r, 1, 7)) %>% 
  dplyr::select(respno, xlat, ylong, round) %>% 
  full_join(r2_nogeo, by = c("respno" = "respno")) %>% 
  mutate(round = as.character(round))

r2 <- r2 %>% filter(!is.na(round))


r3 <- read_csv(here::here("afrob", "afb_full_r3.csv")) %>% 
  mutate(round = "r3",
         dateintr = as.Date(dateintr, "%d-%b-%y")) 


r4 <- read_csv(here::here("afrob", "afb_full_r4.csv")) %>% 
  mutate(round = "r4",
         dateintr = as.Date(dateintr, "%d-%b-%y"))


r5 <- read_csv(here::here("afrob", "afb_full_r5.csv")) %>% 
  mutate(round = "r5",
         dateintr = as.Date(dateintr, "%d-%b-%y"))


r6 <- read_csv(here::here("afrob", "afb_full_r6.csv")) %>% 
  mutate(round = "r6",
         dateintr = as.Date(dateintr, "%d-%b-%y"))


r7 <- readxl::read_excel(here::here("afrob", "afb_r1tor7.xlsx")) %>% 
  filter(round == "r7")

r7_nogeo <- haven::read_sav(here::here("afrob", "merged_byround_nogeo",
                                       "merged_r7_data.sav"))
r7 <-
  r7 %>% mutate(respno = substr(respno_r, 1, 7)) %>% 
  dplyr::select(respno, xlat, ylong, round) %>% 
  full_join(r7_nogeo, by = c("respno" = "RESPNO"))








# Match names to codebook -------------------------------------------------

# Function takes a dataset and codebook and conditionally renames; drops columns with no match
namer <- function(data, codebook) {
  
  names(data) <- codebook[["recode"]][match(tolower(names(data)), tolower(codebook[["original"]]))]
  
  data <- data[!is.na(names(data))]
  
  return(data)
  
}

# Map across rounds and bind to single frame
afrob_full <- map_df(.x = list(r2, r3, r4, r5, r6, r7), 
                     ~namer(data = .x, 
                            # Subset codebook to corresponding round
                            codebook = codetbl[codetbl$round == unique(.x[["round"]]),]) %>% 
                       mutate(round = unique(.x[["round"]])))

afrob_full <- afrob_full %>% mutate(country = substr(respno, 1, 3))


# Check coordinates
table((is.na(afrob_full[["xlat"]])))
afrob_full %>% filter(is.na(xlat)) -> misses
table(misses$country, misses$round)

# all missing coordinates are from round 7


save(afrob_full, file = here::here("afrob", "afrob_full.Rdata"))




