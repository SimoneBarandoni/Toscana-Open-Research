library(readxl)
library(rebus)
library(tidyverse)

esco_original <- read_xlsx("Input/esco_it_skills.xlsx")

isco <- read_xlsx("Input/isco.xlsx")


# SUBSETTING ESCO DATASET -------------------------------------------------

isco <- isco %>% 
  filter(elim != "1")

pattern_tic <- or(SPC %R% "TIC" %R% SPC, SPC %R% "TIC" %R% END)

esco_original_0 <- esco_original %>% 
  distinct(preferredLabel, .keep_all = T) %>% 
  filter(iscoGroup %in% isco$id) %>% 
  mutate(doc_id_esco = row_number()) %>% 
  mutate(tag_tic = if_else(str_detect(preferredLabel, pattern_tic),1,0)) %>% 
  filter(tag_tic != 1) %>% 
  select(-tag_tic) 


esco_original_1 <- esco_original_0

temp <- separate_rows(esco_original_1,altLabels, sep = ";") %>% 
  select(-preferredLabel) %>% 
  mutate(preferredLabel = altLabels) %>% 
  distinct(preferredLabel,.keep_all = TRUE) %>% 
  filter(!is.na(preferredLabel))

esco_original_1 <- rbind(esco_original_1, temp) %>% 
  filter(!is.na(preferredLabel)) %>% 
  distinct(preferredLabel, .keep_all = TRUE) %>% 
  select(-altLabels) %>% 
  mutate(n = str_count(preferredLabel, whole_word(one_or_more(WRD)))) 

# SPLITTING ESCO DATASET --------------------------------------------------

esco_original_1_single_word <- esco_original_1 %>% 
  filter(n<2)

esco_original_1_multiple_word <- esco_original_1 %>% 
  filter(n>=2)

esco_original_1_multiple_word <- esco_original_1_multiple_word %>% 
  filter(n<7)


# OUTPUT GENERATION -------------------------------------------------------

write_rds(esco_original_0, "Intermediate/esco_original_0.rds")

write_rds(esco_original_1_single_word, "Intermediate/esco_original_1_single_word.rds")

write_rds(esco_original_1_multiple_word, "Intermediate/esco_original_1_multiple_word.rds")