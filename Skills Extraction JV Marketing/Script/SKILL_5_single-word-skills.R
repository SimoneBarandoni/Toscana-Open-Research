library(tidyverse)
library(text2vec)
library(readxl)
library(rebus)
library(tidytext)
library(tm)
library(udpipe)

df_1 <- readRDS("Intermediate/df_1.rds")

esco <- readRDS("Intermediate/esco_original_1_single_word.rds")

esco_en <- read_xlsx("Input/esco_en_skills.xlsx")

isco_codes <- read_xlsx("Input/isco_non_rilevanti_esami_umanistici.xlsx")

# ENGLISH ESCO DATASET ----------------------------------------------------


esco_it <- esco %>% 
  select(preferredLabel) %>% 
  rename(skillpreferredLabel = preferredLabel)

esco_en <- esco_en %>% 
  filter(!(iscoGroup %in% isco_codes$id))%>% 
  mutate(n = str_count(skillpreferredLabel, whole_word(one_or_more(WRD)))) %>% 
  filter(n < 3) %>% 
  distinct(skillpreferredLabel)

esco <- bind_rows(esco_it, esco_en) %>% 
  distinct()
 
pat <- or1(whole_word(esco$skillpreferredLabel))

# ONE WORD SKILLS EXTRACTION -------------------------------------------------------

df_1_1 <- df_1 %>%
  rowwise() %>%
  mutate(skill = paste(unlist(str_match_all(str_to_lower(descr), str_to_lower(pat))), collapse = " ; ")) %>% 
  filter(skill != "c") %>% 
  filter(skill != "r")
  
esco <- readRDS("Intermediate/esco_original_1_single_word.rds") %>% 
  select(preferredLabel, doc_id_esco) 

esco_en <- read_xlsx("Input/esco_en_skills.xlsx") %>% 
  mutate(doc_id_esco = row_number() + 100000)%>% 
  select(skillpreferredLabel, doc_id_esco) %>% 
  distinct(skillpreferredLabel, .keep_all = TRUE) %>% 
  rename(preferredLabel = skillpreferredLabel)

esco <- bind_rows(esco, esco_en)%>% 
  distinct(preferredLabel, .keep_all = TRUE) %>% 
  mutate(preferredLabel = str_to_lower(preferredLabel)) 

df_1_1 <- left_join(df_1_1,esco, by = c("skill" = "preferredLabel"))%>% 
  rename(word = descr)
  


# OUTPUT GENERATION -------------------------------------------------------

write_rds(df_1_1, "Intermediate/df_single_word_skills.rds")


