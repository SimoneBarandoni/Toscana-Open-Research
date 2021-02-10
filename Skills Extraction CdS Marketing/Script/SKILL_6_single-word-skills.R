library(tidyverse)
library(readxl)
library(rebus)
library(tidytext)
library(tm)

df_3_1 <- readRDS("Intermediate/df_3_1.rds")

esco <- readRDS("Intermediate/esco_original_1_single_word.rds")

esco_en <- read_xlsx("Input/esco_en_skills.xlsx")

isco_codes <- read_xlsx("Input/isco.xlsx")


# ENGLISH ESCO DATASET PREPARATION ----------------------------------------------------

isco_codes <- isco_codes %>% 
  filter(elim == 1)

esco_it <- esco %>% 
  select(preferredLabel) %>% 
  rename(skillpreferredLabel = preferredLabel)

esco_en <- esco_en %>% 
  filter(!(iscoGroup %in% isco_codes$id))%>% 
  mutate(n = str_count(skillpreferredLabel, whole_word(one_or_more(WRD)))) %>% 
  filter(n < 3) %>% 
  distinct(skillpreferredLabel)

esco_bind <- bind_rows(esco_it, esco_en) %>% 
  distinct()
 
# ONE WORD SKILLS EXTRACTION -------------------------------------------------------

pat <- or1(whole_word(esco_bind$skillpreferredLabel))

skills_dataset <- df_3_1 %>%
  rowwise() %>%
  mutate(skills_list = paste(unlist(str_match_all(str_to_lower(merged_sentences), whole_word(str_to_lower(pat)))), collapse = " ; ")) %>% 
  mutate(skills_list = if_else(skills_list == "c", "", skills_list)) %>% 
  mutate(skills_list = if_else(skills_list == "r", "", skills_list)) 
  
# now that we have extracted skills pattern we have to retrieve skills esco id

esco <- esco %>% 
  select(preferredLabel, doc_id_esco) 

esco_en <- esco_en %>% 
  mutate(doc_id_esco = row_number() + 100000)%>% 
  select(skillpreferredLabel, doc_id_esco) %>% 
  distinct(skillpreferredLabel, .keep_all = TRUE) %>% 
  rename(preferredLabel = skillpreferredLabel)

esco <- bind_rows(esco, esco_en)%>% 
  distinct(preferredLabel, .keep_all = TRUE) %>% 
  mutate(preferredLabel = str_to_lower(preferredLabel)) 

# here the final skills dataset with esco id for each skill extracted
# note that we still have duplicated skills in the same sentence that problem will be fixed in next scripts
# in this case the dataset is already separated in a single skill per row

skills_dataset <- separate_rows(skills_dataset, skills_list, sep = " ; ") %>% 
  left_join(esco, by = c("skills_list" = "preferredLabel")) 

# OUTPUT GENERATION -------------------------------------------------------

write_rds(skills_dataset , "Intermediate/df_single_word_skills.rds")
