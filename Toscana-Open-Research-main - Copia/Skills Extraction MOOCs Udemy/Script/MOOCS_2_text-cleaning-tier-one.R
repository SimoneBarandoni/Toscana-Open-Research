library(rebus)
library(tidyverse)
library(tidytext)

df_1 <- readRDS("Intermediate/df_1.rds")

df_2 <- readRDS("Intermediate/df_2.rds")

# SKILL COLUMN TEXT CLEANING BY RULES --------------------------------------------------

# english language skills sentences cleaning

df_2_2 <- df_2 %>% 
  mutate(skill_clean = if_else(id == 18, "", skill_clean)) #there is only one at id eighteen

# interrogative and exclamative sentences cleaning

df_2_2 <- df_2_2 %>% 
  filter(!str_detect(skill_clean, or("\\!", "\\?")))

# take the negative version of cleaned dataset in order to check the cleaning fairness

df_dirty_2 <- df_2 %>% 
  left_join(df_2_2 %>% select(id, id_sentence) %>% mutate(tag = 1), by = c("id", "id_sentence")) %>% 
  filter(is.na(tag))

# DESCR COLUMN TEXT CLEANING BY RULES --------------------------------------------------

# updates sentences cleaning

df_1_1 <- df_1 %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive("aggiornament") %R% one_or_more(WRD)))) %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive("updat") %R% one_or_more(WRD))))

# welcome sentences cleaning

df_1_1 <- df_1_1 %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive("benvenut") %R% one_or_more(WRD)))) %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive("benarrivat") %R% one_or_more(WRD)))) %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive(or("ciao", "salve", "buongiorno", "buonasera")))))

# interrogative and exclamative sentences cleaning

df_1_1 <- df_1_1 %>% 
  filter(!str_detect(descr_clean, or("\\!", "\\?")))

# course composition sentences cleaning

df_1_1 <- df_1_1 %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive(or("corso", "corsi", "classe", "ore", "ora", 
                                                                 "classi", "lezione", "lezioni")))))

# call to action sentences cleaning

df_1_1 <- df_1_1 %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive("iscriv") %R% one_or_more(WRD)))) %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive("registrat") %R% one_or_more(WRD)))) %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive("affrettat") %R% one_or_more(WRD)))) %>%
  filter(!str_detect(descr_clean, whole_word(case_insensitive(or("dammi retta", "datemi retta", "rimborsato", "che aspetti"
                                                                 , "che aspettate", "gratuitamente")))))

# course content and other stuff cleaning

df_1_1 <- df_1_1 %>% 
  filter(!str_detect(descr_clean, whole_word(case_insensitive(or("slides", "download", "esercizi", "esercitazioni",
                                                                 "esercitazione")))))

# take the negative version of cleaned dataset in order to check the cleaning fairness

df_dirty_1 <- df_1 %>% 
  left_join(df_1_1 %>% select(id, id_sentence) %>% mutate(tag = 1), by = c("id", "id_sentence")) %>% 
  filter(is.na(tag))


# DESCR COLUMN TEXT CLEANING BY LENGHT -----------------------------------

# ultimately because of there is too much noise in the description dataset we try to go to the hard way
# and we hypotize that short sentences in this dataset have little or none chanches to be informative
# to set the trashold we can measure the average lenght of skill column that is much more clean and fair

hist(df_2_2 %>% mutate(skill_clean = str_length(skill_clean)) %>% pull(skill_clean), breaks = 100)

# the distribution is strongly positive skewed

mean(df_2_2 %>% mutate(skill_clean = str_length(skill_clean)) %>% pull(skill_clean))

# in order to confirm our hypoytesis we can measure deleted sentences avg lenght it must be shorter thant 70

mean(df_dirty_1 %>% mutate(descr_clean = str_length(descr_clean)) %>% pull(descr_clean))

# so the lenght trashold must be between 70 or 100

df_1_1 <- df_1_1 %>% 
  filter(str_length(descr_clean) > 100)

#we need a new negative dataset                                                                              
df_dirty_1 <- df_1 %>% 
  left_join(df_1_1 %>% select(id, id_sentence) %>% mutate(tag = 1), by = c("id", "id_sentence")) %>% 
  filter(is.na(tag))

# MERGING COLUMN ----------------------------------------------------------

# merge skill and descr column uniforming their names 

df_1_1 <- df_1_1 %>% 
  select(id, descr, skill, descr_clean) %>% 
  rename(merged_sentences = descr_clean )

df_2_2 <- df_2_2 %>% 
  select(id, descr, skill, skill_clean) %>% 
  rename(merged_sentences = skill_clean )

# filter empty rows and assign a new consistent sentences id

df_3 <- bind_rows(df_1_1, df_2_2) %>% 
  filter(str_detect(merged_sentences, WRD) & !is.na(merged_sentences)) %>% 
  arrange(id) %>% 
  group_by(id) %>% 
  mutate(sentences_id = row_number()) %>% 
  ungroup() 


#temporarly sampling df_3 to test code
df_3 <- df_3 %>%
  slice(1:100)

# OUTPUT GENERATION -------------------------------------------------------

# here the two dataset for each textual field cleaned

write_rds(df_1_1, "Intermediate/df_1_1.rds")

write_rds(df_2_2, "Intermediate/df_2_2.rds")

# this is the merged version of the two above

write_rds(df_3, "Intermediate/df_3.rds")

# this datasets contains the filtered sentences of the two textual field

write_rds(df_dirty_1, "Intermediate/df_dirty_1.rds")

write_rds(df_dirty_2, "Intermediate/df_dirty_2.rds")
