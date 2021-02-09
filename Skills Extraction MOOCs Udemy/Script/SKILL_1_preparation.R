library(tidyverse)
library(rebus)
library(tidytext)

original_df <- readRDS("Input/udemy_mktg.rds")

# ID AND RELEVANT COLUMNS SELECTION -----------------------------------------------------

# create different versions of the dataset to have all the references with the original data
original_df <- original_df %>% 
  ungroup() %>% 
  mutate(id = row_number())

df <- original_df %>% 
  select(id, descr, skill)

# FIXING TEXT -----------------------------------------------------

# fix text copywriting error such as punctuation with no space after
# do it for descr column

df <- df %>% 
  mutate(descr_clean = str_replace_all(descr, capture("\\.") %R% UPPER, paste(REF1, " "))) %>% 
  mutate(descr_clean = str_replace_all(descr_clean, capture(or("\\:", "\\,", "\\;", "\\?", "\\!")), replacement = str_c(REF1, " "))) %>%
  mutate(descr_clean = str_replace_all(descr_clean, SPC %R% one_or_more(SPC), " ")) %>% 
  mutate(descr_clean = str_remove_all(descr_clean, or(START %R% SPC, SPC %R% END)))

# and for skill column

df <- df %>% 
  mutate(skill_clean = str_replace_all(skill, "\\." %R% UPPER, paste(REF1, " "))) %>% 
  mutate(skill_clean = str_replace_all(skill_clean, capture(or("\\:", "\\,", "\\;", "\\?", "\\!")), replacement = str_c(REF1, " "))) %>%
  mutate(skill_clean = str_replace_all(skill_clean, SPC %R% one_or_more(SPC), " ")) %>% 
  mutate(skill_clean = str_remove_all(skill_clean, or(START %R% SPC, SPC %R% END)))

# SENTENCES SPLITTING DESCR COLUMN -----------------------------------------------------

#split text into sentences using punctuation

pat <- or("\\?", "\\!", "\\:", "\\;")

# because of separate rows function will delete the punctuation i ve to put in the text an anchor added
# to punctuation in order to separate rows by the chosen anchor

df_1 <- df %>% 
  mutate(descr_clean = str_replace_all(descr_clean, capture(pat), paste(REF1, "slevin_kelevra", sep = ""))) %>% 
  mutate(descr_clean = str_replace_all(descr_clean, capture("\\.") %R% or(SPC, "\\n"), paste(REF1, "slevin_kelevra", sep = "")))
  
# there is no problem with new line
df_1 <- separate_rows(df_1, descr_clean, sep= "\\n")

# to preserve punctuation i use the anchor slevin kelevra because i guess there is no such word
# in the moocs decription

df_1 <- separate_rows(df_1, descr_clean, sep = "slevin_kelevra") %>%
  mutate(descr_clean = str_remove_all(descr_clean, or(START %R% SPC, SPC %R% END)))

# delete no words remaining strings

df_1 <- df_1 %>%
  filter(str_length(descr_clean) > 2) %>% 
  filter(str_detect(descr_clean, WRD)) 

# delete useless sentences

df_1 <- df_1 %>%
  filter(!(str_length(descr_clean) < 30 & str_detect(descr_clean, "\\:"))) %>% 
  filter(str_detect(descr_clean, ALPHA))

# assign an id to each sentence

df_1 <- df_1 %>% 
  arrange(id) %>% 
  mutate(id_sentence = row_number()) 

# SENTENCES SPLITTING SKILL COLUMN -----------------------------------------------------

#split text into sentences using punctuation

pat <- or("\\?", "\\!", "\\:", "\\;")

# because of separate rows function will delete the punctuation i ve to put in the text an anchor added
# to punctuation in order to separate rows by the chosen anchor

df_2 <- df %>% 
  mutate(skill_clean = str_replace_all(skill_clean, capture(pat), paste(REF1, "slevin_kelevra", sep = ""))) %>% 
  mutate(skill_clean = str_replace_all(skill_clean, capture("\\.") %R% or(SPC, "\\n"), paste(REF1, "slevin_kelevra", sep = "")))

# there is no problem with new line
df_2 <- separate_rows(df_2, skill_clean, sep= "\\n")

# to preserve punctuation i use the anchor slevin kelevra because i guess there is no such word
# in the moocs decription

df_2 <- separate_rows(df_2, skill_clean, sep = "slevin_kelevra")

#there are some composite skills sentence joined by | so they must be separated

df_2 <-  separate_rows(df_2, skill_clean, sep = "\\|") %>% 
  mutate(skill_clean = str_remove_all(skill_clean, or(START %R% SPC, SPC %R% END)))

# delete no words remaining strings

df_2 <- df_2 %>%
  filter(str_length(skill_clean) > 2) %>% 
  filter(str_detect(skill_clean, WRD)) 

# delete useless sentences

df_2 <- df_2 %>%
  filter(!(str_length(skill_clean) < 30 & str_detect(skill_clean, "\\:"))) %>% 
  filter(str_detect(skill_clean, ALPHA))

# assign an id to each sentence

df_2 <- df_2 %>% 
  arrange(id) %>% 
  mutate(id_sentence = row_number()) 

# OUTPUT GENERATION -------------------------------------------------------

# we have two dataset because we have two relevant textual field and is convinent to mantain the two
# in different datsets

write_rds(df_1, "Intermediate/df_1.rds")

write_rds(df_2, "Intermediate/df_2.rds")

  
  

