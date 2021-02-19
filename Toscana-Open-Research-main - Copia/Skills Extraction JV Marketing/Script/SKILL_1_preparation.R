library(tidyverse)
library(textclean)
library(rebus)
library(tm)
library(tidytext)
library(textutils)

original_df <- readRDS("Input/jv_marketing.rds")

# RELEVANT COLUMNS -----------------------------------------------------

df <- original_df %>% 
  distinct(description, job_title,location, company,date, .keep_all = TRUE) %>% 
  select(id, description) %>% 
  rename(descr = description)

# SPACES CLEANING -----------------------------------------------------

df1 <- df %>%   
  mutate(descr = str_replace_all(descr, capture("\\.") %R% UPPER, paste(REF1, " "))) %>% 
  mutate(descr = str_replace_all(descr, capture(or("\\:", "\\,", "\\;", "\\?", "\\!")), replacement = str_c(REF1, " "))) %>%
  mutate(descr = str_replace_all(descr, SPC %R% one_or_more(SPC), " ")) %>% 
  mutate(descr = str_remove_all(descr, or(START %R% SPC, SPC %R% END)))





#split text into sentences using punctuation

pat <- or("\\?", "\\!", "\\:", "\\;", "\\-", "\\–")

# because of separate rows function will delete the punctuation i ve to put in the text an anchor added
# to punctuation in order to separate rows by the chosen anchor

df_1 <- df1 %>% 
  mutate(descr = str_replace_all(descr, capture(pat), paste(REF1, "slevin_kelevra", sep = ""))) %>% 
  mutate(descr = str_replace_all(descr, capture("\\.") %R% or(SPC, "\\n"), paste(REF1, "slevin_kelevra", sep = "")))

# there is no problem with new line
df_1 <- separate_rows(df_1, descr, sep= "\\n")

# to preserve punctuation i use the anchor slevin kelevra because i guess there is no such word
# in the moocs decription

df_1 <- separate_rows(df_1, descr, sep = "slevin_kelevra") %>%
  mutate(descr = str_remove_all(descr, or(START %R% SPC, SPC %R% END)))

# delete no words remaining strings

df_1 <- df_1 %>%
  filter(str_length(descr) > 2) %>% 
  filter(str_detect(descr, WRD)) 

# delete useless sentences

df_1 <- df_1 %>%
  filter(!(str_length(descr) < 30 & str_detect(descr, "\\:"))) %>% 
  filter(str_detect(descr, ALPHA))

# assign an id to each sentence

df_1 <- df_1 %>% 
  arrange(id) %>% 
  mutate(id_sentence = row_number()) %>% 
  filter(descr != "")
  
# OUTPUT GENERATION -------------------------------------------------------


write_rds(df_1, "Intermediate/df_1.rds")

