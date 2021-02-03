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

df <- df %>%   
  mutate(descr = str_replace_all(descr, SPC %R% one_or_more(SPC), " ")) %>% 
  mutate(descr = str_remove_all(descr, or(START %R% SPC, SPC %R% END)))

# SENTENCES SPLITTING -----------------------------------------------------

df_1 <- separate_rows(df, descr, sep= "\\n")

df_1 <- separate_rows(df_1, descr, sep= "\\." %R% SPC)

df_1 <- separate_rows(df_1, descr, sep= "\\:" %R% SPC)

df_1 <- separate_rows(df_1, descr, sep= "\\?" %R% SPC)

df_1 <- separate_rows(df_1, descr, sep= "\\;" %R% SPC)


# ASSIGN AN ID TO EACH SENTENCE -----------------------------------------------

df_1 <- df_1 %>% 
  arrange(id) %>% 
  mutate(id_sentence = row_number()) %>% 
  filter(descr != "")
  
# OUTPUT GENERATION -------------------------------------------------------


write_rds(df_1, "Intermediate/df_1.rds")


  
  

