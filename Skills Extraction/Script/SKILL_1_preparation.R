library(tidyverse)
library(textclean)
library(rebus)
library(tm)
library(tidytext)
library(textutils)

original_df <- readRDS("Input/udemy_mktg.rds")

# ID AND RELEVANT COLUMNS -----------------------------------------------------

original_df <- original_df %>% 
  ungroup() %>% 
  mutate(id = row_number())

df_original_new <- original_df %>% 
  select(id, titolo, descr, n_stud )

df <- original_df %>% 
  select(id, descr)

# SPACES CLEANING -----------------------------------------------------

df <- df %>%   
  mutate(descr = str_replace_all(descr, "\\.", ". ")) %>% 
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

write_rds(df_original_new, "Intermediate/df_original_new.rds")

  
  

