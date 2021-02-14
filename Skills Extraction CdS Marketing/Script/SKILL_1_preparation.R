library(tidyverse)
library(rebus)
library(tidytext)

original_df <- readRDS("Input/cds_marketing_original.rds")

# identifiers

cds <- original_df %>% 
  distinct(corso_di_studio) %>% 
  mutate(id_cds = row_number())

esam <- original_df %>% 
  distinct(corso_di_studio,esame) %>% 
  mutate(id_esame = row_number())

original_df <- left_join(original_df, cds)

original_df <- left_join(original_df, esam)


# the obiettivi formativi of each cds are been cleaned manually so there is no need to clean that column

# FIXING TEXT -----------------------------------------------------

# fix text copywriting error such as punctuation with no space after
# do it for descr column

df1 <- original_df %>% 
  mutate(obiettivi_formativi_esame_clean = str_replace_all(obiettivi_formativi_esame, capture("\\.") %R% UPPER, paste(REF1, " "))) %>% 
  mutate(obiettivi_formativi_esame_clean = str_replace_all(obiettivi_formativi_esame_clean, capture(or("\\:", "\\,", "\\;", "\\?", "\\!")), replacement = str_c(REF1, " "))) %>%
  mutate(obiettivi_formativi_esame_clean = str_replace_all(obiettivi_formativi_esame_clean, SPC %R% one_or_more(SPC), " ")) %>% 
  mutate(obiettivi_formativi_esame_clean = str_remove_all(obiettivi_formativi_esame_clean, or(START %R% SPC, SPC %R% END)))

df2 <- original_df %>% 
  distinct(obiettivi_formativi_clean, .keep_all = T) %>% 
  mutate(obiettivi_formativi_clean = str_replace_all(obiettivi_formativi_clean, capture("\\.") %R% UPPER, paste(REF1, " "))) %>% 
  mutate(obiettivi_formativi_clean = str_replace_all(obiettivi_formativi_clean, capture(or("\\:", "\\,", "\\;", "\\?", "\\!")), replacement = str_c(REF1, " "))) %>%
  mutate(obiettivi_formativi_clean = str_replace_all(obiettivi_formativi_clean, SPC %R% one_or_more(SPC), " ")) %>% 
  mutate(obiettivi_formativi_clean = str_remove_all(obiettivi_formativi_clean, or(START %R% SPC, SPC %R% END)))

# SENTENCES SPLITTING ESAMI COLUMN -----------------------------------------------------

#split text into sentences using punctuation

pat <- or("\\?", "\\!", "\\:", "\\;", "\\-", "\\–")

# because of separate rows function will delete the punctuation i ve to put in the text an anchor added
# to punctuation in order to separate rows by the chosen anchor

df_1 <- df1 %>% 
  mutate(obiettivi_formativi_esame_clean = str_replace_all(obiettivi_formativi_esame_clean, capture(pat), paste(REF1, "slevin_kelevra", sep = ""))) %>% 
  mutate(obiettivi_formativi_esame_clean = str_replace_all(obiettivi_formativi_esame_clean, capture("\\.") %R% or(SPC, "\\n"), paste(REF1, "slevin_kelevra", sep = "")))
  
# there is no problem with new line
df_1 <- separate_rows(df_1, obiettivi_formativi_esame_clean, sep= "\\n")

# to preserve punctuation i use the anchor slevin kelevra because i guess there is no such word
# in the moocs decription

df_1 <- separate_rows(df_1, obiettivi_formativi_esame_clean, sep = "slevin_kelevra") %>%
  mutate(obiettivi_formativi_esame_clean = str_remove_all(obiettivi_formativi_esame_clean, or(START %R% SPC, SPC %R% END)))

# delete no words remaining strings

df_1 <- df_1 %>%
  filter(str_length(obiettivi_formativi_esame_clean) > 2) %>% 
  filter(str_detect(obiettivi_formativi_esame_clean, WRD)) 

# delete useless sentences

df_1 <- df_1 %>%
  filter(!(str_length(obiettivi_formativi_esame_clean) < 30 & str_detect(obiettivi_formativi_esame_clean, "\\:"))) %>% 
  filter(str_detect(obiettivi_formativi_esame_clean, ALPHA))

# assign an id to each sentence

df_1 <- df_1 %>% 
  arrange(id) %>% 
  mutate(id_sentence = row_number()) 

# SENTENCES SPLITTING CDS COLUMN -----------------------------------------------------

#split text into sentences using punctuation

pat <- or("\\?", "\\!", "\\:", "\\;", "\\-", "\\–")

# because of separate rows function will delete the punctuation i ve to put in the text an anchor added
# to punctuation in order to separate rows by the chosen anchor

df_2 <- df2 %>% 
  mutate(obiettivi_formativi_clean = str_replace_all(obiettivi_formativi_clean, capture(pat), paste(REF1, "slevin_kelevra", sep = ""))) %>% 
  mutate(obiettivi_formativi_clean = str_replace_all(obiettivi_formativi_clean, capture("\\.") %R% or(SPC, "\\n"), paste(REF1, "slevin_kelevra", sep = "")))

# there is no problem with new line
df_2 <- separate_rows(df_2, obiettivi_formativi_clean, sep= "\\n")

# to preserve punctuation i use the anchor slevin kelevra because i guess there is no such word
# in the moocs decription

df_2 <- separate_rows(df_2, obiettivi_formativi_clean, sep = "slevin_kelevra")

#there are some composite skills sentence joined by | so they must be separated

df_2 <-  separate_rows(df_2, obiettivi_formativi_clean, sep = "\\|") %>% 
  mutate(obiettivi_formativi_clean = str_remove_all(obiettivi_formativi_clean, or(START %R% SPC, SPC %R% END)))

# delete no words remaining strings

df_2 <- df_2 %>%
  filter(str_length(obiettivi_formativi_clean) > 2) %>% 
  filter(str_detect(obiettivi_formativi_clean, WRD)) 

# delete useless sentences

df_2 <- df_2 %>%
  filter(!(str_length(obiettivi_formativi_clean) < 30 & str_detect(obiettivi_formativi_clean, "\\:"))) %>% 
  filter(str_detect(obiettivi_formativi_clean, ALPHA))

# assign an id to each sentence and set NA to the id_esame, since these sentences come
# from a corso di studi, not from a precise exam

df_2 <- df_2 %>% 
  arrange(id) %>% 
  mutate(id_sentence = row_number()) %>% 
  mutate(id_esame = NA)

# OUTPUT GENERATION -------------------------------------------------------

# we have two dataset because we have two relevant textual field and is convinent to mantain the two
# in different datsets

write_rds(df_1, "Intermediate/df_1.rds")

write_rds(df_2, "Intermediate/df_2.rds")

  
  

