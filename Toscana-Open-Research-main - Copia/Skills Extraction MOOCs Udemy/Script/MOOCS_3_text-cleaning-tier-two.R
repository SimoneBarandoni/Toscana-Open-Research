library(rebus)
library(readxl)
library(tidyverse)
library(tidytext)
library(udpipe)
library(tm)

df_3 <- readRDS("Intermediate/df_3.rds")

it_model <- udpipe::udpipe_load_model("Input/italian-isdt-ud-2.4-190531.udpipe")

not_ex <- read.csv2("Input/not_existing_words.csv")

vocabolario <- read.csv2("Input/dizionario-totale.csv")

wrong_lemmatized <- read_xlsx("Input/wrong_lemmatized.xlsx")

# we want three version of the moocs sentences the first is the as is version the second is a stopwords and
# and special characther cleaned version and the third is a lemmatized and cleaned bag of word version

# INPUT DATASET PREPARATION -----------------------------------------------

# we have to prepare input dataset that would be useful in the lemmatization step

not_ex <- not_ex %>% 
  group_by(lemma) %>% 
  arrange(-in_out) %>% 
  filter(in_out == 1 | in_out == 0) %>% 
  ungroup() %>% 
  distinct(lemma, .keep_all = T)

vocabolario <- vocabolario %>%
  filter(!str_detect(parola, one_or_more(WRD) %R% SPC %R% one_or_more(WRD))) %>%
  filter(!str_detect(parola, PUNCT)) %>%
  mutate(parola = str_to_lower(parola)) %>%
  rename(suggested_token = parola) %>%
  select(suggested_token) %>%
  distinct(suggested_token) %>%
  mutate(tag = 1)

# FUNCTION PREP_FUN ---------------------------------------------------------------

# load a text cleaning function

prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols   
  x = str_replace_all(x, "[^&^_[:^punct:]]", " ")
  # remove stopwords
  x = removeWords(x, stopwords("italian"))
  # collapse multiple spaces
  x = str_replace_all(x, SPC %R% one_or_more(SPC), " ")
  x = str_remove_all(x, or(START %R% SPC, SPC %R% END))
}

# STOPWORDS AND SPECIAL CHARACTER SENTENCE CLEANED VERSION -------------------------

df_3_1 <- df_3 %>% 
  mutate(sentences_clean = merged_sentences )
  
df_3_1$sentences_clean <- prep_fun(df_3_1$sentences_clean)


# LEMMATIZED AND CLEANED SENTENCE VERSION ------------------------------------------

# the first step is text lemmatization
# we need an unique senteces id to give it to udpipe 

df_3_1 <- df_3_1 %>%
  mutate(overall_id = row_number())

# we use a temporary dataframe to work better with udpipe lemmatizaton table

temp <- df_3_1

temp <- udpipe::udpipe_annotate(object = it_model,
                                    x = iconv(temp$merged_sentences),
                                    doc_id = temp$overall_id) %>%
  as_tibble() %>% 
  select(doc_id, token, lemma, upos) %>% 
  mutate(doc_id = as.numeric(doc_id))

# we can fix some lemmatization problem using the input dataset that we have already prepared

temp <- left_join(temp, vocabolario, by = c("token" = "suggested_token")) %>% # fixing lemma that are equals to token
  mutate(lemma = if_else(!is.na(tag), token, lemma)) %>%
  select(-tag) %>% 
  left_join(wrong_lemmatized, by = "lemma") %>% # fixing frequent udpipe lemmatization error
  mutate(lemma = if_else(!is.na(correct), correct, lemma)) %>%
  select(-correct) %>% 
  filter(!is.na(token)) %>%
  mutate(lemma = if_else(is.na(lemma), token, lemma)) %>% 
  left_join(not_ex[,c("lemma","proper_lemma")], by = "lemma") %>% # fixing other udpipe lemmatization error
  mutate(lemma = if_else(is.na(proper_lemma),lemma,proper_lemma)) 

temp <- temp %>% 
  group_by(doc_id) %>% 
  summarise(sentences_lemm = paste(lemma, collapse = " ")) %>% 
  ungroup() 

# special charachter and stopwords cleaning

temp$sentences_lemm <-  prep_fun(temp$sentences_lemm)

# joining udpipe lemmatized sentences with the complete data frame

df_3_1 <- df_3_1 %>% 
  left_join(temp, by = c("overall_id" = "doc_id")) %>% 
  select(id, sentences_id, overall_id, descr, skill, merged_sentences, sentences_clean, sentences_lemm) # reordering columns

# OUTPUT GENERATION -------------------------------------------------------

write_rds(df_3_1, "Intermediate/df_3_1.rds")

