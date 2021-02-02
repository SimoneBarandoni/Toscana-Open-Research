library(readxl)
library(rebus)
library(tidyverse)
library(tidytext)
library(tm)

it_model <- udpipe::udpipe_load_model("Input/italian-isdt-ud-2.4-190531.udpipe")

blacklist <- read.csv2("Input/blacklist1.csv")

not_ex <- read.csv2("Input/not_existing_words.csv")

trigrElim <- read.csv2("Intermediate/eliminaTrigr.csv")

bigrElim <- read.csv2("Intermediate/eliminaBigr.csv")

vocabolario <- read.csv2("Input/dizionario-totale.csv")

wrong_lemmatized <- read_xlsx("Input/wrong_lemmatized.xlsx")

df_1 <- readRDS("Intermediate/df_1.rds")

# FUNCTION PREP_FUN ---------------------------------------------------------------

prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols   
  x = str_replace_all(x, "[^&^_[:^punct:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")
  # remove stopwords
  x = removeWords(x, stopwords("italian"))
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")
}

# BLACKLIST, VOCABULARY AND NOT EXISTING WORDS PREPARATION ------------------------------

# preparation of blacklist and not_existing_words lists
blacklist <- blacklist %>% 
  filter(in_out == 1) %>% 
  as_tibble() %>% 
  pull(Espressione)

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

# SENTENCES LEMMATIZATION AND CLEANING -------------------------------------

df_1_1 <- df_1


df_lemma <- udpipe::udpipe_annotate(object = it_model,
                                             x = iconv(df_1_1$descr),
                                             doc_id = df_1_1$id_sentence) %>%
  as_tibble() %>% 
  select(doc_id, token, lemma, upos )


df_lemma <- left_join(df_lemma,vocabolario, by = c("token" = "suggested_token")) %>%
  mutate(lemma = if_else(!is.na(tag), token, lemma)) %>%
  select(-tag)

df_lemma <- left_join(df_lemma,wrong_lemmatized, by = "lemma") %>%
  mutate(lemma = if_else(!is.na(correct), correct, lemma)) %>%
  select(-correct)

# for the short texts: collapse the lemmas to create a lemmatized text
df_1_1 <- df_lemma %>% 
  filter(!is.na(token)) %>%
  mutate(lemma = if_else(is.na(lemma), token, lemma)) %>% 
  left_join(not_ex[,c("lemma","proper_lemma")], by = "lemma") %>% 
  mutate(lemma = if_else(is.na(proper_lemma),lemma,proper_lemma)) %>% 
  group_by(doc_id) %>% 
  summarise(testoLemm = paste(lemma, collapse = " ")) %>% 
  ungroup()%>% 
  mutate(doc_id = as.numeric(doc_id)) %>% 
  arrange(doc_id)

df_1_1$testoLemm <-  prep_fun(df_1_1$testoLemm)

for(i in 1: nrow(df_1_1)){ # for each abstract
  
  print(str_c("papers done ----->  ", round(i/nrow(df_1_1), digits = 4)*100, "%"))
  
  for(j in 1: nrow(trigrElim)){ # for each multiword
    df_1_1[[i,2]] <- str_replace_all(df_1_1[[i,2]], trigrElim[[j,2]], "")
    
  }
  
}

for(i in 1: nrow(df_1_1)){ # for each abstract
  
  print(str_c("papers done ----->  ", round(i/nrow(df_1_1), digits = 4)*100, "%"))
  
  for(j in 1: nrow(bigrElim))
  {
    df_1_1[[i,2]] <- str_replace_all(df_1_1[[i,2]], bigrElim[[j,2]], "")
  }
  
}

# now eliminate blacklisted lemma
df_1_1 <- df_1_1 %>% 
  unnest_tokens(lemma,testoLemm) %>%
  filter(!lemma %in% blacklist) %>% 
  group_by(doc_id) %>% 
  summarise(testoLemm = paste(lemma, collapse = " ")) %>% 
  ungroup()

df_1_1 <- df_1_1 %>% 
  mutate(n_w = str_count(testoLemm, whole_word(one_or_more(WRD)))) %>% 
  mutate(testoLemm = if_else(n_w < 4, "", testoLemm)) %>% 
  select(-n_w)

# JOINING EXAMS ID WITCH SENTENCES ID -------------------------------------


id_df_1 <- df_1 %>% 
  select(-descr)
 

df_clean <- left_join(df_1_1, id_df_1, by = c("doc_id" = "id_sentence"))

# OUTPUT GENERATION -------------------------------------------------------


write_rds(df_clean, "Intermediate/df_clean.rds")
