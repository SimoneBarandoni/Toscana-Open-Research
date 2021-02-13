library(tidyverse)
library(text2vec)
library(readxl)
library(rebus)
library(tidytext)
library(tm)
library(udpipe)
library(textyr)
library(tibble)

df_clean <- readRDS("Intermediate/df_clean.rds")

df_1 <- readRDS("Intermediate/df_1.rds")

it_model <- udpipe::udpipe_load_model("Input/italian-isdt-ud-2.4-190531.udpipe")

esco_original_1_multiple_word <- readRDS("Intermediate/esco_original_1_multiple_word.rds") %>% 
  sample_n(1000)

aggettivi <- read_xlsx("Input/aggettivi.xlsx")

blacklist <- read.csv2("Input/blacklist1.csv")

not_ex <- read.csv2("Input/not_existing_words.csv")

italian_dictionary <- read.csv2("Input/dizionario-totale.csv")

lemma_correction <- read_xlsx("Input/wrong_lemmatized_ESCO.xlsx")


# BLACKLIST AND LEMMA CORRECTION PREPARATION ------------------------------

italian_dictionary <- italian_dictionary %>%
  filter(!str_detect(parola, one_or_more(WRD) %R% SPC %R% one_or_more(WRD))) %>%
  filter(!str_detect(parola, PUNCT)) %>%
  mutate(parola = str_to_lower(parola)) %>%
  rename(suggested_token = parola) %>%
  select(suggested_token) %>%
  distinct(suggested_token) %>%
  mutate(tag = 1)

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


# PREP FUN ----------------------------------------------------------------

prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^&^_[:^punct:]]", " ")
  #x = str_replace_all(x, "[[:punct:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")
  # remove stopwords
  x = removeWords(x, stopwords("italian"))
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")
}

# PREPARING ESCO DATASET --------------------------------------------

pat_agg <- or1(aggettivi$agg)

esco <- esco_original_1_multiple_word %>%
  select(preferredLabel, doc_id_esco) %>%
  rename(description = preferredLabel) %>%
  mutate(skill_original = description) %>%
  mutate(description = str_remove_all(description, pat_agg)) %>%
  mutate(tag = if_else(str_detect(description, whole_word("TIC")), 1, 0)) %>%
  filter(tag != 1) %>%
  select(-tag) %>%
  mutate(description = str_replace_all(description, one_or_more(SPC), " ")) %>%
  mutate(doc_id_esco = 100000 + doc_id_esco) %>%
  distinct(doc_id_esco, .keep_all = T)

esco1 <- esco %>%
  select(-skill_original)


# ESCO DATASET LEMMATIZATION -----------------------------------------------------------

joined_data <- esco1

joined_data_column <- joined_data[,c("doc_id_esco","description")]

pos_tagging <- udpipe::udpipe_annotate(object = it_model,
                                       x = joined_data_column$description,
                                       doc_id = joined_data_column$doc_id_esco) %>%
  as_tibble() %>%
  select(doc_id, token, lemma, upos )

pos_tagging_1 <- pos_tagging

# fixing wrong lematization

pos_tagging_1 <- left_join(pos_tagging_1,italian_dictionary, by = c("token" = "suggested_token")) %>%
  mutate(lemma = if_else(!is.na(tag), token, lemma)) %>%
  select(-tag)

pos_tagging_1 <- left_join(pos_tagging_1 %>% mutate(lemma = str_to_lower(lemma)),lemma_correction, by = "lemma") %>%
  mutate(lemma = if_else(!is.na(correct), correct, lemma)) %>%
  select(-correct)


pos_tagging_1 <- pos_tagging_1 %>%
  filter(!is.na(token)) %>%
  left_join(not_ex[,c("lemma","proper_lemma")], by = "lemma") %>%
  mutate(lemma = if_else(is.na(proper_lemma), lemma, proper_lemma)) %>%
  mutate(lemma = if_else(is.na(lemma), token, lemma))

# collapsing lemmatized text

POS <- pos_tagging_1 %>%
  rename(doc_id_esco = doc_id) %>%
  group_by(doc_id_esco) %>%
  summarise(description = paste(lemma, collapse = " ")) %>%
  ungroup()%>%
  mutate(doc_id_esco = as.numeric(doc_id_esco)) %>%
  arrange(doc_id_esco)


# PREPARING SENTENCES DATASET ---------------------------------------------------

df_clean_2 <- df_clean %>%
  rename(description = testoLemm)

df_clean_2 <- df_clean_2 %>%
  mutate( n_word = str_count(description, whole_word(one_or_more(WRD)))) %>%
  mutate(description = if_else(n_word < 3, "", description)) %>%
  select(-n_word)


df_clean_2 <- df_clean_2 %>%
  mutate(description = str_remove_all(description, whole_word("sarare venire"))) %>%
  mutate(description = str_remove_all(description, or(START %R% SPC, SPC %R% END))) %>%
  select(description, id_sentence)


df_clean_2 <- df_clean_2 %>%
  unnest_tokens(word, description) %>%
  group_by(id_sentence) %>%
  distinct(word, id_sentence, .keep_all = T) %>% #eliminating duplicates of a word in a phrase to improve tf idf measure
  ungroup() %>%
  group_by(id_sentence) %>%
  summarise(description = paste(word, collapse = " ")) %>%
  ungroup()


# TEMPORARILY ESCO SKILLS AND ESAMI PHRASES TOGHTER ---------------------------


# joined_data is the rowbind between jv and esco datasets
joined_data <- rbind(df_clean_2, POS %>% rename(id_sentence = doc_id_esco))


joined_data$description <- prep_fun(joined_data$description)

pat_agg <- or1(aggettivi$agg)

joined_data <- joined_data %>%
  mutate(description = str_remove_all(description, pat_agg)) %>%
  mutate(description = str_replace_all(description, one_or_more(SPC), " "))


# PREPARING TF IDF WORD LIST FOR EACH DATASET ----------------------------

# preparing the global tf idf list

joined_data_tf_idf <- joined_data %>%
  unnest_tokens(word, description)

word_count <- joined_data_tf_idf %>%
  count(word)

joined_data_tf_idf <- joined_data_tf_idf %>%
  group_by(id_sentence) %>%
  count(word) %>%
  bind_tf_idf(word, id_sentence, n)

# tf idf list for esco skills word

esco_word_tf_idf <- joined_data_tf_idf %>%
  filter(id_sentence >= 100000) %>%
  distinct(word, .keep_all = T) %>%
  arrange(-idf) %>%
  ungroup()

# tf idf list for phrases word
phrases_word_tf_idf <- joined_data_tf_idf %>%
  filter(id_sentence < 100000) %>%
  distinct(word, .keep_all = T) %>%
  arrange(-idf) %>%
  ungroup()


# esco word list
esco_word_tf_idf <- esco_word_tf_idf %>%
  filter(str_count(word, WRD) > 3)

nrow(esco_word_tf_idf)

mean <- mean(phrases_word_tf_idf$idf)
sd <- sd(phrases_word_tf_idf$idf)

#uncommon esami word list
phrases_word_tf_idf_1 <- phrases_word_tf_idf %>%
  filter(idf >= mean + sd) %>%
  filter(str_count(word, WRD) > 3)

nrow(phrases_word_tf_idf_1)

# uncommon esami word not existing in esco
phrases_word_tf_idf_1 <- anti_join(phrases_word_tf_idf_1, esco_word_tf_idf, by = "word" )
nrow(phrases_word_tf_idf)

# phrases word list
phrases_word_tf_idf <- phrases_word_tf_idf %>%
  filter(str_count(word, WRD) > 3)


# uncommon esco word
mean <- mean(esco_word_tf_idf$idf)
sd <- sd(esco_word_tf_idf$idf)

esco_word_tf_idf <- esco_word_tf_idf %>%
  filter(idf > mean - sd ) %>%
  filter(str_count(word, WRD) > 3)

# esco word not existing in esami
esco_word_tf_idf <- anti_join(esco_word_tf_idf, phrases_word_tf_idf, by = "word" ) %>%
  distinct(word)
nrow(esco_word_tf_idf)


# DELETING NOT USEFUL WORDS FROM BOTH DATASETS ----------------------------


phrases_word_tf_idf_1 <- or1(phrases_word_tf_idf_1$word)

df_clean_3 <- df_clean_2 %>% #testo_long_2_1
  mutate(description = str_remove_all(description,whole_word(phrases_word_tf_idf_1))) %>%
  mutate( n_word = str_count(description, whole_word(one_or_more(WRD)))) %>%
  mutate(description = if_else(n_word < 3, "", description)) %>%
  mutate(description = str_replace_all(description, one_or_more(SPC), " ")) %>%
  mutate(description = str_replace_all(description, or(START %R% SPC, SPC %R% END), "")) %>%
  select(-n_word)

esco_word_tf_idf <- or1(esco_word_tf_idf$word)

POS_1 <- POS %>%
  mutate(tag = if_else(str_detect(description, whole_word(esco_word_tf_idf)), 1, 0)) %>%
  filter(tag != 1) %>%
  select(-tag)

#manual detection of not useful skills to be deleted from skills dataset

POS_1$description <- prep_fun(POS_1$description)

not_useful_skills <- c("gestire dipartimento universitario",
                       "utilizzare sistema distribuzione globale",
                       "comprensione prodotto",
                       "utilizzare chiave",
                       "prevenzione inquinamento",
                       "teoria sistema", "consigliare cliente",
                       "utilizzare linguaggio positivo",
                       "monitorare comportamento cliente", "valutare esecuzione",
                       "fare ricerca", "sostenere il piano aziendale", "sviluppare software statistico",
                       "gestire posta", "regole gioco ", "amministrazione ufficio", "software industriale")

POS_1 <- POS_1 %>%
  filter(!(description %in% not_useful_skills))


# BINDING ESCO SKILLS AND ESAMI PHRASES TOGHTER ---------------------------


# joined_data is the rowbind between jv and esco datasets
joined_data <- rbind(df_clean_3, POS_1 %>% rename(id_sentence = doc_id_esco))


joined_data$description <- prep_fun(joined_data$description)

pat_agg <- or1(aggettivi$agg)

joined_data <- joined_data %>%
  mutate(description = str_remove_all(description, pat_agg)) %>%
  mutate(description = str_replace_all(description, one_or_more(SPC), " "))

df_clean_3 <- left_join(df_clean_3,df_1 %>% select(-id), by = c("id_sentence" = "id_sentence"))

# SIMILARITY MEASUREMENT --------------------------------------------------------

# magnitude order for esami sentences id is under 10k

df_1Length <- nrow(joined_data %>% filter(id_sentence < 100000))

escoLength <- nrow(joined_data %>% filter(id_sentence > 100000))

# cosine similarity operations

it = itoken(joined_data$description, progressbar = FALSE)
v = create_vocabulary(it)
#v = prune_vocabulary(v, term_count_min = 2)

# create an object which defines on how to transform list of tokens into vector space
vectorizer = vocab_vectorizer(v)

# create a document-term matrix
dtm = create_dtm(it, vectorizer)

# create TfIdf(Latent semantic analysis) model
tfidf = TfIdf$new()

# apply the model to the document-term matrix
dtm_tfidf = fit_transform(dtm, tfidf)


similarities <- data.frame(x = NA, description = NA, score_BERT = NA, id_sentence = NA, stringsAsFactors = FALSE)

# for each jv sentence
for (i in 1: df_1Length)
{
  # it similarities with the esco skills are computed (they are in the same datasets, but the
  # jv are excluded)
  d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf[i,, drop = F], y = dtm_tfidf[df_1Length+1:escoLength,, drop = F], method = "cosine", norm = "l2")
  x <- as.matrix(d1_d2_tfidf_cos_sim)
  x <- t(x)
  df <- cbind(as.data.frame(x), joined_data[c(df_1Length+1:escoLength),])
  
  frase <- df_clean_3$descr[i]
  
  colnames(df)[1] <- "x"
  
  df <- df %>%
    arrange(-x) %>%
    slice(1:30) %>%
    mutate(fraseOrig = frase) %>%
    select(description, fraseOrig, x, id_sentence) %>% 
    left_join(esco %>% select(-description), by = c("id_sentence" = "doc_id_esco")) %>% 
    select(fraseOrig, skill_original, x, id_sentence, description)

  df <- semantic_similarity(df,
                            method = c("BERT"),
                            lang = "ITA")

  df <- df %>%
    arrange(-score_BERT ) %>%
    slice(1) %>%
    select(x,score_BERT, description, id_sentence )
  
  
  
  similarities <- rbind(similarities,df)
  
  remove(d1_d2_tfidf_cos_sim)
  remove(x)
  remove(df)
  
}

similarities <- similarities %>%
  slice(-1) %>%
  rename(label_esco = description, doc_id_esco = id_sentence) %>%
  rename(cosine_similarity = score_BERT) %>%
  select(-x)

similarities <- cbind(similarities, joined_data[1:df_1Length,])

# warning due to the left join some skill rows are na

similarities_1 <- similarities %>%
  mutate(doc_id_esco = as.character(doc_id_esco)) %>%
  mutate(doc_id_esco = if_else(cosine_similarity > 0.37, doc_id_esco, "")) %>%
  left_join(esco1 %>% rename(skill = description) %>% mutate(doc_id_esco = as.character(doc_id_esco))
            , by = c("doc_id_esco"))

df_clean_3 <- similarities_1 %>%
  select(id_sentence, description, doc_id_esco, skill) %>%
  rename(word = description)

df_clean_3 <- left_join(df_clean_3,df_clean, by = c("id_sentence"="id_sentence"))

df_clean_3 <- df_clean_3 %>%
  select(-word) %>%
  rename(word = testoLemm)

# OUTPUT GENERATION -------------------------------------------------------

write_rds(df_clean_3,"Intermediate/df_semantic_similarity.rds")