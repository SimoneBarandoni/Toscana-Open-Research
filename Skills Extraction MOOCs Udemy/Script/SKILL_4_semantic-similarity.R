library(tidyverse)
library(text2vec)
library(readxl)
library(rebus)
library(tidytext)
library(tm)
library(udpipe)
library(textyr)

df_3_1 <- readRDS("Intermediate/df_3_1.rds")

esco_original_1_multiple_word <- readRDS("Intermediate/esco_original_1_multiple_word.rds")

it_model <- udpipe::udpipe_load_model("Input/italian-isdt-ud-2.4-190531.udpipe")

# this datasets are all used to improve udpipe lemmatizaton performances

aggettivi <- read_xlsx("Input/aggettivi.xlsx")

not_ex <- read.csv2("Input/not_existing_words.csv")

italian_dictionary <- read.csv2("Input/dizionario-totale.csv")

lemma_correction <- read_xlsx("Input/wrong_lemmatized_ESCO.xlsx")

# temporarly reducing dataset rows 
df_3_1 <- df_3_1 %>% slice(1:200)

#esco_original_1_multiple_word <- esco_original_1_multiple_word %>% sample_n(30)

# BLACKLIST AND LEMMA CORRECTION PREPARATION ------------------------------

italian_dictionary <- italian_dictionary %>%
  filter(!str_detect(parola, one_or_more(WRD) %R% SPC %R% one_or_more(WRD))) %>%
  filter(!str_detect(parola, PUNCT)) %>%
  mutate(parola = str_to_lower(parola)) %>%
  rename(suggested_token = parola) %>%
  select(suggested_token) %>%
  distinct(suggested_token) %>%
  mutate(tag = 1)

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
  mutate(skill = description) %>% # we have to mantain an original skill label version 
  mutate(description = str_remove_all(description, pat_agg)) %>%
  mutate(tag = if_else(str_detect(description, whole_word("TIC")), 1, 0)) %>%
  filter(tag != 1) %>%
  select(-tag) %>%
  mutate(description = str_replace_all(description, one_or_more(SPC), " ")) %>%
  mutate(doc_id_esco = 10000 + doc_id_esco) %>%
  distinct(doc_id_esco, .keep_all = T)

# ESCO DATASET LEMMATIZATION -----------------------------------------------------------

# esco dataset must be lemmatized in order to improve text2vec semantic similarity performances
# with bert instead we will use the original skill text version

joined_data <- esco %>%
  select(-skill)


joined_data_column <- joined_data[,c("doc_id_esco", "description")]

pos_tagging <- udpipe::udpipe_annotate(object = it_model,
                                       x = joined_data_column$description,
                                       doc_id = joined_data_column$doc_id_esco) %>%
  as_tibble() %>%
  select(doc_id, token, lemma, upos )

# fixing wrong lemmatization

pos_tagging <- left_join(pos_tagging,italian_dictionary, by = c("token" = "suggested_token")) %>%
  mutate(lemma = if_else(!is.na(tag), token, lemma)) %>%
  select(-tag)

pos_tagging <- left_join(pos_tagging %>% mutate(lemma = str_to_lower(lemma)),lemma_correction, by = "lemma") %>%
  mutate(lemma = if_else(!is.na(correct), correct, lemma)) %>%
  select(-correct)

pos_tagging <- pos_tagging %>%
  filter(!is.na(token)) %>%
  left_join(not_ex[,c("lemma","proper_lemma")], by = "lemma") %>%
  mutate(lemma = if_else(is.na(proper_lemma), lemma, proper_lemma)) %>%
  mutate(lemma = if_else(is.na(lemma), token, lemma))

# collapsing lemmatized text

pos_tagging <- pos_tagging %>%
  rename(doc_id_esco = doc_id) %>%
  group_by(doc_id_esco) %>%
  summarise(description = paste(lemma, collapse = " ")) %>%
  ungroup()%>%
  mutate(doc_id_esco = as.numeric(doc_id_esco)) %>%
  arrange(doc_id_esco)


# BINDING ESCO SKILLS AND ESAMI PHRASES TOGHTER ---------------------------

# joined_data is the rowbind between jv and esco datasets
joined_data <- rbind(df_3_1 %>% select(sentences_lemm, overall_id) %>% 
                       rename( description = sentences_lemm, doc_id = overall_id),
                     pos_tagging %>% rename(doc_id = doc_id_esco))


joined_data$description <- prep_fun(joined_data$description)

pat_agg <- or1(aggettivi$agg)

joined_data <- joined_data %>%
  mutate(description = str_remove_all(description, pat_agg)) %>%
  mutate(description = str_replace_all(description, one_or_more(SPC), " "))


# SIMILARITY MEASUREMENT --------------------------------------------------------

# magnitude order for esami sentences id is under 10k

df_3_1Length <- nrow(joined_data %>% filter(doc_id < 10000))

escoLength <- nrow(joined_data %>% filter(doc_id > 10000))

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


similarities <- data.frame(x = NA, description = NA, score_BERT = NA, doc_id = NA, stringsAsFactors = FALSE)

# for each jv sentence
for (i in 1: df_3_1Length)
{
  # it similarities with the esco skills are computed (they are in the same datasets, but the
  # jv are excluded)
  d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf[i,, drop = F], y = dtm_tfidf[df_3_1Length+1:escoLength,, drop = F], method = "cosine", norm = "l2")
  x <- as.matrix(d1_d2_tfidf_cos_sim)
  x <- t(x)
  df <- cbind(as.data.frame(x), joined_data[c(df_3_1Length+1:escoLength),])
  
  frase <- df_3_1$merged_sentences[i] # taking the original sentence without text cleaning op from moocs datasets
  
  colnames(df)[1] <- "x" # changing column name
  
  df <- df %>% 
    arrange(-x) %>%
    slice(1:10) %>%
    mutate(fraseOrig = frase) %>% # adding a column with the analyzed sentencen without text cleaning op
    select(description, fraseOrig, x, doc_id) %>% 
    left_join(esco, by = c("doc_id" = "doc_id_esco")) %>% 
    select(x, doc_id, fraseOrig, skill_original)
  
  # now we have a daset with the first 30 esco skill for the given 
  
  df <- semantic_similarity(df,
                            method = c("BERT"),
                            lang = "ITA")
  
  df <- df %>%
    arrange(-score_BERT ) %>%
    slice(1) %>%
    select(x,score_BERT, description, doc_id )
  
  
  
  similarities <- rbind(similarities,df)
  
  remove(d1_d2_tfidf_cos_sim)
  remove(x)
  remove(df)
  
}

remove(dtm, v, dtm_tfidf, it_model, italian_dictionary, aggettivi, not_ex, lemma_correction)

similarities <- similarities %>%
  slice(-1) %>%
  rename(label_esco = description, doc_id_esco = doc_id) %>%
  rename(cosine_similarity = score_BERT) %>%
  select(-x)

similarities <- cbind(similarities, df_3_1 %>% select(merged_sentences, overall_id))

# CHOSING SIMILARITY TRASHOLD ---------------------------------------------

hist(similarities$cosine_similarity)
median(similarities$cosine_similarity)
mean(similarities$cosine_similarity)
sd(similarities$cosine_similarity)

x <- similarities %>% arrange(-cosine_similarity) %>% pull(cosine_similarity)
x <- similarities$cosine_similarity

#calculate quantile for the distribution
quantile(x, probs=c(1:100/100)) # probs stand for the number of quantile calculated, in this case 100 to have the percentile fro 1% to 100%

#plot quantile of the distribution
plot(quantile(x, probs=c(1:100/100)), panel.first = grid())

similarities_1 <- similarities %>%
  mutate(doc_id_esco = as.character(doc_id_esco)) %>%
  mutate(doc_id_esco = if_else(cosine_similarity > 0.37, doc_id_esco, "")) %>%
  left_join(esco %>% select(-description) %>% mutate(doc_id_esco = as.character(doc_id_esco)),
            by = c("doc_id_esco"))

skills_dataset <- df_3_1 %>%
  left_join(similarities_1 %>% select(skill, overall_id, doc_id_esco) %>% rename(skills_list = skill),
            by = ("overall_id")) %>% 
  mutate(doc_id_esco = as.numeric(doc_id_esco)) %>% 
  mutate(doc_id_esco = doc_id_esco - 10000) %>%  # return to doc id esco original value
  mutate(doc_id_esco = as.character(doc_id_esco))

# OUTPUT GENERATION -------------------------------------------------------

write_rds(skills_dataset,"Intermediate/df_semantic_similarity.rds")
