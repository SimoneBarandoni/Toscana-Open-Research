library(rebus)
library(tidyverse)
library(tidytext)

df_1 <- readRDS("Intermediate/df_1.rds")

df_2 <- readRDS("Intermediate/df_2.rds")

original_df <- readRDS("Input/cds_marketing_original.rds")

# df_2 are been cleaned manually so there is no need to clean that column


# DESCR COLUMN TEXT CLEANING BY RULES --------------------------------------------------

# cleaning useless sentences with colon

df_1_1 <- df_1 %>% 
  filter(!(str_detect(obiettivi_formativi_esame_clean, "\\:")
           & (str_detect(obiettivi_formativi_esame_clean,
                        whole_word(case_insensitive("student") %R% one_or_more(WRD))) | 
                str_detect(obiettivi_formativi_esame_clean,
                            whole_word(case_insensitive("didattic") %R% one_or_more(WRD))) |
                str_detect(obiettivi_formativi_esame_clean, whole_word(case_insensitive("corso")))
                )))

df_1_1 <- df_1_1 %>% 
  filter(!(str_detect(obiettivi_formativi_esame_clean, "\\:")
           & (str_detect(obiettivi_formativi_esame_clean, case_insensitive("capacità")) &
                str_detect(obiettivi_formativi_esame_clean, case_insensitive("comprensione")) &
                 str_detect(obiettivi_formativi_esame_clean, case_insensitive("conoscenz"))
                  )))
        
# take the negative version of cleaned dataset in order to check the cleaning fairness

df_dirty_1 <- df_1 %>% 
  left_join(df_1_1 %>% select(id, id_sentence) %>% mutate(tag = 1), by = c("id", "id_sentence")) %>% 
  filter(is.na(tag))


# MERGING COLUMN ----------------------------------------------------------

# merge skill and descr column uniforming their names 

df_1_1 <- df_1_1 %>% 
  select(-obiettivi_formativi, -obiettivi_formativi_clean, -obiettivi_formativi_esame) %>% 
  rename(merged_sentences = obiettivi_formativi_esame_clean )

df_2 <- df_2 %>% 
  select(-obiettivi_formativi, -obiettivi_formativi_esame) %>% 
  rename(merged_sentences = obiettivi_formativi_clean )

# retrieve original metadata for each esame from orignal data frame
# filter empty rows and assign a new consistent sentences id

df_3 <- bind_rows(df_1_1, df_2) %>% 
  left_join(original_df %>% select(-ID_RAD, -id, -tipo, -esame, -anno_cds) %>% 
              distinct(ateneo, classe_di_laurea, anno,corso_di_studio, .keep_all = T ), 
            by = c("ateneo", "classe_di_laurea", "anno", "corso_di_studio")) %>% 
  filter(str_detect(merged_sentences, WRD) & !is.na(merged_sentences)) %>% 
  arrange(id) %>% 
  group_by(id) %>% 
  mutate(sentences_id = row_number()) %>% 
  ungroup() 


# OUTPUT GENERATION -------------------------------------------------------

# here the two dataset for each textual field cleaned

write_rds(df_1_1, "Intermediate/df_1_1.rds")

# this is the merged version of the two above

write_rds(df_3, "Intermediate/df_3.rds")

# this datasets contains the filtered sentences of the two textual field

write_rds(df_dirty_1, "Intermediate/df_dirty_1.rds")
