library(tidyverse)
library(text2vec)
library(readxl)
library(rebus)
library(tidytext)
library(tm)
library(udpipe)

df_original <- readRDS("Intermediate/df_3_1.rds")

df_semantic <- readRDS("Intermediate/df_semantic_similarity.rds")

df_sequential <- readRDS("Intermediate/df_sequential_search.rds")

df_single <- readRDS("Intermediate/df_single_word_skills.rds")

esco_single <- readRDS("Intermediate/esco_original_1_single_word.rds")

esco_multiple <- readRDS("Intermediate/esco_original_1_multiple_word.rds")

esco_original_1 <- readRDS("Intermediate/esco_original_0.rds")

esco_en <- read_xlsx("Input/esco_en_skills.xlsx")

# SKILLS METADATA ---------------------------------------------------------

# the skills dataset are merged togheter 

esco <- rbind(esco_single, esco_multiple) %>% 
  distinct(preferredLabel, .keep_all = T)

# SPLITTING ROWS FOR EACH SKILL -------------------------------------------

# skills extractions datasets are merged together

df <- bind_rows(df_semantic %>% mutate(doc_id_esco = as.character(doc_id_esco)),
                df_sequential %>% mutate(doc_id_esco = as.character(doc_id_esco)), 
                df_single %>% mutate(doc_id_esco = as.character(doc_id_esco)) )

# the skills extracted from each sentence are been splitted in duplicated rows so that each observation
# of the dataset is a single skill extracted
# in addiction duplicated skills are been removed becuase in the same sentence must be only one skill
# of a kind

df <- separate_rows(df, skills_list, doc_id_esco, sep = " ; ") %>% 
  distinct(overall_id, skills_list, .keep_all = T) %>% 
  filter(str_detect(skills_list, WRD) & !is.na(skills_list))


# SKILLS PATTERN CORRECTION -----------------------------------------------


#  some correction to skills label here

pattern_fix <- esco_single %>% 
  select(preferredLabel) %>% 
  distinct(preferredLabel) %>% 
  mutate(skill = str_to_lower(preferredLabel))

pattern_fix_en <- esco_en %>% 
  select(skillpreferredLabel) %>% 
  distinct(skillpreferredLabel) %>% 
  rename(preferredLabel = skillpreferredLabel) %>% 
  mutate(skill = str_to_lower(preferredLabel))

# skills label correction are applied to the skills dataset

df  <- df %>% 
  left_join(pattern_fix, by = c("skills_list" = "skill")) %>% 
  mutate(skills_list = if_else(!is.na(preferredLabel), preferredLabel, skills_list)) %>% 
  select(-preferredLabel) %>% 
  left_join(pattern_fix_en, by = c("skills_list" = "skill")) %>% 
  mutate(skills_list = if_else(!is.na(preferredLabel), preferredLabel, skills_list)) %>% 
  select(-preferredLabel) %>%
  mutate(skills_list = if_else(skills_list == "c", "C++", skills_list)) %>% 
  mutate(skills_list = if_else(skills_list == "aspp", "ASP+", skills_list)) %>% 
  mutate(skills_list = if_else(skills_list == "asp", "ASP+", skills_list))

# ALTERNATIVE LABELS REPLACE WITH PREFERRED --------------------------------------------

# ultimately esco alternative labels are replaced with the preferred label to standardize slightly different
# labels refferred to same skill

esco_preferred <- esco_original_1 %>% 
  mutate(tagPref = 1) %>% 
  select(preferredLabel, tagPref)

df <- df %>% 
  left_join(esco_preferred, by = c("skills_list" = "preferredLabel")) 

esco_1 <- esco_original_1 %>% 
  filter(!is.na(altLabels)) %>% 
  rename(skills_list = preferredLabel) 

# the substitution of the alternative label in the skills list column is performed with a for cycle 

for (i in 1:nrow(df))
{
  print(str_c("papers done ----->  ", round(i/nrow(df), digits = 4)*100, "%"))
  if (!is.na(df[i,"skills_list"]))
  {
    if (str_detect(df[i,"skills_list"],WRD))
    {
      if (is.na(df[i,"tagPref"]))
      {
        for (j in 1:nrow(esco_1))
        {
          if (!is.na(esco_1[[j,"skills_list"]]))
          {
            if (str_detect(esco_1[[j,"skills_list"]],whole_word(df[[i,"skills_list"]])))
            {
              break
            }
          }
          if (!is.na(esco_1[[j,"altLabels"]]) )
          {
            if (str_detect(esco_1[[j,"altLabels"]], whole_word(df[[i,"skills_list"]])))
            {
              # substitution with the corresponding preferred label
              df[[i,"skills_list"]] <- esco_1[[j,"skills_list"]] 
            }
          }
          
        }
      }
    }
    
  }
  
}

# finally we have the complete dataset with the skills metadata


# JOIN SKILLS EXTRACTION DATASET WITH THE ORIGINAL SENTENCES DATAS --------

# now is very important to note that due to the left join the original overall_id will be duplicated when
# a single sentences cotains more than one skills so that overall id is not anymore univoque and we have
# to create a new unique id for the new dataset that has the skill as observations

final_dataset <- df_original %>% 
  left_join(df %>% select(skills_list, overall_id, doc_id_esco), by = "overall_id") %>% 
  left_join(esco_original_1 %>% mutate(doc_id_esco = as.character(doc_id_esco)), # nb esco orignal 1 have only the preferred label column as we want
            by = c("doc_id_esco" = "doc_id_esco")) %>%
  arrange(overall_id) %>% 
  mutate(final_dataset_id = row_number())


# OUTPUT GENERATION -------------------------------------------------------

write_rds(final_dataset, "Intermediate/df_complete_skills.rds")

