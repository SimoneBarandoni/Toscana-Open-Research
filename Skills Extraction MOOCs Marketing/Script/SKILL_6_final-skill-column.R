library(tidyverse)
library(text2vec)
library(readxl)
library(rebus)
library(tidytext)
library(tm)
library(udpipe)

df_semantic <- readRDS("Intermediate/df_semantic_similarity.rds")

df_sequential <- readRDS("Intermediate/df_sequential_search.rds")

df_single <- readRDS("Intermediate/df_single_word_skills.rds")

df_original_new <- readRDS("Intermediate/df_original_new.rds")
 
esco_single <- readRDS("Intermediate/esco_original_1_single_word.rds")

esco_multiple <- readRDS("Intermediate/esco_original_1_multiple_word.rds")

esco_original_1 <- readRDS("Intermediate/esco_original_0.rds")

esco_en <- read_xlsx("Input/esco_en_skills.xlsx")

# SKILLS METADATA ---------------------------------------------------------

esco <- rbind(esco_single, esco_multiple)

esco <- esco %>% 
  distinct(preferredLabel, .keep_all = T)

# SPLITTING ROWS FOR EACH SKILL -------------------------------------------

df <- bind_rows(df_semantic %>% select(-doc_id_esco), df_sequential %>% select(-doc_id_esco), df_single %>% select(-doc_id_esco))


df <- separate_rows(df, skill, sep = " ; ")


pattern_fix <- esco_single %>% 
  select(preferredLabel) %>% 
  distinct(preferredLabel) %>% 
  mutate(skill = str_to_lower(preferredLabel))

pattern_fix_en <- esco_en %>% 
  select(skillpreferredLabel) %>% 
  distinct(skillpreferredLabel) %>% 
  rename(preferredLabel = skillpreferredLabel) %>% 
  mutate(skill = str_to_lower(preferredLabel))

df  <- df %>% 
  left_join(pattern_fix, by = "skill") %>% 
  mutate(skill = if_else(!is.na(preferredLabel), preferredLabel, skill)) %>% 
  select(-preferredLabel) %>% 
  left_join(pattern_fix_en, by = "skill") %>% 
  mutate(skill = if_else(!is.na(preferredLabel), preferredLabel, skill)) %>% 
  select(-preferredLabel) %>%
  mutate(skill = if_else(skill == "c", "C++", skill)) %>% 
  mutate(skill = if_else(skill == "aspp", "ASP+", skill)) %>% 
  mutate(skill = if_else(skill == "asp", "ASP+", skill))

# ALTERNATIVE LABELS REPLACE WITH PREFERRED --------------------------------------------

esco_preferred <- esco_original_1 %>% 
  mutate(tagPref = 1) %>% 
  select(preferredLabel,tagPref)

df <- df %>% 
  left_join(esco_preferred, by = c("skill" = "preferredLabel")) 

esco_1 <- esco_original_1 %>% 
  filter(!is.na(altLabels)) %>% 
  rename(skill = preferredLabel) 

for (i in 1:nrow(df))
{
  print(str_c("papers done ----->  ", round(i/nrow(df), digits = 4)*100, "%"))
  if (!is.na(df[i,"skill"]))
  {
    if (str_detect(df[i,"skill"],WRD))
    {
      if (is.na(df[i,"tagPref"]))
      {
        for (j in 1:nrow(esco_1))
        {
          if (!is.na(esco_1[[j,"skill"]]))
          {
            if (str_detect(esco_1[[j,"skill"]],whole_word(df[[i,"skill"]])))
            {
              break
            }
          }
          if (!is.na(esco_1[[j,"altLabels"]]) )
          {
            if (str_detect(esco_1[[j,"altLabels"]],whole_word(df[[i,"skill"]])))
            {
              # substitution with the corresponding preferred label
              df[[i,"skill"]] <- esco_1[[j,"skill"]] 
            }
          }
          
        }
      }
    }
    
  }
  
}

df <- left_join(df, esco, by = c("skill" = "preferredLabel")) %>% 
  select(-jobPreferredLabel, -jobAlternativeLabel, -n, -tagPref, -doc_id_esco)


# OUTPUT GENERATION -------------------------------------------------------

write_rds(df, "Intermediate/df_complete_skills.rds")

