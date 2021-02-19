library(tidyverse)
library(rebus)

df_3_1 <- readRDS("Intermediate/df_clean.rds")

esco <- readRDS("Intermediate/esco_original_1_single_word.rds")


# ONE WORD SKILLS EXTRACTION -------------------------------------------------------

pat <- or1(whole_word(esco$preferredLabel))

skills_dataset <- df_3_1 %>%
  rowwise() %>%
  mutate(skills_list = paste(unlist(str_match_all(str_to_lower(lemma_txt), whole_word(str_to_lower(pat)))), collapse = " ; ")) %>% 
  mutate(skills_list = if_else(skills_list == "c", "C#", skills_list)) %>% 
  mutate(skills_list = if_else(skills_list == "r", "R", skills_list)) %>% 
  ungroup()

# here the final skills dataset with esco id for each skill extracted
# note that we still have duplicated skills in the same sentence that problem will be fixed in next scripts
# in this case the dataset is already separated in a single skill per row

skills_dataset <- separate_rows(skills_dataset, skills_list, sep = " \\; ") %>% #be very careful because separate rows deletes empty obs
  left_join(esco, by = c("skills_list" = "preferredLabel")) 

# OUTPUT GENERATION -------------------------------------------------------

write_rds(skills_dataset , "Intermediate/df_single_word_skills.rds")
