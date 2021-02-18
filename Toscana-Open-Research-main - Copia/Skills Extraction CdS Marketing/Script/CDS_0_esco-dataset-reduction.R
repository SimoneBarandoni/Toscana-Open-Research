library(readxl)
library(rebus)
library(tidyverse)

esco_original <- read_xlsx("Input/esco_it_skills.xlsx") #don't know why but a large amount of skills have multiple spaces

keywords <- readRDS("Input/keyword_list.rds")

ont <- readRDS("Input/ontology.rds") 

esco_en <- read_xlsx("Input/esco_en_skills.xlsx")


# SUBSETTING ESCO DATASET -------------------------------------------------

pattern_tic <- or(SPC %R% "TIC" %R% SPC, SPC %R% "TIC" %R% END)

# we esclude from the isco code list the 2320 because that code ar referred to education professionals
# their skills are out of scope fo us

isco <- c("2431", "1221", "1420", "1222", "3334", "2141", "1349", "1223", "1323", "2166", "1213", "1411", "3332", "1120", "5244")

esco_original_0 <- esco_original %>% 
  filter(iscoGroup %in% isco) %>% 
  mutate(iscoGroup = as.character(iscoGroup)) %>% 
  mutate(preferredLabel = str_replace_all(preferredLabel, SPC %R% one_or_more(SPC), " ")) %>% 
  mutate(preferredLabel = str_remove_all(preferredLabel, or(START %R% SPC, SPC %R% END))) %>% 
  mutate(tag_tic = if_else(str_detect(preferredLabel, pattern_tic),1,0)) %>% 
  filter(tag_tic != 1) %>% 
  select(-tag_tic) 

esco_original_1 <- esco_original_0

temp <- separate_rows(esco_original_0, altLabels, sep = or(";", "\\n")) %>% 
  select(-preferredLabel) %>% 
  mutate(preferredLabel = altLabels) %>% 
  distinct(preferredLabel,.keep_all = TRUE) %>% 
  filter(!is.na(preferredLabel))

esco_original_1 <- rbind(esco_original_1, temp) %>% 
  filter(!is.na(preferredLabel)) %>% 
  distinct(preferredLabel, .keep_all = TRUE) %>% 
  select(-altLabels) %>% 
  distinct(preferredLabel, .keep_all = T)


# PREPARING SKILLS ADD ON DATASET -----------------------------------------


keywords <- keywords %>% 
  rename(keyword = keyword_new) %>% 
  mutate(keyword = str_to_lower(keyword)) %>% 
  group_by(keyword) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  filter(!(str_to_lower(keyword) %in% str_to_lower(esco_original_1$preferredLabel)))  #include alternative label and esco eng also

# trashold selection

keywords <- keywords %>% 
  mutate(cumsum = cumsum(n) * 100 / sum(keywords$n)) 

# if the distribution is too flat and then using the 80 20 rule we will take on too mutch observations
# select an arbitrary trashold with the aid of the quantile plot

x <- keywords$n

#calculate quantile for the distribution
quantile(x, probs=c(1:1000/1000)) # probs stand for the number of quantile calculated, in this case 100 to have the percentile fro 1% to 100%


#plot quantile of the distribution
plot(quantile(x, probs=c(1:1000/1000)), panel.first = grid(), main = " x ticks every 25")
xtick <- seq(0, 1000, by=25)
axis(side=1, at=xtick, labels = F)
ytick <- seq(0, 45, by = 2)
axis(side=2, at=ytick, labels = F)

abline(v = xtick, h = ytick, col = "grey")

# set the treshold on 6

keywords <- keywords %>% 
  filter(n > 5) %>% 
  mutate(keyword = str_replace(keyword, fixed("e-commerce"), "ecommerce")) # we have to make this fix to avoid problem further


# ADDING KEYWORDS TO THE ESCO DATASET -------------------------------------


keywords <- keywords %>% 
  select(keyword) %>% 
  rename(preferredLabel = keyword) %>% 
  mutate(skillType = "add_on", iscoGroup = "")

esco_original_1 <- esco_original_1 %>% 
  select(-relationType, -reuseLevel, -jobAlternativeLabel, -jobPreferredLabel)

esco_original_1 <- rbind(esco_original_1, keywords) %>% 
  mutate(n = str_count(preferredLabel, whole_word(one_or_more(WRD))))


# ADDING ESCO EN ----------------------------------------------------------

esco_en_1 <- esco_en %>% 
  select(skillType, skillpreferredLabel, iscoGroup) %>% 
  distinct(skillpreferredLabel, .keep_all = T) %>% 
  rename(preferredLabel = skillpreferredLabel) %>% 
  mutate(n = str_count(preferredLabel, whole_word(one_or_more(WRD)))) %>% 
  filter(n < 2) %>% 
  mutate(skillType = "esco_en", iscoGroup = "") %>% 
  filter(!(str_to_lower(preferredLabel) %in% str_to_lower(esco_original_1$preferredLabel)))

esco_original_1 <- rbind(esco_original_1, esco_en_1) %>% 
  mutate(n = str_count(preferredLabel, whole_word(one_or_more(WRD)))) %>% 
  filter(n < 7) %>% 
  filter(str_detect(preferredLabel, ALNUM)) %>% 
  mutate(doc_id_esco = row_number())

#temporarly reducing esco dataset
#esco_original_1 <- esco_original_1 %>% sample_n(1000)


# SPLITTING ESCO DATASET --------------------------------------------------


esco_original_1_single_word <- esco_original_1 %>% 
  filter(n < 2)

esco_original_1_multiple_word <- esco_original_1 %>% 
  filter(n >= 2)


# ONTOLOGY ----------------------------------------------------------------


ont <- ont %>% 
  select(-id, -other) 

#note that the skills that will have na value to ontology metadata are the alternative labels

esco_original_1 <- left_join(esco_original_1, ont %>%
                               filter(!((skill %in% keywords$preferredLabel) | (skill %in% esco_en_1$preferredLabel))),
                             by = c("preferredLabel" = "skill")) %>% 
  select(-Level.3.code) %>% 
  mutate(Level.3.preferred.term = if_else(skillType == "add_on", "add_on", Level.3.preferred.term)) %>% 
  mutate(Level.2.preferred.term = if_else(skillType == "add_on", "add_on", Level.2.preferred.term)) %>% 
  mutate(Level.1.preferred.term = if_else(skillType == "add_on", "add_on", Level.1.preferred.term)) %>% 
  mutate(Level.0.preferred.term = if_else(skillType == "add_on", "add_on", Level.0.preferred.term)) %>% 
  mutate(Level.3.preferred.term = if_else(skillType == "esco_en", "esco_en", Level.3.preferred.term)) %>% 
  mutate(Level.2.preferred.term = if_else(skillType == "esco_en", "esco_en", Level.2.preferred.term)) %>% 
  mutate(Level.1.preferred.term = if_else(skillType == "esco_en", "esco_en", Level.1.preferred.term)) %>% 
  mutate(Level.0.preferred.term = if_else(skillType == "esco_en", "esco_en", Level.0.preferred.term))

esco_original_1_single_word <- left_join(esco_original_1_single_word, ont %>%
                                           filter(!((skill %in% keywords$preferredLabel) | (skill %in% esco_en_1$preferredLabel))),
                                         by = c("preferredLabel" = "skill")) %>% 
  select(-Level.3.code) %>% 
  mutate(Level.3.preferred.term = if_else(skillType == "add_on", "add_on", Level.3.preferred.term)) %>% 
  mutate(Level.2.preferred.term = if_else(skillType == "add_on", "add_on", Level.2.preferred.term)) %>% 
  mutate(Level.1.preferred.term = if_else(skillType == "add_on", "add_on", Level.1.preferred.term)) %>% 
  mutate(Level.0.preferred.term = if_else(skillType == "add_on", "add_on", Level.0.preferred.term)) %>% 
  mutate(Level.3.preferred.term = if_else(skillType == "esco_en", "esco_en", Level.3.preferred.term)) %>% 
  mutate(Level.2.preferred.term = if_else(skillType == "esco_en", "esco_en", Level.2.preferred.term)) %>% 
  mutate(Level.1.preferred.term = if_else(skillType == "esco_en", "esco_en", Level.1.preferred.term)) %>% 
  mutate(Level.0.preferred.term = if_else(skillType == "esco_en", "esco_en", Level.0.preferred.term))

esco_original_1_multiple_word <- left_join(esco_original_1_multiple_word, ont %>%
                                             filter(!((skill %in% keywords$preferredLabel) | (skill %in% esco_en_1$preferredLabel))),
                                           by = c("preferredLabel" = "skill"))  %>% 
  select(-Level.3.code) %>% 
  mutate(Level.3.preferred.term = if_else(skillType == "add_on", "add_on", Level.3.preferred.term)) %>% 
  mutate(Level.2.preferred.term = if_else(skillType == "add_on", "add_on", Level.2.preferred.term)) %>% 
  mutate(Level.1.preferred.term = if_else(skillType == "add_on", "add_on", Level.1.preferred.term)) %>% 
  mutate(Level.0.preferred.term = if_else(skillType == "add_on", "add_on", Level.0.preferred.term))

esco_original_1_single_word %>% filter(is.na(Level.3.preferred.term)) %>% View()

esco_original_1_multiple_word %>% filter(is.na(Level.3.preferred.term)) %>% View()


# CREATE A SWITCH TABLE FOR ALTERNATIVE LABELS ----------------------------

# creating a switch table that shows the link between the skills used for the extraction
# and the preferred label of each skill

switch_table <- separate_rows(esco_original_0, altLabels, sep = or(";", "\\n")) %>% 
  distinct(altLabels, .keep_all = TRUE) %>% 
  filter(!is.na(altLabels) & str_detect(altLabels, ALNUM)) %>% 
  left_join(esco_original_1 %>% select(preferredLabel, doc_id_esco), by = "preferredLabel") %>% 
  mutate(n = str_count(preferredLabel, whole_word(one_or_more(WRD)))) %>% 
  select(preferredLabel, altLabels, doc_id_esco) %>% 
  rename(doc_id_esco_x = doc_id_esco) %>% 
  mutate(doc_id_esco_x = as.character(doc_id_esco_x))


# OUTPUT GENERATION -------------------------------------------------------


write_rds(switch_table, "Intermediate/switch_table.rds") # switch table to be used to replace alt labels

write_rds(esco_original_1, "Intermediate/esco_original_1.rds")

write_rds(esco_original_1_single_word, "Intermediate/esco_original_1_single_word.rds")

write_rds(esco_original_1_multiple_word, "Intermediate/esco_original_1_multiple_word.rds")
