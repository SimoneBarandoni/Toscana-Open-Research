library(readxl)
library(rebus)
library(tidyverse)

esco_original <- read_xlsx("Input/esco_it_skills.xlsx")

keywords <- readRDS("Input/keyword_list.rds")

ont <- readRDS("Input/ontology.rds") 

# SUBSETTING ESCO DATASET -------------------------------------------------

pattern_tic <- or(SPC %R% "TIC" %R% SPC, SPC %R% "TIC" %R% END)

# we esclude from the isco code list the 2320 because that code ar referred to education professionals
# their skills are out of scope fo us

isco <- c("2431", "1221", "1420", "1222", "3334", "2141", "1349", "1223", "1323", "2166", "1213", "1411", "3332", "1120", "5244")

esco_original_0 <- esco_original %>% 
  filter(iscoGroup %in% isco) %>% 
  mutate(iscoGroup = as.character(iscoGroup)) %>% 
  mutate(tag_tic = if_else(str_detect(preferredLabel, pattern_tic),1,0)) %>% 
  filter(tag_tic != 1) %>% 
  select(-tag_tic) 

esco_original_1 <- esco_original_0

temp <- separate_rows(esco_original_0,altLabels, sep = ";") %>% 
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

# set the trahsold on 6

keywords <- keywords %>% 
  filter(n > 5)


# ADDING KEYWORDS TO THE ESCO DATASET -------------------------------------

keywords <- keywords %>% 
  select(keyword) %>% 
  rename(preferredLabel = keyword) %>% 
  mutate(skillType = "add_on", reuseLevel = "", jobPreferredLabel = "", iscoGroup =  "", 
         jobAlternativeLabel = "", relationType = "")

esco_original_1 <- rbind(esco_original_1, keywords) %>% 
  mutate(doc_id_esco = row_number()) %>% 
  mutate(n = str_count(preferredLabel, whole_word(one_or_more(WRD)))) 
  
# SPLITTING ESCO DATASET --------------------------------------------------

esco_original_1_single_word <- esco_original_1 %>% 
  filter(n<2)

esco_original_1_multiple_word <- esco_original_1 %>% 
  filter(n>=2)

esco_original_1_multiple_word <- esco_original_1_multiple_word %>% 
  filter(n<7)


# ONTOLOGY ----------------------------------------------------------------

ont <- ont %>% 
  select(-id, -other) 

esco_original_1 <- left_join(esco_original_1, ont, by = c("preferredLabel" = "skill")) %>% 
  select(-relationType, -skillType, -reuseLevel, -jobAlternativeLabel)

esco_original_1_single_word <- left_join(esco_original_1_single_word, ont, by = c("preferredLabel" = "skill"))%>% 
  select(-relationType, -skillType, -reuseLevel, -jobAlternativeLabel)

esco_original_1_multiple_word <- left_join(esco_original_1_multiple_word, ont, by = c("preferredLabel" = "skill"))%>% 
  select(-relationType, -skillType, -reuseLevel, -jobAlternativeLabel)

# OUTPUT GENERATION -------------------------------------------------------

write_rds(esco_original_1, "Intermediate/esco_original_1.rds")

write_rds(esco_original_1_single_word, "Intermediate/esco_original_1_single_word.rds")

write_rds(esco_original_1_multiple_word, "Intermediate/esco_original_1_multiple_word.rds")
