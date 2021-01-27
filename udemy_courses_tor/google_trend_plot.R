library(tidyverse)
library(zoo)

trend_1 <- read.csv("Input/trend_1.csv", row.names=NULL)

trend_2 <- read.csv("Input/trend_2.csv", row.names=NULL)

#there is a problem utilizing rolling mean on three weeks because the plot is no more
#in the range zero to one on the interest and this doesn't really make sense

# FIRST PLOT --------------------------------------------------------------

names(trend_1) = c("date", "ORE_BUSINESS_SCHOOL", "LIFE_LEARNING", "LACERBA", "STUDIOSAMO", "CORSI.IT")

# calculating rollmean every four dates aka one month 

trend_1 <- trend_1 %>% mutate(date =  as.Date(date)) %>% 
  mutate(ORE_BUSINESS_SCHOOL= rollmean(ORE_BUSINESS_SCHOOL, 4, fill = NA, align=c("right"))) %>% 
  mutate(LIFE_LEARNING= rollmean(LIFE_LEARNING, 4, fill = NA, align=c("right"))) %>% 
  mutate(LACERBA= rollmean(LACERBA, 4, fill = NA, align=c("right"))) %>% 
  mutate(STUDIOSAMO= rollmean(STUDIOSAMO, 4, fill = NA, align=c("right"))) %>% 
  mutate(CORSI.IT= rollmean(CORSI.IT, 4, fill = NA, align=c("right"))) %>% 
  filter_all(all_vars(!is.na(.)))

# editing dataset to get it in the tidy format 

t1 <- trend_1 %>% 
  select(ORE_BUSINESS_SCHOOL, date) %>% 
  rename(interest = ORE_BUSINESS_SCHOOL) %>% 
  mutate(search_term = "24 Ore Business School")

t2 <- trend_1 %>% 
  select(LIFE_LEARNING, date) %>% 
  rename(interest = LIFE_LEARNING) %>% 
  mutate(search_term = "Life Learning")

t3 <- trend_1 %>% 
  select(LACERBA, date) %>% 
  rename(interest = LACERBA) %>% 
  mutate(search_term = "Lacerba")

t4 <- trend_1 %>% 
  select(STUDIOSAMO, date) %>% 
  rename(interest = STUDIOSAMO) %>% 
  mutate(search_term = "Studiosamo")

t5 <- trend_1 %>% 
  select(CORSI.IT, date) %>% 
  rename(interest = CORSI.IT) %>% 
  mutate(search_term = "Corsi.it")

trend_1 <- bind_rows(t1,t2,t3,t4,t5)

# line plot

ggplot(aes(x = date, y = interest), data = trend_1) +
  geom_line(aes(color = search_term), size = 1) +
  theme_minimal() +
  ggsave("Output/italian_learning_site_gtrend.png",
         width = 10, height = 4, dpi = 300, units = "in", device='png')

# SECOND PLOT --------------------------------------------------------------

names(trend_2) = c("date", "UDEMY", "COURSERA", "LIFE_LEARNING")

# calculating rollmean every four dates aka one month 

trend_2 <- trend_2 %>% mutate(date =  as.Date(date)) %>% 
  mutate(UDEMY= rollmean(UDEMY, 4, fill = NA, align=c("right"))) %>% 
  mutate(COURSERA= rollmean(COURSERA, 4, fill = NA, align=c("right"))) %>% 
  mutate(LIFE_LEARNING= rollmean(LIFE_LEARNING, 4, fill = NA, align=c("right"))) %>% 
  filter_all(all_vars(!is.na(.)))

# editing dataset to get it in the tidy format 

t1 <- trend_2 %>% 
  select(UDEMY, date) %>% 
  rename(interest = UDEMY) %>% 
  mutate(search_term = "Udemy")

t2 <- trend_2 %>% 
  select(COURSERA, date) %>% 
  rename(interest = COURSERA) %>% 
  mutate(search_term = "Coursera")

t3 <- trend_2 %>% 
  select(LIFE_LEARNING, date) %>% 
  rename(interest = LIFE_LEARNING) %>% 
  mutate(search_term = "Life Learning")

trend_2 <- bind_rows(t1,t2,t3,t4,t5)

# line plot

ggplot(aes(x = date, y = interest), data = trend_2) +
  geom_line(aes(color = search_term), size = 1) +
  theme_minimal() +
  ggsave("Output/overall_learning_site_gtrend.png",
         width = 10, height = 4, dpi = 300, units = "in", device='png')
