library(tidyverse)
library(rebus)

x <- read.csv("Input/udemy_mktg.csv", encoding = "UTF-8")

pat <- char_class("|+\\[()\\]")

y <- x %>% 
  filter(str_length(titoli) > 10) %>%
  mutate(titoli = str_remove_all(titoli, pat)) %>% 
  select(titoli)

y <- or1(y$titoli)

p <- x %>% 
  mutate(link = str_remove_all(link, pat)) %>% 
  rowwise() %>% 
  mutate(link = paste0(unlist(str_extract(link, y)), collapse = " ")) %>% 
  filter(link != "NA")

t1 <- p %>% 
  select(link, autor) %>% 
  distinct(link, autor) %>% 
  filter(autor != "") %>% 
  group_by(link) %>% 
  summarise(autor = paste(autor, collapse = " and ")) %>% 
  ungroup()

t2 <- p %>% 
  select(link, date) %>% 
  distinct(link, date) %>% 
  filter(date != "") %>% 
  group_by(link) %>% 
  summarise(date = paste(date, collapse = " and ")) %>% 
  ungroup()

t3 <- p %>% 
  select(link, skill) %>% 
  distinct(link, skill) %>% 
  filter(skill != "") %>% 
  group_by(link) %>% 
  summarise(skill = paste(skill, collapse = " ")) %>% 
  ungroup()

t4 <- p %>% 
  select(link, descr) %>% 
  distinct(link, descr) %>% 
  filter(descr != "") %>% 
  group_by(link) %>% 
  summarise(descr = paste(descr, collapse = " ")) %>% 
  ungroup()


p <- p %>% 
  select(X.U.FEFF.web.scraper.order, link) %>% 
  distinct(link, .keep_all = T) %>% 
  left_join(t1, by = "link") %>% 
  left_join(t2, by = "link") %>%
  left_join(t3, by = "link") %>%
  left_join(t4, by = "link")
  
p <- p %>% 
  mutate(descr = str_remove_all(descr, or(START %R% "Descrizione", "Mostra di piùMostra meno"))) %>% 
  mutate(skill = str_remove_all(skill, or(START %R% "Cosa imparerai"))) %>% 
  rename(titolo = link, order = X.U.FEFF.web.scraper.order) %>% 
  arrange(order) 

p[is.na(p)]<- ""
p[p=="null"]<- ""
p[p=="NA"]<- ""

udemy_mktg <- p


# OUTPUT GENERATION -------------------------------------------------------

write_rds(udemy_mktg, "Output/udemy_mktg.rds")

