library(tidyverse)
library(rebus)

x <- read.csv("Input/udemy_mktg_v_2.csv", encoding = "UTF-8")

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
  select(link, link.href,  autor) %>% 
  distinct(link.href, autor) %>% 
  filter(autor != "") %>% 
  group_by(link.href) %>% 
  summarise(autor = paste(autor, collapse = " and ")) %>% 
  ungroup()

t2 <- p %>% 
  select(link, link.href,  date) %>% 
  distinct(link.href, date) %>% 
  filter(date != "") %>% 
  group_by(link.href) %>% 
  summarise(date = paste(date, collapse = " and ")) %>% 
  ungroup()

t3 <- p %>% 
  select(link, link.href,  skill) %>% 
  distinct(link.href, skill) %>% 
  filter(skill != "") %>% 
  group_by(link.href) %>% 
  summarise(skill = paste(skill, collapse = " ")) %>% 
  ungroup()

t4 <- p %>% 
  select(link, link.href,  descr) %>% 
  distinct(link.href, descr) %>% 
  filter(descr != "") %>% 
  group_by(link.href) %>% 
  summarise(descr = paste(descr, collapse = " ")) %>% 
  ungroup()

t5 <- p %>% 
  select(link, link.href,  media) %>% 
  distinct(link.href, media) %>% 
  filter(media != "") %>% 
  group_by(link.href) %>% 
  summarise(media = paste(media, collapse = " ")) %>% 
  ungroup()

t6 <- p %>% 
  select(link, link.href,  n_val) %>% 
  distinct(link.href, n_val) %>% 
  filter(n_val != "") %>% 
  group_by(link.href) %>% 
  summarise(n_val = paste(n_val, collapse = " ")) %>% 
  ungroup()


t7 <- p %>% 
  select(link, link.href,  n_stud) %>% 
  distinct(link.href, n_stud) %>% 
  filter(n_stud != "") %>% 
  group_by(link.href) %>% 
  summarise(n_stud = paste(n_stud, collapse = " ")) %>% 
  ungroup()

p <- p %>% 
  select(X.U.FEFF.web.scraper.order, link, link.href) %>% 
  distinct(link.href, .keep_all = T) %>% 
  left_join(t1, by = "link.href") %>% 
  left_join(t2, by = "link.href") %>%
  left_join(t3, by = "link.href") %>%
  left_join(t4, by = "link.href") %>% 
  left_join(t5, by = "link.href") %>%
  left_join(t6, by = "link.href") %>% 
  left_join(t7, by = "link.href")

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

write_rds(udemy_mktg, "Output/udemy_mktg_v_2.rds")

