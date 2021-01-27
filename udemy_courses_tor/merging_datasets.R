library(tidyverse)

x <- read_rds("Output/udemy_biz.rds")

y <- read_rds("Output/udemy_itech.rds")

z <- read_rds("Output/udemy_finance.rds")


# MERGING DATASETS --------------------------------------------------------

x <- x %>% 
  mutate(origin = "biz")

y <- y %>% 
  mutate(origin = "itech")

z <- z %>% 
  mutate(origin = "finance")

udemy_ing <- bind_rows(x,y,z)


# OUTPUT GENERATION -------------------------------------------------------

write_rds(udemy_ing, "Output/udemy_ing.rds")

