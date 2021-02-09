library(tidyverse)
library(readxl)

#27 sheets

x <- read_xlsx("Cds Marketing.xlsx", sheet = 2)

for( i in 3:27){ 

b <- read_xlsx("Cds Marketing.xlsx", sheet = i)

x <- rbind(x,b)
  
  
}

x <- x %>% 
  mutate(id = row_number())

y <- x %>% 
  distinct(obiettivi_formativi_esame, .keep_all= T)

z <- x %>% 
  filter(!(id %in% y$id))


