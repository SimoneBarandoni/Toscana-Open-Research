library(readxl)
library(rebus)
library(tidyverse)
library(tidytext)
library(tm)

df_3_1 <- readRDS("Intermediate/df_3_1.rds")

aggettivi <- read_xlsx("Input/aggettivi.xlsx")

esco_original_1_multiple_word <- readRDS("Intermediate/esco_original_1_multiple_word.rds")

start_time <- Sys.time()

# temporary sampling datasets to reduce computational time
#esco_original_1_multiple_word <- esco_original_1_multiple_word %>% sample_n(1000)

#df_3_1 <- df_3_1 %>% slice(1:200)

# FUNCTION PREP_FUN ---------------------------------------------------------------

# load a text cleaning function

prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols   
  x = str_replace_all(x, "[^&^_[:^punct:]]", " ")
  # remove stopwords
  x = removeWords(x, stopwords("italian"))
  # collapse multiple spaces
  x = str_replace_all(x, SPC %R% one_or_more(SPC), " ")
  x = str_remove_all(x, or(START %R% SPC, SPC %R% END))
}


# FUNCTION PREFIX_MATCH ---------------------------------------------------

# prefix match is used to detect word with a common root without aid of lemmatization 

prefix_match = function(w1, w2, l1, l2) 
{
  if(is.na(w1) | is.na(w2) | is.na(l1) |is.na(l2) | w1=="" | w2=="" )
  {
    return (FALSE)
  }
  # if the strings have too different lengths, stop the function
  if ((l1 > l2*2) | (l2 > l1*2))
  {
    return (FALSE)
  }
  w1 <- unlist(str_split(w1, ""))
  w2 <- unlist(str_split(w2, ""))
  # the shorter one is the pattern to be searched in the other one
  if (l1 < l2)
  {
    pat <- w1
    str <- w2
  }
  else
  {
    pat <- w2
    str <- w1
  }
  common_prefix = 0
  # iterate the words comparing letter by letter
  for (i in 1:length(pat))
  {
    # till the letters are the same, increase the common prefix counter
    if (str[[i]]==pat[[i]])
    {
      common_prefix = common_prefix + 1
    }
    else
    {
      break
    }
  }
  # if the common prefix is not zero, the words have some letters in common
  # in the prefix
  if (common_prefix > 0)
  {
    # calculate the percentage over the longer word to decide whether to
    # consider the two words as similar or not
    perc <- common_prefix/length(str)
    if (perc >= 0.6)
    {
      return (TRUE)
    }
    else
    {
      return(FALSE)
    }
  }
  else
  {
    return(FALSE)
  }
}


# PREPARATION OF ESCO AND JV DATA -----------------------------------------

pat_agg <- or1(aggettivi$agg)

esco_original_1_multiple_word <- esco_original_1_multiple_word %>%
  select(preferredLabel, doc_id_esco) %>%
  mutate(id = row_number()) %>%
  mutate(original_txt = preferredLabel) %>%
  mutate(preferredLabel = str_to_lower(preferredLabel)) %>%
  rename(word = preferredLabel) %>% 
  mutate(word = str_remove_all(word, pat_agg)) %>% 
  mutate(tag = if_else(str_detect(word, whole_word("TIC")), 1, 0)) %>%
  filter(tag != 1) %>% 
  select(-tag) %>% 
  mutate(word = str_replace_all(word, one_or_more(SPC), " ")) %>% 
  mutate(word = str_remove(word, fixed("++")))

esco_original_1_multiple_word$word <- prep_fun(esco_original_1_multiple_word$word)

df_3_1 <- df_3_1 %>%
  rename(word = merged_sentences) %>%
  mutate( skills_list = "") %>% 
  mutate( doc_id_esco = "")

df_3_1 <- df_3_1 %>% 
  mutate(sentences_clean = str_remove_all(sentences_clean, pat_agg))


# ARRANGING SKILLS DATASET ------------------------------------------------

#arranging skills dataset by higher frequency word in order to increase next skills extraction

list <- c("gestire forniture",	
          "utilizzare abilità cliniche avanzate",
          "fornire consulenza procedure fallimento"	,
          "sicurezza informatica",
          "sistemi giuridici settore costruzioni"	,
          "eseguire acquisto parti",	
          "prodotti finanziari"	,
          "produzione abbigliamento",	
          "sviluppare sistemi risparmio energetico"	,
          "comprendere vallone scritto"	,
          "effettuare manutenzione macchine rotanti",	
          "attività sommelier"	,
          "gestione progetto agile"	,
          "applicare principi diplomatici",	
          "manutenzione munizioni"	,
          "attrezzature illuminazione aeroportuale",	
          "metodi consulenza"	,
          "valutare proprietà nutrizionali alimenti"	,
          "tecniche taglio"	,
          "dati strutturati",	
          "convincere clienti alternative",	
          "garantire rispetto norme ambientali"	,
          "controllare partenze treni"	,
          "analizzare tendenze culturali")

esco_original_1_multiple_word <- esco_original_1_multiple_word %>% 
  mutate(order = if_else(word %in% list, 1, 0)) %>% 
  arrange(-order) %>% 
  select(-order)

# SKILLS EXTRACTION  ------------------------------------------------------

for (frase in 1:nrow(df_3_1))
{
  print(str_c("execution progress ----->  ", round(frase/nrow(df_3_1), digits = 4)*100, "%"))
  
  subset <- esco_original_1_multiple_word  
  
  vet <- unlist(str_split(df_3_1[[frase, "word"]], " "))
  
  shift <- FALSE
  
  competenza <- 1
  # for each sentence of the competenze
  while (competenza <= nrow(subset))
  {
    if (competenza > nrow(subset))
    {
      break
    }
    if (shift == TRUE)
    {
      competenza <- 1
    }
    shift <- FALSE
    comp <- unlist(str_split(subset[[competenza,"word"]], " "))
    cont <- 1
    temp_2 <- 0
    # iterate each word of the competenza
    for (i in 1:length(comp))
    {
      if (comp[[i]] == "")
      {
        next
      }
      temp_1 <- FALSE
      # iterate each word of the sentence of the dataset, to compare them
      # with prefix_match()
      for (j in cont:length(vet))
      {
        w1 <- comp[[i]]
        w2 <- vet[[j]]
        l1 <- str_length(comp[[i]])
        l2 <- str_length(vet[[j]])
        ris <- prefix_match(w1,w2,l1,l2)
        # if the two words are different, move to the next word 
        if( ris == FALSE)
        {
          next 
        }
        # otherwise the 'for' is stopped, to stop the search of this 
        # competenza in the sentence (it has been found right now)
        # cont is set equal to j, because the next iteration
        # will start from where it has stopped now (to respect the order of the competenza)
        else
        {
          temp_1 <- TRUE
          temp_2 <- temp_2 +1
          cont <- j
          break
        }
        
      }
      # if the search of a word of the competenza finished but the 'temp_1' remained 
      # FALSE, the word wasn't found, so the search of the whole competenza 
      # is stopped with a break, since it would be useless to keep searching the
      # other words
      if (temp_1 == FALSE)
      {
        
        found <- 0
        for (k in 1:cont)
        {
          w1 <- comp[[i]]
          w2 <- vet[[k]]
          l1 <- str_length(comp[[i]])
          l2 <- str_length(vet[[k]])
          ris_2 <- prefix_match(w1,w2,l1,l2)
          if( ris_2 == FALSE)
          {
            next 
          }
          else
          {
            found <- 1
            break
          }
        }
        if (found == 0)
        {
          nrow_before <- nrow(subset)
          subset <- subset %>% 
            filter(!str_detect(word,whole_word(comp[[i]])))
          nrow_after <- nrow(subset)
          if(nrow_before != nrow_after)
          {
            shift <- TRUE
          }
        }
        else
        {
          id_remove <- subset[[competenza,"id"]]
          subset <- subset %>% 
            filter(id!=id_remove)
          shift <- TRUE
        }
        
        break
      }
      # if all of the competenze words have been found, the competenza is added
      # to the row of the sentence in the dataset
      if (temp_2 == length(comp))
      {
        df_3_1[[frase,"skills_list"]] <- paste(df_3_1[[frase,"skills_list"]],subset[[competenza,"original_txt"]],sep = " ; ")
        df_3_1[[frase,"doc_id_esco"]] <- paste(df_3_1[[frase,"doc_id_esco"]],subset[[competenza,"doc_id_esco"]],sep = " ; ")
        id_remove <- subset[[competenza,"id"]]
        subset <- subset %>% 
          filter(id!=id_remove)
        shift <- TRUE
        
      }
      
    }
    competenza <- competenza + 1
  }
  
} 

remove(subset, aggettivi)

skills_dataset <- df_3_1 %>% 
  rename(merged_sentences = word)

end_time <- Sys.time()

print(end_time - start_time)

# OUTPUT GENERATION -------------------------------------------------------

write_rds(skills_dataset,"Intermediate/df_sequential_search.rds")


