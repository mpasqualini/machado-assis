library(tidyverse)
library(tidytext)
library(lexiconPT)
library(gutenbergr)

dom.casm <- gutenberg_download(gutenberg_id = "55752")

# data prep ----

# fixing special characters 
dom.casm <- 
  dom.casm %>% 
  mutate(text = map(text, function(x) iconv(x, from = "ISO-8859-1", to = "UTF-8")))

# removing "chapter rows"

dom.casm <- 
  dom.casm %>% 
  mutate(chapter = cumsum(str_detect(text, regex("^[MDCLXVI\\.]+$", 
                                                 ignore_case = TRUE)))) 
  
# chapter 0 and every row below "FIM" can be removed


# tidying!
tidy.dc <- dom.casm %>% unnest_tokens(word, text)
