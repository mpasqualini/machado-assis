library(tidyverse)
library(tidytext)
library(lexiconPT)
library(gutenbergr)
library(udpipe)

dom_casm <- gutenberg_download(gutenberg_id = "55752")

udp_model <- udpipe_download_model(language = "portuguese")
x <- udpipe_load_model(udp_model)

# Pre processing data ----

# fixing special characters 
dom_casm <- 
  dom_casm %>% 
  mutate(text = map(text, function(x) iconv(x, from = "ISO-8859-1", to = "UTF-8")))

# removing "chapter rows"
# chapter 0 (header) and every row below "INDICE" (footer) can be removed

dom_casm <- 
  dom_casm %>% 
  mutate(chapter = cumsum(str_detect(text, regex("^[MDCLXVI\\.]+$", 
                                                 ignore_case = TRUE)))) %>% filter(chapter != 0)
  
dom_casm <- dom_casm[-(which(dom_casm$text == "INDICE"):nrow(dom_casm)),]

dom_casm <- dom_casm %>% mutate(text = stringi::stri_trans_general(text, "Latin-ASCII"))
dom_casm <- dom_casm %>% mutate(text = gsub("[^A-Za-z0-9 ]","",text))


stop_words <- stopwords::stopwords(language = "pt") %>% as.data.frame()
colnames(stop_words) <- "word"
stop_words <- stop_words %>% mutate(word = stringi::stri_trans_general(word, "Latin-ASCII"))
stop_words <- stop_words %>% add_row(word = c("elle", "ella", "tao"))

# tidying!
tidy_dom <- 
  dom_casm %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words)

annotate <- udpipe_annotate(x, pull(tidy_dom, word), tokenizer = "vertical") %>% as.data.frame()

tidy_dom %>%
  count(word, sort = TRUE) %>% 
  filter(n > 100) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

sent <- 
  tidy_dom %>% inner_join(oplexicon_v3.0, by = c("word" = "term")) %>% 
  group_by(chapter) %>% 
  mutate(sentimento = sum(polarity)) %>% filter(sentimento != 0 ) %>% 
  mutate(sentimento_cat = ifelse(sentimento < 0, "Negativo", "Positivo")) 


sent %>% 
ggplot(aes(x = chapter, y = sentimento, fill = sentimento_cat)) +
geom_col()

sent %>% 
  group_by(word, sentimento_cat) %>% tally()
