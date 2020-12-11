library(tidyverse)
library(tidytext)
library(lexiconPT)
library(gutenbergr)
library(udpipe)
library(ggthemes)
library(viridis)
library(topicmodels)

dom_casm <- gutenberg_download(gutenberg_id = "55752")

udp_model <- udpipe_download_model(language = "portuguese")
udp_model <- udpipe_load_model(udp_model)

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
dom_casm <- dom_casm %>% mutate(text = gsub("[^-0-9A-Za-z ]","",text))


stop_words <- stopwords::stopwords(language = "pt") %>% as.data.frame()
colnames(stop_words) <- "word"
stop_words <- stop_words %>% mutate(word = stringi::stri_trans_general(word, "Latin-ASCII"))
stop_words <- stop_words %>% add_row(word = c("elle", "ella", "tao"))

# tidying!
tidy_dom <- 
  dom_casm %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# lemmatization 
annotate <- udpipe_annotate(udp_model, pull(tidy_dom, word), tokenizer = "vertical") %>% as.data.frame()

processed_dc <- tidy_dom %>% bind_cols(annotate) %>% select(chapter, word, lemma, upos)

processed_dc %>%
  count(word, sort = TRUE) %>% 
  filter(n > 100) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

sent <- 
  processed_dc %>% inner_join(oplexicon_v3.0, by = c("word" = "term")) %>% 
  group_by(chapter) %>% 
  mutate(sentimento = sum(polarity)) %>% 
  ungroup() %>% 
  filter(sentimento != 0) %>% 
  mutate(sentimento_cat = ifelse(sentimento < 0, "Negativo", "Positivo"))

# sentiment analysis ----
sent_plot <- sent %>% 
  ggplot(aes(x = chapter, y = sentimento, fill = sentimento_cat)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_viridis_d(option = "cividis", begin = 0.34, end = 0.94) +
  labs(fill = "Polaridade do sentimento") +
  theme_fivethirtyeight()

sent_plot + 
  annotate(geom = "curve", x = 78, xend = 84, y = -16, yend = -10, curvature = -0.25) +
  annotate(geom = "text", x = 78, y = -17, label = "Capítulo 85: O Defunto") +
  annotate(geom = "curve", x = 101, xend = 105, y = 30, yend = 33, curvature = -0.3) +
  annotate(geom = "text", x = 116, y = 33, label = "Capítulo 100: Tu serás feliz, Bentinho") 

sent %>% 
  group_by(sentimento_cat) %>% 
  mutate(cumsum = cumsum(sentimento)) %>% 
  ggplot(aes(x = chapter, y = cumsum, color = sentimento_cat)) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "cividis", begin = 0.34, end = 0.94) +
  labs(color = "Polaridade do sentimento") +
  theme_fivethirtyeight()

# topic model ----

