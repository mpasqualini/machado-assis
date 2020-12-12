library(tidyverse)
library(data.table)
library(tidytext)
library(lexiconPT)
library(gutenbergr)
library(udpipe)
library(ggthemes)
library(viridis)
library(topicmodels)

# Getting books and model for text annotation ----
livros_machado <- gutenberg_download(c(55752, 55682, 54829))

udp_model <- udpipe_download_model(language = "portuguese")
udp_model <- udpipe_load_model(udp_model)

# Data prep ----

# separating chapters and removing special char 
livros_cap <- 
  livros_machado %>% 
  group_by(gutenberg_id) %>% 
  mutate(text = replace(text, text == "CAPITULO PRIMEIRO", "CAPITULO I")) %>% 
  filter(gutenberg_id != 55752) %>% 
  mutate(chapter = cumsum(str_detect(text, regex("CAPITULO [MDCLXVI\\.]+$", 
                                                 ignore_case = TRUE)))) %>% ungroup()

livros <- livros_machado %>% 
  filter(gutenberg_id == 55752) %>% 
  mutate(chapter = cumsum(str_detect(text, regex("^[MDCLXVI\\.]+$", 
                                                 ignore_case = TRUE)))) %>% rbind(livros_cap) %>% 
  mutate(text = map(text, function(x) iconv(x, from = "ISO-8859-1", to = "UTF-8"))) %>% 
  mutate(text = stringi::stri_trans_general(text, "Latin-ASCII")) %>% 
  mutate(text = gsub("[^-0-9A-Za-z ]","",text)) %>% 
  mutate(text = str_squish(text))

# removing header and footer
livros <- as.data.table(livros) %>% filter(chapter != 0)
livros <- livros[, remove := .I %in% which(text == "FIM"):.N, by = gutenberg_id]
livros <- livros[remove == FALSE]
livros <- livros[-(16703:16876)] %>% select(-remove)

# stopwords
stop_words <- stopwords::stopwords(language = "pt") %>% as.data.frame()
colnames(stop_words) <- "word"
stop_words <- 
  stop_words %>% mutate(word = stringi::stri_trans_general(word, "Latin-ASCII")) %>%
  add_row(word = c("elle", "ella", "tao", "capitulo", "d"))

# tidying
tidy_machado <- 
  livros %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# lemmatization 
books_udpipe <- udpipe_annotate(udp_model, pull(tidy_machado, word), tokenizer = "vertical") %>% as.data.frame()

processed_machado <- 
  tidy_machado %>% bind_cols(books_udpipe) %>% select(gutenberg_id, chapter, word, lemma, upos) %>% 
  mutate(book = factor(case_when(gutenberg_id == 55682 ~ "Quincas Borba (1891)",
                                    gutenberg_id == 54829 ~ "Memórias Póstumas de Brás Cubas (1881)",
                                    gutenberg_id == 55752 ~ "Dom Casmurro (1899)"), 
                       levels = c("Memórias Póstumas de Brás Cubas (1881)", "Quincas Borba (1891)", "Dom Casmurro (1899)")))

processed_machado %>%
  group_by(book) %>% 
  count(word, sort = TRUE) %>% 
  top_n(n = 25) %>%
  ggplot(aes(x = n, y = reorder(word, n), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free") +
  scale_fill_viridis_d(option = "cividis", begin = 0.34, end = 0.94) +
  theme_fivethirtyeight()

# Sentiment analysis ----

sent <- 
  processed_machado %>% inner_join(oplexicon_v3.0, by = c("word" = "term")) %>% 
  group_by(book, chapter) %>% 
  mutate(sentimento = sum(polarity)) %>% 
  ungroup() %>% 
  filter(sentimento != 0) %>% 
  mutate(sentimento_cat = ifelse(sentimento < 0, "Negativo", "Positivo"))

# Beautiful plots :-)

sent_plot_dc <- 
  sent %>% 
  filter(gutenberg_id == 55752) %>% 
  ggplot(aes(x = chapter, y = sentimento, fill = sentimento_cat)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_viridis_d(option = "cividis", begin = 0.34, end = 0.94) +
  labs(fill = "Polaridade do sentimento") +
  theme_fivethirtyeight()

sent_plot_dc + 
  annotate(geom = "curve", x = 78, xend = 84, y = -16, yend = -10, curvature = -0.25) +
  annotate(geom = "text", x = 78, y = -17, label = "Capítulo 85: O Defunto") +
  annotate(geom = "curve", x = 101, xend = 105, y = 30, yend = 33, curvature = -0.3) +
  annotate(geom = "text", x = 116, y = 33, label = "Capítulo 100: Tu serás feliz, Bentinho")

sent_books <- 
  sent %>% 
  ggplot(aes(x = chapter, y = sentimento, fill = sentimento_cat)) +
  geom_bar(stat = "identity", position = "identity") +
  facet_grid(~book, scales = "free") +
  scale_fill_viridis_d(option = "cividis", begin = 0.34, end = 0.94) +
  labs(fill = "Polaridade do sentimento") +
  theme_fivethirtyeight()

sent_cumulative <- 
  sent %>% 
  group_by(book, sentimento_cat) %>% 
  mutate(cumsum = cumsum(sentimento)) %>% 
  ggplot(aes(x = chapter, y = cumsum, color = sentimento_cat)) +
  geom_line(size = 1) +
  facet_grid(~book, scales = "free") +
  scale_color_viridis_d(option = "cividis", begin = 0.34, end = 0.94) +
  labs(color = "Polaridade do sentimento") +
  theme_fivethirtyeight()

# Topic model ----

tidy_count <- tidy_machado %>% 
  group_by(gutenberg_id) %>% 
  count(word, sort = TRUE) %>% ungroup() %>% rename("document" = "gutenberg_id")

dtm <- tidy_count %>% cast_dtm(term = word, document = document,  value = n)

fit_lda <- LDA(dtm, k = 3, control = list(seed = 758))
fit_lda

machado_topics <- tidy(fit_lda, matrix = "beta")

machado_top_terms <- machado_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

machado_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  scale_fill_viridis_d(option = "cividis", begin = 0.34, end = 0.94) +
  theme_fivethirtyeight()

beta_spread <- machado_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))  

beta_spread %>% 
  top_n(20) %>% 
  ggplot(aes(x = log_ratio, y = term)) +
  geom_col()
