########################################################################### Essai STM ###########################################################################

#### 0.1 -  library ####

Packages <- c("tidyverse", "quanteda", "stm", "tidytext", "lubridate", "striprtf")
lapply(Packages, library, character.only = T)
options(tibble.print_max = Inf,scipen=999)

#### 0.2 - Data ####

# data from the lipad project: https://www.lipad.ca/data/

data_path <- "/Users/adrien/Dropbox/Travail/Universite_Laval/CLESSN/_SharedFolder_article_syrie-ukraine/2022-UkraineSyriaProject/Data/BaseArticles/CA-Globe&Mail-Syrie"
files <- dir(data_path, pattern = "*.rtf")

Data_merge <- files %>%
  map(~ read_rtf(file.path(data_path, .))) %>%
  reduce(rbind)

saveRDS(Data_merge, file = "my_data.rds")


Data_merge2 <- Data_merge %>%
  as.data.frame(Data_merge)
#   tidyr::separate_rows(V7233,
#            sep = 'Document')

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html  # to eliminate undesired regular expressions

#### ___ ####

#### 1 - Cleaning ####

#### Get stopwords + other parliamentary words ####

stop_words_new <- c(stopwords("en"), "speaker", "yea", "nay", "can", "also", "take",
                    "madame", "mister", "bill", "mr", "us", "must", "many", "see",
                    "order", "house", "adjourn", "please", "like", "want")
#### ___ ####

#### 2 - Clean the text variable + analysis (On analyse comme un topic model normal) ####

# On analyse ca par parti (parti covariate) - on prend l'emergency dabate text!

Data_clean <- Data_merge %>%
  filter(maintopic == "Emergency Debate") |> # Essai filtre les textes parlementaires
  mutate(text_id = row_number())  |>
  unnest_tokens(word, speechtext) |>
  filter(!word %in% c(stop_words_new)) |>
  count(speakerparty, word, sort = TRUE) |>
  distinct() |>
  cast_dfm(speakerparty, word, n)

topic_model <- stm(Data_clean, K = 3,
                   verbose = FALSE, init.type = "LDA")

# Tiré du site de julia silge: https://juliasilge.com/blog

td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")


#### ___ ####

#### 3 - Use covariates to estimate the effect of parties on subject ####

Data_stm_covariates <- Data_merge %>%
  filter(maintopic == "Emergency Debate") |> # Essai filtre les textes parlementaires
  select(speechtext, speakerparty) |>
  mutate(text_id = row_number())  |>
  filter(!speechtext %in% c(stop_words_new)) |>
  na.omit()

temp <- textProcessor(Data_stm_covariates$speechtext,
                      metadata = Data_stm_covariates,
                      lowercase=T,
                      removestopwords=T,
                      removenumbers=T,
                      removepunctuation=T,
                      wordLengths=c(3,Inf),
                      stem=T,
                      onlycharacter= T,
                      striphtml=T)

docs <- temp$documents
meta <- temp$meta
vocab <- temp$vocab

tds_stm <- stm(documents=docs,
               data=meta,
               vocab=vocab,
               prevalence =~speakerparty,
               K=3,
               init.type = "LDA")

summary(tds_stm)

td_beta_new <- tidy(tds_stm)

td_beta_new %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")





