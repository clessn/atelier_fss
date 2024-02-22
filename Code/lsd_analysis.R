library(dplyr)

data <- readRDS("_SharedFolder_article_syrie-ukraine/Data/analysis/dataset.rds")

corpus_text <- quanteda::corpus(data_lsd$text)

tokens_text <-  quanteda::tokens(corpus_text, what = "word", remove_punct = TRUE, 
                                                  remove_numbers = TRUE, 
                                                  remove_symbols = TRUE)

tokens_text <- quanteda::tokens_tolower(tokens_text)

tokens_text <- quanteda::tokens_remove(tokens_text, pattern = quanteda::stopwords("english"), padding = TRUE)

tokens_text <- quanteda::tokens(corpus_text)