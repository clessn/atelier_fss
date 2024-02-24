library(dplyr)
library(quanteda)

source("Code/lsd_prep/custom_stopwords.R")

# Load your preprocessed dataset
data <- readRDS("_SharedFolder_article_syrie-ukraine/Data/analysis/dataset_prepped.rds")

# Create a corpus from the preprocessed text data
corpus_text <- corpus(data$text_prepped, docnames = data$id_sentence)

# Tokenize the text
tokens_text <- tokens(corpus_text, what = "word", remove_punct = TRUE, 
                                                  remove_numbers = TRUE, 
                                                  remove_symbols = TRUE)

# Convert tokens to lowercase
tokens_text <- tokens_tolower(tokens_text)

all_stopwords <- c(stopwords("english"), stopWords_en, keywords, after_job_en)

# Remove stopwords from tokens
tokens_text <- tokens_remove(tokens_text, pattern = all_stopwords, padding = TRUE)

# Create a document-feature matrix from tokens (This is the missing step)
dfm_text <- dfm(tokens_text)

# Load your sentiment dictionary (assuming it's already loaded as data_dictionary_LSD2015)
# Apply the sentiment dictionary to the dfm
sentiment <- dfm_lookup(dfm_text, dictionary = data_dictionary_LSD2015)

# Calculate positive sentiment scores
positive_scores <- rowSums(sentiment[, "positive"], na.rm = TRUE)

# Calculate negative sentiment scores
negative_scores <- rowSums(sentiment[, "negative"], na.rm = TRUE)

# Calculate the total number of words in each document after cleaning but before dictionary crossing
total_words <- rowSums(dfm_text)

# Calculate the proportions of positive and negative words per document
proportion_positive <- positive_scores / total_words
proportion_negative <- negative_scores / total_words

# Calculate the tone index y as the difference between the proportions of positive and negative words
tone_index <- proportion_positive - proportion_negative

# Add the tone index to your dataset
data$tone_index <- tone_index

# Calculate net sentiment scores by subtracting negative from positive scores
net_sentiment_scores <- positive_scores - negative_scores

data$net_sentiment_scores <- net_sentiment_scores

# Find the current min and max sentiment scores
min_score <- min(data$net_sentiment_scores, na.rm = TRUE)
max_score <- max(data$net_sentiment_scores, na.rm = TRUE)

# Rescale the sentiment scores to the range [-1, 1]
data$net_sentiment_scores_rescaled <- 2 * ((data$net_sentiment_scores - min_score) / (max_score - min_score)) - 1

table(data$country)

# Save the updated dataset
saveRDS(data, "_SharedFolder_article_syrie-ukraine/Data/data_pub_syrie_ukraine_tone_no_scale.rds")
