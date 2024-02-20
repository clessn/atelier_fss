# Load required libraries
library(tidyverse)
library(tidytext)
library(readr)

# Load the data
data <- read_csv("_SharedFolder_article_syrie-ukraine/Data/_previous/dataset_refugees.rds")

# Ensure there's an ID column for each text entry
data <- data %>%
  mutate(id_sentence = row_number())

# Tokenize the text into words, remove stopwords, punctuation, and numbers
data_tokens <- data %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(word = str_remove_all(word, "[[:punct:]]")) %>%
  filter(!str_detect(word, "\\d"))

# Load the NRC sentiment dictionary
nrc <- get_sentiments("nrc")

# Perform sentiment analysis by joining tokens with the NRC dictionary
data_nrc <- data_tokens %>%
  inner_join(nrc, by = "word", relationship = "many-to-many") %>%
  count(id_sentence, sentiment) %>%
  spread(sentiment, n, fill = 0)

# Aggregate sentiment scores by sentence
data_sentiment <- data_nrc %>%
  group_by(id_sentence) %>%
  summarise(across(everything(), sum))

# Plotting the sentiment scores
ggplot(data_sentiment, aes(x = id_sentence)) +
  geom_bar(aes(y = positive), stat = "identity", fill = "green") +
  geom_bar(aes(y = -negative), stat = "identity", fill = "red") +
  theme_minimal() +
  labs(y = "Sentiment Score", x = "Document ID", title = "Sentiment Analysis Results") +
  coord_flip()


# Save the plot
ggsave("sentiment_scores_plot.png", width = 10, height = 6, dpi = 300)
