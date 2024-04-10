# Charger les packages nécessaires pour les manipulations de données et l'analyse textuelle
library(dplyr)
library(quanteda)

# Charger des mots vides personnalisés qui seront utilisés pour filtrer le texte
source("data/lsd_prep_sources/custom_stopwords.R")

# Charger le jeu de données prétraité
data <- readRDS("data/data_prepped.rds")

# Créer un corpus à partir des données textuelles prétraitées
corpus_text <- corpus(data$text_prepped, docnames = data$id_sentence)

# Tokeniser le texte, c'est-à-dire le diviser en mots tout en supprimant la ponctuation, les nombres et les symboles
tokens_text <- tokens(corpus_text, what = "word", remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)

# Convertir les tokens en minuscules pour normaliser le texte
tokens_text <- tokens_tolower(tokens_text)

# Fusionner les mots vides par défaut en anglais avec des listes personnalisées
all_stopwords <- c(stopwords("english"), stopWords_en, keywords, after_job_en)

# Supprimer les mots vides des tokens pour se concentrer sur les mots significatifs
tokens_text <- tokens_remove(tokens_text, pattern = all_stopwords, padding = TRUE)

# Créer une matrice document-terme à partir des tokens
dfm_text <- dfm(tokens_text)

# Charger le dictionnaire de sentiments LSD (supposé préalablement chargé sous le nom de data_dictionary_LSD2015)
# Appliquer le dictionnaire de sentiments à la matrice document-terme
sentiment <- dfm_lookup(dfm_text, dictionary = data_dictionary_LSD2015)

# Calculer les scores de sentiments positifs
positive_scores <- rowSums(sentiment[, "positive"], na.rm = TRUE)

# Calculer les scores de sentiments négatifs
negative_scores <- rowSums(sentiment[, "negative"], na.rm = TRUE)

# Calculer le nombre total de mots dans chaque document après le nettoyage mais avant l'application du dictionnaire
total_words <- rowSums(dfm_text)

# Calculer les proportions de mots positifs et négatifs par document
proportion_positive <- positive_scores / total_words
proportion_negative <- negative_scores / total_words

# Calculer l'indice de tonalité comme la différence entre les proportions de mots positifs et négatifs
tone_index <- proportion_positive - proportion_negative

# Ajouter l'indice de tonalité à votre jeu de données
data$tone_index <- tone_index

# Calculer les scores de sentiment net en soustrayant les scores négatifs des scores positifs
net_sentiment_scores <- positive_scores - negative_scores

data$net_sentiment_scores <- net_sentiment_scores

# Trouver les scores de sentiment minimum et maximum actuels
min_score <- min(data$net_sentiment_scores, na.rm = TRUE)
max_score <- max(data$net_sentiment_scores, na.rm = TRUE)

# Rééchelonner les scores de sentiment dans la plage [-1, 1]
data$net_sentiment_scores_rescaled <- 2 * ((data$net_sentiment_scores - min_score) / (max_score - min_score)) - 1

# Afficher un tableau récapitulatif des pays présents dans le jeu de données (optionnel)
table(data$country)

# Sauvegarder le jeu de données mis à jour
saveRDS(data, "data/data_analyse_textuelle.rds")
