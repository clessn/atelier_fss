########################################################################################################### ##
################################################### Packages #################################################
########################################################################################################### ##

# Cleaning
library(readtext)
library(tidyverse)
library(tidytext)
library(textdata)
library(tm)
library(pdftools)
library(rebus)
library(stringr)
library(reshape2) # pour function acast (transformer df en matrix)
library(lubridate)
# Sentiment
library(quanteda)
# Topic modeling
library(topicmodels)
# Ploting
library(ggthemes)
library(sp)
library(RColorBrewer)
library(wordcloud)
library(viridisLite)
library(SnowballC)
library(plotrix)
library(dendextend)
library(radarchart) # pour faire des radar chart
library(htmlwidgets) # pour enregistrer des radar chart
library(webshot) # pour enregistrer des radar chart
library(treemap)
library(gridExtra)
library(stargazer)

########################################################################################################### ##
####################################################### DATA #################################################
########################################################################################################### ##


data <- readRDS("_SharedFolder_article_syrie-ukraine/Data/dataset.rds") %>%
  unnest_sentences(text, text) %>%
  filter(grepl('refugee|refugees|migrant|migrants', text))
#  filter(opinion != 0) %>%


## Enregistrer le csv
#saveRDS(data, paste0("_SharedFolder_article_syrie-ukraine/Data/", "dataset_refugees.rds"))

dataSyrie2015 <- data %>%
  filter(country == "Syrie") %>%
  filter(between(date, as.Date('2015-01-01'), as.Date('2015-12-31'))) %>%
  filter(source %in% c("The Globe and Mail", "Toronto Star"))
 # filter(opinion == 0)  %>%
#  filter(opinion != 0)

## Enregistrer le csv
#write_csv(dataSyrie2015, paste0("_SharedFolder_article_syrie-ukraine/Data/", "dataset_refugees-Syria.csv"))

# dataSyrie2015_op <- data %>%
#   filter(country == "Syrie") %>%
#   filter(between(date, as.Date('2015-01-01'), as.Date('2015-12-31'))) %>%
#   filter(opinion != 0)

dataUkraine <- data %>%
  filter(country == "Ukraine") %>%
  filter(between(date, as.Date('2022-01-01'), as.Date('2022-12-31')))  %>%
  filter(source %in% c("The Globe and Mail", "Toronto Star"))
  #filter(opinion == 0)

#write_csv(dataUkraine, paste0("_SharedFolder_article_syrie-ukraine/Data/", "dataset_refugees-Ukraine.csv"))


# dataUkraine_op <- data %>%
#   filter(country == "Ukraine") %>%
#   filter(between(date, as.Date('2022-01-01'), as.Date('2022-12-31'))) %>%
#   filter(opinion != 0)

nb_refugies <- read.csv("_SharedFolder_article_syrie-ukraine/Data/nb_refugies.csv", sep = ";") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))


########################################################################################################### ##
################################################# Corpus anglo ###############################################
########################################################################################################### ##

# Créer un DataframeSource total
#data_source <- DataframeSource(data)
# Convertir en corpus volatile total
#data_corpus <- VCorpus(data_source)

# Syrie
#dataSyrie_source <- DataframeSource(dataSyrie)
#dataSyrie_corpus <- VCorpus(dataSyrie_source)

# Syrie
dataSyrie2015_source <- DataframeSource(dataSyrie2015)
dataSyrie2015_corpus <- VCorpus(dataSyrie2015_source)

# Ukraine
dataUkraine_source <- DataframeSource(dataUkraine)
dataUkraine_corpus <- VCorpus(dataUkraine_source)

########################################################################################################### ##
################################################## Cleaning EN ###############################################
########################################################################################################### ##

# Utiliser cette ligne pour faire des test
#Weed_corpus <- tm_map(Weed_corpus, removeWords, words = c(stopwords("en")))

# les mots à enlever
stopWords_en <-
  c(# Nom
    "nom", "people", "new", "old", "back", "way", "thing", "things", "left", "right", "mr", "ms",
    # Origines et politique
    # "ontario", "ottawa", "toronto", "halifax", "quebec", "montreal", "york", "united", "states",
    # "vancouver", "canadian", "american",
    # Marqueur de relations, déterminants
    "also", "per", "just", "like", "even", "still", "much", "since", "around", "well", "really", "might",
    "across", "whether", "least", "already",
    # Verbes
    "said", "says", "say", "will", "can", "get", "got", "found", "may", "told", "make", "made", "going",
    "take", "took", "think", "including", "want", "see", "called", "know", "known", "according",
    "ask", "asked", "put", "away", "among", "set", "show", "find", "went", "call", "come", "came",
    "need", "go",
    # Nombre et quantités
    "number", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "cent", "lot",
    "first", "second", "last", "end", "many", "former", "later", "next", "never", "always", "with", "without",
    "every", "several", "big", "short", "long", "little", "small", "less", "something", "somethings",
    # Temps et lieux
    "time", "times", "now", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche",
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "gmt", "bst",
    "décembre", "janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre",
    "octobre", "novembre", "december", "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november", "feb",
    "today", "yesterday", "another", "day", "days", "week", "weeks", "month", "months", "year", "years",
    "ago", "near", "far", "place", "early", "yet",
    # Relatif au journalisme et aux médias
    "media", "presse", "plus", "journal", "cbc", "devoir", "radio-canada", "agence", "qmi",
    "mediaqmi", "star", "cbc", "news", "press", "reuters", "reuter", "cp", "ap", "nouvelles",
    "published", "rights", "guardian", "copyright", "reserved", "timeupdated", "updated",
    "globe", "mail", "block", "related", "grdn", "anglais", "sun", "thesun", "newspapers",
    "limited", "washington", "post", "httpwwwwashingtonpostcom", "co", "tor", "ont",

    # Autres
    "x", "h", "s", "t", "th", "à") # ajouter d'autres mots

keywords <- # Pour enlever les mots clés liés au conflit
  c("refugees", "refugee", "ukraine", "syria", "syrian", "syrians", "ukrainians", "ukrainian",
    "war", "crisis", "migrants", "forces", "invasion", "military", "attack", "attacks", "conflict",
    "kill", "killed", "threat", "fight", "fights", "violence", "fighting", "death", "dead",
    "deads", "crime", "crimes", "aggression", "aggressions", "emergency")

after_job_en <- # Pour enlever les résidus d'après nettoyage
  c("s", "t", "th", "à", "a", "u", "x", "h", "tri", "st", "am", "pm", "m", "re", "c")

# Créer une fonction pour rapidement nettoyer notre corpus
clean_corpusEN <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, words = stopWords_en)
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = T)
  corpus <- tm_map(corpus, removeNumbers)
 # corpus <- tm_map(corpus, stemDocument) # stemming SEULEMENT pour le topic modeling
  corpus <- tm_map(corpus, removeWords, words = keywords)
  corpus <- tm_map(corpus, removeWords, words = after_job_en)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Appliquer la fonction
# Peut prendre plusieurs minutes, selon la taille du corpus
#clean_corp <- clean_corpusEN(data_corpus)
#cleanSyrie_corp <- clean_corpusEN(dataSyrie_corpus)
cleanSyrie2015_corp <- clean_corpusEN(dataSyrie2015_corpus)
cleanUkraine_corp <- clean_corpusEN(dataUkraine_corpus)

# Vérifier un texte nettoyé
# content(clean_corpEN[[12]])

# Voir le vieux text pas nettoyé
# DataWeedMedia$text[12]

#meta(clean_corpEN[1])

########################################################################################################### ##
###################################################### DTM / TDM #############################################
########################################################################################################### ##

# Créer une document-term matrix à partir du corpus nettoyé

# TF-IDF est utilisé quand on a un large corpus avec peu de diversité de termes (ce qui n'est pas notre cas).
# Weed_dtm <- DocumentTermMatrix(clean_corp, control = list(weighting = weightTfIdf))

#data_dtm <- DocumentTermMatrix(clean_corp)

#dataSyrie_dtm <- DocumentTermMatrix(cleanSyrie_corp)

dataSyrie2015_dtm <- DocumentTermMatrix(cleanSyrie2015_corp)

dataUkraine_dtm <- DocumentTermMatrix(cleanUkraine_corp)


# On peut aussi créer une term-document matrix (notamment pour dendrogram)
#Weed_tdmFR <- TermDocumentMatrix(clean_corpFR)
#Weed_tdmEN <- TermDocumentMatrix(clean_corpEN)

# Voir
# Weed_dtm

########################################################################################################### ##
####################################################### Matrix ###############################################
########################################################################################################### ##

# Convertir en matrice
#data_matrix <- as.matrix(data_dtm)

#Weed_mEN <- as.matrix(Weed_dtmEN)

# Imprimer les dimensions de la matrix
# dim(Weed_m)

# Imprimer une portion de la matrice
#Weed_mEN[,1:12]

########################################################################################################### ##
###################################################### Top_n ############################################
########################################################################################################### ##

# Créer la bd
data_n <- tidy(cleanSyrie2015_corp) %>%
  bind_cols(dataSyrie2015) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -text...10, -id) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Ajouter un filtre par pays ou média pour gérer le nettoyage en détails
# data_n <- data_n %>%
#  # filter(country == "Ukraine")
#   filter(source == "Toronto Star")

# Calculer le top 50
top50_words <- data_n %>%
  group_by(word) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(50)

########################################################################################################### ##
############################################### Nombre d'articles ############################################
########################################################################################################### ##

#color <- bpy.colors(n = 8, cutoff.tails = 0.1, alpha = 1.0)

count1 <- dataUkraine %>%
  group_by(date, country) %>%
#  filter(source != "The Gardian") %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(date = as.POSIXct(date))

count2 <- dataSyrie2015 %>%
  group_by(date, country) %>%
  #  filter(source != "The Gardian") %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(date = as.POSIXct(date))

plot1 <- ggplot(count1, aes(x = date, y = n)) +
  geom_line(aes(color = country), size = 1, color = "#5B9BD5") +
  scale_x_datetime("", date_labels = "%b", date_breaks = "1 month") +
  geom_vline(xintercept = as.POSIXct("2022-03-15"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
 # geom_text(x = as.POSIXct("2022-03-15"), y = max(count$n) * 1.3, label = "Migration peak", angle = 90, vjust = -0.6) +
  scale_y_continuous(name="", breaks = c(0, 10, 20, 30, 40, 50, 60), limits = c(0, 65)) +
  theme_clean() +
  ggtitle("Ukraine (2022)") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = c(.85, .90),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        panel.border = element_blank(),
        legend.margin = margin(6, 6, 6, 6))

plot2 <- ggplot(count2, aes(x = date, y = n)) +
  geom_line(aes(color = country), size = 1, color = "#CC9933") +
  scale_x_datetime("", date_labels = "%b", date_breaks = "1 month") +
  # geom_vline(xintercept = as.POSIXct("2015-10-01"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-10-01"), y = max(count$n) * 1.3, label = "Migration peak", angle = 90, vjust = -0.60) +
  # geom_vline(xintercept = as.POSIXct("2015-09-02"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-09-02"), y = max(count$n) * 1.26, label = "Death of Alan Kurdi", angle = 90, vjust = -0.60) +
  # geom_vline(xintercept = as.POSIXct("2015-11-13"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-11-13"), y = max(count$n) * 1.32, label = "Paris attacks", angle = 90, vjust = -0.60) +
  # scale_y_continuous(name="Number of articles per month\n", breaks = c(0, 10, 20, 30, 40, 50, 60), limits = c(0, 65)) +
  theme_clean() +
  ggtitle("Syria (2015)") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = c(.85, .90),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        panel.border = element_blank(),
        legend.margin = margin(6, 6, 6, 6))

plot <- grid.arrange(plot2, plot1, nrow = 1)


ggsave("../_SharedFolder_article_syrie-ukraine/nbArticles/nbArticles-syrie-ukraine.png", plot, width = 30, height = 17, units = c("cm"))


########################################################################################################### ##
################################### Sentiment EN Syrie VS Ukraine ############################
########################################################################################################### ##

# Créer une fonction pour analyser le ton des mots avec le Lexicoder Sentiment Dictionary
runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy <- tidy(cleanUkraine_corp) %>%
  bind_cols(dataUkraine) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity <- data_tidy %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(date) %>%
  mutate(total_words = n())

# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton <- runDictionaryFunction(dataA = polarity,
                                    word = word,
                                    dictionaryA = data_dictionary_LSD2015)

# Pour le top négatif et positif
topcount <- bind_cols(polarity, data_ton) %>%
  group_by(word) %>%
  summarise(negative = sum(negative),
            positive = sum(positive))

# Ukraine
graphdata_ukraine <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Ukraine")

# Syria
graphdata_syria <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Syria")

# On met tous ensemble et on calcule
graphdata <- graphdata_ukraine %>%
  bind_rows(graphdata_syria) %>%
  group_by(country) %>%
  summarise(
    n=n(),
    mean=mean(diffProp),
    sd=sd(diffProp)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard deviation
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard deviation")

# Standard Error
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard error")

# Confidence Interval
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")

# Permutation

# Chargement des bibliothèques nécessaires
library(boot)

# Calcul de la différence de ton observée entre les deux groupes
diff_observed <-  graphdata_ukraine %>%
  bind_rows(graphdata_syria) %>%
  ungroup() %>%
  summarise(mean=mean(diffProp))

# Fonction pour générer une statistique de test (différence de moyennes)
statistic <- function(data, indices) {
  mean(graphdata_ukraine$diffProp[indices]) - mean(graphdata_syria$diffProp[indices])
}

# Utilisation de la fonction boot pour effectuer le test de permutation
boot_results <- boot(data = data.frame(ton_syriens, ton_ukrainiens), statistic, R = 1000)

# Calcul de la p-valeur
p_value <- sum(abs(boot_results$t) >= abs(diff_observed)) / length(boot_results$t)

# Création de vos données simulées (remplacez par vos propres données)
set.seed(123) # pour la reproductibilité
ton_syriens <- rnorm(100, mean = 0.2, sd = 0.1) # Ton pour les réfugiés syriens
ton_ukrainiens <- rnorm(100, mean = 0.25, sd = 0.1) # Ton pour les réfugiés ukrainiens

# Test t de Student
t_test <- t.test(graphdata_ukraine$diffProp, graphdata_syria$diffProp, alternative = "two.sided", var.equal = TRUE)

# Affichage des résultats
cat("Statistique de test t:", t_test$statistic, "\n")
cat("P-valeur:", t_test$p.value, "\n")


# # On plot
# ggplot(graphdata, aes(x = date, y = diffProp)) +
#   geom_hline(yintercept = 0, color = "red") +
#  geom_line() +
#  # geom_point(alpha = 0.4) +
#   geom_smooth(se = F, span = 0.4, size = 2.5) +
#   scale_x_datetime("", date_labels = "%m-%Y", date_breaks = "1 month") +
#   # scale_color_viridis_d(labels = c("CBC", "Toronto Star"),
#   #                       limits = c("CBC", "TStar")) +
#   theme_clean() +
#   # labs(title = "Évolution du ton des articles sur le cannabis dans les médias anglophones (1985 à 2019)",
#   #      # subtitle = "Version 1: aucune pondération.",
#   #      subtitle = "",
#   #      # caption = "Dictionnaire: version francophone du Lexicoder (Duval et Pétry, 2016)
#   #      # Pondéré en fonction du nombre moyen de mots par mois, après nettoyage des textes et application du dictionnaire") +
#   #      caption = "n = 14 771 articles
#   #      Dictionnaire: Lexicoder (Soroka and Young, 2015)
#   #      L'indice de ton est calculé en fonction de la différence entre les proportions de mots positifs et négatifs par mois") +
#   ylab("Proportion de mots négatifs\n") +
#   theme(plot.title = element_text(hjust = -1.5, size = 18),
#         plot.subtitle=element_text(size = 13),
#         plot.caption = element_text(hjust = 1, size = 11),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 14),
#         legend.position = "top") +
#   guides(color=guide_legend(title="Country"))
#
# ggsave("../_SharedFolder_article_syrie-ukraine/_v2/lexicoder/lexicoder-ukraine.png", width = 30, height = 17, units = c("cm"))

########################################################################################################### ##
########################################## Sentiment EN Opinion Vs article ###################################
########################################################################################################### ##

# Créer une fonction pour analyser le ton des mots avec le Lexicoder Sentiment Dictionary
runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy <- tidy(cleanSyrie2015_corp) %>%
  bind_cols(dataSyrie2015) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity <- data_tidy %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(date) %>%
  mutate(total_words = n())

# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton <- runDictionaryFunction(dataA = polarity,
                                  word = word,
                                  dictionaryA = data_dictionary_LSD2015)

# Pour le top négatif et positif
topcount <- bind_cols(polarity, data_ton) %>%
  group_by(word) %>%
  summarise(negative = sum(negative),
            positive = sum(positive))

# Opinion
graphdata_opinion <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  filter(opinion == 1) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(type = "Opinion")

# News
graphdata_news <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  filter(opinion == 0) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(type = "News")

# On met tous ensemble et on calcule
graphdata <- graphdata_news %>%
  bind_rows(graphdata_opinion) %>%
  group_by(type) %>%
  summarise(
    n=n(),
    mean=mean(diffProp),
    sd=sd(diffProp)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard deviation
ggplot(graphdata) +
  geom_bar( aes(x=type, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=type, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard deviation")

# Standard Error
ggplot(graphdata) +
  geom_bar( aes(x=type, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=type, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard error")

# Confidence Interval
ggplot(graphdata) +
  geom_bar( aes(x=type, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=type, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")

# # On plot
# ggplot(graphdata, aes(x = date, y = diffProp)) +
#   geom_hline(yintercept = 0, color = "red") +
#  geom_line() +
#  # geom_point(alpha = 0.4) +
#   geom_smooth(se = F, span = 0.4, size = 2.5) +
#   scale_x_datetime("", date_labels = "%m-%Y", date_breaks = "1 month") +
#   # scale_color_viridis_d(labels = c("CBC", "Toronto Star"),
#   #                       limits = c("CBC", "TStar")) +
#   theme_clean() +
#   # labs(title = "Évolution du ton des articles sur le cannabis dans les médias anglophones (1985 à 2019)",
#   #      # subtitle = "Version 1: aucune pondération.",
#   #      subtitle = "",
#   #      # caption = "Dictionnaire: version francophone du Lexicoder (Duval et Pétry, 2016)
#   #      # Pondéré en fonction du nombre moyen de mots par mois, après nettoyage des textes et application du dictionnaire") +
#   #      caption = "n = 14 771 articles
#   #      Dictionnaire: Lexicoder (Soroka and Young, 2015)
#   #      L'indice de ton est calculé en fonction de la différence entre les proportions de mots positifs et négatifs par mois") +
#   ylab("Proportion de mots négatifs\n") +
#   theme(plot.title = element_text(hjust = -1.5, size = 18),
#         plot.subtitle=element_text(size = 13),
#         plot.caption = element_text(hjust = 1, size = 11),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 14),
#         legend.position = "top") +
#   guides(color=guide_legend(title="Country"))
#
# ggsave("../_SharedFolder_article_syrie-ukraine/_v2/lexicoder/lexicoder-ukraine.png", width = 30, height = 17, units = c("cm"))

########################################################################################################### ##
########################################## Sentiment EN Opinion Syrie Vs Opinion Ukraine #####################
########################################################################################################### ##

# Créer une fonction pour analyser le ton des mots avec le Lexicoder Sentiment Dictionary
runDictionaryFunction <- function(corpusA, dataA, word, dfmA, dataB, dictionaryA) {
  corpusA <- corpus(dataA$word)
  dfmA    <- dfm(corpusA, dictionary = dictionaryA)
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
data_tidy <- tidy(cleanUkraine_corp) %>%
  bind_cols(dataUkraine) %>%
  select(-author, -datetimestamp, -description, -heading, -language, -origin, -doc_id, -id, -text...10) %>%
  rename(text = text...8) %>%
  unnest_tokens(word, text)

# Pour obtenir des %
polarity <- data_tidy %>%
  mutate(date = as.POSIXct(date))  %>%
  group_by(date) %>%
  mutate(total_words = n())

# On utilise le lexicoder anglo du package de Quanteda, qui se nomme data_dictionary_LSD2015
data_ton <- runDictionaryFunction(dataA = polarity,
                                  word = word,
                                  dictionaryA = data_dictionary_LSD2015)

# Pour le top négatif et positif
topcount <- bind_cols(polarity, data_ton) %>%
  group_by(word) %>%
  summarise(negative = sum(negative),
            positive = sum(positive))

# Ukraine
graphdata_Ukraine <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  filter(opinion == 0) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Ukraine")

# Syria
graphdata_Syria <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  filter(opinion == 0) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Syria")

# On met tous ensemble et on calcule
graphdata <- graphdata_Ukraine %>%
  bind_rows(graphdata_Syria) %>%
  group_by(country) %>%
  summarise(
    n=n(),
    mean=mean(diffProp),
    sd=sd(diffProp)) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))


# Standard deviation
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard deviation")

# Standard Error
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard error")

# Confidence Interval
ggplot(graphdata) +
  geom_bar( aes(x=country, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=country, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")

# # On plot
# ggplot(graphdata, aes(x = date, y = diffProp)) +
#   geom_hline(yintercept = 0, color = "red") +
#  geom_line() +
#  # geom_point(alpha = 0.4) +
#   geom_smooth(se = F, span = 0.4, size = 2.5) +
#   scale_x_datetime("", date_labels = "%m-%Y", date_breaks = "1 month") +
#   # scale_color_viridis_d(labels = c("CBC", "Toronto Star"),
#   #                       limits = c("CBC", "TStar")) +
#   theme_clean() +
#   # labs(title = "Évolution du ton des articles sur le cannabis dans les médias anglophones (1985 à 2019)",
#   #      # subtitle = "Version 1: aucune pondération.",
#   #      subtitle = "",
#   #      # caption = "Dictionnaire: version francophone du Lexicoder (Duval et Pétry, 2016)
#   #      # Pondéré en fonction du nombre moyen de mots par mois, après nettoyage des textes et application du dictionnaire") +
#   #      caption = "n = 14 771 articles
#   #      Dictionnaire: Lexicoder (Soroka and Young, 2015)
#   #      L'indice de ton est calculé en fonction de la différence entre les proportions de mots positifs et négatifs par mois") +
#   ylab("Proportion de mots négatifs\n") +
#   theme(plot.title = element_text(hjust = -1.5, size = 18),
#         plot.subtitle=element_text(size = 13),
#         plot.caption = element_text(hjust = 1, size = 11),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 14),
#         legend.position = "top") +
#   guides(color=guide_legend(title="Country"))
#
# ggsave("../_SharedFolder_article_syrie-ukraine/_v2/lexicoder/lexicoder-ukraine.png", width = 30, height = 17, units = c("cm"))


########################################################################################################### ##
################################################## Time series ###############################################
########################################################################################################### ##

library(stats)

days <- seq(1, 365)
date <- as.Date(days, origin = "2014-12-31")
dates_syria <- data.frame(date)

timeSeries_syrie <- graphdata_Syrie %>%
  mutate(date = as.Date(date)) %>%
  select(date, diffProp) %>%
  full_join(dates_syria, by = "date") %>%
  arrange(date) %>%
  mutate(country = "Syrie")

days <- seq(1, 365)
date <- as.Date(days, origin = "2021-12-31")
dates_ukraine <- data.frame(date)

timeSeries_ukraine <- graphdata_ukraine %>%
  mutate(date = as.Date(date)) %>%
  select(date, diffProp) %>%
  full_join(dates_ukraine, by = "date") %>%
  arrange(date) %>%
  mutate(country = "Ukraine")

timeSeries <- timeSeries_syrie %>%
  bind_rows(timeSeries_ukraine)

aov_result <- aov(diffProp ~ date, data = timeSeries)
summary(aov_result)

timeSeries_graph <- dates_ukraine %>%
  bind_cols(timeSeries_syrie) %>%
  select(-date...2, date = date...1) %>%
  bind_rows(timeSeries_ukraine) %>%
  mutate(date = as.POSIXct(date, format = "%m-%d")) %>%
  na.omit() %>%
  group_by(country) %>%
  mutate(mean = mean(diffProp))


# On plot
ggplot(timeSeries_graph, aes(x = date, y = diffProp, color = country)) +
  geom_hline(yintercept = 0, color = "red") +
 # geom_line() +
   geom_point(alpha = 0.4) +
  geom_smooth(se = T, span = 0.6, size = 2.5) +
  scale_x_datetime("", date_labels = "%d-%m", date_breaks = "1 month") +
  # scale_color_viridis_d(labels = c("CBC", "Toronto Star"),
  #                       limits = c("CBC", "TStar")) +
  theme_clean() +
  # labs(title = "Évolution du ton des articles sur le cannabis dans les médias anglophones (1985 à 2019)",
  #      # subtitle = "Version 1: aucune pondération.",
  #      subtitle = "",
  #      # caption = "Dictionnaire: version francophone du Lexicoder (Duval et Pétry, 2016)
  #      # Pondéré en fonction du nombre moyen de mots par mois, après nettoyage des textes et application du dictionnaire") +
  #      caption = "n = 14 771 articles
  #      Dictionnaire: Lexicoder (Soroka and Young, 2015)
  #      L'indice de ton est calculé en fonction de la différence entre les proportions de mots positifs et négatifs par mois") +
  ylab("Tone Index\n") +
  theme(plot.title = element_text(hjust = -1.5, size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "top") +
  guides(color=guide_legend(title="Country"))

ggsave("../_SharedFolder_article_syrie-ukraine/_v2/timeSeries/time-series.png", width = 30, height = 17, units = c("cm"))

########################################################################################################### ##
################################################ Sentiment NRC ###############################################
########################################################################################################### ##

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
syrie_tidy <- tidy(dataSyrie2015_dtm)

# Sortir le dictiononaire
nrc <- get_sentiments("nrc")

syrie_nrc <- syrie_tidy %>%
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment) %>%
  summarize(total_count = sum(count))

# Plot total_count vs. sentiment
ggplot(syrie_nrc, aes(x = sentiment, y = total_count)) +
  geom_col()

#ggsave("graphs/_v2/nrc/syria_nrc.png", width = 30, height = 17, units = c("cm"))

### Syrie

syrie_nrc_time <- syrie_tidy %>%
  rename(doc_id = document) %>%
  left_join(dataSyrie2015, by = "doc_id") %>%
  group_by(date) %>%
  mutate(total_words = n(),
         date = as.POSIXct(date)) %>%
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(sentiment %in% c("fear", "sadness")) %>%
  group_by(date, sentiment, total_words) %>%
  summarize(total_count = sum(count)) %>%
  mutate(propSentiment = total_count/total_words)
  # pivot_wider(id_cols = date, names_from = sentiment, values_from = propSentiment) %>%
  # mutate(diffProp = positive - negative)

### Ukraine

# Pour changer un DTM en format tidy, on utilise tidy() du broom package.
ukraine_tidy <- tidy(dataUkraine_dtm)

# Sortir le dictiononaire
nrc <- get_sentiments("nrc")

ukraine_nrc <- ukraine_tidy %>%
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment) %>%
  summarize(total_count = sum(count))

# Plot total_count vs. sentiment
ggplot(ukraine_nrc, aes(x = sentiment, y = total_count)) +
  geom_col()

#ggsave("graphs/_v2/nrc/ukraine_nrc.png", width = 30, height = 17, units = c("cm"))

ukraine_nrc_time <- ukraine_tidy %>%
  rename(doc_id = document) %>%
  left_join(dataUkraine, by = "doc_id") %>%
  group_by(date) %>%
  mutate(total_words = n(),
         date = as.POSIXct(date)) %>%
  inner_join(nrc, by = c("term" = "word")) %>%
  filter(sentiment %in% c("fear", "sadness")) %>%
  group_by(date, sentiment, total_words) %>%
  summarize(total_count = sum(count)) %>%
  mutate(propSentiment = total_count/total_words)

# On plot
plotNRC1 <- ggplot(syrie_nrc_time, aes(x = date, y = propSentiment, color = sentiment)) +
 # geom_hline(yintercept = 0, color = "red") +
  geom_line() +
  geom_smooth(se = F, span = 0.6, size = 2.5) +
  scale_x_datetime("", date_labels = "%b", date_breaks = "1 month") +
  geom_vline(xintercept = as.POSIXct("2015-10-01"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2015-10-01"), y = 0.225, label = "Migration peak", angle = 90, vjust = -0.60, show.legend = F) +
  geom_vline(xintercept = as.POSIXct("2015-09-02"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2015-09-02"), y = 0.219, label = "Death of Alan Kurdi", angle = 90, vjust = -0.60, show.legend = F) +
  geom_vline(xintercept = as.POSIXct("2015-11-13"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2015-11-13"), y = 0.2275, label = "Paris attacks", angle = 90, vjust = -0.60, show.legend = F) +
  scale_y_continuous(name="Proportion of words related to sentiment\n",
                     breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25),
                     limits = c(0, 0.26),
                     expand = c(0,0)) +
  scale_color_manual(values = c("fear" = "#FF0000", "sadness" = "#0000FF")) +
  theme_clean() +
  ggtitle("Syria (2015)") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "none",
        panel.border = element_blank(),
        legend.margin = margin(6, 6, 6, 6))

# On plot
plotNRC2 <- ggplot(ukraine_nrc_time, aes(x = date, y = propSentiment, color = sentiment)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_line() +
  geom_smooth(se = F, span = 0.6, size = 2.5) +
  geom_vline(xintercept = as.POSIXct("2022-03-15"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  geom_text(x = as.POSIXct("2022-03-15"), y = 0.225, label = "Migration peak", angle = 90, vjust = -0.6, show.legend = F) +
  scale_y_continuous(name="",
                     breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25),
                     limits = c(0, 0.26),
                     expand = c(0,0)) +
  scale_color_manual(values = c("fear" = "#FF0000", "sadness" = "#0000FF")) +
  scale_x_datetime("", date_labels = "%b", date_breaks = "1 month") +
  theme_clean() +
  ggtitle("Ukraine (2022)") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = c(.85, .99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  guides(color=guide_legend(title="Sentiments"))

plot <- grid.arrange(plotNRC1, plotNRC2, nrow = 1)


ggsave("../_SharedFolder_article_syrie-ukraine/graphs/_v2/nrc/nrc-syrie-ukraine.png", plot, width = 30, height = 17, units = c("cm"))



########################################################################################################### ##
################################################# Topic modeling #############################################
########################################################################################################### ##

# D'abord, on a besoin d'un dtm  (voir section DTM/TDM)

# Run une LDA (Latent dirichlet allocation) avec 2 sujets et un «sampler» Gibbs
# seed: set a seed so that the output of the model is predictable (random seed for reproducability)
# α : Parameter that sets the topic distribution for the documents,
# the higher the more spread out the documents will be across the specified number of topics (K).
# η : Parameter that sets the topic distribution for the words,
# the higher the more spread out the words will be across the specified number of topics (K).

# On ne veut pas nécessairement être trop restrictif dans nos thèmes, puisque notre analyse est «par document»
# et que dans chacun de nos document, on a beaucoup d'articles, donc beaucoup de thèmes. On peut laisser un
# peu de liberté à notre alpha.

# Test 1, avec paramètres de bases selon tidy R
# lda_out <- LDA(
#   Weed_dtmTStar,
#   k = 3,
#   method = "Gibbs",
#   control = list(seed = 64)
#   )

# test 2 avec les paramètres du cours datacamp Topic Modeling (et j'ai mis seed 64, au lieu de 12345)
lda_out <- LDA(
  dataUkraine_dtm,
#  dataUkraine_dtm,
  k = 3,
  method = "Gibbs",
  control = list(alpha = 0.1,
                 delta = 0.1,
                 seed = 64))

# On jette un coup d'oeil
# glimpse(lda_out2)

# On tidy la matrice des probabilités des mots (bêta)
lda_topics <- lda_out %>%
  tidy(matrix="beta") %>%
  arrange(desc(beta))

# Sélectionnez les 15 premiers termes par topic et on les réorganise
word_probs <- lda_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

word_probs <- as.data.frame(word_probs)

# setter l'ordres des thèmes
#order <- c("War with Russia", "Impact on populations", "International response")
#order <- c("European migration", "Terrorism", "Refugee (new) life", "British politics", "Canadian politics")

# Graphiques de cadre
#word_probs$topic2 <- NA
#word_probs$topic2[word_probs$topic==1]<- "War with Russia"
#word_probs$topic2[word_probs$topic==2]<- "Impact on populations"
#word_probs$topic2[word_probs$topic==3]<- "International response"

# word_probs$topic2 <- NA
# word_probs$topic2[word_probs$topic==1]<- "European migration"
# word_probs$topic2[word_probs$topic==2]<- "Terrorism"
# word_probs$topic2[word_probs$topic==3]<- "Refugee (new) life"
# word_probs$topic2[word_probs$topic==4]<- "British politics"
# word_probs$topic2[word_probs$topic==5]<- "Canadian politics"

#word_probs$topic <- factor(word_probs$topic2, levels = order)

#levels(word_probs$topic) <- order

# on plot par topic
ggplot(word_probs, aes(x=term2, y=beta, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  # labs(title = "Modélisation thématique dans les articles sur le cannabis des médias francophones (1988-2019)",
  #      caption = "n = 15 184 articles",
  #     # caption = "n = 14 771 articles",
  #      x="") +
  ylab("\nProbability (beta) of words being associated with each topic") +
  xlab("") +
  #  scale_fill_manual(values = c("#9e0015", "#9e9300")) +
  #  scale_fill_manual(values = c("#20a8a0", "#c32c6a", "#2b0057")) +
#  scale_fill_manual(values = c("#440154FF", "#33638CFF", "#1F968BFF", "#FDE725FF")) +
  #  scale_fill_manual(values = c("#6f2194", "#9e9300", "#026629", "#002d9e", "#9e0015")) +
  #  scale_fill_manual(values = c("#9e0015", "#9e9300", "#002d9e", "#026629", "#9e4400", "#6f2194")) +
  ggthemes::theme_clean() +
  theme(plot.title = element_text(hjust = -0.2, size = 16),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 14),
        strip.text.x = element_text(size = 15))

ggsave("../_SharedFolder_article_syrie-ukraine/graphs/_v2/lda_syrie_2015/3k_LDAbeta-syrie2015.png", width = 30, height = 17, units = c("cm"))

# renvoie les probabilités des topics.
# posterior(lda_out2)$topics

# Voir les probabilités d'appartenance d'un mot aux différents topics
# tidy(mod, matrix="beta") %>%
#   filter(term == "government")

# On plot (plus nice)
word_probs2 <- tidy(lda_out, "gamma") %>%
  rename(doc_id = document) %>%
  left_join(dataUkraine_op, by = "doc_id") %>%
  mutate(date = as.POSIXct(date))

#word_probs2$topic2 <- NA
#word_probs2$topic2[word_probs2$topic==1]<- "War with Russia"
#word_probs2$topic2[word_probs2$topic==2]<- "Impact on populations"
#word_probs2$topic2[word_probs2$topic==3]<- "International response"

# word_probs2$topic2 <- NA
# word_probs2$topic2[word_probs2$topic==1]<- "European migration"
# word_probs2$topic2[word_probs2$topic==2]<- "Terrorism"
# word_probs2$topic2[word_probs2$topic==3]<- "Refugee (new) life"
# word_probs2$topic2[word_probs2$topic==4]<- "British politics"
# word_probs2$topic2[word_probs2$topic==5]<- "Canadian politics"

#word_probs2$topic <- factor(word_probs2$topic2, levels=order)

ggplot(word_probs2, aes(x = date, y = gamma)) +
  #  geom_point(aes(color=factor(topic)), alpha = 0.3) +
  geom_smooth(se = F, aes(color=factor(topic)), size = 2.5) +
  # labs(title = "Modélisation thématique dans les articles sur le cannabis dans les médias francophones (1988-2019)",
  #      caption = "n = 15 184 articles") +
  # caption = "n = 14 771 articles",
  ylab("Probability (gamma) of articles being associated with each topic\n") +
  # scale_color_manual(name = "Thèmes",
  #                      labels = c("Économie", "Criminalité", "Politique"),
  #                      values = c("#9e9300", "#9e0015", "#002d9e")) +
 # scale_color_manual(name = "Topics",
  #                    #   labels = c("Légalisation", "Criminalité"),
  #                    #  values = c("#9e0015", "#9e9300")) +
#  values = c("#20a8a0", "#c32c6a", "#2b0057"))+
  #                    values = c("#440154FF", "#33638CFF", "#1F968BFF", "#FDE725FF")) +
  #  values = c("#9e0015", "#9e9300", "#002d9e", "#026629")) +
  # values = c("#9e0015", "#9e9300", "#002d9e", "#026629", "#9e4400")) +
#  values = c("#6f2194", "#9e9300", "#026629", "#002d9e", "#9e0015")) +
#  geom_vline(xintercept = as.POSIXct("2022-03-15"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
#  geom_text(x = as.POSIXct("2022-03-15"), y = 0.468, label = "Migration peak", angle = 90, vjust = -0.6, show.legend = F) +
  scale_x_datetime("", date_labels = "%m-%Y", date_breaks = "2 month") +
  # geom_vline(xintercept = as.POSIXct("2015-10-01"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-10-01"), y = 0.358, label = "Migration peak", angle = 90, vjust = -0.60, show.legend = F) +
  # geom_vline(xintercept = as.POSIXct("2015-09-02"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-09-02"), y = 0.345, label = "Death of Alan Kurdi", angle = 90, vjust = -0.60, show.legend = F) +
  # geom_vline(xintercept = as.POSIXct("2015-11-13"), color = "darkgrey", size = 0.8  , linetype = "dashed") +
  # geom_text(x = as.POSIXct("2015-11-13"), y = 0.362, label = "Paris attacks", angle = 90, vjust = -0.60, show.legend = F) +
  theme_clean() +
  theme(plot.title = element_text(hjust = 0.15, size = 16),
        plot.subtitle=element_text(size = 13),
        plot.caption = element_text(hjust = 1.19, size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  guides(alpha = FALSE)

#  scale_color_manual(values=brewer.pal(n=4, "Set1"), name="Topic")
ggsave("../_SharedFolder_article_syrie-ukraine/graphs/_v2/lda_syrie_2015/3k_LDAgamma-syrie2015.png", width = 30, height = 17, units = c("cm"))

########################################################################################################### ##
################################################## Regressions ###############################################
########################################################################################################### ##

# Ukraine
graphdata_Ukraine <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  mutate(date = floor_date(as_date(date), "month")) %>%
#  filter(opinion == 0) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Ukraine") %>%
  group_by(country, date) %>%
  summarise(ton = mean(diffProp))

# Syria
graphdata_Syria <- bind_cols(polarity, data_ton) %>%
  group_by(date, total_words) %>%
  mutate(date = floor_date(as_date(date), "month")) %>%
#  filter(opinion == 0) %>%
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%
  mutate(propNeg = (negative/total_words),
         propPos = (positive/total_words)) %>%
  mutate(diffProp = propPos - propNeg) %>%
  mutate(country = "Syria") %>%
  group_by(country, date) %>%
  summarise(ton = mean(diffProp))

reg <- bind_rows(graphdata_Syria, graphdata_Ukraine) %>%
  right_join(nb_refugies, by = "date")

# Modèle 1: Juste classe
model_1 <- lm(ton ~ nb_refugies, data = reg)
summary(model_1)

# Modèle 1: Juste classe
model_2 <- lm(ton ~ nb_refugies + country.y, data = reg)
summary(model_2)

# Modèle 1: Juste classe
model_3 <- lm(ton ~ nb_refugies + country.y + nb_refugies*country.y, data = reg)
summary(model_3)

# Stargazer pour clusters
stargazer(model_1, model_2, model_3,
          type = 'latex',

          header=FALSE, # to get rid of r package output text

          single.row = TRUE, # to put coefficients and standard errors on same line

          no.space = F, # to remove the spaces after each line of coefficients

          column.sep.width = "3pt", # to reduce column width

          font.size = "small" # to make font size smaller

)

########################################################################################################### ##
####################################################### FIN ##################################################
########################################################################################################### ##

