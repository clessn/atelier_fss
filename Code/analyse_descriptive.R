### Fichier d'analyse descriptive issus des articles de journaux pour l'article sur l'asymétrie de l'empathie des réfugiés
### Ukrainiens et Syriens (Alexandre Pelletier, Yannick Dufresne, Axel Déry, Adrien Cloutier & Nadjim Fréchet)



library(tm)
#library(stm)     # À évaluer avec des variables nettoyées
library(haven)
library(readxl)
library(crayon)
library(textcat)  # À raffiner
library(foreign)
library(quanteda)
library(tidytext)
library(tidyverse)
library(seededlda)

# 1. Loader les données des articles du Globe and Mail

data <- read_rds("_SharedFolder_article_syrie-ukraine/Data/dataset.rds")


# DF avec juste les articles syrie
data1 <- data %>%
  filter(country == "Syrie")

# DF avec juste les article Ukraine
data2 <- data %>%
  filter(country == "Ukraine")




# get dictionnaire hublot : celui ci est le dictionnaire issues
credentials <- hublot::get_credentials(
  Sys.getenv("HUB3_URL"),
  Sys.getenv("HUB3_USERNAME"),
  Sys.getenv("HUB3_PASSWORD"))

# Loader le dictionnaire
issues_dict <- clessnverse::get_dictionary('issues', c("en","fr"), credentials) # changer le dictionnaire ici -> subcategories ou issues


# Pour rouler les dictionnaires pour analyses textuelles (fonction en construction)
runDictionary1 <- function(dataA, word, dictionaryA) {
  tictoc::tic()
  dataA <- dataA %>% mutate(word = {{word}})
  corpusA <- tokens(dataA$word)
  dfmA    <- dfm(tokens_lookup(corpusA, dictionaryA, nested_scope = "dictionary"))
  message(green("100% expressions/words found"))
  tictoc::toc()
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}

runDictionary2 <- function(dataA, word, dictionaryA) {
  tictoc::tic()
  dataA <- dataA %>% mutate(word = {{word}})
  corpusA <- tokens(dataA$word)
  dfmA    <- dfm(tokens_lookup(corpusA, dictionaryA, nested_scope = "dictionary"))
  message(green("100% expressions/words found"))
  tictoc::toc()
  dataB   <- convert(dfmA, to = "data.frame")
  return(dataB)
}
#### 1 - data clean (for topic modeling) ####

#stopwordsFrench  <- c(tm::stopwords("fr"), "jai", "quil", "tout", "toutes", "fait", "faire", "etc", "tous","qu'il", "qu'elle", "qu'ils", "qu'elles", "l'", "sais", "être")
#stopwordsEnglish <- tm::stopwords("en")
#
#dataClean1 <- data1 %>%
#  select(text) %>%
#  na.omit() %>%
#  mutate(id = 1:nrow(.), open = tolower(removePunctuation(text)),
#         lang = textcat(open),
#         lang = case_when(lang == "english"  ~ "english",
#                          lang == "french"   ~ "french",
#                          lang != c("english") ~ "Other")) %>%
#  unnest_tokens(word, text) %>%
#  filter(!(word %in% stopwordsEnglish),
#         !(word %in% stopwordsFrench)) %>%
#  group_by(id) %>%
#  mutate(text = paste(word, collapse = " ")) %>%
#  select(-word) %>%
#  distinct()

#### 3.1 - Analyse du dictionnaire sur la question 6 ####

# Prepare data

#DataDictMerge <- DataMerged %>%
#  select(openQ) %>%
#  na.omit()

#**************************************************#
#*******************   Syrie    *******************#
#**************************************************#

# Run dictionary analysis (Syrie)

dataForAnal1 <- runDictionary1(dataA = data1, word = text, issues_dict) %>%
  mutate(other = aboriginal + agriculture + constitutional_natl_unity + energy + environment + fisheries + foreign_trade +
          forestry + intergovernmental) %>% # merge les colonnes qui nous intéresse moins
  select(doc_id, civil_rights, crime, culture, defence, education, finance, government_ops, healthcare, housing, immigration,
         intl_affairs, labour, macroeconomics, religion, social_welfare, other) %>%  # garder les colonnes qui nous interesse
  pivot_longer(!doc_id, names_to = "enjeu", values_to = "n") %>%
  # select(-doc_id) %>%
  mutate(mention = ifelse(n > 0, yes = 1, no = 0)) %>%
  group_by(enjeu) %>%
  summarise(n=sum(mention)) %>%
  mutate(prop  = round(n/sum(n),4)*100,
         enjeu = fct_reorder(enjeu, prop)) %>%
top_n(prop, 5) # keep les 5 enjeux les plus mentionnés
#saveRDS(DataDictMerged, "2022data.rds")

# graphique 1 Syrie
ggplot(dataForAnal1, aes(x=enjeu, y=prop)) +
  geom_bar(stat="identity", show.legend = F, color="black", fill="blue", size=1.5, width = 0.75) +
  geom_text(aes(label=as.character(prop)), color="black", size=10, position=position_dodge(width=0.1), vjust=0.8, hjust=-0.3) +
  coord_flip() +
  expand_limits(y=0:100) +
  theme_classic(base_size = 25) +
  ggtitle("\nEnjeux associés aux réfugiés Syriens") +
  scale_x_discrete(labels=c("civil_rights" = "Droit civil", "crime" = "Criminalité", "culture" = "Culture",
                            "defence" = "Défense", "education" = "Éducation", "finance" = "Finance",
                            "government_ops" = "Opération gouvernementale", "healthcare" = "Santé", "housing" = "Logement",
                            "immigration" = "Immigration", "intl_affairs" = "Affaires internationales", "labour" = "Emploi",
                            "macroeconomics" = "Économie", "religion" = "Religion", "social_welfare" = "Sécurité sociale",
                            "other" = "Autre")) +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 50),
        panel.background = element_rect(fill="transparent"),
        legend.position="top",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 30, colour = "black"),
        axis.text.y = element_text(size = 30, colour = "black"),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=30, colour = "black",face = "bold"),
        plot.caption = element_text(hjust = 0, size = 37, colour = "black"),
        legend.text = element_text(size = 35, colour = "black"),
        legend.key.width = unit(2,"line"))
#ggsave("graph-MII-omnibus-datagotchi-merged.pdf", path = "/Users/axeldery/Dropbox/RECHERCHE/CLESSN/Élections provinciale 2022/histoire-inflation/graphs", width = 20, height = 14, units = "in")






#*****************************************************************#
#*************************   Ukraine    **************************#
#*****************************************************************#
# Run dictionary analysis (Ukraine)

dataForAnal2 <- runDictionary2(dataA = data2, word = text, issues_dict) %>%
  mutate(other = aboriginal + agriculture + constitutional_natl_unity + energy + environment + fisheries + foreign_trade +
           forestry + intergovernmental) %>% # merge les colonnes qui nous intéresse moins
  select(doc_id, civil_rights, crime, culture, defence, education, finance, government_ops, healthcare, housing, immigration,
         intl_affairs, labour, macroeconomics, religion, social_welfare, other) %>%  # garder les colonnes qui nous interesse
  pivot_longer(!doc_id, names_to = "enjeu", values_to = "n") %>%
  # select(-doc_id) %>%
  mutate(mention = ifelse(n > 0, yes = 1, no = 0)) %>%
  group_by(enjeu) %>%
  summarise(n=sum(mention)) %>%
  mutate(prop  = round(n/sum(n),4)*100,
         enjeu = fct_reorder(enjeu, prop)) #%>%
#top_n(prop, 5) # keep les 5 enjeux les plus mentionnés
saveRDS(DataDictMerged, "2022data.rds")


# graphique 2 Ukraine
ggplot(dataForAnal2, aes(x=enjeu, y=prop)) +
  geom_bar(stat="identity", show.legend = F, color="black", fill="blue", size=1.5, width = 0.75) +
  geom_text(aes(label=as.character(prop)), color="black", size=10, position=position_dodge(width=0.1), vjust=0.8, hjust=-0.3) +
  coord_flip() +
  expand_limits(y=0:100) +
  theme_classic(base_size = 25) +
  ggtitle("\nEnjeux associés aux réfugiés Ukrainiens") +
  scale_x_discrete(labels=c("civil_rights" = "Droit civil", "crime" = "Criminalité", "culture" = "Culture",
                            "defence" = "Défense", "education" = "Éducation", "finance" = "Finance",
                            "government_ops" = "Opération gouvernementale", "healthcare" = "Santé", "housing" = "Logement",
                            "immigration" = "Immigration", "intl_affairs" = "Affaires internationales", "labour" = "Emploi",
                            "macroeconomics" = "Économie", "religion" = "Religion", "social_welfare" = "Sécurité sociale",
                            "other" = "Autre")) +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 50),
        panel.background = element_rect(fill="transparent"),
        legend.position="top",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 30, colour = "black"),
        axis.text.y = element_text(size = 30, colour = "black"),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size=30, colour = "black",face = "bold"),
        plot.caption = element_text(hjust = 0, size = 37, colour = "black"),
        legend.text = element_text(size = 35, colour = "black"),
        legend.key.width = unit(2,"line"))
#ggsave("graph-MII-omnibus-datagotchi-merged.pdf", path = "/Users/axeldery/Dropbox/RECHERCHE/CLESSN/Élections provinciale 2022/histoire-inflation/graphs", width = 20, height = 14, units = "in")











