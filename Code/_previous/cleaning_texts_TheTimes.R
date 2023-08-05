##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MOT DE BIENVENUE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Bienvenue dans le PREMIER Code R de l'article 2 du mémoire d'Adrien Cloutier
# sur l'évolution de l'opinion publique par rapport au cannabis mesuré AU TRAVERS des médias
# J'espère que vous le trouverez simple et logique.

# À NOTER: IL S'AGIT SEULEMENT DU CODE DE NETTOYAGE DES PDF TIRÉS D'EUREKA.

# Pour toute question ou demande de reproduction, merci de me contacter au 418-590-2605
# ou adrien.cloutier.1@ulaval.ca.
# Au plaisir!f


##### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Notes et rappels à moi-même %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

########################################################################################################### ##
####################################################### RTF ########################################$#########
########################################################################################################### ##

#base R
# setwd("..")
# Lines <- readLines("Data/BaseArticles/CA-Globe&Mail-Syrie/CA-Globe&Mail-Syrie-1.rtf",
#                    warn = F)

#striprtf
#install.packages("striprtf")
library(striprtf)
library(tidyverse)

data <- read_rtf("../Data/BaseArticles/UK-The Times-Syrie/UK-The Times-Syrie-1.rtf")
name <- "UK-TheTimes-Syrie-1"

df_data <- as.data.frame(data) %>%
  slice(-1)

df_data2 <- df_data %>%
  mutate(variable =
           ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                              grep("février [1-9]", data, value=T),
                              grep("mars [1-9]", data, value=T),
                              grep("avril [1-9]", data, value=T),
                              grep("mai [1-9]", data, value=T),
                              grep("juin [1-9]", data, value=T),
                              grep("juillet [1-9]", data, value=T),
                              grep("août [1-9]", data, value=T),
                              grep("septembre [1-9]", data, value=T),
                              grep("octobre [1-9]", data, value=T),
                              grep("novembre [1-9]", data, value=T),
                              grep("décembre [1-9]", data, value=T)), "date",
           ifelse(grepl("mots", data), "words",
           ifelse(grepl("Document [A-Z]+", data), "doc_id",
           "text"))),
         id =
           ifelse(variable == "doc_id", 1, 0)) %>%
  filter(!data %in% c("NYTF", "Foreign Desk; SECTA", "Anglais", "Late Edition - Final",
                      "Copyright 2022 The New York Times Company.  All Rights Reserved.",
                      "LETTER Editorial Desk; SECTA", "GUEST ESSAY Editorial Desk; SECTA ", "LETTERS Editorial Desk",
                      "Book Review Desk", "NONFICTION Book Review Desk", "Arts and Leisure Desk; SECTAR",
                      "The Arts/Cultural Desk; SECTC", "Editorial Desk; SECTA", "National Desk; SECTA",
                      "Book Review Desk", "Sunday Review Desk; SECTSR", "Classified", "LETTERS", "SpecialSections; SECTF",
                      "LETTER", "GUEST ESSAY", "Obituary;", "OpEd; SECT", "FASHION REVIEW Style Desk; SECTD",
                      "OFF THE MENU Dining In, Dining Out / Style Desk; SECTD",
                      "ON TECH NEWSLETTER Business/Financial Desk; SECTB"))

increments <- which(df_data2$id == 1)

real_ending <- max(increments)

df_data2 <- head(df_data2, real_ending)

vector <- c()
for (i in 1:length(increments)){
  ix <- increments[i]
  times <- ix - length(vector)
  vectori <- rep(i, times)
  vector <- c(vector, vectori)
  print(i)
}

df_data2$article_id <- vector

library(chron)
Sys.setlocale("LC_TIME", "fr_FR")
df_data3 <- df_data2 %>%
  select(-id) %>%
  filter(data != "") %>%
  pivot_wider(names_from = variable, values_from = data,
              id_cols = "article_id") %>%
  mutate(country = "Syrie",
         source = "The Times") %>%
  select(doc_id, text, date, source, country) %>%
  rowwise() %>%
  mutate(doc_id = gsub("Document", "", doc_id),
         date = as.Date(date, "%d %B %Y"))

texts <- c()
for(i in 1:nrow(df_data3)){
  texts[i] <- paste0(eval(parse(text = df_data3$text[i])), collapse = " ")
  print(i)
}

df_data3$text <- texts


########################################################################################################### ##
################################################## Enregistrer ###############################################
########################################################################################################### ##


# Enregistrer le csv
saveRDS(df_data3, paste0("../Data/clean_ukraine/", name, ".rds"))

########################################################################################################### ##
#################################################### Tout bind ###############################################
########################################################################################################### ##

# test <- readRDS("clean/CA-Globe&Mail-Syrie-2.rds")
#
# rds_combo <- list.files( path = "clean/", pattern = "*.rds", full.names = TRUE ) %>%
#   map_dfr(readRDS)

########################################################################################################### ##
####################################################### FIN ##################################################
########################################################################################################### ##
