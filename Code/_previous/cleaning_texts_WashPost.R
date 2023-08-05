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

data <- read_rtf("../Data/BaseArticles/US-WaPo-Syrie/US-WaPo-Syrie-8.rtf")
name <- "US-WaPo-Syrie-1"

df_data <- as.data.frame(data) %>%
  slice(-1)

df_data2 <- df_data %>%
  mutate(variable =
           ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                              grep("[1-9] f$", data, value=T),
                              grep("février [1-9]", data, value=T),
                              grep("mars [1-9]", data, value=T),
                              grep("avril [1-9]", data, value=T),
                              grep("mai [1-9]", data, value=T),
                              grep("juin [1-9]", data, value=T),
                              grep("juillet [1-9]", data, value=T),
                              grep("[0-9] ao$", data, value=T),
                              grep("août [1-9]", data, value=T),
                              grep("septembre [1-9]", data, value=T),
                              grep("octobre [1-9]", data, value=T),
                              grep("novembre [1-9]", data, value=T),
                              grep("[1-9] d$", data, value=T),
                              grep("décembre [1-9]", data, value=T)), "date",
           ifelse(grepl("mots", data), "words",
           ifelse(data %in% c(grep("^t [1-9]", data, value=T),
                              grep("^vrier [1-9]", data, value=T),
                              grep("^cembre [1-9]", data, value=T)), "year",
           ifelse(grepl("Document [A-Z]+", data), "doc_id",
           "text")))),
         id =
           ifelse(variable == "doc_id", 1, 0)) %>%
  filter(!data %in% c("WP", "Anglais", "FINAL", "Copyright 2021, The Washington Post Co. All Rights Reserved",
                      "A-Section", "Editorial-Opinion"))

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
# Définir le format de date en français
Sys.setlocale("LC_TIME", "fr_FR")
# Utiliser ifelse() pour vérifier si la colonne "year" existe et la créer si elle n'existe pas
df_data3 <- df_data2 %>%
  select(-id) %>%
  filter(data != "") %>%
  pivot_wider(names_from = variable, values_from = data,
              id_cols = "article_id") %>%
  mutate(source = "The Washington Post",
         country = "Syrie") %>%
  # Vérifier si la colonne "year" existe et la créer si elle n'existe pas
  mutate(year = ifelse("year" %in% names(data), year, "")) %>%
  # Modifier la colonne "year" en utilisant les gsub() que vous avez déjà définis
  mutate(year = gsub("cembre ([1-9])", "écembre \\1", year),
         year = gsub("t ([1-9])", "ût \\1", year),
         year = gsub("vrier ([1-9])", "évrier \\1", year),
         year = gsub("NULL", "", year)) %>%
  # Unir la colonne "date" et la colonne "year" en une seule colonne "date"
  unite(date, date, year, sep = "") %>%
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
saveRDS(df_data3, paste0("../Data/clean_syrie/", name, ".rds"))

########################################################################################################### ##
#################################################### Tout bind ###############################################
########################################################################################################### ##

#test <- readRDS("../clean_syrie/CA-Globe&Mail-Syrie-1.rds")
#
# rds_combo <- list.files( path = "clean_syrie/", pattern = "*.rds", full.names = TRUE ) %>%
#   map_dfr(readRDS)

# Avec le bon path on peut mettre tout les rds et les bind ensemble et créer un autre fichier de code "analyse descriptive".

########################################################################################################### ##
####################################################### FIN ##################################################
########################################################################################################### ##
