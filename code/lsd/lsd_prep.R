# Charger le package dplyr pour les manipulations de données
library(dplyr)

# Charger le script de préparation pour LSD qui contient des fonctions nécessaires pour le prétraitement des textes
source(file = "data/lsd_prep_sources/LSDprep_dec2017.R")

# Lire le jeu de données stocké dans un fichier RDS et le stocker dans la variable 'data'
data <- readRDS("data/data.rds")

# Préparation des données pour l'analyse LSD
data_lsd <- data  %>% 
    # Diviser le texte en phrases pour faciliter l'analyse de chaque phrase individuellement
    tidytext::unnest_sentences(text, text) %>%
    # Filtrer les phrases contenant des mots-clés spécifiques relatifs aux réfugiés et migrants
    filter(grepl('refugee|refugees|migrant|migrants', text)) %>%
    # Dissocier le groupe de données pour éviter des erreurs dans les manipulations futures
    ungroup() %>%
    # Ajouter un identifiant unique à chaque phrase pour faciliter le suivi
    mutate(id_sentence = row_number())

# Initialiser une nouvelle colonne pour stocker le texte préparé pour l'analyse LSD
data_lsd$text_prepped <- NA

# Départ du chronomètre
start_time <- Sys.time()

# Appliquer une série de fonctions de préparation du texte définies dans le script LSDprep
data_lsd$text_prepped <- sapply(data_lsd$text, LSDprep_contr)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, LSDprep_dict_punct)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, remove_punctuation_from_acronyms)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, remove_punctuation_from_abbreviations)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, LSDprep_punctspace)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, LSDprep_negation)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, LSDprep_dict)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, mark_proper_nouns)

# Fin du chronomètre
end_time <- Sys.time()

# Calculer la durée de l'opération de préparation
duration <- end_time - start_time
print(duration)


# Enregistrer le jeu de données préparé dans un fichier RDS pour une utilisation ultérieure dans l'analyse LSD
saveRDS(data_lsd, "data/data_prepped.rds")
