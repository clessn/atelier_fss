# Charger le package dplyr pour les manipulations de données
library(dplyr)

# Charger le jeu de données préparé pour l'analyse
data_reg <- readRDS("data/data_analyse_textuelle.rds")

# Extraire le mois et l'année de la date et les ajouter en tant que nouvelles colonnes
data <- data_reg %>%
  mutate(month = substr(date, 6, 7),  # Extraire le mois de la colonne date
         year = substr(date, 1, 4))  # Extraire l'année de la colonne date

# Convertir la source des articles en facteur et rééchelonner avec "The New York Times" comme référence
data$source <- factor(data$source)
data <- within(data, source <- relevel(source, ref = "The New York Times"))

# Convertir le pays en facteur avec des niveaux spécifiques et définir "Syrie" comme catégorie de référence
data$country <- factor(data$country, levels = c("Syria", "Ukraine", "Iraq"))
data <- within(data, country <- relevel(country, ref = "Syria"))

# Convertir l'année en facteur et définir "2007" comme année de référence
data$year <- factor(data$year)
data <- within(data, year <- relevel(year, ref = "2007"))

# Définir une liste de modèles de régression linéaire pour analyser l'effet des variables indépendantes sur l'indice de tonalité
models <- list(
  "Model 1" = lm(tone_index ~ country, data = data),  # Effet du pays uniquement
  "Model 2" = lm(tone_index ~ country + year, data = data),  # Effet du pays et de l'année
  "Model 3" = lm(tone_index ~ country + source, data = data),  # Effet du pays et de la source
  "Model 4" = lm(tone_index ~ country + year + source,  data = data)  # Effet combiné du pays, de l'année et de la source
)

# Créer un tableau indiquant les effets fixes inclus dans chaque modèle
fixed_effects <- tibble::tribble(
  ~Term, ~Model1, ~Model2, ~Model3, ~Model4,
  "Years Fixed Effect", "", "$\\checkmark$", "", "$\\checkmark$",
  "Sources Fixed Effect", "", "", "$\\checkmark$", "$\\checkmark$"
)

# Résumer les modèles et sauvegarder le tableau dans un fichier LaTeX
modelsummary::modelsummary(models,
             output = "data/reg_table/main.tex",  # Chemin du fichier de sortie
             stars = TRUE,  # Afficher les étoiles pour les niveaux de significativité
             coef_omit = "year|source|Intercept",  # Omettre les coefficients pour l'année, la source et l'interception
             gof_omit = 'DF|Deviance|AIC|BIC|Log|RMSE|R2',  # Omettre certaines mesures de qualité d'ajustement
             coef_rename = c("countrySyria" = "Articles about Syria",  # Renommer les coefficients pour une meilleure compréhension
                             "countryUkraine" = "Articles about Ukraine",
                             "countryIraq" = "Articles about Iraq"),
             add_rows = fixed_effects)  # Ajouter le tableau des effets fixes au résumé
