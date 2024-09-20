# Bienvenue dans le code

# Charger les packages nécessaires pour la manipulation de données et la création de graphiques
library(dplyr)
library(ggplot2)

# Charger le jeu de données préparé pour l'analyse
data <- readRDS("data/data_analyse_textuelle.rds")

# Créer un graphique en barres représentant la moyenne de l'indice de tonalité par pays
ggplot(data, aes(x = country, y = tone_index, fill = country)) +  # Définir les variables pour l'axe des x et la couleur de remplissage des barres
  stat_summary(fun = mean, geom = "bar", na.rm = TRUE,  # Calculer et afficher la moyenne de l'indice de tonalité pour chaque pays en tant que barres
               position = position_dodge(width = 0.9), alpha = 0.7) +
  stat_summary(
    fun.data = mean_se, geom = "linerange", na.rm = TRUE,  # Ajouter des intervalles d'erreur pour montrer la variabilité autour de la moyenne
    aes(ymin = ..ymin.., ymax = ..ymax.., color = country),
    position = position_dodge(width = 0.9), size = 1) +
  scale_fill_manual(values = c("Syrie" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7")) +  # Personnaliser les couleurs de remplissage des barres
  scale_color_manual(values = c("Syrie" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7")) +  # Personnaliser les couleurs des intervalles d'erreur
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1, alpha = 0.7) +  # Ajouter une ligne horizontale à y=0 pour référence
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Incliner les étiquettes de l'axe des x pour une meilleure lisibilité
        axis.title.x = element_text(hjust = 0.5),  # Centrer le titre de l'axe des x
        axis.title.y = element_text(hjust = 0.5)) +  # Centrer le titre de l'axe des y
  labs(x = "Country", y = "Tone mean", title = "Tone mean by country")  # Définir les étiquettes des axes et le titre du graphique

# Sauvegarder le graphique en tant qu'image PNG avec des dimensions spécifiques
ggsave("data/graphs/tone_by_country.png", width = 8, height = 6, units = "in")
