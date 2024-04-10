# Charger les packages nécessaires pour les manipulations de données et la création de graphiques
library(dplyr)
library(ggplot2)

# Charger le jeu de données préparé pour l'analyse
data <- readRDS("data/data_analyse_textuelle.rds")

# Créer un graphique de séries temporelles avec ggplot
ggplot(data, aes(x = date, y = tone_index, color = country)) +  # Définir les axes et la couleur des lignes selon le pays
  geom_smooth(method = "loess", se = TRUE, aes(group = country)) +  # Ajouter une ligne lissée pour chaque pays avec intervalle de confiance
  scale_x_date(limits = c(as.Date("2013-01-02"), NA)) +  # Limiter l'axe des x à partir d'une date spécifique
  theme_classic() +  # Utiliser un thème classique pour un aspect épuré
  labs(x = "Date", y = "Tone", color = "Country") +  # Définir les étiquettes des axes et de la légende
  scale_color_manual(values = c("Syrie" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7"))  # Personnaliser les couleurs pour chaque pays

# Sauvegarder le graphique en tant qu'image PNG avec des dimensions spécifiques
ggsave("data/graphs/time_series.png", width = 10, height = 6, units = "in")
