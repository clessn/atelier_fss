
### Code d'introduction à R

# Quand je mets un # au début d'une ligne, ça met la ligne «en commentaire». On ne peut pas rouler une ligne en commentaire.

# Pour rouler une ligne de code, on peut cliquer sur «Run», ci-dessus à droite, ou utiliser le raccourci clavier (sur mac: command + enter)
# après avoir mis notre curseur sur la ligne à rouler.

# Ci-dessous, je télécharge du Web le package «Tidyverse», qui n'est pas encore sur mon ordi.

# install.packages("tidyverse")

# Une fois le package téléchargé, on peut commenter la ligne ci-dessus. On n'a besoin de télécharger un package qu'une seule fois sur un ordi.

# Ensuite, j'utilise la fonction R de base «library» pour indiquer à mon RStudio que je vais utiliser ce package dans mon code.
# Cette ligne, quant à elle, est à rouler à CHAQUE nouvelle ouverture de RStudio.
library("tidyverse")

## On s'amuse avec des tests simple
print("Vive la métho!")

#Pour obtenir de l'information sur une fonction
?print()

#### 1. Exercice opérateur numérique ####
(5 + 67 + 25 + 102 + 78) / 5

#### 2. Exercice opérateur d'assignement ####
data <- (5 + 67 + 25 + 102 + 78) / 5
#moyenne_b = (5 + 67 + 25 +102 + 78) / 5 NE PAS FAIRE


# Exercice bol de fruits
# Assigner un nombre à bananes
nbBananes <- 2

# Assigner un nom à bananes
names(nbBananes) <- "banane"

# Assigner un nombre à pommes
nbPommes <- 5

# Assigner un nom à pommes
names(nbPommes) <- "pomme"

names(nbBananes) <- "Banane"

# Créer vecteur avec bananes et pommes
fruits <- c(nbBananes, nbPommes)
fruits

sum(fruits)    #retourne la somme de tous les éléments du vecteur
mean(fruits)   #retourne la moyenne de tous les éléments du vecteur
median(fruits) #retourne la médiane de tous les éléments du vecteur
range(fruits)  #retourne le min et le max d'un vecteur
min(fruits)    #retourne le min d'un vecteur
max(fruits)    #retourne le max d'un vecteur
sd(fruits)     #retourne l'écart type d'un vecteur
var(fruits)    #retourne la variance d'un vecteur
quantile(fruits) #retourne les quartiles d'un vecteur


# Je load la base de données mtcars, qui est intégrée déjà dans R, et je l'assigne à «data».
data <- mtcars

# Je jette un coup d'oeil à la bd dans ma console.
head(mtcars)
str(mtcars)

# Je jette un coup d'oeil à la bd qu'on a assigné à «data» dans ma console. C'est la même chose que mtcars, on l'a juste changée de nom!
tail(data)
str(data)

# Je crée plein de vecteurs, et je les assigne.
name <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet",
          "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", 'Gas giant')
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

# Je crée un dataframe à partir de tout ces vecteurs.
planets_df <- data.frame(name, type, diameter,rotation, rings)

# J'ouvre le dataframe dans ma console. Chaque colonne est une variable (un vector) qu'on a créé, chaque ligne est une observation!
planets_df

# Noms des planètes
planete <- c("Mercure", "Vénus", "Terre", "Mars", "Jupiter", "Saturne", "Uranus", "Neptune")

# Type de planète
type <- c("rocheuse", "rocheuse", "rocheuse", "rocheuse", "gazeuse", "gazeuse", "gazeuse", "gazeuse")

# Diamètre des planètes (en km)
diametre <- c(4879, 12104, 12756, 6792, 142984, 120536, 51118, 49528)

# Masse des planètes (en 10^24 kg)
masse <- c(0.330, 4.87, 5.97, 0.642, 1898, 568, 86.8, 102)

# Distance moyenne au Soleil (en millions de km)
distance_au_soleil <- c(57.9, 108.2, 149.6, 227.9, 778.6, 1433.5, 2872.5, 4495.1)

# Nombre de lunes
lunes <- c(0, 0, 1, 2, 79, 82, 27, 14)

# Présence d'anneaux
anneaux <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)


# Création de la base de données
planetes_df <- data.frame(planete, type, diametre, masse, distance_au_soleil, lunes, anneaux)

# Affichage de la base de données
print(planetes_df) # ça imprime mes données!



##########################################
### Quand on a une base de données sur notre ordinateur
##########################################

# J'ajoute mon chemin d'arborescence vers les données que j'ai téléchargées sont sur mon ordi
# (Ajoutez vos données dans un dossier que vous pourriez nommer «AtelierFSS», par exemple.
# Copiez ensuite votre propre «path» (chemin), ou arborescence, pour pointer vers les données).
# Pour copier un path sur Windows: https://www.howtogeek.com/670447/how-to-copy-the-full-path-of-a-file-on-windows-10/#:~:text=Find%20the%20file%20or%20folder,select%20%22Copy%20As%20Path.%22
# Sur Mac: Command-Option-C
dataTitanic <- readRDS("/Users/adriencloutier/Library/CloudStorage/Dropbox/Etudes/Universite_Laval/Doctorat/_SharedFolder_trousseEtudianteDoctoratAdri/Conferences/FSS_2024/atelier_fss_2024_03_15/data/titanic.rds")

# Pour calculer la moyenne d'âge des passagers du Titanic.
# À noter, l'argument na.rm de la fonction mean() est de base à False.
# Puisqu'on a des valeurs manquantes (NA) dans notre base de données, on doit le mettre à True (ou juste T) pour calculer la moyenne.
mean(dataTitanic$age, na.rm = T)

# Pour calculer la médiane d'âge des passagers du Titanic.
median(dataTitanic$age, na.rm = T)

# Pour calculer l'écart type (standard deviation, en anglais) de l'âge des passagers du Titanic.
sd(dataTitanic$age, na.rm = T)

# Pour mesurer l'association entre le fait d'être une femme, et le fait d'avoir survécu au naufrage.
cor(dataTitanic$femme, dataTitanic$survie, method = "kendall")

# On y voit en effet une association positive
# On utilise le tau de kendall, puisque nos variables ici sont catégoriques.
# Voir à ce propos les pages 57 à 60 du livre de Vincent Arel.

# On joue avec une nouvelle bd
dataMovie <- readRDS("/Users/adriencloutier/Library/CloudStorage/Dropbox/Etudes/Universite_Laval/Doctorat/_SharedFolder_trousseEtudianteDoctoratAdri/Conferences/FSS_2024/atelier_fss_2024_03_15/data/movieData.rds")

summary(dataMovie)
summary(dataMovie$runtimeMinutes)

head(dataMovie)
str(dataMovie)

table(dataMovie$Adventure)
table(dataMovie$original_language)

mean(dataMovie$budget)

dataMovie[3, 6]

#### Exemple de la visualisation des données ####

hist(dataMovie$year)

library(ggplot2) # mais déjà dans Tidyverse!
library(ggthemes) # N'oubliez pas de l'installer d'abord avec la fonction install.packages()!

ggplot(dataMovie, aes(x = year)) +
  geom_bar(stat = "count", fill = "blue") +
  xlab("Année de production") +
  ylab("Fréquence de films") +
  ggtitle("La production des films de 1915 à 2017") +
  theme_fivethirtyeight()

# Histogramme
ggplot(dataMovie, aes(x = runtimeMinutes)) +
  geom_histogram()

# Bars
ggplot(dataMovie, aes(x = runtimeMinutes, y = ..count..)) +
  geom_bar()

# Moustaches
ggplot(dataMovie, aes(x = runtimeMinutes)) +
  geom_boxplot()

# Densité
ggplot(dataMovie, aes(x = runtimeMinutes)) +
  geom_density()

# Points
ggplot(dataMovie, aes(x = year, y = revenue)) +
  geom_point()

# Jitter + alpha
ggplot(dataMovie, aes( x = year, y = revenue)) +
  geom_jitter(alpha=0.5)

# Ligne
GraphData <- as.data.frame(table(dataMovie$year))
names(GraphData) <- c("year", "freq")

ggplot(GraphData, aes(x = year, y = freq, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#smooth
ggplot(GraphData, aes(x = year, y = freq, group = 1)) +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# Principe de couche
ggplot(GraphData, aes(x = year, y = freq, group = 1)) +
  geom_point(alpha = 0.5) + #couche 1 (points)
  geom_line() +           #couche 2 (ligne noire)
  geom_smooth() +         #couche 3 (ligne bleue)
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#Comparons la distribution des films Familiaux et Fantastiques
dataMovie$subTypes <- NA
dataMovie$subTypes[dataMovie$Family == 1] <- "Films familiaux"
dataMovie$subTypes[dataMovie$Fantasy == 1] <- "Films fantastiques"

GraphData2 <- na.omit(dataMovie)

ggplot(GraphData2, aes(x = revenue, group = subTypes, fill = subTypes, color = subTypes)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean(dataMovie$revenue[dataMovie$Family == 1]), color = "#2166ac") + #la moyenne pour family
  geom_vline(xintercept = mean(dataMovie$revenue[dataMovie$Fantasy == 1]), color = "#b2182b") + #la moyenne pour Fantasy
  scale_color_manual(values = c("#2166ac", "#b2182b")) + #spécification des couleurs en hex code
  scale_fill_manual( values = c("#2166ac", "#b2182b")) + #spécification des couleurs en hex code
  scale_y_continuous("Densité\n",
                     expand=c(0,0)) +
  scale_x_continuous("\nRevenue au box office (million de USD)",
                     labels = seq(0,500,50),
                     breaks = seq(0,500000000,50000000),
                     limits = c(0,500000000),
                     expand = c(0,0)) +
  labs(title = "Distribution du revenu des fdfsdfsfs") + #ajout du titre
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(hjust = 1),
        axis.text.y = element_text(angle = 90,hjust = 0))

ggsave("/Users/adriencloutier/Library/CloudStorage/Dropbox/Etudes/Universite_Laval/Doctorat/_SharedFolder_trousseEtudianteDoctoratAdri/Conferences/FSS_2024/atelier_fss_2024_03_15/graphs/graphAdrien.png",
       width = 8, height = 4.5)



### dplyr

library(tidyverse) # mais déjà dans Tidyverse!

## select()

## filter()

## arrange()

## mutate()

## summarize()

## group_by()

dataMovie2 <- dataMovie %>%
  dplyr::filter(original_language == "en") %>%
  select(year, originalTitle, runtimeMinutes) %>%
  arrange(-year) %>%
  group_by(year) %>%
  mutate(mean = mean(runtimeMinutes, na.rm = T))


ggplot(data = dataMovie2, aes(x = year, y = mean)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


##########################################
### Nouvelle base de données
##########################################

dataMovie <- readRDS("")

####  Analyse - Description ####
#### test-t ####
#Est-ce que les films romantiques ont systématiquement plus de revenus que les films d'action?
names(dataMovie)

cashRomance <- dataMovie$revenue[dataMovie$Romance==1]
cashAction <- dataMovie$revenue[dataMovie$Action==1]
t.test(x=cashRomance,
       y=cashAction,
       conf.level=0.95)

# Réponse:

####  Régression ####

#Pour qu'il y ait une différence, on veut un grand |t|

# Et des p-value très petit
# p < 0.001 => ***
# p < 0.01  => **
# p < 0.05  => *

#Est ce que les films plus longs sont plus chers?!
modelA <- lm(averageRating ~ budget,
             data = dataMovie)

summary(modelA)





