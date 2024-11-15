# Guide d'utilisation du projet d'analyse de sentiment et de régression

Ce guide détaillé vous accompagne à travers les différentes étapes nécessaires à l'utilisation de ce projet GitHub, conçu pour l'analyse de sentiment et l'analyse de régression sur des données textuelles. Ce README est destiné aux débutants et explique chaque étape de manière exhaustive, depuis la préparation des données jusqu'à la visualisation des résultats.
Prérequis

Avant de commencer, assurez-vous d'avoir les éléments suivants :

- Une installation fonctionnelle de R et RStudio.

- Les packages R suivants installés : dplyr, tidytext, quanteda, ggplot2, et modelsummary. Vous pouvez les installer en exécutant les commandes suivantes dans RStudio :

```R
    install.packages("dplyr")
    install.packages("tidytext")
    install.packages("quanteda")
    install.packages("ggplot2")
    install.packages("modelsummary")
    install.packages("factoextra")
    install.packages("cluster.datasets") ## Ce package contient des jeux de données pertinents pour faire du clustering
    install.packages("ggrepel") ## Pour gérer les labels qui se chevauchent dans les graphiques
    install.packages("tidytext") ## Pour ordonner le texte sur les axes des graphiques
```

## Téléchargement et Ouverture du Projet

Pour télécharger et commencer à travailler sur ce projet dans RStudio, suivez ces étapes :

1. Télécharger le projet : Rendez-vous sur la page GitHub du projet et cliquez sur le bouton Code, puis sélectionnez Download ZIP. Cela téléchargera une archive du projet sur votre ordinateur.
2. Extraire le projet : Localisez le fichier ZIP téléchargé et extrayez-le dans le dossier de votre choix.
3. Ouvrir le projet dans RStudio : Ouvrez RStudio, puis allez dans File > Open Project... et naviguez jusqu'au dossier extrait. Sélectionnez le fichier .Rproj pour ouvrir le projet. Vous pouvez aussi simplement double-cliquer sur le fichier .Rproj pour ouvrir le projet directement dans RStudio.

### Pour les plus aventuriers : Clonage du Projet

Si vous êtes familier avec Git et GitHub, vous pouvez également cloner le projet directement sur votre ordinateur. Voici comment procéder :

1. Cloner le projet : Ouvrez Git Bash ou un terminal de votre choix et exécutez la commande suivante pour cloner le projet sur votre ordinateur :

```bash
git clone https://github.com/clessn/analyse_textuelle_atelier_fss.git
```

2. Ouvrir le projet dans RStudio : Ouvrez RStudio, puis allez dans File > Open Project... et naviguez jusqu'au dossier cloné. Sélectionnez le fichier .Rproj pour ouvrir le projet. Vous pouvez aussi simplement double-cliquer sur le fichier .Rproj pour ouvrir le projet directement dans RStudio.

## Structure du projet

Le dépôt GitHub contient les dossiers et fichiers suivants :

`data/` : Dossier pour stocker les jeux de données, les résultats intermédiaires et les graphiques.


`code/` : Contient tous les scripts R nécessaires à l'exécution du projet.

Assurez-vous de maintenir cette structure pour le bon fonctionnement des scripts.

## Étape 1 : Préparation des données

Le premier script, `code/lsd/lsd_prep.R`, prépare les données pour l'analyse. Il effectue un nettoyage et une structuration du texte.

Ouvrez et exécutez le script `code/lsd/lsd_prep.R` dans RStudio. Ce script traitera votre texte pour l'analyse sentimentale, y compris le nettoyage et la division en phrases.

Une fois le script exécuté, un fichier data_prepped.rds sera créé dans le dossier data/, contenant les données préparées.

## Étape 2 : Analyse de sentiment avec le Lexicoder

Le script `code/lsd/lsd_analysis.R` utilise le Lexicoder Sentiment Dictionary pour évaluer le sentiment du texte.

Assurez-vous que le fichier data_prepped.rds est présent dans le dossier data/.
Exécutez le script `code/lsd/lsd_analysis.R`. Il lira les données préparées, les analysera à l'aide du Lexicoder, et calculera les scores de sentiment.
Le script sauvegardera les résultats de l'analyse sentimentale dans un nouveau fichier RDS, data_analyse_textuelle.rds, dans le dossier data/.

## Étape 3 : Régressions

Le script regressions.R effectue des analyses de régression pour étudier les relations entre le ton des textes et d'autres variables.

Ouvrez et exécutez `code/regression/regressions.R`. Ce script nécessite le fichier data_analyse_textuelle.rds généré à l'étape précédente.
Le script exécutera plusieurs modèles de régression et sauvegardera les tableaux de résultats sous forme de fichiers LaTeX dans le dossier data/reg_table/.

## Étape 4 : Génération de graphiques

Enfin, les scripts `code/graphs/bar_graph_country_tone_index.R` et `code/graphs/time_series.R` génèrent des visualisations des analyses.

Pour chaque script de graphique dans le dossier scripts/, ouvrez-le et exécutez-le dans RStudio.
Les scripts liront les données analysées et généreront des graphiques, qui seront sauvegardés dans le dossier `data/graphs/`.

## Conclusion

Félicitations ! Vous avez maintenant exécuté l'ensemble du processus d'analyse de sentiment et de régression. Explorez le dossier graphs/ pour visualiser les résultats et le dossier data/reg_table/ pour les tableaux de régression. N'hésitez pas à modifier les scripts et à expérimenter avec vos propres données pour explorer de nouvelles analyses.

Si vous rencontrez des difficultés ou avez des questions, consultez d'abord la documentation des packages R utilisés. Pour des problèmes spécifiques au projet, n'hésitez pas à ouvrir une issue dans le dépôt GitHub.
