# Guide d'utilisation de cet atelier

Ce guide détaillé vous accompagne à travers les différentes étapes nécessaires à l'utilisation de ce projet GitHub, conçu pour deux exercices d'analyse de données en R. Ce README est destiné aux débutants et explique chaque étape de manière exhaustive, depuis la préparation des données jusqu'à la visualisation des résultats.

## Table des matières
- [Guide d'utilisation de cet atelier](#guide-dutilisation-de-cet-atelier)
- [Prérequis](#prérequis)
- [Téléchargement et Ouverture du Projet](#téléchargement-et-ouverture-du-projet)
- [Structure du projet](#structure-du-projet)
- [Exercices: Utilisation de Quarto](#exercices-utilisation-de-quarto)
- [Difficultés? Problèmes?](#difficultés-problèmes)

## Prérequis

Avant de commencer, assurez-vous d'avoir les éléments suivants :

- Une installation fonctionnelle de R et RStudio.

- Les packages R suivants installés : `dplyr`, `tidytext`, `quanteda`, `ggplot2`, `modelsummary`, `factoextra`, `cluster.datasets`, `ggrepel` et `tidytext`.
  - Pour vérifier si un package est installé, exécutez `library(nom_du_package)`. Si une erreur apparaît, cela signifie que le package doit être installé.
  - Vous pouvez les installer en exécutant les commandes suivantes dans RStudio :
``` r
    install.packages("dplyr")
    install.packages("quanteda")
    install.packages("ggplot2")
    install.packages("modelsummary")
    install.packages("factoextra")
    install.packages("cluster.datasets") ## Ce package contient des jeux de données pertinents pour faire du clustering
    install.packages("ggrepel") ## Pour gérer les labels qui se chevauchent dans les graphiques
    install.packages("tidytext") ## Pour ordonner le texte sur les axes des graphiques
```

- Installation de Quarto (outil pour créer des documents dynamiques; détails dans la section [Exercices: Utilisation de Quarto](#exercices-utilisation-de-quarto)). Voici les instructions pour installer Quarto:
  
  1. Dans un terminal, vérifiez si Quarto est installé en exécutant la commande suivante :
  ```bash
  quarto --version
  ```
  2. Si Quarto n'est pas installé, visitez le [site officiel de Quarto](https://quarto.org/docs/get-started/) pour télécharger et installer l'outil.

## Téléchargement et Ouverture du Projet

Pour télécharger et commencer à travailler sur ce projet dans RStudio, suivez ces étapes :

1.  **Télécharger le projet** : Rendez-vous sur la page GitHub du projet et cliquez sur le bouton `Code`, puis sélectionnez `Download ZIP`. Cela téléchargera une archive du projet sur votre ordinateur.
2.  **Extraire le projet** : Localisez le fichier ZIP téléchargé et extrayez-le dans le dossier de votre choix.
3.  **Ouvrir le projet dans RStudio** : Ouvrez RStudio, puis allez dans File \> Open Project... et naviguez jusqu'au dossier extrait. Sélectionnez le fichier `atelier_fss.Rproj` pour ouvrir le projet. Vous pouvez aussi simplement double-cliquer sur le fichier .Rproj pour ouvrir le projet directement dans RStudio.

### Pour les Plus Aventuriers : Clonage du Projet (Facultatif)

> **Note :** Cette section est facultative et destinée aux utilisateurs familiers avec Git et GitHub. Si vous ne souhaitez pas utiliser Git, vous pouvez ignorer cette section et suivre les étapes de téléchargement classiques.

Si vous êtes familier avec Git et GitHub, vous pouvez également cloner le projet directement sur votre ordinateur. Voici comment procéder :

1. **Cloner le projet** : Ouvrez Git Bash ou un terminal de votre choix, naviguez jusqu'au dossier où vous voulez cloner le projet avec la commande `cd` et exécutez la commande suivante pour cloner le projet sur votre ordinateur :

    ```bash
    git clone https://github.com/clessn/atelier_fss.git
    ```

2. **Ouvrir le projet dans RStudio** : Ouvrez RStudio, puis allez dans File \> Open Project... et naviguez jusqu'au dossier cloné. Sélectionnez le fichier `.Rproj` pour ouvrir le projet. Vous pouvez aussi simplement double-cliquer sur le fichier `.Rproj` pour ouvrir le projet directement dans RStudio.


## Structure du projet

Le dépôt GitHub contient les dossiers et fichiers suivants :

`exercice1_analyse_textuelle/` : Dossier qui contient les scripts pour exécuter le premier exercice d'analyse textuelle.

`exercice2_pca_clustering/` : Dossier qui contient les scripts pour exécuter le deuxième exercice de PCA et de clustering.

`data/` : Dossier pour stocker les jeux de données et les résultats intermédiaires.

`code/` : Contient des scripts R nécessaires à l'exécution des exercices.

Assurez-vous de maintenir cette structure pour le bon fonctionnement des scripts.

## Exercices: Utilisation de Quarto

Pour nos deux exercices, nous utilisons **Quarto**, un outil open-source permettant de créer des documents dynamiques, des rapports et des présentations à partir de fichiers `.qmd` (Quarto Markdown). Quarto est particulièrement utile pour combiner du texte, du code et des visualisations dans un même document, facilitant ainsi le partage et la reproductibilité des analyses de données.

Pour chaque exercice, vous pouvez ouvrir les fichiers `.qmd` et suivre les instructions. Les fichiers sont numérotés pour indiquer l'ordre dans lequel ils doivent être exécutés. En exécutant chaque fichier, vous obtiendrez un rapport contenant les analyses et visualisations correspondant à l'exercice.

## Difficultés? Problèmes?

Si vous rencontrez des difficultés ou avez des questions, consultez d'abord la documentation des packages R utilisés. Pour des problèmes spécifiques au projet, n'hésitez pas à ouvrir une issue dans le dépôt GitHub.
