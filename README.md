# Guide d'utilisation du projet d'analyse de sentiment et de régression

Ce guide détaillé vous accompagne à travers les différentes étapes nécessaires à l'utilisation de ce projet GitHub, conçu pour l'analyse de sentiment et l'analyse de régression sur des données textuelles. Ce README est destiné aux débutants et explique chaque étape de manière exhaustive, depuis la préparation des données jusqu'à la visualisation des résultats.
Prérequis

Avant de commencer, assurez-vous d'avoir les éléments suivants :

    Une installation fonctionnelle de R et RStudio.

    Les packages R suivants installés : dplyr, tidytext, quanteda, ggplot2, et modelsummary. Vous pouvez les installer en exécutant les commandes suivantes dans RStudio :

    R

    install.packages("dplyr")
    install.packages("tidytext")
    install.packages("quanteda")
    install.packages("ggplot2")
    install.packages("modelsummary")

Structure du projet

Le dépôt GitHub contient les dossiers et fichiers suivants :

    data/ : Dossier pour stocker les jeux de données et les résultats intermédiaires.
    scripts/ : Contient tous les scripts R nécessaires à l'exécution du projet.
    graphs/ : Dossier où les graphiques générés seront sauvegardés.

Assurez-vous de maintenir cette structure pour le bon fonctionnement des scripts.
Étape 1 : Préparation des données

Le premier script, preparation_donnees.R, prépare les données pour l'analyse. Il effectue un nettoyage et une structuration du texte.

    Placez votre jeu de données initial dans le dossier data/ sous le nom data.rds.
    Ouvrez et exécutez le script scripts/preparation_donnees.R dans RStudio. Ce script traitera votre texte pour l'analyse sentimentale, y compris le nettoyage et la division en phrases.
    Une fois le script exécuté, un fichier data_prepped.rds sera créé dans le dossier data/, contenant les données préparées.

Étape 2 : Analyse de sentiment avec le Lexicoder

Le script analyse_sentiment_lexicoder.R utilise le Lexicoder Sentiment Dictionary pour évaluer le sentiment du texte.

    Assurez-vous que le fichier data_prepped.rds est présent dans le dossier data/.
    Exécutez le script scripts/analyse_sentiment_lexicoder.R. Il lira les données préparées, les analysera à l'aide du Lexicoder, et calculera les scores de sentiment.
    Le script sauvegardera les résultats de l'analyse sentimentale dans un nouveau fichier RDS, data_analyse_textuelle.rds, dans le dossier data/.

Étape 3 : Régressions

Le script regressions.R effectue des analyses de régression pour étudier les relations entre le ton des textes et d'autres variables.

    Ouvrez et exécutez scripts/regressions.R. Ce script nécessite le fichier data_analyse_textuelle.rds généré à l'étape précédente.
    Le script exécutera plusieurs modèles de régression et sauvegardera les tableaux de résultats sous forme de fichiers LaTeX dans le dossier data/reg_table/.

Étape 4 : Génération de graphiques

Enfin, les scripts graphique_series_temporelles.R et graphique_tone_by_country.R génèrent des visualisations des analyses.

    Pour chaque script de graphique dans le dossier scripts/, ouvrez-le et exécutez-le dans RStudio.
    Les scripts liront les données analysées et généreront des graphiques, qui seront sauvegardés dans le dossier graphs/.

Conclusion

Félicitations ! Vous avez maintenant exécuté l'ensemble du processus d'analyse de sentiment et de régression. Explorez le dossier graphs/ pour visualiser les résultats et le dossier data/reg_table/ pour les tableaux de régression. N'hésitez pas à modifier les scripts et à expérimenter avec vos propres données pour explorer de nouvelles analyses.

Si vous rencontrez des difficultés ou avez des questions, consultez d'abord la documentation des packages R utilisés. Pour des problèmes spécifiques au projet, n'hésitez pas à ouvrir une issue dans le dépôt GitHub.