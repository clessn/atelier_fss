library(striprtf)
library(tidyverse)
library(beepr)

# ------------------------- The Sun --------------------------------------------

clean_sun_rtf <- function(path_to_folder, file_name, country) {

  #Décommenter ces lignes juste pour tester l'intérieur de la fonction
  path_to_folder <- "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Sun-Syrie"
  file_name <- "UK-The Sun-Syrie-2.rtf"
  country = "Syrie"

  path <- file.path(path_to_folder, file_name)

  data <- read_rtf(path)

  df_data <- as.data.frame(data) %>%
    slice(-1)

  df_data2 <- df_data %>%
    mutate(variable =
             ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                                grep("[0-9] f$", data, value=T),
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
                                grep("[0-9] d$", data, value=T),
                                grep("décembre [1-9]", data, value=T)), "date",
                    ifelse(grepl("mots", data), "words",
                           ifelse(data %in% c(grep("^t [1-9]", data, value=T),
                                              grep("^vrier [1-9]", data, value=T),
                                              grep("^cembre [1-9]", data, value=T)), "year",
                                  ifelse(grepl("Document [A-Z]+", data), "doc_id",
                                         "text")))),
           id =
             ifelse(variable == "doc_id", 1, 0))

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

  opinions <- df_data2 %>%
    mutate(opinion = ifelse(data %in% c(grep("[O]pinion", data, value=T),
                                        grep("[O]pinions", data, value=T),
                                        grep("[E]ditorial", data, value=T),
                                        grep("[E]ditorials", data, value=T),
                                        grep("[C]omment", data, value=T),
                                        grep("[C]omments", data, value=T),
                                        grep("[O]p-Ed", data, value=T),
                                        grep("[O]p-Eds", data, value=T),
                                        grep("[C]olumn", data, value=T),
                                        grep("[C]olumns", data, value=T)), 1, 0)) %>%
    group_by(article_id) %>%
    summarise(opinion = max(opinion))

  library(chron)
  # Définir le format de date en français
  Sys.setlocale("LC_TIME", "fr_FR")
  # Utiliser ifelse() pour vérifier si la colonne "year" existe et la créer si elle n'existe pas
  df_data3 <- df_data2 %>%
    select(-id) %>%
    filter(data != "") %>%
    pivot_wider(names_from = variable, values_from = data,
                id_cols = "article_id") %>%
    # On ajoute l'indication des opinions
    left_join(opinions, by = "article_id") %>%
    mutate(source = "The Sun",
           country = country)

  # Vérifier si la colonne "year" existe et la créer si elle n'existe pas
  if (!"year" %in% colnames(df_data3)) {
    df_data3 <- df_data3 %>%
      mutate(year = NA)
  }

  # Modifier la colonne "year" en utilisant les gsub() que vous avez déjà définis
  df_data4 <- df_data3 %>%
    mutate(year = gsub("cembre ([1-9])", "écembre \\1", year),
           year = gsub("t ([1-9])", "ût \\1", year),
           year = gsub("vrier ([1-9])", "évrier \\1", year),
           year = gsub("NULL", "", year)) %>%
    # Unir la colonne "date" et la colonne "year" en une seule colonne "date"
    unite(date, date, year, sep = "") %>%
    select(doc_id, text, date, source, country, opinion) %>%
    rowwise() %>%
    mutate(doc_id = gsub("Document", "", doc_id),
           date = as.Date(date, "%d %B %Y"))

  texts <- c()
  for(i in 1:nrow(df_data4)){
    texts[i] <- paste0(eval(parse(text = df_data4$text[i])), collapse = " ")
    print(i)
  }

  df_data4$text <- texts

  return(df_data4)

}

########################################################################################################### ##
###################################################### The Star ##############################################
########################################################################################################### ##

clean_star_rtf <- function(path_to_folder, file_name, country) {

  # Décommenter ces lignes juste pour tester l'intérieur de la fonction
  #  path_to_folder <- "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Toronto Star-Syrie"
  #  file_name <- "CA-Toronto Star-Syrie-1.rtf"
  #  country = "Syrie"


  path <- file.path(path_to_folder, file_name)

  data <- read_rtf(path)

  df_data <- as.data.frame(data) %>%
    slice(-1)

  df_data2 <- df_data %>%
    mutate(variable =
             ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                                grep("[0-9] f$", data, value=T),
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
                                grep("[0-9] d$", data, value=T),
                                grep("décembre [1-9]", data, value=T)), "date",
                    ifelse(grepl("mots", data), "words",
                           ifelse(data %in% c(grep("^t [1-9]", data, value=T),
                                              grep("^vrier [1-9]", data, value=T),
                                              grep("^cembre [1-9]", data, value=T)), "year",
                                  ifelse(grepl("Document [A-Z]+", data), "doc_id",
                                         "text")))),
           id =
             ifelse(variable == "doc_id", 1, 0))

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

  opinions <- df_data2 %>%
    mutate(opinion = ifelse(data %in% c(grep("[O]pinion", data, value=T),
                                        grep("[O]pinions", data, value=T),
                                        grep("[E]ditorial", data, value=T),
                                        grep("[E]ditorials", data, value=T),
                                        grep("[C]omment", data, value=T),
                                        grep("[C]omments", data, value=T),
                                        grep("[O]p-Ed", data, value=T),
                                        grep("[O]p-Eds", data, value=T),
                                        grep("[C]olumn", data, value=T),
                                        grep("[C]olumns", data, value=T)), 1, 0)) %>%
    group_by(article_id) %>%
    summarise(opinion = max(opinion))


  library(chron)
  # Définir le format de date en français
  Sys.setlocale("LC_TIME", "fr_FR")
  # Utiliser ifelse() pour vérifier si la colonne "year" existe et la créer si elle n'existe pas
  df_data3 <- df_data2 %>%
    select(-id) %>%
    filter(data != "") %>%
    pivot_wider(names_from = variable, values_from = data,
                id_cols = "article_id") %>%
    # On ajoute l'indication des opinions
    left_join(opinions, by = "article_id") %>%
    mutate(source = "Toronto Star",
           country = country)

  # Vérifier si la colonne "year" existe et la créer si elle n'existe pas
  if (!"year" %in% colnames(df_data3)) {
    df_data3 <- df_data3 %>%
      mutate(year = NA)
  }

  # Modifier la colonne "year" en utilisant les gsub() que vous avez déjà définis
  df_data4 <- df_data3 %>%
    mutate(year = gsub("cembre ([1-9])", "écembre \\1", year),
           year = gsub("t ([1-9])", "ût \\1", year),
           year = gsub("vrier ([1-9])", "évrier \\1", year),
           year = gsub("NULL", "", year)) %>%
    # Unir la colonne "date" et la colonne "year" en une seule colonne "date"
    unite(date, date, year, sep = "") %>%
    select(doc_id, text, date, source, country, opinion) %>%
    rowwise() %>%
    mutate(doc_id = gsub("Document", "", doc_id),
           date = as.Date(date, "%d %B %Y"))

  texts <- c()
  for(i in 1:nrow(df_data4)){
    texts[i] <- paste0(eval(parse(text = df_data4$text[i])), collapse = " ")
    print(i)
  }

  df_data4$text <- texts

  return(df_data4)

}

########################################################################################################### ##
####################################################### The Globe ############################################
########################################################################################################### ##

clean_globe_rtf <- function(path_to_folder, file_name, country) {

  # Décommenter ces lignes juste pour tester l'intérieur de la fonction
  # path_to_folder <- "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Globe&Mail-Syrie"
  # file_name <- "CA-Globe&Mail-Syrie-1.rtf"
  # country = "Syrie"

  path <- file.path(path_to_folder, file_name)

  data <- read_rtf(path)

  df_data <- as.data.frame(data) %>%
    slice(-1)

  df_data2 <- df_data %>%
    mutate(variable =
             ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                                grep("[0-9] f$", data, value=T),
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
                                grep("[0-9] d$", data, value=T),
                                grep("décembre [1-9]", data, value=T)), "date",
                    ifelse(grepl("mots", data), "words",
                           ifelse(data %in% c(grep("^t [1-9]", data, value=T),
                                              grep("^vrier [1-9]", data, value=T),
                                              grep("^cembre [1-9]", data, value=T)), "year",
                                  ifelse(grepl("Document [A-Z]+", data), "doc_id",
                                         "text")))),
           id =
             ifelse(variable == "doc_id", 1, 0))

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

  opinions <- df_data2 %>%
    mutate(opinion = ifelse(data %in% c(grep("[O]pinion", data, value=T),
                                        grep("[O]pinions", data, value=T),
                                        grep("[E]ditorial", data, value=T),
                                        grep("[E]ditorials", data, value=T),
                                        grep("[C]omment", data, value=T),
                                        grep("[C]omments", data, value=T),
                                        grep("[O]p-Ed", data, value=T),
                                        grep("[O]p-Eds", data, value=T),
                                        grep("[C]olumn", data, value=T),
                                        grep("[C]olumns", data, value=T)), 1, 0)) %>%
    group_by(article_id) %>%
    summarise(opinion = max(opinion))

  library(chron)
  # Définir le format de date en français
  Sys.setlocale("LC_TIME", "fr_FR")
  # Utiliser ifelse() pour vérifier si la colonne "year" existe et la créer si elle n'existe pas
  df_data3 <- df_data2 %>%
    select(-id) %>%
    filter(data != "") %>%
    pivot_wider(names_from = variable, values_from = data,
                id_cols = "article_id") %>%
    # On ajoute l'indication des opinions
    left_join(opinions, by = "article_id") %>%
    mutate(source = "The Globe and Mail",
           country = country)

  # Vérifier si la colonne "year" existe et la créer si elle n'existe pas
  if (!"year" %in% colnames(df_data3)) {
    df_data3 <- df_data3 %>%
      mutate(year = NA)
  }

  # Modifier la colonne "year" en utilisant les gsub() que vous avez déjà définis
  df_data4 <- df_data3 %>%
    mutate(year = gsub("cembre ([1-9])", "écembre \\1", year),
           year = gsub("t ([1-9])", "ût \\1", year),
           year = gsub("vrier ([1-9])", "évrier \\1", year),
           year = gsub("NULL", "", year)) %>%
    # Unir la colonne "date" et la colonne "year" en une seule colonne "date"
    unite(date, date, year, sep = "") %>%
    select(doc_id, text, date, source, country, opinion) %>%
    rowwise() %>%
    mutate(doc_id = gsub("Document", "", doc_id),
           date = as.Date(date, "%d %B %Y"))

  texts <- c()
  for(i in 1:nrow(df_data4)){
    texts[i] <- paste0(eval(parse(text = df_data4$text[i])), collapse = " ")
    print(i)
  }

  df_data4$text <- texts

  return(df_data4)
}

########################################################################################################### ##
################################################### The Gardian ##############################################
########################################################################################################### ##

clean_gardian_rtf <- function(path_to_folder, file_name, country) {

  # Décommenter ces lignes juste pour tester l'intérieur de la fonction
  # path_to_folder <- "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-Guardian-Syrie"
  # file_name <- "UK-Guardian-Syrie-8.rtf"
  # country = "Syrie"

  path <- file.path(path_to_folder, file_name)

  data <- read_rtf(path)

  df_data <- as.data.frame(data) %>%
    slice(-1)

  df_data2 <- df_data %>%
    mutate(variable =
             ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                                grep("[0-9] f$", data, value=T),
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
                                grep("[0-9] d$", data, value=T),
                                grep("décembre [1-9]", data, value=T)), "date",
                    ifelse(grepl("mots", data), "words",
                           ifelse(data %in% c(grep("^t [1-9]", data, value=T),
                                              grep("^vrier [1-9]", data, value=T),
                                              grep("^cembre [1-9]", data, value=T)), "year",
                                  ifelse(grepl("Document [A-Z]+", data), "doc_id",
                                         "text")))),
           id =
             ifelse(variable == "doc_id", 1, 0))

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

  opinions <- df_data2 %>%
    mutate(opinion = ifelse(data %in% c(grep("[O]pinion", data, value=T),
                                        grep("[O]pinions", data, value=T),
                                        grep("[E]ditorial", data, value=T),
                                        grep("[E]ditorials", data, value=T),
                                        grep("[C]omment", data, value=T),
                                        grep("[C]omments", data, value=T),
                                        grep("[O]p-Ed", data, value=T),
                                        grep("[O]p-Eds", data, value=T),
                                        grep("[C]olumn", data, value=T),
                                        grep("[C]olumns", data, value=T)), 1, 0)) %>%
    group_by(article_id) %>%
    summarise(opinion = max(opinion))

  library(chron)
  # Définir le format de date en français
  Sys.setlocale("LC_TIME", "fr_FR")
  # Utiliser ifelse() pour vérifier si la colonne "year" existe et la créer si elle n'existe pas
  df_data3 <- df_data2 %>%
    select(-id) %>%
    filter(data != "") %>%
    pivot_wider(names_from = variable, values_from = data,
                id_cols = "article_id") %>%
    # On ajoute l'indication des opinions
    left_join(opinions, by = "article_id") %>%
    mutate(source = "The Gardian",
           country = country)

    # Vérifier si la colonne "year" existe et la créer si elle n'existe pas
    if (!"year" %in% colnames(df_data3)) {
      df_data3 <- df_data3 %>%
        mutate(year = NA)
    }

    # Modifier la colonne "year" en utilisant les gsub() que vous avez déjà définis
  df_data4 <- df_data3 %>%
    mutate(year = gsub("cembre ([1-9])", "écembre \\1", year),
           year = gsub("t ([1-9])", "ût \\1", year),
           year = gsub("vrier ([1-9])", "évrier \\1", year),
           year = gsub("NULL", "", year)) %>%
    # Unir la colonne "date" et la colonne "year" en une seule colonne "date"
    unite(date, date, year, sep = "") %>%
    select(doc_id, text, date, source, country, opinion) %>%
    rowwise() %>%
    mutate(doc_id = gsub("Document", "", doc_id),
           date = as.Date(date, "%d %B %Y"))

  texts <- c()
  for(i in 1:nrow(df_data4)){
    texts[i] <- paste0(eval(parse(text = df_data4$text[i])), collapse = " ")
    print(i)
  }

  df_data4$text <- texts

  return(df_data4)

}

########################################################################################################### ##
####################################################### The NYT ##############################################
########################################################################################################### ##

clean_nyt_rtf <- function(path_to_folder, file_name, country) {

  # Décommenter ces lignes juste pour tester l'intérieur de la fonction
  # path_to_folder <- "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-NYTimes-Syrie"
   # file_name <- "US-NYTimes-Syrie-10.rtf"
    #country = "Syrie"

  path <- file.path(path_to_folder, file_name)

  data <- striprtf::read_rtf(path, encoding = "UTF-8")

  df_data <- as.data.frame(data) %>%
    slice(-1)

  df_data2 <- df_data %>%
    mutate(variable =
             ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                                grep("[0-9] f$", data, value=T),
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
                                grep("[0-9] d$", data, value=T),
                                grep("décembre [1-9]", data, value=T)), "date",
                    ifelse(grepl("mots", data), "words",
                           ifelse(data %in% c(grep("^t [1-9]", data, value=T),
                                              grep("^vrier [1-9]", data, value=T),
                                              grep("^cembre [1-9]", data, value=T)), "year",
                                  ifelse(grepl("Document [A-Z]+", data), "doc_id",
                                         "text")))),
           id =
             ifelse(variable == "doc_id", 1, 0))

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

  opinions <- df_data2 %>%
    mutate(opinion = ifelse(data %in% c(grep("[O]pinion", data, value=T),
                                        grep("[O]pinions", data, value=T),
                                        grep("[E]ditorial", data, value=T),
                                        grep("[E]ditorials", data, value=T),
                                        grep("[C]omment", data, value=T),
                                        grep("[C]omments", data, value=T),
                                        grep("[O]p-Ed", data, value=T),
                                        grep("[O]p-Eds", data, value=T),
                                        grep("[C]olumn", data, value=T),
                                        grep("[C]olumns", data, value=T)), 1, 0)) %>%
    group_by(article_id) %>%
    summarise(opinion = max(opinion))

  library(chron)
  # Définir le format de date en français
  Sys.setlocale("LC_TIME", "fr_FR")
  # Utiliser ifelse() pour vérifier si la colonne "year" existe et la créer si elle n'existe pas
  df_data3 <- df_data2 %>%
    select(-id) %>%
    filter(data != "") %>%
    pivot_wider(names_from = variable, values_from = data,
                id_cols = "article_id") %>%
    # On ajoute l'indication des opinions
    left_join(opinions, by = "article_id") %>%
    mutate(source = "The New York Times",
           country = country)

  # Vérifier si la colonne "year" existe et la créer si elle n'existe pas
  if (!"year" %in% colnames(df_data3)) {
    df_data3 <- df_data3 %>%
      mutate(year = NA)
  }

  # Modifier la colonne "year" en utilisant les gsub() que vous avez déjà définis
  df_data4 <- df_data3 %>%
    mutate(year = gsub("cembre ([1-9])", "écembre \\1", year),
           year = gsub("t ([1-9])", "ût \\1", year),
           year = gsub("vrier ([1-9])", "évrier \\1", year),
           year = gsub("NULL", "", year)) %>%
    # Unir la colonne "date" et la colonne "year" en une seule colonne "date"
    unite(date, date, year, sep = "") %>%
    select(doc_id, text, date, source, country, opinion) %>%
    rowwise() %>%
    mutate(doc_id = gsub("Document", "", doc_id),
           date = as.Date(date, "%d %B %Y"))

  texts <- c()
  for(i in 1:nrow(df_data4)){
    texts[i] <- paste0(eval(parse(text = df_data4$text[i])), collapse = " ")
    print(i)
  }

  df_data4$text <- texts

  return(df_data4)

}

########################################################################################################### ##
#################################################### The UK Times ############################################
########################################################################################################### ##

clean_timesUK_rtf <- function(path_to_folder, file_name, country) {

  # Décommenter ces lignes juste pour tester l'intérieur de la fonction
  #  path_to_folder <- "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-The Times-Ukraine"
  # file_name <- "UK-The Times-Ukraine-1.rtf"
  # country = "Ukraine"

  path <- file.path(path_to_folder, file_name)

  data <- striprtf::read_rtf(path, encoding = "UTF-8")

  df_data <- as.data.frame(data) %>%
    slice(-1)

  df_data2 <- df_data %>%
    mutate(variable =
             ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                                grep("[0-9] f$", data, value=T),
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
                                grep("[0-9] d$", data, value=T),
                                grep("décembre [1-9]", data, value=T)), "date",
                    ifelse(grepl("mots", data), "words",
                           ifelse(data %in% c(grep("^t [1-9]", data, value=T),
                                              grep("^vrier [1-9]", data, value=T),
                                              grep("^cembre [1-9]", data, value=T)), "year",
                                  ifelse(grepl("Document [A-Z]+", data), "doc_id",
                                         "text")))),
           id =
             ifelse(variable == "doc_id", 1, 0))


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

  opinions <- df_data2 %>%
    mutate(opinion = ifelse(data %in% c(grep("[O]pinion", data, value=T),
                                        grep("[O]pinions", data, value=T),
                                        grep("[E]ditorial", data, value=T),
                                        grep("[E]ditorials", data, value=T),
                                        grep("[C]omment", data, value=T),
                                        grep("[C]omments", data, value=T),
                                        grep("[O]p-Ed", data, value=T),
                                        grep("[O]p-Eds", data, value=T),
                                        grep("[C]olumn", data, value=T),
                                        grep("[C]olumns", data, value=T)), 1, 0)) %>%
    group_by(article_id) %>%
    summarise(opinion = max(opinion))

  library(chron)
  # Définir le format de date en français
  Sys.setlocale("LC_TIME", "fr_FR")
  # Utiliser ifelse() pour vérifier si la colonne "year" existe et la créer si elle n'existe pas
  df_data3 <- df_data2 %>%
    select(-id) %>%
    filter(data != "") %>%
    pivot_wider(names_from = variable, values_from = data,
                id_cols = "article_id") %>%
    # On ajoute l'indication des opinions
    left_join(opinions, by = "article_id") %>%
    mutate(source = "The UK Times",
           country = country) %>%
    # Vérifier si les colonnes "year" et "opinion" existent et les créer si elles n'existent pas
    mutate(year = ifelse("year" %in% names(data), year, ""))

  # Vérifier si la colonne "year" existe et la créer si elle n'existe pas
  if (!"year" %in% colnames(df_data3)) {
    df_data3 <- df_data3 %>%
      mutate(year = NA)
  }

  # Modifier la colonne "year" en utilisant les gsub() que vous avez déjà définis
  df_data4 <- df_data3 %>%
    mutate(year = gsub("cembre ([1-9])", "écembre \\1", year),
           year = gsub("t ([1-9])", "ût \\1", year),
           year = gsub("vrier ([1-9])", "évrier \\1", year),
           year = gsub("NULL", "", year)) %>%
    # Unir la colonne "date" et la colonne "year" en une seule colonne "date"
    unite(date, date, year, sep = "") %>%
    select(doc_id, text, date, source, country, opinion) %>%
    rowwise() %>%
    mutate(doc_id = gsub("Document", "", doc_id),
           date = as.Date(date, "%d %B %Y"))

  texts <- c()
  for(i in 1:nrow(df_data4)){
    texts[i] <- paste0(eval(parse(text = df_data4$text[i])), collapse = " ")
    print(i)
  }

  df_data4$text <- texts

  return(df_data4)

}

########################################################################################################### ##
####################################################### Wash Post ############################################
########################################################################################################### ##

clean_WPost_rtf <- function(path_to_folder, file_name, country) {

  # Décommenter ces lignes juste pour tester l'intérieur de la fonction
  # path_to_folder <- "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-WaPo-Syrie"
  # file_name <- "US-WaPo-Syrie-12.rtf"
  # country = "Syria"

  path <- file.path(path_to_folder, file_name)

  data <- striprtf::read_rtf(path, encoding = "UTF-8")

  df_data <- as.data.frame(data) %>%
    slice(-1)

  df_data2 <- df_data %>%
    mutate(variable =
             ifelse(data %in% c(grep("janvier [1-9]", data, value=T),
                                grep("[0-9] f$", data, value=T),
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
                                grep("[0-9] d$", data, value=T),
                                grep("décembre [1-9]", data, value=T)), "date",
                    ifelse(grepl("mots", data), "words",
                           ifelse(data %in% c(grep("^t [1-9]", data, value=T),
                                              grep("^vrier [1-9]", data, value=T),
                                              grep("^cembre [1-9]", data, value=T)), "year",
                                  ifelse(grepl("Document [A-Z]+", data), "doc_id",
                                         "text")))),
           id =
             ifelse(variable == "doc_id", 1, 0))

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

  opinions <- df_data2 %>%decade. Philippe Corbé, a French journalist, put it in a nutshell when talking about the Ukrainian war, he said: ``We're not talking about Syrians fleeing the bombing by the Vladimir Putin-backed Syrian regime, we're talking about Europeans living in their cars that look like our cars, and just trying to save their lives''.

This study compares the treatment of Syrian and Ukrainian refugees in the six most read newspapers from Canada, the United States, and the United Kingdom. The sample contains over 25 000 articles %[ADRI?] 
collected from Factiva. First, the analysis uses a sentiment-based dictionary to analyze the nature and tone used in the articles. A tone index is calculated as the difference between the proportion of positive and negative words when speaking about Syrian or Ukrainian refugees. Second, the analysis uses an unsupervised topic modeling classification method, an algorithm built to identify topics in each journal article. This method helps to identify the issues addressed in these articles when referring to Syrian and Ukrainian migrants. These two methods combined allow for a systematic study of the sentiment conveyed by the news coverage of Syrian and Ukrainian refugees (positive or negative; hostile or empathetic). 

While differences exist, we argue in this paper that the empathy of Western media was indeed asymmetric. Syrian refugees were seen more negatively and through the prism of security, while Ukrainian refugees were seen more positively and through a humanitarian prism.

This paper thus fills an important gap in the literature by comparing and contrasting the media treatment of two distinct groups. It also tests a now widespread assumption about what is perceived as a more positive treatment of Ukrainian refugees. Understanding media empathy toward migrants is crucial since media, both directly and indirectly, shape public view of migrants, calls for action, and the broader policy agenda. Moreover, this study makes a methodological contribution by using Latent Dirichlet Allocation (LDA), a topic modeling method, to systematically assess the frames and the tone used to depict certain groups of migrants by the media. 

\section{Media, Refugees, and Empathy}
There is some perception that the international press treated Ukrainian refugees differently than other refugees. Early on, media and journalists appeared baffled by 
                                        grep("[C]omment", data, value=T),
                                        grep("[C]omments", data, value=T),
                                        grep("[O]p-Ed", data, value=T),
                                        grep("[O]p-Eds", data, value=T),
                                        grep("[C]olumn", data, value=T),
                                        grep("[C]olumns", data, value=T)), 1, 0)) %>%
    group_by(article_id) %>%
    summarise(opinion = max(opinion))

  library(chron)
  # Définir le format de date en français
  Sys.setlocale("LC_TIME", "fr_FR")
  # Utiliser ifelse() pour vérifier si la colonne "year" existe et la créer si elle n'existe pas
  df_data3 <- df_data2 %>%
    select(-id) %>%
    filter(data != "") %>%
    pivot_wider(names_from = variable, values_from = data,
                id_cols = "article_id") %>%
    # On ajoute l'indication des opinions
    left_join(opinions, by = "article_id") %>%
    mutate(source = "The Washington Post",
           country = country)


  # Vérifier si la colonne "year" existe et la créer si elle n'existe pas
  if (!"year" %in% colnames(df_data3)) {
    df_data3 <- df_data3 %>%
      mutate(year = NA)
  }

  # Modifier la colonne "year" en utilisant les gsub() que vous avez déjà définis
  df_data4 <- df_data3 %>%
    mutate(year = gsub("cembre ([1-9])", "écembre \\1", year),
           year = gsub("t ([1-9])", "ût \\1", year),
           year = gsub("vrier ([1-9])", "évrier \\1", year),
           year = gsub("NULL", "", year)) %>%
    # Unir la colonne "date" et la colonne "year" en une seule colonne "date"
    unite(date, date, year, sep = "") %>%
    select(doc_id, text, date, source, country, opinion) %>%
    rowwise() %>%
    mutate(doc_id = gsub("Document", "", doc_id),
           date = as.Date(date, "%d %B %Y"))

  texts <- c()
  for(i in 1:nrow(df_data4)){
    texts[i] <- paste0(eval(parse(text = df_data4$text[i])), collapse = " ")
    print(i)
  }

  df_data4$text <- texts

  return(df_data4)

}

########################################################################################################### ##
######################################################## Loop ################################################
########################################################################################################### ##

# # The Sun - Syrie ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Sun-Syrie")
# for (file_name in file_names) {
#   dfi <- clean_sun_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Sun-Syrie",
#                        file_name =  file_name,
#                        country =  "Syrie")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }
#
# # The Sun - Ukraine ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-TheSun-Ukraine")
# for (file_name in file_names) {
#   dfi <- clean_sun_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-TheSun-Ukraine",
#                        file_name = file_name,
#                        country = "Ukraine")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }

# The Sun - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-The Sun-Iraq")
for (file_name in file_names) {
  dfi <- clean_sun_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-The Sun-Iraq",
                       file_name = file_name,
                       country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(file_name)
}

# # Globe - Syrie ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Globe&Mail-Syrie")
# for (file_name in file_names) {
#   dfi <- clean_globe_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Globe&Mail-Syrie",
#                         file_name = file_name,
#                         country = "Syrie")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }
#
# # Globe - Ukraine ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/CA-Globe&Mail-Ukraine")
# for (file_name in file_names) {
#   dfi <- clean_globe_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/CA-Globe&Mail-Ukraine",
#                          file_name = file_name,
#                          country = "Ukraine")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }

# Globe - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/CA-Globe&Mail-Iraq")
for (file_name in file_names) {
  dfi <- clean_globe_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/CA-Globe&Mail-Iraq",
                         file_name = file_name,
                         country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(file_name)
}

# # Gardian - Syrie ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-Guardian-Syrie")
# for (file_name in file_names) {
#   dfi <- clean_gardian_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-Guardian-Syrie",
#                          file_name = file_name,
#                          country = "Syrie")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }
#
# # Gardian - Ukraine ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-Guardian-Ukraine")
# for (file_name in file_names) {
#   dfi <- clean_gardian_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-Guardian-Ukraine",
#                          file_name = file_name,
#                          country = "Ukraine")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }

# Gardian - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-Guardian-Iraq")
for (file_name in file_names) {
  dfi <- clean_gardian_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-Guardian-Iraq",
                           file_name = file_name,
                           country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(file_name)
}
#
# # Star - Syrie ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Toronto Star-Syrie")
# for (file_name in file_names) {
#   dfi <- clean_star_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Toronto Star-Syrie",
#                          file_name = file_name,
#                          country = "Syrie")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }

# # Star - Ukraine ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/CA-Toronto Star-Ukraine")
# for (file_name in file_names) {
#   dfi <- clean_star_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/CA-Toronto Star-Ukraine",
#                          file_name = file_name,
#                          country = "Ukraine")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }

# Star - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/CA-Toronto Star-Iraq")
for (file_name in file_names) {
  dfi <- clean_star_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/CA-Toronto Star-Iraq",
                        file_name = file_name,
                        country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(file_name)
}

# # NYT - Syrie ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-NYTimes-Syrie")
# #for (file_name in file_names[12:18]) {
# for (file_name in file_names) {
#   dfi <- clean_nyt_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-NYTimes-Syrie",
#                        file_name = file_name,
#                        country = "Syrie")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }
#
# # NYT - Ukraine ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/US-NYTimes-Ukraine")
# for (file_name in file_names) {
#   dfi <- clean_nyt_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/US-NYTimes-Ukraine",
#                        file_name = file_name,
#                        country = "Ukraine")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }

# NYT - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/US-NYTimes-Irak")
for (file_name in file_names) {
  dfi <- clean_nyt_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/US-NYTimes-Irak",
                       file_name = file_name,
                       country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(file_name)
}

# # Times UK - Syrie ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Times-Syrie")
# for (file_name in file_names) {
#   dfi <- clean_timesUK_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Times-Syrie",
#                            file_name = file_name,
#                            country = "Syrie")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }
#
# # Times UK - Ukraine ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-The Times-Ukraine")
# for (file_name in file_names) {
#   dfi <- clean_timesUK_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-The Times-Ukraine",
#                            file_name = file_name,
#                            country = "Ukraine")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }

# Times UK - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-The Times-Iraq")
for (file_name in file_names) {
  dfi <- clean_timesUK_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-The Times-Iraq",
                           file_name = file_name,
                           country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(file_name)
}

# # WPost - Syrie ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-WaPo-Syrie")
# for (file_name in file_names) {
#   dfi <- clean_WPost_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-WaPo-Syrie",
#                          file_name = file_name,
#                          country = "Syrie")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }
#
# # WPost - Ukraine ####
# file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/US-WaPo-Ukraine")
# for (file_name in file_names) {
#   dfi <- clean_WPost_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/US-WaPo-Ukraine",
#                          file_name = file_name,
#                          country = "Ukraine")
#   saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
#   print(file_name)
# }

# WPost - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/US-WaPo-Iraq")
for (file_name in file_names) {
  dfi <- clean_WPost_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/US-WaPo-Iraq",
                         file_name = file_name,
                         country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(file_name)
}

# Tout charger les RDS en même temps
 rds_all <- list.files( path = "_SharedFolder_article_syrie-ukraine/Data/clean", pattern = "*.rds", full.names = TRUE) %>%
  map_dfr(readRDS) %>%
   mutate(media_country = ifelse(source == "Toronto Star", "Canada",
                                 ifelse(source == "The Globe and Mail", "Canada",
                                        ifelse(source == "The New York Times", "United States",
                                               ifelse(source == "The Washington Post", "United States",
                                                      ifelse(source == "The Sun", "UK",
                                                             ifelse(source == "The Gardian", "UK",
                                                                    ifelse(source == "The UK Times", "UK", "Error!!"))))))))
# Enregistrer le csv complet
 saveRDS(rds_all, paste0("_SharedFolder_article_syrie-ukraine/Data/", "dataset.rds"))
