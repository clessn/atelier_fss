library(dplyr)

source("Code/factiva_functions.R")

# # The Sun - Syrie ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Sun-Syrie")

for (file_name in file_names) {
  dfi <- clean_sun_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Sun-Syrie",
                       file_name =  file_name,
                       country =  "Syrie")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # The Sun - Ukraine ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-TheSun-Ukraine")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_sun_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-TheSun-Ukraine",
                       file_name = file_name,
                       country = "Ukraine")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# The Sun - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-The Sun-Iraq")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_sun_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-The Sun-Iraq",
                       file_name = file_name,
                       country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # Globe - Syrie ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Globe&Mail-Syrie")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_globe_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Globe&Mail-Syrie",
                        file_name = file_name,
                        country = "Syrie")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # Globe - Ukraine ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/CA-Globe&Mail-Ukraine")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_globe_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/CA-Globe&Mail-Ukraine",
                         file_name = file_name,
                         country = "Ukraine")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# Globe - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/CA-Globe&Mail-Iraq")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_globe_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/CA-Globe&Mail-Iraq",
                         file_name = file_name,
                         country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # Gardian - Syrie ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-Guardian-Syrie")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))p
  dfi <- clean_gardian_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-Guardian-Syrie",
                         file_name = file_name,
                         country = "Syrie")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # Gardian - Ukraine ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-Guardian-Ukraine")

for (file_name in file_names) {
  dfi <- clean_gardian_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-Guardian-Ukraine",
                         file_name = file_name,
                         country = "Ukraine")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# Gardian - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-Guardian-Iraq")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_gardian_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-Guardian-Iraq",
                           file_name = file_name,
                           country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # Star - Syrie ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Toronto Star-Syrie")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_star_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/CA-Toronto Star-Syrie",
                         file_name = file_name,
                         country = "Syrie")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # Star - Ukraine ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/CA-Toronto Star-Ukraine")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_star_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/CA-Toronto Star-Ukraine",
                         file_name = file_name,
                         country = "Ukraine")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# Star - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/CA-Toronto Star-Iraq")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_star_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/CA-Toronto Star-Iraq",
                        file_name = file_name,
                        country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # NYT - Syrie ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-NYTimes-Syrie")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_nyt_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-NYTimes-Syrie",
                       file_name = file_name,
                       country = "Syrie")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # NYT - Ukraine ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/US-NYTimes-Ukraine")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_nyt_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/US-NYTimes-Ukraine",
                       file_name = file_name,
                       country = "Ukraine")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# NYT - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/US-NYTimes-Irak")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_nyt_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/US-NYTimes-Irak",
                       file_name = file_name,
                       country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # Times UK - Syrie ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Times-Syrie")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_timesUK_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/UK-The Times-Syrie",
                           file_name = file_name,
                           country = "Syrie")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # Times UK - Ukraine ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-The Times-Ukraine")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_timesUK_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/UK-The Times-Ukraine",
                           file_name = file_name,
                           country = "Ukraine")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# Times UK - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-The Times-Iraq")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_timesUK_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/UK-The Times-Iraq",
                           file_name = file_name,
                           country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # WPost - Syrie ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-WaPo-Syrie")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_WPost_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_syrie/US-WaPo-Syrie",
                         file_name = file_name,
                         country = "Syrie")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# # WPost - Ukraine ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/US-WaPo-Ukraine")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_WPost_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_ukraine/US-WaPo-Ukraine",
                         file_name = file_name,
                         country = "Ukraine")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
}

# WPost - Irak ####
file_names <- list.files("_SharedFolder_article_syrie-ukraine/Data/raw_irak/US-WaPo-Iraq")

for (file_name in file_names) {
  print(paste("Starting processing file:", file_name))
  dfi <- clean_WPost_rtf(path_to_folder = "_SharedFolder_article_syrie-ukraine/Data/raw_irak/US-WaPo-Iraq",
                         file_name = file_name,
                         country = "Iraq")
  saveRDS(dfi, paste0("_SharedFolder_article_syrie-ukraine/Data/clean/", file_name, ".rds"))
  print(paste("Finished processing file:", file_name))
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
