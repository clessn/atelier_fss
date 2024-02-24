library(dplyr)

data_80k <- readRDS("_SharedFolder_article_syrie-ukraine/Data/data_pub_syrie_ukraine_tone_no_scale.rds")

data_35k <- readRDS("_SharedFolder_article_syrie-ukraine/Data/data_pub_syrie_ukraine.rds")

nrow(data_80k) - nrow(data_35k)
