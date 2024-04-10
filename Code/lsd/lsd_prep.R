library(dplyr)

source(file = "data/lsd_prep_sources/LSDprep_dec2017.R")

data <- readRDS("data/data.rds")

data_lsd <- data  %>% 
    tidytext::unnest_sentences(text, text) %>%
    filter(grepl('refugee|refugees|migrant|migrants', text)) %>%
    ungroup() %>%
    mutate(id_sentence = row_number())

data_lsd$text_prepped <- NA
data_lsd$text_prepped <- sapply(data_lsd$text, LSDprep_contr)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, LSDprep_dict_punct)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, remove_punctuation_from_acronyms)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, remove_punctuation_from_abbreviations)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, LSDprep_punctspace)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, LSDprep_negation)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, LSDprep_dict)
data_lsd$text_prepped <- sapply(data_lsd$text_prepped, mark_proper_nouns)

saveRDS(data_lsd, "data/data_prepped.rds")
