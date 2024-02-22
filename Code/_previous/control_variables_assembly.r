library(tidyverse)

data_path <- "_SharedFolder_article_syrie-ukraine/Data/"
data_religion <- read.csv(paste0(data_path, 
                                 "control_dataset/religious_controls.csv"))
data_econ <- read.csv(paste0(data_path, 
                             "control_dataset/economic_controls.csv"))

data_controls <- merge(data_religion, data_econ, 
                       by = c("country", "year"), all = TRUE)

data_controls <- data_controls[!duplicated(data_controls), ]

data_controls <- data_controls %>%
    select(country, year, chrpc_RCS, muspc_RCS, nrepc_RCS, info_flow_KOF, overallGlob_index_KOF, migrant_per_WDI, pop_den_WDI, pop_urb_WDI) %>%
    filter(year == 2015) %>%
    mutate(country = ifelse(country == "United Kin", "UK", country)) %>%
    mutate(country = ifelse(country == "United Sta", "United States", country)) %>%
    rename(media_country = country) %>%
    select(-year)

saveRDS(data_controls, paste0(data_path, "control_dataset/controls.rds"))

data_regression <- readRDS(paste0(data_path, "data_descriptive.RDS"))

unique(data_regression$media_country)

data_regression_controls <- merge(data_regression, data_controls, 
                         by = "media_country", all = TRUE) %>%
                         rename(christian100 = chrpc_RCS,
                                muslim100 = muspc_RCS,
                                non_religious100 = nrepc_RCS,
                                information_flow = info_flow_KOF,
                                globalization_index = overallGlob_index_KOF,
                                migrant100 = migrant_per_WDI,
                                pop_density = pop_den_WDI,
                                pop_urban_density = pop_urb_WDI)

saveRDS(data_regression_controls, paste0(data_path, 
                                "control_dataset/data_regression_controls.RDS"))
