library(dplyr) 

data <- readRDS("_SharedFolder_article_syrie-ukraine/Data/data_pub_syrie_ukraine.rds") 

data <- data %>%
  mutate(month = substr(date, 6, 7),
         year = substr(date, 1, 4))

data$source <- factor(data$source)
data <- within(data, source <- relevel(source, ref = "The New York Times"))

data$country <- factor(data$country)
data <- within(data, country <- relevel(country, ref = "Iraq"))

data$media_country <- factor(data$media_country)
data <- within(data, media_country <- relevel(media_country, ref = "Canada"))

data$year <- factor(data$year)
data <- within(data, year <- relevel(year, ref = "2006"))

models <- list(
  "Model 1" = lm(ton ~ country, data = data),
  "Model 2" = lm(ton ~ country + year, data = data),
  "Model 3" = lm(ton ~ country + source, data = data),
  "Model 4" = lm(ton ~ country + year + source + total_words_day1000 + opinion + family_sum + men_sum + days_since_conflict_start100,  data = data)
)

fixed_effects <- tibble::tribble(
  ~Term, ~Model1, ~Model2, ~Model3, ~Model4,
  "Year Fixed Effects", "", "$\\checkmark$", "", "$\\checkmark$",
  "Source Fixed Effects", "", "", "$\\checkmark$", "$\\checkmark$"
)


modelsummary::modelsummary(models,
             output = "~/Dropbox/Apps/Overleaf/pub_syrie_ukraine/graphs/table.tex", 
             stars = TRUE,
             coef_omit = "year|source|Intercept",  # Omit year coefficients
             gof_omit = 'DF|Deviance|AIC|BIC|Log|RMSE',
             coef_rename = c("countrySyria" = "Articles about Syria",
                             "countryUkraine" = "Articles about Ukraine",
                             "total_words_day1000" = "Total 1000 words per day",
                             "opinion" = "Opinion text",
                             "family_sum" = "Sum of terms about family",
                             "men_sum" = "Sum of terms about men",
                             "days_since_conflict_start100" = "Days since conflict start"),
             add_rows = fixed_effects)  