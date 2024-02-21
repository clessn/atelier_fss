library(dplyr) 

data <- readRDS("_SharedFolder_article_syrie-ukraine/Data/data_pub_syrie_ukraine.rds") 

data_reg <- data %>%
  mutate(month = substr(date, 6, 7),
         year = substr(date, 1, 4))

data_reg$source <- factor(data_reg$source)
data_reg <- within(data_reg, source <- relevel(source, ref = "The New York Times"))

data_reg$country <- factor(data_reg$country)
data_reg <- within(data_reg, country <- relevel(country, ref = "Iraq"))

data_reg$media_country <- factor(data_reg$media_country)
data_reg <- within(data_reg, media_country <- relevel(media_country, ref = "Canada"))

data_reg$year <- factor(data_reg$year)
data_reg <- within(data_reg, year <- relevel(year, ref = "2006"))

models <- list(
  "Model 1" = lm(ton ~ country, data = data_reg),
  "Model 2" = lm(ton ~ country + year, data = data_reg),
  "Model 3" = lm(ton ~ country + source, data = data_reg),
  "Model 4" = lm(ton ~ country + year + source + total_words_day1000 + opinion + family_sum + men_sum + days_since_conflict_start100,  data = data_reg)
)

add_rows_df <- tibble::tribble(
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
             add_rows = add_rows_df)  


