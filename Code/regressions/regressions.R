library(dplyr) 

data_reg <- readRDS("data/data_analyse_textuelle.rds") 

data <- data %>%
  mutate(month = substr(date, 6, 7),
         year = substr(date, 1, 4))

data$source <- factor(data$source)
data <- within(data, source <- relevel(source, ref = "The New York Times"))

data$country <- factor(data$country, levels = c("Syria", "Ukraine", "Iraq"))
data <- within(data, country <- relevel(country, ref = "Syria"))

data$year <- factor(data$year)
data <- within(data, year <- relevel(year, ref = "2007"))

models <- list(
  "Model 1" = lm(tone_index ~ country, data = data),
  "Model 2" = lm(tone_index ~ country + year, data = data),
  "Model 3" = lm(tone_index ~ country + source, data = data),
  "Model 4" = lm(tone_index ~ country + year + source,  data = data)
)

fixed_effects <- tibble::tribble(
  ~Term, ~Model1, ~Model2, ~Model3, ~Model4,
  "Years Fixed Effect", "", "$\\checkmark$", "", "$\\checkmark$",
  "Sources Fixed Effect", "", "", "$\\checkmark$", "$\\checkmark$"
)

modelsummary::modelsummary(models,
             output = "data/reg_table/main.tex", 
             stars = TRUE,
             coef_omit = "year|source|Intercept",  # Omit year coefficients
             gof_omit = 'DF|Deviance|AIC|BIC|Log|RMSE|R2',
             coef_rename = c("countrySyria" = "Articles about Syria",
                             "CountrySyrie" = "Articles about Syria",
                             "countryUkraine" = "Articles about Ukraine",
                             "countryIraq" = "Articles about Iraq",
                             "total_words_day1000" = "Total 1000 words per day",
                             "opinion" = "Opinion text",
                             "family_sum" = "Sum of terms about family",
                             "men_sum" = "Sum of terms about men",
                             "days_since_conflict_start100" = "Days since conflict start"),
             add_rows = fixed_effects)
