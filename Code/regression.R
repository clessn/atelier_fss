# Load required libraries
library(dplyr)  # for data manipulation
library(stargazer)  # for displaying regression tables

# Load the dataset prepared for regression analysis
reg <- readRDS("_SharedFolder_article_syrie-ukraine/Data/data_descriptive.RDS") %>%
  mutate(month = substr(date, 6, 7),
         year = substr(date, 1, 4))

# Define factors and reference levels for categorical variables
reg$source <- factor(reg$source)
reg <- within(reg, source <- relevel(source, ref = "The New York Times"))

reg$country <- factor(reg$country)
reg <- within(reg, country <- relevel(country, ref = "Syria"))

reg$media_country <- factor(reg$media_country)
reg <- within(reg, media_country <- relevel(media_country, ref = "Canada"))

# Model 1: Tone ~ Country * Year
model_1 <- lm(ton ~ country * as.factor(year), data = reg)
summary(model_1)

# Model 2: Tone ~ Country + Refugees per 1000
model_2 <- lm(ton ~ country + refugies1000, data = reg)
summary(model_2)

# Model 3: Tone ~ Country + Refugees per 1000 + Source
model_3 <- lm(ton ~ country + refugies1000 + source, data = reg)
summary(model_3)

# Model 4: Comprehensive Model
model_4 <- lm(ton ~ country + refugies1000 + source +
                total_words_day1000 + opinion + family_sum + men_sum + days_since_conflict_start100,  data = reg)
summary(model_4)

# Display regression tables using stargazer
stargazer(model_1, model_2, model_3, model_4,
          type = 'text',
          single.row = TRUE,
          no.space = F,
          column.sep.width = "1pt",
          font.size = "footnotesize")

models <- list(
  "lm 1"     = lm(ton ~ country * as.factor(year), data = reg),
  "lm 2" = lm(ton ~ country + refugies1000, data = reg),
  "lm 3"     = lm(ton ~ country + refugies1000 + source, data = reg),
  "lm 4" = lm(ton ~ country + refugies1000 + source +
                total_words_day1000 + opinion + family_sum + men_sum + days_since_conflict_start100,  data = reg)
)

modelsummary(models,
             output = "~/Dropbox/Apps/Overleaf/pub_syrie_ukraine/graphs/table.tex", 
             stars = TRUE)
