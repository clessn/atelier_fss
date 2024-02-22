library(dplyr)
library(ggplot2)

data <- readRDS("_SharedFolder_article_syrie-ukraine/Data/data_pub_syrie_ukraine.rds")

ggplot(data, aes(x = date, y = ton, color = country)) + 
  geom_smooth(method = "loess", se = TRUE, aes(group = country)) + 
  scale_x_date(limits = c(as.Date("2013-01-02"), NA)) +
  theme_classic() +
  labs(x = "Date", y = "Tone", color = "Country") +
  scale_color_manual(values = c("Syria" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7"))

ggsave("_SharedFolder_article_syrie-ukraine/graphs/v4_21_02_2024/time_series.png", width = 10, height = 6, units = "in")
