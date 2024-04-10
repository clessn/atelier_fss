library(dplyr)
library(ggplot2)

data <- readRDS("data/data_analyse_textuelle.rds")

ggplot(data, aes(x = date, y = tone_index, color = country)) + 
  geom_smooth(method = "loess", se = TRUE, aes(group = country)) + 
  scale_x_date(limits = c(as.Date("2013-01-02"), NA)) +
  theme_classic() +
  labs(x = "Date", y = "Tone", color = "Country") +
  scale_color_manual(values = c("Syrie" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7"))

ggsave("data/graphs/time_series.png", width = 10, height = 6, units = "in")