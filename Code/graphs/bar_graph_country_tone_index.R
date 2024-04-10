library(dplyr)
library(ggplot2)

data <- readRDS("data/data_analyse_textuelle.rds")

ggplot(data, aes(x = country, y = tone_index, fill = country)) +
  stat_summary(fun = mean, geom = "bar", na.rm = TRUE, 
               position = position_dodge(width = 0.9), alpha = 0.7) +
  stat_summary(
    fun.data = mean_se, geom = "linerange", na.rm = TRUE,
    aes(ymin = ..ymin.., ymax = ..ymax.., color = country),
    position = position_dodge(width = 0.9), size = 1) +
  scale_fill_manual(values = c("Syrie" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7")) +
  scale_color_manual(values = c("Syrie" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1, alpha = 0.7) +
  clessnverse::theme_clean_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) + 
  labs(x = "Country", y = "Tone mean", title = "Tone mean by country")

ggsave("data/graphs/tone_by_country.png", width = 8, height = 6, units = "in")


