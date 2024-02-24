library(dplyr)
library(ggplot2)

data <- readRDS("_SharedFolder_article_syrie-ukraine/Data/data_pub_syrie_ukraine.rds")

ggplot(data, aes(x = country, y = ton, fill = country)) +
  stat_summary(fun = mean, geom = "bar", na.rm = TRUE, 
               position = position_dodge(width = 0.9), alpha = 0.7) +
  stat_summary(
    fun.data = mean_se, geom = "linerange", na.rm = TRUE,
    aes(ymin = ..ymin.., ymax = ..ymax.., color = country),
    position = position_dodge(width = 0.9), size = 1) +
  scale_fill_manual(values = c("Syria" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7")) +
  scale_color_manual(values = c("Syria" = "#CE1126", "Iraq" = "#007A3D", "Ukraine" = "#0057B7")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1, alpha = 0.7) +
  clessnverse::theme_clean_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5)) + 
  labs(x = "Country", y = "Tone mean", title = "Tone mean by country")

ggsave("_SharedFolder_article_syrie-ukraine/graphs/v4_21_02_2024/bar_graph_country.png", width = 8, height = 6, units = "in")


