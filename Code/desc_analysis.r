library(tidyverse)

df <- readRDS("_SharedFolder_article_syrie-ukraine/Data/data_descriptive.RDS")

ggplot(df, aes(x=date, y=total_words_day1000, color=country)) +
  geom_line() +
  labs(title="Coverage Over Time", x="Date", y="Total Words (normalized by 1000)") +
  theme_minimal()

ggplot(df, aes(x=date, y=ton, color=country)) +
  geom_line() +
  labs(title="Tone of Coverage Over Time", x="Date", y="Tone") +
  theme_minimal()

ggplot(df, aes(x=total_words_sentence, fill=country)) +
  geom_histogram(binwidth=5, position="identity", alpha=0.5) +
  labs(title="Word Count Distribution", x="Total Words in Sentence", y="Frequency") +
  theme_minimal()

ggplot(df, aes(x=ton, fill=country)) +
  geom_histogram(binwidth=0.05, position="identity", alpha=0.5) +
  labs(title="Tone Distribution", x="Tone", y="Frequency") +
  theme_minimal()
ggplot(df, aes(x=propPos, y=propNeg, color=country)) +
  geom_point() +
  labs(title="Comparison of Positive vs Negative Coverage", x="Proportion of Positive Words", y="Proportion of Negative Words") +
  theme_minimal()

ggplot(df, aes(x=source, fill=country)) +
  geom_bar(position="dodge") +
  labs(title="Coverage by Source", x="Media Source", y="Number of Sentences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
ggplot(df, aes(x=date, y=total_words_day1000, color=country)) +
  geom_line() +
  labs(title="Coverage Over Time", x="Date", y="Total Words (normalized by 1000)") +
  theme_minimal() +
  scale_color_manual(values=c("Syria"="blue", "Ukraine"="red"))

ggplot(df, aes(x=date, y=ton, color=country)) +
  geom_line() +
  labs(title="Tone of Coverage Over Time", x="Date", y="Tone") +
  theme_minimal() +
  scale_color_manual(values=c("Syria"="blue", "Ukraine"="red"))

ggplot(df, aes(x=total_words_sentence, fill=country)) +
  geom_histogram(binwidth=5, position="identity", alpha=0.5) +
  labs(title="Word Count Distribution", x="Total Words in Sentence", y="Frequency") +
  theme_minimal() +
  scale_fill_manual(values=c("Syria"="blue", "Ukraine"="red"))

ggplot(df, aes(x=ton, fill=country)) +
  geom_histogram(binwidth=0.05, position="identity", alpha=0.5) +
  labs(title="Tone Distribution", x="Tone", y="Frequency") +
  theme_minimal() +
  scale_fill_manual(values=c("Syria"="blue", "Ukraine"="red"))

ggplot(df, aes(x=propPos, y=propNeg, color=country)) +
  geom_point() +
  labs(title="Comparison of Positive vs Negative Coverage", x="Proportion of Positive Words", y="Proportion of Negative Words") +
  theme_minimal() +
  scale_color_manual(values=c("Syria"="blue", "Ukraine"="red"))

ggplot(df, aes(x=source, fill=country)) +
  geom_bar(position="dodge") +
  labs(title="Coverage by Source", x="Media Source", y="Number of Sentences") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("Syria"="blue", "Ukraine"="red"))


# Filter data for Canadian media coverage
library(lubridate)

# Normalize the dates for Syria and Ukraine to a common starting point
df_canada <- df[df$media_country == "Canada",]
df_canada <- df_canada %>%
  group_by(country) %>%
  mutate(day_number = as.integer(difftime(date, min(date), units = "days")) + 1)

# Plotting tone over time for Syria vs. Ukraine on a normalized date axis
# Plotting smoothed tone over time for Syria vs. Ukraine on a normalized date axis
ggplot(df_canada, aes(x=day_number, y=ton, color=country)) +
  geom_smooth(method="loess", se=FALSE, size=1.5, linetype="solid") +
  labs(title="Smoothed Tone of Coverage Over Time by Canadian Media", 
       x="Day Number (from start of coverage)", 
       y="Tone (Difference between Proportion of Positive and Negative Words)") +
  theme_minimal() +
  scale_color_manual(values=c("Syria"="blue", "Ukraine"="red")) +
  theme(legend.title = element_blank())


