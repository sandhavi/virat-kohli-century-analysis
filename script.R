# Virat Kohli Century Analysis

# Load required libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(nortest)
library(car)

# Read the data
kohli_data <- read.csv("IS 2105 - Proposal - Group18 - Dataset.csv")

# Replace "T20" with "IPL" in Format column
kohli_data$Format <- ifelse(kohli_data$Format == "T20", "IPL", kohli_data$Format)

# 1. Univariate Analysis

# Summary statistics for quantitative variables
summary_stats <- data.frame(
  Score = c(mean(kohli_data$Score), sd(kohli_data$Score), min(kohli_data$Score), max(kohli_data$Score)),
  Balls = c(mean(kohli_data$Balls), sd(kohli_data$Balls), min(kohli_data$Balls), max(kohli_data$Balls)),
  Strike_Rate = c(mean(kohli_data$Strike.Rate), sd(kohli_data$Strike.Rate), min(kohli_data$Strike.Rate), max(kohli_data$Strike.Rate)),
  Team_Total = c(mean(kohli_data$Team.Total), sd(kohli_data$Team.Total), min(kohli_data$Team.Total), max(kohli_data$Team.Total)),
  Wickets_lost = c(mean(kohli_data$Wickets.lost), sd(kohli_data$Wickets.lost), min(kohli_data$Wickets.lost), max(kohli_data$Wickets.lost))
)
rownames(summary_stats) <- c("Mean", "SD", "Min", "Max")

print(summary_stats)

# Frequency distributions for categorical variables
cat_vars <- c("Format", "Inning", "Position", "Against", "Year", "Not.Out", "MOTM", "Win", "Captain")

for (var in cat_vars) {
  print(table(kohli_data[[var]]))
}

# Visualizations
# Histogram for Score
ggplot(kohli_data, aes(x = Score)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Virat Kohli's Century Scores", x = "Score", y = "Frequency")

# Bar chart for Format
ggplot(kohli_data, aes(x = Format)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Virat Kohli's Centuries by Format", x = "Format", y = "Count")

# 2. Bivariate Analysis

# Correlation matrix for quantitative variables
quant_vars <- kohli_data %>% select(Score, Balls, Strike.Rate, Team.Total, Wickets.lost)
cor_matrix <- cor(quant_vars)
corrplot(cor_matrix, method = "circle")

# Scatter plot: Score vs Strike Rate, colored by Format
ggplot(kohli_data, aes(x = Strike.Rate, y = Score, color = Format)) +
  geom_point() +
  labs(title = "Score vs Strike Rate by Format", x = "Strike Rate", y = "Score")

# Two-way table: Format vs Win
format_win_table <- table(kohli_data$Format, kohli_data$Win)
print(format_win_table)

# 3. Regression Analysis

# Linear regression: Score ~ Balls + Strike.Rate
lm_model <- lm(Score ~ Balls + Strike.Rate, data = kohli_data)
summary(lm_model)

# T-test: Compare Score between ODI and Test formats
odi_scores <- kohli_data$Score[kohli_data$Format == "ODI"]
test_scores <- kohli_data$Score[kohli_data$Format == "Test"]
t_test_result <- t.test(odi_scores, test_scores)
print(t_test_result)

# Boxplot: Score by Format
ggplot(kohli_data, aes(x = Format, y = Score)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Scores by Format", x = "Format", y = "Score")

# ANOVA: Score across different Positions
anova_result <- aov(Score ~ Position, data = kohli_data)
print(summary(anova_result))

# Histogram: Distribution of Strike Rates
ggplot(kohli_data, aes(x = Strike.Rate)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Strike Rates", x = "Strike Rate", y = "Frequency")

# Scatter plot: Score vs Team Total
ggplot(kohli_data, aes(x = Team.Total, y = Score, color = Format)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Score vs Team Total by Format", x = "Team Total", y = "Score")

# Chi-square test: Association between MOTM and Win
chisq_test <- chisq.test(table(kohli_data$MOTM, kohli_data$Win))
print(chisq_test)

# Stacked bar plot: MOTM vs Win
ggplot(kohli_data, aes(x = MOTM, fill = Win)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Wins by Man of the Match", x = "Man of the Match", y = "Proportion")

# Paired t-test: Score vs Half of Team Total
half_team_total <- kohli_data$Team.Total / 2
paired_t_test <- t.test(kohli_data$Score, half_team_total, paired = TRUE)
print(paired_t_test)

# Density plot: Score distribution by Captain status
ggplot(kohli_data, aes(x = Score, fill = Captain)) +
  geom_density(alpha = 0.5) +
  labs(title = "Score Distribution by Captain Status", x = "Score", y = "Density")

# Correlation heatmap
cor_matrix <- cor(kohli_data[, c("Score", "Balls", "Strike.Rate", "Team.Total", "Wickets.lost")])
ggplot(data = reshape2::melt(cor_matrix)) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")

# Hypothesis test: Is the average score in winning matches significantly different?
win_scores <- kohli_data$Score[kohli_data$Win == "Yes"]
lose_scores <- kohli_data$Score[kohli_data$Win == "No"]
win_lose_test <- t.test(win_scores, lose_scores)
print(win_lose_test)

# Box plot: Score distribution for wins vs losses
ggplot(kohli_data, aes(x = Win, y = Score, fill = Win)) +
  geom_boxplot() +
  labs(title = "Score Distribution: Wins vs Losses", x = "Match Result", y = "Score")

# Time series analysis: Average score over the years
yearly_avg <- aggregate(Score ~ Year, data = kohli_data, FUN = mean)
ggplot(yearly_avg, aes(x = Year, y = Score)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Score Trend Over Years", x = "Year", y = "Average Score")

# Levene's test for homogeneity of variance across formats
levene_test <- leveneTest(Score ~ Format, data = kohli_data)
print(levene_test)

# Save plots (uncomment to save)
# ggsave("score_by_format_boxplot.png", width = 10, height = 6)
# ggsave("strike_rate_histogram.png", width = 10, height = 6)
# ggsave("score_vs_team_total_scatter.png", width = 10, height = 6)
# ggsave("motm_vs_win_stacked_bar.png", width = 10, height = 6)
# ggsave("score_distribution_by_captain.png", width = 10, height = 6)
# ggsave("correlation_heatmap.png", width = 10, height = 8)
# ggsave("score_wins_vs_losses_boxplot.png", width = 10, height = 6)
# ggsave("average_score_trend.png", width = 10, height = 6)