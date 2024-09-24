# TABLE OF CONTENTS #
# 1. Data Preparation
# 2. Univariate Analysis
# 3. Bivariate Analysis
# 4. Time Series Analysis
# 5. Outlier Detection
# 6. Regression Analysis
# 7. Format-Specific Analysis
# 8. Hypothesis Testing

# 1. Data Preparation

# Load required libraries
library(tidyverse)
library(corrplot)
library(nortest)
library(car)
library(BSDA)

# Read the data
data <- read.csv("IS 2105 - Proposal - Group18 - Dataset.csv")
View(data)

# Replace "T20i" with "T20" in Format column
data$Format <- ifelse(data$Format == "T20i", "T20", data$Format)
# In the Format column, "T20" means "T20 International(T20I)" and "Indian Premie League(IPL)"
view(data)
head(data)

# Define the function to generate random colors from the rainbow spectrum
random_color <- function(n) {
  colors <- rainbow(100)
  sample(colors, n)
}

# 2. Univariate Analysis

# 2.1 Numerical Variables

# Score
hist(data$Score,
     main = "Distribution of Scores",
     xlab = "Score",
     col = random_color(1))

# Balls
hist(data$Balls,
     main = "Distribution of Balls",
     xlab="Balls",
     col = random_color(1))

# Strike Rate
hist(data$Strike.Rate,
     main = "Distribution of Strike Rates",
     xlab = "Strike Rate",
     col = random_color(1))

# Team Total
hist(data$Team.Total,
     main = "Distribution of Team Totals",
     xlab = "Team Total",
     col = random_color(1))

# 2.2 Categorical Variables

# Format
format_table <- table(data$Format)
percentages <- round(100 * format_table / sum(format_table), 1)
labels <- paste(names(format_table), "\n", format_table, " (", percentages, "%)", sep = "")

pie(format_table,
    main = "Centuries by Format",
    labels = labels,
    col = random_color(length(format_table)))

# Position
table(data$Position)
barplot(table(data$Position),
        main = "Centuries by Position",
        xlab = "Position",
        ylab = "Count",
        col = random_color(1))

# Year
table(data$Year)
barplot(table(data$Year),
        main = "Centuries by Year",
        xlab = "Year",
        ylab = "Count",
        col = random_color(1),
        las = 2,
        cex.names = 0.7)

# Not Out
not_out_table <- table(data$Not.Out)
percentages <- round(100 * not_out_table / sum(not_out_table), 1)
labels <- paste(names(not_out_table), "\n", not_out_table, " (", percentages, "%)", sep = "")

pie(not_out_table,
    main = "Centuries by Not Out",
    labels = labels,
    col = random_color(length(not_out_table)))

# Man of the Match (MOTM)
motm_table <- table(data$MOTM)
percentages <- round(100 * motm_table / sum(motm_table), 1)
labels <- paste(names(motm_table), "\n", motm_table, " (", percentages, "%)", sep = "")

pie(motm_table,
    main = "Centuries by Man of the Match",
    labels = labels,
    col = random_color(length(motm_table)))

# Win
table(data$Win)
barplot(table(data$Win),
        main = "Centuries by Win",
        xlab = "Win",
        ylab = "Count",
        col = random_color(1))

# Captain
captain_table <- table(data$Captain)
percentages <- round(100 * captain_table / sum(captain_table), 1)
labels <- paste(names(captain_table), "\n", captain_table, " (", percentages, "%)", sep = "")

pie(captain_table,
    main = "Centuries by Captain",
    labels = labels,
    col = random_color(length(captain_table)))

# 3. Bivariate Analysis

# Scatter plot for two numerical variables
ggplot(data, aes(x = Balls, y = Score, color = Format)) +
  geom_point() +
  labs(title = "Score vs Balls by Format", x = "Balls", y = "Score")

ggplot(data, aes(x = Strike.Rate, y = Score, color = Format)) +
  geom_point() +
  labs(title = "Score vs Strike Rate by Format", x = "Strike Rate", y = "Score")

ggplot(data, aes(x = Score, y = Team.Total, color = Format)) +
  geom_point() +
  labs(title = "Score vs Team Total by Format", x = "Score", y = "Team Total")

# Numerical vs Categorical
# Boxplot

boxplot(Strike.Rate ~ MOTM,
        data = data,
        main = "Strike Rate by Man of the Match",
        xlab = "MOTM",
        ylab = "Strike Rate",
        col = random_color(2))

boxplot(Score ~ Captain,
        data = data,
        main = "Score by Captain",
        xlab = "Captain",
        ylab = "Score",
        col = random_color(2))

# Bar Plot
table(data$Format, data$Not.Out)
ggplot(data, aes(x = Not.Out, fill = Format)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Not Outs by Format", x = "Not Out", y = "Proportion")

table(data$Format, data$MOTM)
ggplot(data, aes(x = MOTM, fill = Format)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Man of the Match by Format", x = "MOTM", y = "Proportion")

# Two-way table for two categorical variables
table(data$Format, data$Win)
table(data$Format, data$MOTM)
table(data$Format, data$Captain)

table(data$Position, data$Win)
table(data$Position, data$MOTM)

# 4. Time Series Analysis

# Line Plot
# Average Score with year
yearly_avg <- aggregate(Score ~ Year, data = data, FUN = mean)
ggplot(yearly_avg, aes(x = Year, y = Score)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Score Trend Over Years", x = "Year", y = "Average Score")

# 5. Outlier Detection

# Score
summary(data$Score)
boxplot(data$Score, main = "Boxplot of Score", col = random_color(1))

score_Q1 <- quantile(data$Score, 0.25)
score_Q3 <- quantile(data$Score, 0.75)
score_IQR <- score_Q3 - score_Q1
score_lower_bound <- score_Q1 - 1.5 * score_IQR
score_upper_bound <- score_Q3 + 1.5 * score_IQR
score_outliers <- data$Score[data$Score < score_lower_bound | data$Score > score_upper_bound]
score_outliers

# Balls
summary(data$Balls)
boxplot(data$Balls, main = "Boxplot of Balls", col = random_color(1))

balls_Q1 <- quantile(data$Balls, 0.25)
balls_Q3 <- quantile(data$Balls, 0.75)
balls_IQR <- balls_Q3 - balls_Q1
balls_lower_bound <- balls_Q1 - 1.5 * balls_IQR
balls_upper_bound <- balls_Q3 + 1.5 * balls_IQR
balls_outliers <- data$Balls[data$Balls < balls_lower_bound | data$Balls > balls_upper_bound]
balls_outliers

# Strike Rate
summary(data$Strike.Rate)
boxplot(data$Strike.Rate, main = "Boxplot of Strike Rate", col = random_color(1))

strike_rate_Q1 <- quantile(data$Strike.Rate, 0.25)
strike_rate_Q3 <- quantile(data$Strike.Rate, 0.75)
strike_rate_IQR <- strike_rate_Q3 - strike_rate_Q1
strike_rate_lower_bound <- strike_rate_Q1 - 1.5 * strike_rate_IQR
strike_rate_upper_bound <- strike_rate_Q3 + 1.5 * strike_rate_IQR
strike_rate_outliers <- data$Strike.Rate[data$Strike.Rate < strike_rate_lower_bound | data$Strike.Rate > strike_rate_upper_bound]
strike_rate_outliers

# Team total
summary(data$Team.Total)
boxplot(data$Team.Total, main = "Boxplot of Team Total", col = random_color(1))

team_total_Q1 <- quantile(data$Team.Total, 0.25)
team_total_Q3 <- quantile(data$Team.Total, 0.75)
team_total_IQR <- team_total_Q3 - team_total_Q1
team_total_lower_bound <- team_total_Q1 - 1.5 * team_total_IQR
team_total_upper_bound <- team_total_Q3 + 1.5 * team_total_IQR
team_total_outliers <- data$Team.Total[data$Team.Total < team_total_lower_bound | data$Team.Total > team_total_upper_bound]
team_total_outliers

# 6. Regression Analysis

# Predicting Score based on Balls Faced
lm_model_balls <- lm(Score ~ Balls, data = data)
summary(lm_model_balls)

new_ball <- data.frame(Balls = c(50, 150, 250, 350))
predicted_scores <- predict(lm_model_balls, new_ball)
predicted_scores

# Predicting Strike Rate based on Score
lm_model_strike_rate <- lm(Strike.Rate ~ Score, data = data)
summary(lm_model_strike_rate)

new_score <- data.frame(Score = c(50, 100, 150, 200))
predicted_strike_rates <- predict(lm_model_strike_rate, new_score)
predicted_strike_rates

# Predicting Team Total based on Score
lm_model_team_total <- lm(Team.Total ~ Score, data = data)
summary(lm_model_team_total)

new_score <- data.frame(Score = c(50, 100, 150, 200))
predicted_team_totals <- predict(lm_model_team_total, new_score)
predicted_team_totals

# 7. Format-Specific Analysis

# 7.1 Test Match Analysis
# Filter the dataset to include only rows where the 'Format' is 'Test'
test_data <- data %>%
  filter(data$Format == "Test")
view(test_data)

# Univariate Analysis
# Test Inning
test_inning_table <- table(test_data$Inning)
percentages <- round(100 * test_inning_table / sum(test_inning_table), 1)
labels <- paste(names(test_inning_table), "\n", test_inning_table, " (", percentages, "%)", sep = "")

pie(test_inning_table,
    main = "Test Centuries by Inning",
    labels = labels,
    col = random_color(length(test_inning_table)))

# Position
test_position_table <- table(test_data$Position)
percentages <- round(100 * test_position_table / sum(test_position_table), 1)
labels <- paste(names(test_position_table), "\n", test_position_table, " (", percentages, "%)", sep = "")

pie(test_position_table,
    main = "Test Centuries by Position",
    labels = labels,
    col = random_color(length(test_position_table)))

# Against
table(test_data$Against)
barplot(table(test_data$Against),
        main = "Test Centuries by Against",
        xlab = "Country",
        ylab = "Count",
        col = random_color(1),
        las = 2,
        cex.names = 0.7)

# Host Nation
table(test_data$Host.Nation)
barplot(table(test_data$Host.Nation),
        main = "Test Centuries by Host Nation",
        xlab = "Country",
        ylab = "Count",
        col = random_color(1),
        las = 2,
        cex.names = 0.7)

# Win
test_win_table <- table(test_data$Win)
percentages <- round(100 * test_win_table / sum(test_win_table), 1)
labels <- paste(names(test_win_table), "\n", test_win_table, " (", percentages, "%)", sep = "")

pie(test_win_table,
    main = "Test Centuries by Win",
    labels = labels,
    col = random_color(length(test_win_table)))


# Bivariate Analysis

# Test Score vs Inning
summary_test_inning_score <- by(test_data$Score, test_data$Inning, summary)
print(summary_test_inning_score)

boxplot(Score ~ Inning,
        data = test_data,
        main = "Test Score by Inning",
        xlab = "Inning",
        ylab = "Score",
        col = random_color(length(test_data$Inning)))

# Test Balls vs Inning
summary_test_inning_balls <- by(test_data$Balls, test_data$Inning, summary)
print(summary_test_inning_balls)

boxplot(Balls ~ Inning,
        data = test_data,
        main = "Test Balls by Inning",
        xlab = "Inning",
        ylab = "Balls",
        col = random_color(length(test_data$Inning)))

# Test Team Total vs Score by MOTM
ggplot(test_data, aes(x = Score, y = Team.Total, color = MOTM)) +
  geom_point() +
  labs(title = "Test Team Total vs Score by Man of the Match", x = "Score", y = "Team Total")

# Proportion of Inning by Test Win
table(test_data$Inning, test_data$Win)
ggplot(test_data, aes(x = Inning, fill = Win)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Inning by Test Win", x = "Inning", y = "Proportion")

# Proportion of Host Nation by Test Win
table(test_data$Host.Nation, test_data$Win)
ggplot(test_data, aes(x = Host.Nation, fill = Win)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Host Nation by Test Win", x = "Country", y = "Proportion")

# Proportion of Test Wins by Opponent Country
table(test_data$Against, test_data$Win)
ggplot(test_data, aes(x = Against, fill = Win)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Test Wins by Opponent Country", x = "Country", y = "Proportion")

# 7.2 ODI Analysis
# Filter the dataset to include only rows where the 'Format' is 'ODI'
odi_data <- data %>%
  filter(data$Format == "ODI")
view(odi_data)

# Univariate Analysis
# ODI Inning
odi_inning_table <- table(odi_data$Inning)
percentages <- round(100 * odi_inning_table / sum(odi_inning_table), 1)
labels <- paste(names(odi_inning_table), "\n", odi_inning_table, " (", percentages, "%)", sep = "")

pie(odi_inning_table,
    main = "ODI Centuries by Inning",
    labels = labels,
    col = random_color(length(odi_inning_table)))

# ODI Win
odi_win_table <- table(odi_data$Win)
percentages <- round(100 * odi_win_table / sum(odi_win_table), 1)
labels <- paste(names(odi_win_table), "\n", odi_win_table, " (", percentages, "%)", sep = "")

pie(odi_win_table,
    main = "ODI Centuries by Win",
    labels = labels,
    col = random_color(length(odi_win_table)))

# ODI Man of the Match
odi_motm_table <- table(odi_data$MOTM)
percentages <- round(100 * odi_motm_table / sum(odi_motm_table), 1)
labels <- paste(names(odi_motm_table), "\n", odi_motm_table, " (", percentages, "%)", sep = "")

pie(odi_motm_table,
    main = "ODI Centuries by MOTM",
    labels = labels,
    col = random_color(length(odi_motm_table)))

# ODI Score
hist(odi_data$Score,
     main = "Distribution of ODI Scores",
     xlab = "Score",
     col = random_color(1))

# ODI SR
hist(odi_data$Strike.Rate,
     main = "Distribution of ODI Strike Rate",
     xlab = "Strike Rate",
     col = random_color(1))

# Bivariate Analysis
# ODI Strike Rate vs Score by Win
ggplot(odi_data, aes(x = Score, y = Strike.Rate, color = Win)) +
  geom_point() +
  labs(title = "ODI Strike Rate vs Score by Win", x = "Score", y = "Strike Rate")

# Proportion of Inning by ODI Win
table(odi_data$Inning, odi_data$Win)
ggplot(odi_data, aes(x = Inning, fill = Win)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Inning by ODI Win", x = "Inning", y = "Proportion")

# Proportion of Not Out by ODI Win
table(odi_data$Not.Out, odi_data$Win, odi_data$Inning)
ggplot(odi_data, aes(x = Not.Out, fill = Win)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Not Out by ODI Win", x = "Not Out", y = "Proportion") +
  facet_wrap(~ Inning)

# ODI Team Total vs Score by MOTM
ggplot(odi_data, aes(x = Score, y = Team.Total, color = MOTM)) +
  geom_point() +
  labs(title = "ODI Team Total vs Score by Man of the Match", x = "Score", y = "Team Total")

# ODI Position vs Score
summary_odi_position_score <- by(test_data$Score, test_data$Position, summary)
print(summary_odi_position_score)

boxplot(Score ~ Position,
        data = odi_data,main = "ODI Score by Position",
        xlab = "Position",
        ylab = "Score",
        col = random_color(length(odi_data$Position)))

# 7.3 T20 Analysis
# Filter the dataset to include only rows where the 'Format' is 'T20'
t20_data <- data %>%
  filter(data$Format == "T20")
view(t20_data)

# Univariate Analysis
# T20 Inning
t20_inning_table <- table(t20_data$Inning)
percentages <- round(100 * t20_inning_table / sum(t20_inning_table), 1)
labels <- paste(names(t20_inning_table), "\n", t20_inning_table, " (", percentages, "%)", sep = "")

pie(t20_inning_table,
    main = "T20 Centuries by Inning",
    labels = labels,
    col = random_color(length(t20_inning_table)))

# T20 Win
t20_win_table <- table(t20_data$Win)
percentages <- round(100 * t20_win_table / sum(t20_win_table), 1)
labels <- paste(names(t20_win_table), "\n", t20_win_table, " (", percentages, "%)", sep = "")

pie(t20_win_table,
    main = "T20 Centuries by Win",
    labels = labels,
    col = random_color(length(t20_win_table)))

# T20 Man of the Match
t20_motm_table <- table(t20_data$MOTM)
percentages <- round(100 * t20_motm_table / sum(t20_motm_table), 1)
labels <- paste(names(t20_motm_table), "\n", t20_motm_table, " (", percentages, "%)", sep = "")

pie(t20_motm_table,
    main = "T20 Centuries by MOTM",
    labels = labels,
    col = random_color(length(t20_motm_table)))

# T20 Score
hist(t20_data$Score,
     main = "Distribution of T20 Scores",
     xlab = "Score",
     col = random_color(1))

# T20 SR
hist(t20_data$Strike.Rate,
     main = "Distribution of T20 Strike Rate",
     xlab = "Strike Rate",
     col = random_color(1))

#T20 Year
table(t20_data$Year)
barplot(table(t20_data$Year),
        main = "T20 Centuries by Year",
        xlab = "Year",
        ylab = "Count",
        col = random_color(1))

# Bivariate Analysis
# T20 Strike Rate vs Score by Win
ggplot(t20_data, aes(x = Score, y = Strike.Rate, color = Win)) +
  geom_point() +
  labs(title = "T20 Strike Rate vs Score by Win", x = "Score", y = "Strike Rate")

# Proportion of Inning by T20 Win
table(t20_data$Inning, t20_data$Win)
ggplot(t20_data, aes(x = Inning, fill = Win)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Inning by T20 Win", x = "Inning", y = "Proportion")

# T20 Captain vs SR
summary_t20_captain_sr <- by(t20_data$Strike.Rate, t20_data$Captain, summary)
print(summary_t20_captain_sr)

boxplot(Strike.Rate ~ Captain,
        data = t20_data,
        main = "T20 Strike Rate by Captain",
        xlab = "Captain",
        ylab = "Strike Rate",
        col = random_color(length(t20_data$Year)))

# T20 Win vs Strike Rate
summary_t20_win_sr <- by(t20_data$Strike.Rate, t20_data$Win, summary)
print(summary_t20_win_sr)

boxplot(Strike.Rate ~ Win,
        data = t20_data,
        main = "T20 Strike Rate by Win",
        xlab = "Win",
        ylab = "Strike Rate",
        col = random_color(length(t20_data$Win)))

# T20 Position vs Score
summary_t20_position_score <- by(t20_data$Score, t20_data$Position, summary)
print(summary_t20_position_score)

boxplot(Score ~ Position,
        data = t20_data,
        main = "T20 Score by Position",
        xlab = "Position",
        ylab = "Score",
        col = random_color(length(t20_data$Position)))

# 8. Hypothesis Testing

# 8.1 T-tests

# Test if there's a significant difference in scores between ODI and Test formats
t_test_odi_test <- t.test(data$Score[data$Format == "ODI"],
                          data$Score[data$Format == "Test"])
print("T-test: ODI vs Test Scores")
print(t_test_odi_test)

# Test if there's a significant difference in strike rates between winning and losing matches
t_test_win_lose <- t.test(data$Strike.Rate[data$Win == "Yes"],
                          data$Strike.Rate[data$Win == "No"])
print("T-test: Strike Rate in Wins vs Losses")
print(t_test_win_lose)

# Test if there's a significant difference in scores when Kohli is captain vs when he's not
t_test_captain <- t.test(data$Score[data$Captain == "Yes"],
                         data$Score[data$Captain == "No"])
print("T-test: Scores as Captain vs Non-Captain")
print(t_test_captain)

# Test if there's a significant difference in scores between matches where Kohli was Man of the Match vs not
t_test_motm <- t.test(data$Score[data$MOTM == "Yes"],
                      data$Score[data$MOTM == "No"])
print("T-test: Scores in Man of the Match vs Non-Man of the Match")
print(t_test_motm)

# Test if there's a significant difference in strike rates between T20 and ODI formats
t_test_t20_odi <- t.test(data$Strike.Rate[data$Format == "T20"],
                         data$Strike.Rate[data$Format == "ODI"])
print("T-test: Strike Rates in T20 vs ODI")
print(t_test_t20_odi)

# 8.2 Z-tests

# Z-test: Is the average score significantly different from 120?
z_test_score <- z.test(data$Score, mu = 120, sigma.x = sd(data$Score))
print("Z-test: Average score ≠ 120")
print(z_test_score)

# Z-test: Is the average strike rate in ODIs significantly different from 95?
odi_strike_rates <- data$Strike.Rate[data$Format == "ODI"]
z_test_odi_sr <- z.test(odi_data$Strike.Rate, mu = 95, sigma.x = sd(odi_strike_rates))
print("Z-test: Average ODI strike rate ≠ 95")
print(z_test_odi_sr)

# Z-test: Is the average number of balls faced in Test centuries significantly different from 200?
test_balls <- data$Balls[data$Format == "Test"]
z_test_test_balls <- z.test(test_balls, mu = 200, sigma.x = sd(test_balls))
print("Z-test: Average balls faced in Test centuries ≠ 200")
print(z_test_test_balls)

# Z-test: Is there a significant difference in average scores between home and away matches?
home_scores <- data$Score[data$Host.Nation == "India"]
away_scores <- data$Score[data$Host.Nation != "India"]
z_test_home_away <- z.test(home_scores, away_scores,
                           sigma.x = sd(home_scores), sigma.y = sd(away_scores))
print("Z-test: Average score in home matches ≠ away matches")
print(z_test_home_away)

# 8.3 Other Hypothesis Tests

# Hypothesis: Kohli's average score in winning matches is greater than 110
winning_scores <- data$Score[data$Win == "Yes"]
t_test_winning <- t.test(winning_scores, mu = 110, alternative = "greater")
print("One-sample t-test: Average score in winning matches > 110")
print(t_test_winning)

# Hypothesis: The average strike rate in T20 matches is greater than 140
t20_strike_rates <- data$Strike.Rate[data$Format == "T20"]
t_test_t20_sr <- t.test(t20_strike_rates, mu = 140, alternative = "greater")
print("One-sample t-test: Average strike rate in T20 matches > 140")
print(t_test_t20_sr)

# Hypothesis: The average score in Test matches is different from 150
test_scores <- data$Score[data$Format == "Test"]
t_test_test_scores <- t.test(test_scores, mu = 150)
print("One-sample t-test: Average score in Test matches ≠ 150")
print(t_test_test_scores)

# Hypothesis: The proportion of centuries resulting in wins is greater than 0.7
prop_test_wins <- prop.test(sum(data$Win == "Yes"), nrow(data), p = 0.7, alternative = "greater")
print("Proportion test: Proportion of centuries resulting in wins > 0.7")
print(prop_test_wins)

# Hypothesis: The proportion of centuries scored as captain is different from 0.5
prop_test_captain <- prop.test(sum(data$Captain == "Yes"), nrow(data), p = 0.5)
print("Proportion test: Proportion of centuries as captain ≠ 0.5")
print(prop_test_captain)

# Hypothesis: The average score in home matches is higher than in away matches
home_scores <- data$Score[data$Host.Nation == "India"]
away_scores <- data$Score[data$Host.Nation != "India"]
t_test_home_away <- t.test(home_scores, away_scores, alternative = "greater")
print("Two-sample t-test: Average score in home matches > away matches")
print(t_test_home_away)

# Hypothesis: The proportion of centuries in wins is greater than 0.75
z_test_win_prop <- prop.test(sum(data$Win == "Yes"), nrow(data), p = 0.75,
                             alternative = "greater", correct = FALSE)
print("Z-test for proportion: Proportion of centuries in wins > 0.75")
print(z_test_win_prop)