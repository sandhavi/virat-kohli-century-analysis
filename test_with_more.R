# Virat Kohli Century Analysis

# Load required libraries
library(tidyverse)
library(corrplot)
library(nortest)
library(car)
library(BSDA)  # For z.test function

# Read the data
data <- read.csv("IS 2105 - Proposal - Group18 - Dataset.csv")
View(data)

# Replace "T20i" with "T20" in Format column
data$Format <- ifelse(data$Format == "T20i", "T20", data$Format)
# In the Format column, "T20" means "T20 International(T20I)" and "Indian Premie League(IPL)"
view(data)

# Define the function to generate random colors from the rainbow spectrum
random_color <- function(n) {
  colors <- rainbow(100)
  sample(colors, n)
}

# 1. Univariate Analysis

# Summary statistics for quantitative variables
# Numerical Variables

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

# Summary statistics for qualitative variables
# Categorical Variables

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

# 2. Bivariate Analysis

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

# Two-way table for two categorical variables
table(data$Format, data$Win)
table(data$Format, data$MOTM)
table(data$Format, data$Captain)

table(data$Position, data$Win)
table(data$Position, data$MOTM)

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
ggplot(data, aes(x = Not.Out, fill = Format)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Not Outs by Format", x = "Not Out", y = "Proportion")

ggplot(data, aes(x = MOTM, fill = Format)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Man of the Match by Format", x = "MOTM", y = "Proportion")

# Time Series Analysis

# Line Plot
# Average Score with year
yearly_avg <- aggregate(Score ~ Year, data = data, FUN = mean)
ggplot(yearly_avg, aes(x = Year, y = Score)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Score Trend Over Years", x = "Year", y = "Average Score")

# Outlier Detection
# Boxplot
boxplot(data$Score, main="Boxplot of Score", col="blue")
boxplot(data$Balls, main="Boxplot of Balls", col="green")
boxplot(data$Strike.Rate, main="Boxplot of Strike Rate", col="red")
boxplot(data$Team.Total, main="Boxplot of Team Total", col="yellow")
boxplot(data$Wickets.lost, main="Boxplot of Wickets Lost", col="orange")

# Hypothesis Testing
# T-test
t.test(data$Score[data$Format == "ODI"], data$Score[data$Format == "Test"])
t.test(data$Score[data$Win == "Yes"], data$Score[data$Win == "No"])
t.test(data$Score[data$Not.Out == "Yes"], data$Score[data$Not.Out == "No"])
t.test(data$Score[data$MOTM == "Yes"], data$Score[data$MOTM == "No"])
t.test(data$Score[data$Captain == "Yes"], data$Score[data$Captain == "No"])

# 3. Regression Analysis
lm_model <- lm(Score ~ Balls + Strike.Rate, data = data)
summary(lm_model)

lm_model <- lm(Score ~ Team.Total + Wickets.lost, data = data)
summary(lm_model)

# Filter the dataset to include only rows where the 'Format' is 'Test'
test_data <- data %>%
  filter(Format == "Test")
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

# Filter the dataset to include only rows where the 'Format' is 'ODI'
odi_data <- data %>%
  filter(Format == "ODI")
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
        data = odi_data,
        main = "ODI Score by Position",
        xlab = "Position",
        ylab = "Score",
        col = random_color(length(odi_data$Position)))

# Filter the dataset to include only rows where the 'Format' is 'T20'
t20_data <- data %>%
  filter(Format == "T20")
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


###########################################################################
# T-tests and Hypothesis Testing for Virat Kohli Century Analysis

# 1. T-tests

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

# 2. Z-tests

# Z-test: Is the average score significantly different from 120?
z_test_score <- z.test(data$Score, mu = 120, sigma.x = sd(data$Score))
print("Z-test: Average score ≠ 120")
print(z_test_score)

# Z-test: Is the average strike rate in ODIs significantly different from 95?
odi_strike_rates <- data$Strike.Rate[data$Format == "ODI"]
z_test_odi_sr <- z.test(odi_strike_rates, mu = 95, sigma.x = sd(odi_strike_rates))
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

# 3. Hypothesis testing

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