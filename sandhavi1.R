library(tidyverse)
library(ggplot2)
library(corrplot)
library(nortest)
library(car)
library(dplyr)

# Read the data
data <- read.csv("IS 2105 - Proposal - Group18 - Dataset.csv")
View(data)

# 1. Univariate Analysis

# Summary statistics for qualiative variables
# Numerical Variables
# Score
hist(data$Score, main="Distribution of Scores", xlab="Score", col="blue")

# Balls
hist(data$Balls, main="Distribution of Balls", xlab="Balls", col="green")

# Strike Rate
hist(data$Strike.Rate, main="Distribution of Strike Rates", xlab="Strike Rate", col="red")

# Team Total
hist(data$Team.Total, main="Distribution of Team Totals", xlab="Team Total", col="yellow")

# Wickets Lost
hist(data$Wickets.lost, main="Distribution of Wickets Lost", xlab="Wickets Lost", col="orange")

# Summary statistics for quantitative variables
# Categorical Variables

# Format
table(data$Format)
barplot(table(data$Format), main="Centuries by Format", xlab="Format")

# Inning
table(data$Inning)
barplot(table(data$Inning), main="Centuries by Inning", xlab="Inning")

# Position
table(data$Position)
barplot(table(data$Position), main="Centuries by Position", xlab="Position")

# Against
table(data$Against)
barplot(table(data$Against), main="Centuries Against Teams", xlab="Teams")

# Venue
table(data$Venue)
barplot(table(data$Venue), main="Matches at Venues", xlab="Venues")

# Year
table(data$Year)
barplot(table(data$Year), main="Centuries by Year", xlab="Year")

# Not Out
table(data$Not.Out)
barplot(table(data$Not.Out), main="Centuries by Not Out", xlab="Not Out")

# MOTM
table(data$MOTM)
barplot(table(data$MOTM), main="Centuries by Man of the Match", xlab = "Man of the Match")

# Win
table(data$Win)
barplot(table(data$Win), main="Centuries by Win", xlab="Win")

# Captain
table(data$Captain)
barplot(table(data$Captain), main="Centuries by Captain", xlab="Captain")

# 2. Bivariate Analysis

# Colleration for Numerical Variables
cor(data$Score, data$Balls)
cor(data$Strike.Rate, data$Score)
cor(data$Team.Total, data$Score)
cor(data$Wickets.lost, data$Score)

# Scatter plot for two numerical variables
ggplot(data, aes(x = Balls, y = Score, color = Format)) +
  geom_point() +
  labs(title = "Score vs Balls by Format", x = "Balls", y = "Score")

ggplot(data, aes(x = Strike.Rate, y = Score, color = Format)) +
    geom_point() +
    labs(title = "Score vs Strike Rate by Format", x = "Strike Rate", y = "Score")

ggplot(data, aes(x = Team.Total, y = Score, color = Format)) +
    geom_point() +
    labs(title = "Score vs Team Total by Format", x = "Team Total", y = "Score")

ggplot(data, aes(x = Wickets.lost, y = Score, color = Format)) +
    geom_point() +
    labs(title = "Score vs Wickets Lost by Format", x = "Wickets Lost", y = "Score")

# Two-way table for two categorical variables
table(data$Format, data$Win)
table(data$Format, data$Not.Out)
table(data$Format, data$MOTM)
table(data$Format, data$Captain)

table(data$Position, data$Win)
table(data$Position, data$Not.Out)
table(data$Position, data$MOTM)
table(data$Position, data$Captain)

# Numerical vs Categorical
# Boxplot
boxplot(Score ~ Win,
        data = data,
        main = "Score by Win",
        xlab = "Win",
        ylab = "Score",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"))

boxplot(Score ~ Not.Out,
        data=data,
        main="Score by Not Out",
        xlab="Not Out",
        ylab="Score",
        col = c("lightblue", "lightgreen"))
boxplot(Score ~ MOTM,
        data=data,
        main = "Score by Man of the Match",
        xlab = "MOTM",
        ylab = "Score",
        col = c("lightpink", "lightyellow"))

boxplot(Score ~ Captain,
        data=data,
        main = "Score by Captain",
        xlab = "Captain",
        ylab = "Score",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"))

# Density Plot
ggplot(data, aes(x = Score, fill = Win)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Score by Win", x = "Score", y = "Density")

ggplot(data, aes(x = Score, fill = Not.Out)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Score by Not Out", x = "Score", y = "Density")

ggplot(data, aes(x = Score, fill = MOTM)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Score by MOTM", x = "Score", y = "Density")

ggplot(data, aes(x = Score, fill = Captain)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Score by Captain", x = "Score", y = "Density")

# Bar Plot
ggplot(data, aes(x = Win, fill = Format)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Wins by Format", x = "Win", y = "Proportion")

ggplot(data, aes(x = Not.Out, fill = Format)) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Not Outs by Format", x = "Not Out", y = "Proportion")

ggplot(data, aes(x = MOTM, fill = Format)) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of MOTH by Format", x = "MOTM", y = "Proportion")

ggplot(data, aes(x = Captain, fill = Format)) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Captains by Format", x = "Captain", y = "Proportion")

# Time Series Analysis
# Line Plot
yearly_avg <- aggregate(Score ~ Year, data = data, FUN = mean)
ggplot(yearly_avg, aes(x = Year, y = Score)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Score Trend Over Years", x = "Year", y = "Average Score")

# Line Plot
# Team Total with year
total_avg <- aggregate(Team.Total ~ Year, data = data, FUN = mean)
ggplot(total_avg, aes(x = Year, y = Team.Total)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Team Total Trend Over Years", x = "Year", y = "Average Team Total")

#Score with wCIKERT LOSS
wicket_avg <- aggregate(Score ~ Wickets.lost, data = data, FUN = mean)
ggplot(wicket_avg, aes(x = Wickets.lost, y = Score)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Score Trend Over Wickets Lost", x = "Wickets Lost", y = "Average Score")

# Score with Balls
# balls_avg <- aggregate(Score ~ Balls, data = data, FUN = mean)
# ggplot(balls_avg, aes(x = Balls, y = Score)) +
#   geom_line() +
#   geom_point() +
#   labs(title = "Average Score Trend Over Balls", x = "Balls", y = "Average Score")

# Score with Strike Rate
# strike_avg <- aggregate(Score ~ Strike.Rate, data = data, FUN = mean)
# ggplot(strike_avg, aes(x = Strike.Rate, y = Score)) +
#   geom_line() +
#   geom_point() +
#   labs(title = "Average Score Trend Over Strike Rate", x = "Strike Rate", y = "Average Score")


# Hypothesis Testing
# T-test
t.test(data$Score[data$Format == "ODI"], data$Score[data$Format == "Test"])
t.test(data$Score[data$Win == "Yes"], data$Score[data$Win == "No"])
t.test(data$Score[data$Not.Out == "Yes"], data$Score[data$Not.Out == "No"])
t.test(data$Score[data$MOTM == "Yes"], data$Score[data$MOTM == "No"])
t.test(data$Score[data$Captain == "Yes"], data$Score[data$Captain == "No"])

# ANOVA
anova(lm(Score ~ Format, data = data))
anova(lm(Score ~ Win, data = data))
anova(lm(Score ~ Not.Out, data = data))
anova(lm(Score ~ MOTM, data = data))
anova(lm(Score ~ Captain, data = data))

# 3. Regression Analysis
lm_model <- lm(Score ~ Balls + Strike.Rate, data = data)
summary(lm_model)

lm_model <- lm(Score ~ Team.Total + Wickets.lost, data = data)
summary(lm_model)

# Normality Test
# Shapiro-Wilk Test
shapiro.test(data$Score)
shapiro.test(data$Balls)
shapiro.test(data$Strike.Rate)

# Anderson-Darling Test
ad.test(data$Score)
ad.test(data$Balls)
ad.test(data$Strike.Rate)

# Chi-Square Test
chisq.test(table(data$MOTM, data$Win))

# Paired T-Test
t.test(data$Score, data$Team.Total / 2, paired = TRUE)


# Outlier Detection
# Boxplot
boxplot(data$Score, main="Boxplot of Score", col="blue")
boxplot(data$Balls, main="Boxplot of Balls", col="green")
boxplot(data$Strike.Rate, main="Boxplot of Strike Rate", col="red")
boxplot(data$Team.Total, main="Boxplot of Team Total", col="yellow")
boxplot(data$Wickets.lost, main="Boxplot of Wickets Lost", col="orange")

# Scatter Plot
plot(data$Score, data$Balls, main="Score vs Balls", xlab="Score", ylab="Balls")
plot(data$Score, data$Strike.Rate, main="Score vs Strike Rate", xlab="Score", ylab="Strike Rate")
plot(data$Score, data$Team.Total, main="Score vs Team Total", xlab="Score", ylab="Team Total")
plot(data$Score, data$Wickets.lost, main="Score vs Wickets Lost", xlab="Score", ylab="Wickets Lost")

# Save the data
# write.csv(data, "IS 2105 - Proposal - Group18 - Dataset.csv", row.names = FALSE)













