# Virat Kohli Century Analysis

# Load required libraries
library(tidyverse)
library(corrplot)
library(nortest)
library(car)

# Read the data
data <- read.csv("IS 2105 - Proposal - Group18 - Dataset.csv")
View(data)

# Replace "T20" with "IPL" in Format column
data$Format <- ifelse(data$Format == "T20", "IPL", data$Format)
view(data)

# 1. Univariate Analysis

# Summary statistics for quantitative variables
# Numerical Variables

# Score
hist(data$Score, main="Distribution of Scores", xlab="Score", col="blue")

# Balls
hist(data$Balls, main="Distribution of Balls", xlab="Balls", col="green")

# Strike Rate
hist(data$Strike.Rate, main="Distribution of Strike Rates", xlab="Strike Rate", col="red")

# Team Total
hist(data$Team.Total, main="Distribution of Team Totals", xlab="Team Total", col="yellow")

# Summary statistics for qualitative variables
# Categorical Variables

# Format
table(data$Format)
pie(table(data$Format), 
    main = "Centuries by Format", 
    labels = names(table(data$Format)))

# Position
table(data$Position)
barplot(table(data$Position), main="Centuries by Position", xlab="Position")

# Against
table(data$Against)
barplot(table(data$Against), main="Centuries Against Teams", xlab="Teams")

# Year
table(data$Year)
barplot(table(data$Year), main="Centuries by Year", xlab="Year")

# Not Out
table(data$Not.Out)
pie(table(data$Not.Out), 
    main = "Centuries by Not Out", 
    labels = names(table(data$Not.Out)))

# Man of the Match (MOTM)
table(data$MOTM)
pie(table(data$MOTM), 
    main = "Centuries by Man of the Match", 
    labels = names(table(data$MOTM)))

# Win
table(data$Win)
barplot(table(data$Win), main="Centuries by Win", xlab="Win")

# Captain
table(data$Captain)
pie(table(data$Captain), 
    main = "Centuries by Captain", 
    labels = names(table(data$Captain)))

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
        data=data,
        main = "Strike Rate by Man of the Match",
        xlab = "MOTM",
        ylab = "Strike Rate",
        col = c("lightpink", "lightyellow"))

boxplot(Score ~ Captain,
        data=data,
        main = "Score by Captain",
        xlab = "Captain",
        ylab = "Score",
        col = c("lightblue", "lightgreen"))

# Density Plot
ggplot(data, aes(x = Score, fill = Win)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Score by Win", x = "Score", y = "Density")

ggplot(data, aes(x = Score, fill = Captain)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Score by Captain", x = "Score", y = "Density")

# Bar Plot
ggplot(data, aes(x = Not.Out, fill = Format)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Not Outs by Format", x = "Not Out", y = "Proportion")

ggplot(data, aes(x = MOTM, fill = Format)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of MOTH by Format", x = "MOTM", y = "Proportion")

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
