# Virat Kohli Century Analysis

# Load required libraries
library(tidyverse)
library(corrplot)
library(nortest)
library(car)

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
        col = random_color(1))

# Year
table(data$Year)
barplot(table(data$Year), 
        main = "Centuries by Year", 
        xlab = "Year",
        col = random_color(1))

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

# Filter the dataset to include only rows where the 'Format' is 'Test'
test_data <- data %>%
  filter(Format == "Test")
view(test_data)

#Univariate
#inning pie chart
#position bar graph
#against bar graph
#host nation bar graph
#win pie chart

#Bivariate
#inning, score boxplot
#inning, balls boxplot
#score, team total scatterplot colored by inning
#year, score boxplot
#inning, win multiple bar graph

# Filter the dataset to include only rows where the 'Format' is 'ODI'
odi_data <- data %>%
  filter(Format == "ODI")
view(odi_data)

#Univariate 
#Inning pie chart
#Win pie chart 
#MOTM pie chart 
#Score histogram
#SR histogram

#Bivariate 
#Score, SR scatterplot colored by win
#Inning, win multiple bar graph
#Not out, win multiple bar graph
#Win, SR box plot
#Year, score box plot

# Filter the dataset to include only rows where the 'Format' is 'T20'
t20_data <- data %>%
  filter(Format == "T20")
view(t20_data)

#Univariate 
#Inning pie chart
#Win pie chart 
#MOTM pie chart 
#Score histogram
#SR histogram

#Bivariate 
#Score, SR scatterplot colored by win
#Inning, win multiple bar graph
#Not out, win multiple bar graph
#Win, SR box plot
#Year, score box plot