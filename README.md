# Virat Kohli Century Analysis

This project analyzes Virat Kohli's century data across various match formats, exploring key performance metrics through statistical analysis and data visualizations. The analysis focuses on univariate and bivariate statistics, regression analysis, and hypothesis testing.

### Install Required Packages
```
install.packages(c("tidyverse", "ggplot2", "corrplot", "nortest", "car"))
```

## Table of Contents
- [Introduction](#introduction)
- [Data](#data)
- [Analysis](#analysis)
    - [1. Univariate Analysis](#1-univariate-analysis)
    - [2. Bivariate Analysis](#2-bivariate-analysis)
    - [3. Regression and Hypothesis Testing](#3-regression-and-hypothesis-testing)
- [Visualizations](#visualizations)
- [Conclusion](#conclusion)
- [Dependencies](#dependencies)
- [Usage](#usage)

## Introduction

This project focuses on analyzing the centuries scored by Indian cricketer Virat Kohli across different match formats. We explore key performance metrics like score, strike rate, team performance, and the influence of factors like match format, innings, and captaincy on his performances. The goal is to uncover insights through statistical analysis and data visualizations.

## Data

The dataset used in this analysis contains the following columns:
- **Score**: Runs scored by Virat Kohli.
- **Balls**: Balls faced during the century.
- **Strike Rate**: Strike rate for the century.
- **Team Total**: Total score by the team.
- **Wickets Lost**: Wickets lost by the team during Kohli's century.
- **Format**: Match format (e.g., ODI, Test, T20).
- **Inning**: Inning in which the century was scored.
- **Position**: Kohli's batting position.
- **Against**: Opponent team.
- **Year**: Year the century was scored.
- **Not Out**: Whether Kohli remained not out (Yes/No).
- **MOTM**: Whether Kohli was awarded the Man of the Match (Yes/No).
- **Win**: Whether the team won the match (Yes/No).
- **Captain**: Whether Kohli was the captain during the match (Yes/No).

## Analysis

### 1. Univariate Analysis
- Summary statistics (mean, standard deviation, min, max) are calculated for quantitative variables like **Score**, **Balls**, **Strike Rate**, **Team Total**, and **Wickets Lost**.
- Frequency distributions for categorical variables such as **Format**, **Inning**, **Position**, **Year**, etc.

### 2. Bivariate Analysis
- Correlation matrix for quantitative variables like **Score**, **Balls**, **Strike Rate**, **Team Total**, and **Wickets Lost**.
- Scatter plots to explore relationships between **Score** and **Strike Rate**, and **Score** and **Team Total**.
- Two-way tables for categorical variables, such as **Format** vs **Win**.

### 3. Regression and Hypothesis Testing
- **Linear Regression**: Model to predict Kohli's **Score** based on **Balls** faced and **Strike Rate**.
- **T-test**: Comparing **Score** between ODI and Test formats to assess if there's a significant difference.
- **ANOVA**: Analysis of variance on **Score** across different batting positions.
- **Chi-square Test**: Checking for association between **Man of the Match (MOTM)** and **Win**.
- **Paired T-test**: Comparing **Score** against half of the **Team Total**.

## Visualizations
The project includes several visualizations to better understand Kohli's performances:
- Histograms of **Score** and **Strike Rate** distributions.
- Bar charts showing the number of centuries by match format.
- Box plots comparing **Score** by match format and win/loss results.
- Scatter plots visualizing the relationship between **Score** and **Strike Rate**, and **Score** vs **Team Total**.
- Density plots showing the distribution of scores when Kohli is a captain versus when he's not.
- A time series plot for the trend of average scores over the years.

## Conclusion

This analysis provides valuable insights into Virat Kohli's century performances. Key takeaways include the impact of match format, strike rate, and team performance on his centuries, as well as differences in performance based on his role as a captain and match outcome.

## Dependencies

This project requires the following R packages:
- `tidyverse`: For data manipulation and visualization.
- `ggplot2`: For creating plots and charts.
- `corrplot`: For visualizing correlation matrices.
- `nortest`: For normality tests.
- `car`: For ANOVA and hypothesis testing.

## Usage

To run the analysis, load the dataset and execute the analysis steps. The code includes visualizations, regression models, and hypothesis tests, which can be adapted to similar datasets. Simply follow the structure in the script to reproduce the analysis.
