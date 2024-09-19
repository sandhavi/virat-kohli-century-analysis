# Virat Kohli Century Analysis

This project analyzes Virat Kohli's centuries across different cricket formats using R. It includes various statistical analyses and visualizations to gain insights into Kohli's performance.

## Table of Contents

1. [Project Overview](#project-overview)
2. [Data](#data)
3. [Prerequisites](#prerequisites)
4. [Installation](#installation)
5. [Usage](#usage)
6. [Project Structure](#project-structure)
7. [Analysis Sections](#analysis-sections)


## Project Overview

This R script performs a comprehensive analysis of Virat Kohli's centuries in cricket. It covers various aspects such as:

- Data preparation and cleaning
- Univariate and bivariate analyses
- Time series analysis
- Outlier detection
- Regression analysis
- Format-specific analysis (Test, ODI, T20)
- Hypothesis testing

The analysis aims to provide insights into Kohli's performance across different formats, conditions, and years.

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

## Prerequisites

To run this analysis, you need:

- R (version 3.6.0 or higher recommended)
- RStudio (optional, but recommended)

## Installation

1. Clone this repository or download the R script.
2. Install the required R packages by running the following commands in R:

```R
install.packages(c("tidyverse", "corrplot", "nortest", "car", "BSDA"))
```

## Usage

1. Open the R script in RStudio or your preferred R environment.
2. Set the working directory to the folder containing the script and data file.
3. Run the script section by section or as a whole.

Note: Make sure the data file "IS 2105 - Proposal - Group18 - Dataset.csv" is in the same directory as the script.

## Project Structure

The main components of this project are:

- `virat_kohli_analysis.R`: The main R script containing all the analysis code.
- `IS 2105 - Proposal - Group18 - Dataset.csv`: The dataset containing Virat Kohli's century information.
- `README.md`: This file, providing an overview and instructions for the project.

## Analysis Sections

The R script is organized into the following main sections:

1. Data Preparation
2. Univariate Analysis
  - Numerical Variables
  - Categorical Variables
3. Bivariate Analysis
4. Time Series Analysis
5. Outlier Detection
6. Regression Analysis
7. Format-Specific Analysis
  - Test Match Analysis
  - ODI Analysis
  - T20 Analysis
8. Hypothesis Testing
  - T-tests
  - Z-tests
  - Other Hypothesis Tests

Each section contains various visualizations and statistical tests to provide comprehensive insights into Kohli's centuries.
