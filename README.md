# Titanic Survival Analysis – Shiny Web Application

## Overview

This project is an interactive web application built with **R Shiny** to explore survival patterns in the Titanic dataset.

The goal of the app is to perform an exploratory data analysis (EDA) using reactive programming, allowing the user to dynamically visualize survival trends across different categorical variables.

---

## Features

The application includes:

- Distribution of selected categorical variables (Class, Sex, Age)
- Stacked survival counts (Survived vs Did Not Survive)
- Survival rate per group
- Heatmap of survival counts
- Interaction plot (Class × Sex)
- Logistic regression model with predicted survival probabilities
- Chi-square test and Cramér’s V to measure statistical association

---

## Technologies Used

- R
- Shiny
- ggplot2
- dplyr

---

## Dataset

The dataset used is the built-in **Titanic** dataset available in R.  
The original dataset is a multi-dimensional contingency table that was transformed into an expanded data frame for analysis.

---

## Author

Carolina Lopez Larraz