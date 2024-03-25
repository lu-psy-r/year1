# PSYC122: Week 13 - Simple linear regression in R - Demo

# Our hypothesis is that higher IQ predicts higher reading ability.

# Run this code to empty the R environment
rm(list=ls())                            

# Run this code to load relevant libraries
library(broom)
library(car)
library(tidyverse)

# TASK: Read in the data (using the read_csv() function), have a look at the layout of the data and
# familiarise yourself with it. You can use the head() function for that.

# Read in data and have a look at the table
mh <- read_csv("MillerHadenData.csv")                            # Read in the data files

head(mh)                                                         # Look at the data frames

# Calculate means and standard deviations for anxiety and engagement
descriptives <- mh %>%
  summarise(mean_Abil = mean(Abil, na.rm = TRUE),
            sd_Abil = sd(Abil, na.rm = TRUE),
            mean_IQ = mean(IQ, na.rm = TRUE),
            sd_IQ = sd(IQ, na.rm = TRUE))

descriptives

# Create a scatterplot
ggplot(mh, aes(x = IQ, y = Abil)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "IQ", y = "Reading ability") +
  theme_bw()

# HINT: lm(outcome ~ predictor, data = my_data) 

# Run the code to build the regression model
mod <- lm(Abil ~ IQ, data = mh)
mod_summary <- summary(mod)
mod_summary

# Checking the assumptions
# HINT: crPlots() to check linearity
#       qqPlot() to check normality of the residuals
#       residualPlot() to check homoscedasticity of the residuals

crPlots(mod)                  # Plot linear line and line that best fits the data to check the relationship between outcome and predictor is linear

qqPlot(mod$residuals)         # Create qq-plot to check residuals are normally distributed

residualPlot(mod)             # Create residual plot to check residual show homoscedasticity

# Write up:
# HINT: The Purdue writing lab website (https://owl.purdue.edu/owl/research_and_citation/apa6_style/apa_formatting_and_style_guide/statistics_in_apa.html)
# is helpful for guidance on punctuating statistics.

# A simple linear regression was performed with reading ability (M = 55.12, SD = 6.08) as the outcome variable and
# IQ (M = 100.04, SD = 9.04) as the predictor variable. The results of the regression indicated that the
# model significantly predicted reading ability (F(1, 23) = 5.88, p = .024, R2 = 0.20), accounting
# for 20% of the variance. IQ was a significant positive predictor (beta = 0.30, p = .024).
