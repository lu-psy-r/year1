# PSYC122: Week 13 - Lab activity 2

# Step 1: Load packages ---------------------------------------------------
library(broom)
library(car)
library(tidyverse)

# Step 2: Read in the data ------------------------------------------------
stars <- read_csv("stars2.csv")
engage <- read_csv("psess.csv")

head(stars) # Look at the data frames
head(engage)

# Step 3: Calculate mean anxiety scores ------------------------------
stars_means <- stars %>%
  group_by(ID) %>%
  summarise(mean_anxiety = mean(Score, na.rm = TRUE))

# Join dataframes -------------------------------------------
joined <- inner_join(stars_means, engage, "ID")

# Step 4: Calculate descriptive statistics ----------------------------------
descriptives <- joined %>%
  summarise(mean_anx = mean(mean_anxiety, na.rm = TRUE),
            sd_anx = sd(mean_anxiety, na.rm = TRUE),
            mean_weeks = mean(n_weeks, na.rm = TRUE),
            sd_weeks = sd(n_weeks, na.rm = TRUE))

# Step 5: Visualise the data ------------------------------------------------
ggplot(joined, aes(x = mean_anxiety, y = n_weeks)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Anxiety", y = "Engagement") +
  theme_bw()

# Step 6: Build the regression model -----------------------------------------------
mod <- lm(n_weeks ~ mean_anxiety, data = joined)
mod_summary <- summary(mod)
mod_summary

# Step 7: Check the assumptions ----------------------------------------------------
crPlots(mod)                  # Plot linear line and line that best fits the data to check the relationship between outcome and predictor is linear

qqPlot(mod$residuals)         # Create qq-plot to check residuals are normally distributed

residualPlot(mod)             # Create residual plot to check residual show homoscedasticity
