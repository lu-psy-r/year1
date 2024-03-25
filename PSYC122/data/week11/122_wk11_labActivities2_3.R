# Week 11: Correlation ----------------------------------------------------

# Load packages
library(broom)
library(tidyverse)

# clean the environment
rm(list=ls())

# Lab activity 2: Miller and Haden data -----------------------------------

# read in the data
mh <- read_csv("MillerHadenData.csv")
mh

# plot the relationship between home and TV using a scatterplot and a line of best fit
ggplot(mh, aes(x = Home, y = TV)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Time spend reading at home", y = "Time spend watching TV at home")

# conduct a correlation analysis, using Pearson's r
results <- cor.test(mh$Home, 
                    mh$TV, 
                    method = "pearson", 
                    alternative = "two.sided") %>% 
  tidy()

results

# pull out Pearson's r, the degrees of freedom and the p-value for reporting the results
r <- results %>%
  pull(estimate) %>%
  round(2)

df <- results %>%
  pull(parameter)

pvalue <- results %>%
  pull(p.value) %>%
  round(3)

rsquared <- r*r
rsquaredPercent <- round(rsquared * 100, 0)

# Lab activity 3: Hazardous alcohol use and impulsivity -------------------

# clean the environment
rm(list=ls())

# read in the data
data <- read_csv("alcoholUse_Impulsivity.csv")
data

# plot the relationship between hazard alcohol use and impulsivity using a scatterplot and a line of best fit
ggplot(data, aes(x = hau, y = imp)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Hazardous Alcohol Use", y = "Impulsivity")

# conduct a correlation analysis, using Pearson's r
results <- cor.test(data$hau, 
                    data$imp, 
                    method = "pearson", 
                    alternative = "two.sided") %>% 
  tidy()

results

# pull out Pearson's r, the degrees of freedom and the p-value for reporting the results
r <- results %>%
  pull(estimate) %>%
  round(2)

df <- results %>%
  pull(parameter)

pvalue <- results %>%
  pull(p.value) %>%
  round(3)

rsquared <- r*r
rsquaredPercent <- round(rsquared * 100, 0)
