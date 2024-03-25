# Week 12: Correlations 2 - Assumptions, issues and intercorrelations------

# Step 0: Clean your environment ------------------------------------------
rm(list=ls())   

# Step 1: Set your working directory --------------------------------------
getwd()

# Step 2: Load packages ---------------------------------------------------
## We'll need the following packages:
library(broom)
library(car)
library(lsr)
library(Hmisc)
library(tidyverse)

# Step 3: Read in the data ----------------------------------------------
dat <- read_csv("VapingData.csv")

# Step 4: Data wrangling --------------------------------------------------
dat <- dat %>% 
  filter(IAT_BLOCK3_Acc < 1) %>%                            # 1) only keep participants with accuracy scores equal to or smaller than 1 from block 3
  filter(IAT_BLOCK5_Acc < 1) %>%                            # 1) only keep participants with accuracy scores equal to or smaller than 1 from block 5
  mutate(IAT_ACC = (IAT_BLOCK3_Acc + IAT_BLOCK5_Acc)/2) %>% # 2) calculate an average IAT accuracy score across blocks 3 and 5
  filter(IAT_ACC > .8) %>%                                  # 2) only keep participants with an average score greater than .8.
  mutate(IAT_RT = IAT_BLOCK5_RT - IAT_BLOCK3_RT)            # 3) compute the IAT_RT score

# Step 5: Calculating descriptive statistics ------------------------------

descriptives <- dat %>%
  summarise(n = n(),
            mean_IAT_ACC = mean(IAT_ACC),
            mean_IAT_RT = mean(IAT_RT),
            mean_VPQ = mean(VapingQuestionnaireScore, na.rm = TRUE))

# Step 6: Check the assumptions -------------------------------------------

# Missing data
dat <- dat %>% 
  filter(!is.na(VapingQuestionnaireScore)) %>% 
  filter(!is.na(IAT_RT))

# Normality
ggplot(dat, aes(x = VapingQuestionnaireScore)) + 
  geom_histogram(binwidth = 10) +
  theme_bw()

qqPlot(x = dat$VapingQuestionnaireScore)

ggplot(dat, aes(x = IAT_RT)) + 
  geom_histogram(binwidth = 10) +
  theme_bw()

qqPlot(x = dat$IAT_RT)

# Linearity and homoscedasticity
ggplot(dat, aes(x = IAT_RT, y = VapingQuestionnaireScore)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = "Implicit attitude", y = "Explicit attitude")

# Step 7: Conduct a correlation analysis ----------------------------------
results <- cor.test(dat$VapingQuestionnaireScore, 
                    dat$IAT_RT, 
                    method = "pearson") %>% 
  tidy()

results

correlation <- results %>% 
  pull(estimate) %>%
  round(2)

df <- results %>% 
  pull(parameter) %>%
  round(0)

pvalue <- results %>% 
  pull(p.value) %>%
  round(3)

# Step 8: Intercorrelations -----------------------------------------------

# Create a new data frame that only include the relevant variables
dat_matrix <- dat %>%
  select(Age, IAT_RT, VapingQuestionnaireScore) %>%
  as.data.frame() # Make sure tell R that dat is a data frame

# Create a matrix of scatterplots
pairs(dat_matrix)

# Conduct intercorrelation (multiple correlations)
intercor_results <- correlate(x = dat_matrix, # our data
                          test = TRUE, # compute p-values
                          corr.method = "spearman", # run a spearman test 
                          p.adjust.method = "bonferroni") # use the bonferroni correction
intercor_results