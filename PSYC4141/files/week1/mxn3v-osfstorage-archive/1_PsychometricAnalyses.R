#' This code performs the psychometric analyses of the BMW-3 data.
#' It includes the evaluation of the internal consistency, the factor structure, 
#' and the stability of the BMW-3 questionnaire.
#' 
#' Please note, to ensure that all files are loaded correctly and the relative
#' paths work, you need to first open the R-project file "BMW3-Analyses.Rproj"
#' and then open the respective R scripts.
#' 
#' If you have questions regarding theses scripts please contact:
#'    Anna-Lena Schubert, anna-lena.schubert@uni-mainz.de OR
#'    Gidon Frischkorn, gidon.frischkorn@psychologie.uzh.ch


# start fresh
rm(list = ls())   # clean up workspace
graphics.off()  # switch off graphics device

# load libraries 
pacman::p_load(tidyverse, psych, lavaan, semPlot, patchwork, apaTables, here,
               emmeans, lme4, lmerTest, sjPlot, effectsize, cocor)

# load data
data_BMW3_merged <- read.table(file = here("PrepData", "cleanData_BMW3_merged.txt"), header = TRUE, sep = ",")
data_ZH_retest <- read.table(file = here("PrepData","cleanData_ZH_retest.txt"), header = TRUE, sep = ",")

# specify which fit indices to print for SEM
print_fit <- c("npar","chisq","df","pvalue","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper")

# descriptive statistics ----
summary_stats <- as.data.frame(describe(data_BMW3_merged[,5:16]))
summary_stats <- summary_stats %>% 
  mutate(item_difficulty = (mean/max)*100)

# This code creates a table of summary statistics for the variables in the dataset
knitr::kable(round(summary_stats,2))

# create histograms of the BMW3 variables in the data_BMW3_merged dataset. The histogram displays the distribution of the variable, along with the normal distribution curve.
p_UN1 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_UN_01)) + 
  geom_histogram(mapping = aes(x = BMW3_UN_01, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_UN_01), sd = sd(data_BMW3_merged$BMW3_UN_01))) +
  theme_classic()

p_UN2 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_UN_02)) + 
  geom_histogram(mapping = aes(x = BMW3_UN_02, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_UN_02), sd = sd(data_BMW3_merged$BMW3_UN_02))) +
  theme_classic()

p_UN3 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_UN_03)) + 
  geom_histogram(mapping = aes(x = BMW3_UN_03, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_UN_03), sd = sd(data_BMW3_merged$BMW3_UN_03))) +
  theme_classic()

p_UN4 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_UN_04)) + 
  geom_histogram(mapping = aes(x = BMW3_UN_04, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_UN_04), sd = sd(data_BMW3_merged$BMW3_UN_04))) +
  theme_classic()

p_IN1 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_IN_01)) + 
  geom_histogram(mapping = aes(x = BMW3_IN_01, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_IN_01), sd = sd(data_BMW3_merged$BMW3_IN_01))) +
  theme_classic()

p_IN2 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_IN_02)) + 
  geom_histogram(mapping = aes(x = BMW3_IN_02, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_IN_02), sd = sd(data_BMW3_merged$BMW3_IN_02))) +
  theme_classic()

p_IN3 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_IN_03)) + 
  geom_histogram(mapping = aes(x = BMW3_IN_03, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_IN_03), sd = sd(data_BMW3_merged$BMW3_IN_03))) +
  theme_classic()

p_IN4 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_IN_04)) + 
  geom_histogram(mapping = aes(x = BMW3_IN_04, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_IN_04), sd = sd(data_BMW3_merged$BMW3_IN_04))) +
  theme_classic()

p_ME1 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_ME_01)) + 
  geom_histogram(mapping = aes(x = BMW3_ME_01, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_ME_01), sd = sd(data_BMW3_merged$BMW3_ME_01))) +
  theme_classic()

p_ME2 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_ME_02)) + 
  geom_histogram(mapping = aes(x = BMW3_ME_02, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_ME_02), sd = sd(data_BMW3_merged$BMW3_ME_02))) +
  theme_classic()

p_ME3 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_ME_03)) + 
  geom_histogram(mapping = aes(x = BMW3_ME_03, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_ME_03), sd = sd(data_BMW3_merged$BMW3_ME_03))) +
  theme_classic()

p_ME4 <- data_BMW3_merged %>% 
  ggplot(aes(x = BMW3_ME_04)) + 
  geom_histogram(mapping = aes(x = BMW3_ME_04, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_BMW3_merged$BMW3_ME_04), sd = sd(data_BMW3_merged$BMW3_ME_04))) +
  theme_classic()

plot_itemDist <- (p_UN1 | p_UN2 | p_UN3 | p_UN4) / (p_IN1 | p_IN2 | p_IN3 | p_IN4) / (p_ME1 | p_ME2 | p_ME3 | p_ME4)

ggsave(filename = here("figures","itemDist_German.jpg"),
       width = 12, height = 9)


# PCA ----

# inspect scree plot

scree(data_BMW3_merged[,5:16],pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 

# pca with rotates components
pca_results <- principal(data_BMW3_merged[,5:16], nfactors = 3, rotate= "oblimin", covar = FALSE)
round(pca_results[["r.scores"]],2) # Are the three factors correlated?

pca_results[["loadings"]]

# Internal consistencies ----

alpha(data_BMW3_merged[,5:8])
alpha(data_BMW3_merged[,13:16])
alpha(data_BMW3_merged[,9:12])


# CFA ----

## one-factor model
# read SEM syntax for 1-factor model
model_1_factor <- readLines(here("SEM_models","BMW3_1Factor.sem"))

# fit 1-factor model
fit_1_factor <- sem(model = model_1_factor, data = data_BMW3_merged, estimator = "ML" , missing = "fiml")

# print results
fitMeasures(fit_1_factor, fit.measures = print_fit)
summary(fit_1_factor, fit.measures=TRUE,standardized = TRUE,rsquare=T)
semPaths(fit_1_factor, intercepts = FALSE, color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                                                        man = rgb(155, 253, 175, maxColorValue = 255)), mar = c(10, 5, 10, 5))

## two-factor model
# read SEM syntax for 2-factor model
model_2_factors <- readLines(here("SEM_models","BMW3_2Factor.sem"))

# fit 2-factor model
fit_2_factors <- sem(model = model_2_factors, data = data_BMW3_merged, estimator = "ML" , missing="fiml")

# print results
fitMeasures(fit_2_factors, fit.measures = print_fit)
summary(fit_2_factors, fit.measures=TRUE,standardized = TRUE,rsquare=T)
semPlot::semPaths(fit_2_factors, intercepts = FALSE, color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                                                         man = rgb(155, 253, 175, maxColorValue = 255)), mar = c(10, 5, 10, 5))

## three-factor model - best-fitting model

# read SEM syntax for 3-factor model
model_3_factors <- readLines(here("SEM_models","BMW3_3Factor.sem"))

# fit 3-factor model
fit_3_factors <- sem(model = model_3_factors, data = data_BMW3_merged, estimator = "ML" , missing="fiml")

# print results
fitMeasures(fit_3_factors, fit.measures = print_fit)
summary(fit_3_factors, fit.measures=TRUE,standardized = TRUE,rsquare=T)
semPlot::semPaths(fit_3_factors, intercepts = FALSE, color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                                                         man = rgb(155, 253, 175, maxColorValue = 255)), mar = c(10, 5, 10, 5))

# print standardized solution
standardizedsolution(fit_3_factors, level = .90)

## three-factor model uncorrelated
# read SEM syntax for 3-factor model
# model_3_factors_orthogonal <- readLines(here("SEM_models","BMW3_3Factor_orthogonal.sem"))

# fit 3-factor model without correlations between factors
fit_3_factors_orthogonal <- sem(model = model_3_factors, data = data_BMW3_merged, estimator = "ML" , missing="fiml",
                                orthogonal = TRUE) # constrain correlations between factors to zero

# print results
fitMeasures(fit_3_factors_orthogonal, fit.measures = print_fit)
summary(fit_3_factors_orthogonal, fit.measures=TRUE,standardized = TRUE,rsquare=T)
semPlot::semPaths(fit_3_factors_orthogonal, intercepts = FALSE, color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                                                                    man = rgb(155, 253, 175, maxColorValue = 255)), mar = c(10, 5, 10, 5))

# compare 1-, 2-, and 3-factor model of the BMW-3
anova(fit_1_factor, fit_2_factors, fit_3_factors)

# evaluate if correlated or uncorrelated model fits better
anova(fit_3_factors, fit_3_factors_orthogonal)

# stability ----

data_retest <- data_ZH_retest %>% 
  select(BMW3_UN_t1, BMW3_UN_t2, BMW3_IN_t1, BMW3_IN_t2, BMW3_ME_t1, BMW3_ME_t2)

apa.cor.table(data_retest)
# This code computes the correlation between the BMW3_UN, BMW3_IN, and BMW3_ME variables at time 1 and time 2. 
cor.test(data_retest$BMW3_UN_t1, data_retest$BMW3_UN_t2)
cor.test(data_retest$BMW3_IN_t1, data_retest$BMW3_IN_t2)
cor.test(data_retest$BMW3_ME_t1, data_retest$BMW3_ME_t2)

cocor(~BMW3_UN_t1 + BMW3_UN_t2 | BMW3_IN_t1 + BMW3_IN_t2, data_retest)
cocor(~BMW3_UN_t1 + BMW3_UN_t2 | BMW3_ME_t1 + BMW3_ME_t2, data_retest)
cocor(~BMW3_ME_t1 + BMW3_ME_t2 | BMW3_IN_t1 + BMW3_IN_t2, data_retest)

# read SEM syntax and fit model
model_LST <- readLines(here("SEM_models","BMW3_LST.sem"))
fit_LST <- sem(model = model_LST, data = data_ZH_retest, estimator = "ML" , missing="fiml")

# print results
fitMeasures(fit_LST, fit.measures = print_fit)
summary(fit_LST, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_LST, level = .90)

# read SEM syntax and fit model
model_retest <- readLines(here("SEM_models","BMW3_ReTest.sem"))
fit_retest <- sem(model = model_retest, data = data_ZH_retest, estimator = "ML" , missing="fiml")

# print results
fitMeasures(fit_retest, fit.measures = print_fit)
summary(fit_retest, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_retest, level = .90)
