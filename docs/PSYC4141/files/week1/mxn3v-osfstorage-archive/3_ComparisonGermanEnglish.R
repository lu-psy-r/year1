#' This script contains the code for the comparison between the german and
#' the english version of the BMW3 questionnaire.
#' 
#' First the same psychometrics analyses that have been performed for the german
#' version in the script "1_PsychometricAnalyses.R" are also performed for the english
#' version. Then, the german and english version of the BMW3 are evaluated for 
#' measurement invariance. And finally, the relationship of the BMW3 scales with
#' personality questionnaires is evaluated for the English version of the BMW3.
#' 
#' Please note, to ensure that all files are loaded correctly and the relative
#' paths work, you need to first open the R-project file "BMW3-Analyses.Rpro"
#' and then open the respective R scripts.
#' 
#' If you have questions regarding theses scripts please contact:
#'    Anna-Lena Schubert, anna-lena.schubert@uni-mainz.de OR
#'    Gidon Frischkorn, gidon.frischkorn@psychologie.uzh.ch

# start fresh
rm(list = ls())   # clean up workspace
graphics.off()  # switch off graphics device

# load libraries ----
pacman::p_load(tidyverse, psych, lavaan, semPlot, patchwork, apaTables, here,
               emmeans, lme4, lmerTest, sjPlot, effectsize, cocor)

# load data
data_UNCG_personality_merged <- read.table(file = here("PrepData", "cleanData_UNCG_personality_merged.txt"), header = TRUE, sep = ",")
data_measurement_invariance <- read.table(file = here("PrepData", "cleanData_measurement_invariance.txt"), header = TRUE, sep = ",")
data_personality_merged <- read.table(file = here("PrepData", "cleanData_personality_merged.txt"), header = TRUE, sep = ",")

# specify which fit indices to print for SEM
print_fit <- c("npar","chisq","df","pvalue","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper")

#################################################################################################
## Set of analyses designed to evaluate the English version of the questionnaire               ##
#################################################################################################

# descriptive statistics ----

summary_stats <- as.data.frame(describe(data_UNCG_personality_merged[,8:19]))
summary_stats <- summary_stats %>% 
  mutate(item_difficulty = (mean/max)*100)

knitr::kable(round(summary_stats,2))

p_UN1 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_UN_01)) + 
  geom_histogram(mapping = aes(x = BMW3_UN_01, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_UN_01), sd = sd(data_UNCG_personality_merged$BMW3_UN_01))) +
  theme_classic()

p_UN2 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_UN_02)) + 
  geom_histogram(mapping = aes(x = BMW3_UN_02, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_UN_02), sd = sd(data_UNCG_personality_merged$BMW3_UN_02))) +
  theme_classic()

p_UN3 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_UN_03)) + 
  geom_histogram(mapping = aes(x = BMW3_UN_03, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_UN_03), sd = sd(data_UNCG_personality_merged$BMW3_UN_03))) +
  theme_classic()

p_UN4 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_UN_04)) + 
  geom_histogram(mapping = aes(x = BMW3_UN_04, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_UN_04), sd = sd(data_UNCG_personality_merged$BMW3_UN_04))) +
  theme_classic()


p_IN1 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_IN_01)) + 
  geom_histogram(mapping = aes(x = BMW3_IN_01, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_IN_01), sd = sd(data_UNCG_personality_merged$BMW3_IN_01))) +
  theme_classic()

p_IN2 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_IN_02)) + 
  geom_histogram(mapping = aes(x = BMW3_IN_02, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_IN_02), sd = sd(data_UNCG_personality_merged$BMW3_IN_02))) +
  theme_classic()

p_IN3 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_IN_03)) + 
  geom_histogram(mapping = aes(x = BMW3_IN_03, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_IN_03), sd = sd(data_UNCG_personality_merged$BMW3_IN_03))) +
  theme_classic()

p_IN4 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_IN_04)) + 
  geom_histogram(mapping = aes(x = BMW3_IN_04, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_IN_04), sd = sd(data_UNCG_personality_merged$BMW3_IN_04))) +
  theme_classic()

p_ME1 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_ME_01)) + 
  geom_histogram(mapping = aes(x = BMW3_ME_01, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_ME_01), sd = sd(data_UNCG_personality_merged$BMW3_ME_01))) +
  theme_classic()

p_ME2 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_ME_02)) + 
  geom_histogram(mapping = aes(x = BMW3_ME_02, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_ME_02), sd = sd(data_UNCG_personality_merged$BMW3_ME_02))) +
  theme_classic()

p_ME3 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_ME_03)) + 
  geom_histogram(mapping = aes(x = BMW3_ME_03, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_ME_03), sd = sd(data_UNCG_personality_merged$BMW3_ME_03))) +
  theme_classic()

p_ME4 <- data_UNCG_personality_merged %>% 
  ggplot(aes(x = BMW3_ME_04)) + 
  geom_histogram(mapping = aes(x = BMW3_ME_04, y=..density..),bins = 5, position = "dodge",fill = "steelblue", colour="black") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_UNCG_personality_merged$BMW3_ME_04), sd = sd(data_UNCG_personality_merged$BMW3_ME_04))) +
  theme_classic()

plot_itemDist <- (p_UN1 | p_UN2 | p_UN3 | p_UN4) / (p_IN1 | p_IN2 | p_IN3 | p_IN4) / (p_ME1 | p_ME2 | p_ME3 | p_ME4)

ggsave(filename = here("figures","itemDist_English.jpg"), width = 12, height = 9)

# PCA ----

# inspect scree plot

scree(data_UNCG_personality_merged[,8:19],pc=TRUE,main="Scree plot",hline=NULL,add=FALSE) 

# pca with rotates components
pca_results <- principal(data_UNCG_personality_merged[,8:19], nfactors = 3, rotate= "oblimin", covar = FALSE)
round(pca_results[["r.scores"]],2) # Are the three factors correlated?

pca_results[["loadings"]]

# Internal consistencies ----

alpha(data_UNCG_personality_merged[,8:11]) 
alpha(data_UNCG_personality_merged[,12:15])
alpha(data_UNCG_personality_merged[,16:19]) 

# CFA ----

# read SEM syntax for 1-factor model
model_1_factor <- readLines(here("SEM_models","BMW3_1Factor.sem"))

# fit 1-factor model
fit_1_factor <- sem(model = model_1_factor, data = data_UNCG_personality_merged, estimator = "ML" , missing = "fiml")

# print results
fitMeasures(fit_1_factor, fit.measures = print_fit)
summary(fit_1_factor, fit.measures=TRUE,standardized = TRUE,rsquare=T)
semPlot::semPaths(fit_1_factor, intercepts = FALSE, color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                                                        man = rgb(155, 253, 175, maxColorValue = 255)), mar = c(10, 5, 10, 5))

## two-factor model
# read SEM syntax for 2-factor model
model_2_factors <- readLines(here("SEM_models","BMW3_2Factor.sem"))

# fit 2-factor model
fit_2_factors <- sem(model = model_2_factors, data = data_UNCG_personality_merged, estimator = "ML" , missing="fiml")

# print results
fitMeasures(fit_2_factors, fit.measures = print_fit)
summary(fit_2_factors, fit.measures=TRUE,standardized = TRUE,rsquare=T)
semPlot::semPaths(fit_2_factors, intercepts = FALSE, color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                                                                  man = rgb(155, 253, 175, maxColorValue = 255)), mar = c(10, 5, 10, 5))

## three-factor model - best-fitting model

# read SEM syntax for 3-factor model
model_3_factors <- readLines(here("SEM_models","BMW3_3Factor.sem"))

# fit 3-factor model
fit_3_factors <- sem(model = model_3_factors, data = data_UNCG_personality_merged, estimator = "ML" , missing="fiml")

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
fit_3_factors_orthogonal <- sem(model = model_3_factors, data = data_UNCG_personality_merged, estimator = "ML" , missing="fiml",
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

# measurement invariance ----

model_invariance <- readLines(here("SEM_models","BMW3_3Factor.sem"))

fit_configural <- sem(model = model_invariance, data = data_measurement_invariance, estimator = "ML" , missing="fiml", group = "language")
fitMeasures(fit_configural, fit.measures = print_fit)
summary(fit_configural, fit.measures=TRUE,standardized = TRUE,rsquare=T)

fit_metric <- sem(model = model_invariance, data = data_measurement_invariance, estimator = "ML" , missing="fiml", group = "language", 
                  group.equal = c("loadings"))
fitMeasures(fit_metric, fit.measures = print_fit)
summary(fit_metric, fit.measures=TRUE,standardized = TRUE,rsquare=T)

fit_metric_cov <- sem(model = model_invariance, data = data_measurement_invariance, estimator = "ML" , missing="fiml", group = "language", 
                      group.equal = c("loadings", "lv.covariances"))
fitMeasures(fit_metric_cov, fit.measures = print_fit)
summary(fit_metric_cov, fit.measures=TRUE,standardized = TRUE,rsquare=T)

fit_scalar <- sem(model = model_invariance, data = data_measurement_invariance, estimator = "ML" , missing="fiml", group = "language",
                  group.equal = c("loadings", "intercepts"))
fitMeasures(fit_scalar, fit.measures = print_fit)
summary(fit_scalar, fit.measures=TRUE,standardized = TRUE,rsquare=T)

lavTestLRT(fit_configural, fit_metric, fit_metric_cov, fit_scalar)

# correlations between MW questionnaires ----

data_MW_questionnaires <- data_UNCG_personality_merged %>% 
  select(BMW3_UN, BMW3_IN, BMW3_ME, MW_S, MW_D,MAAS) 

apa.cor.table(data_MW_questionnaires)

model_MW_questionnaires <-  readLines(here("SEM_models","BMW3_ConvergentValidity.sem"))
fit_MW_questionnaires <- sem(model = model_MW_questionnaires, data = data_UNCG_personality_merged, estimator = "ML" , missing="fiml")

fitMeasures(fit_MW_questionnaires, fit.measures = print_fit)
summary(fit_MW_questionnaires, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_MW_questionnaires, level = .90)
# correlations with BIG 5 ----

data_BIG5 <- data_UNCG_personality_merged %>% 
  select(BMW3_UN, BMW3_IN, BMW3_ME, IPIP_E, IPIP_A, IPIP_S, IPIP_I, IPIP_C) 

apa.cor.table(data_BIG5)

model_BIG5 <- readLines(here("SEM_models","BMW3_Big5.sem"))
fit_BIG5 <- sem(model = model_BIG5, data = data_UNCG_personality_merged, estimator = "ML" , missing="fiml")

fitMeasures(fit_BIG5, fit.measures = print_fit)
summary(fit_BIG5, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_BIG5, level = .90)
