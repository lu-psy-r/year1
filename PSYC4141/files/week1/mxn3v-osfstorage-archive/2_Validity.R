#' This script contains the code for the analyses of the validity of the BMW-3.
#' It contains the following analyses:
#'   1. Convergent validity with other Mind Wandering Questionnaires
#'   2. Convergent/divergent validity with personality questionnaires
#'   3. Relationships with cognitive abilities (intelligence & WMC)
#'   4. Relationships with self-reported attentional control
#'   5. Relationships with experience sampling data of Task-Unrelated Thoughts
#'   6. Relationships with depression, rumination, and emotion regulation
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
data_personality_merged <- read.table(file = here("PrepData", "cleanData_personality_merged.txt"), header = TRUE, sep = ",")
data_cog_abilities_merged <- read.table(file = here("PrepData", "cleanData_cog_abilities_merged.txt"), header = TRUE, sep = ",")
data_WMC_merged <- read.table(file = here("PrepData", "cleanData_WMC_merged.txt"), header = TRUE, sep = ",")

data_HD_4 <- read.table(file = here("PrepData", "cleanData_HD4_TUT.txt"), header = TRUE, sep = ",")
data_HD_4_ACS <- read.table(file = here("PrepData", "cleanData_HD4_ACS.txt"), header = TRUE, sep = ",")

data_HD_2_exp_sampling  <- read.table(file = here("PrepData", "cleanData_HD2_ExpSampling.txt"), header = TRUE, sep = ",")
data_HD_2_emo <- read.table(file = here("PrepData", "cleanData_HD2_Emotion.txt"), header = TRUE, sep = ",")

# specify which fit indices to print for SEM
print_fit <- c("npar","chisq","df","pvalue","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper")

###########################################################################
## Set of analyses designed to evaluate the convergent/divergent validity #
## of the questionnaire                                                   #
###########################################################################

# correlations between MW questionnaires ----
data_MW_questionnaires <- data_personality_merged %>% 
  select(BMW3_UN, BMW3_IN, BMW3_ME, MW_S, MW_D, MAAS) 

apa.cor.table(data_MW_questionnaires)

model_MW_questionnaires <- readLines(here("SEM_models","BMW3_ConvergentValidity.sem"))
fit_MW_questionnaires <- sem(model = model_MW_questionnaires, data = data_personality_merged, estimator = "ML")

fitMeasures(fit_MW_questionnaires, fit.measures = print_fit)
summary(fit_MW_questionnaires, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_MW_questionnaires, level = .90)

# correlations with BIG 5 ----

data_BIG5 <- data_personality_merged %>% 
  select(BMW3_UN, BMW3_IN, BMW3_ME, IPIP_E, IPIP_A, IPIP_S, IPIP_I, IPIP_C) 

apa.cor.table(data_BIG5)

model_BIG5 <- readLines(here("SEM_models","BMW3_Big5.sem"))
fit_BIG5 <- sem(model = model_BIG5, data = data_personality_merged, estimator = "ML")

fitMeasures(fit_BIG5, fit.measures = print_fit)
summary(fit_BIG5, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_BIG5, level = .90)

# correlations with cognitive abilities ----

# general cognitive abilities 
data_g <- data_cog_abilities_merged %>% 
  select(BMW3_UN, BMW3_IN, BMW3_ME, zPC_Sum, zPS_Sum, zM_Sum, zC_Sum) 

apa.cor.table(data_g)

model_g <- readLines(here("SEM_models","BMW3_CognitiveAbilities.sem"))
fit_g <- sem(model = model_g, data = data_cog_abilities_merged, estimator = "ML", missing = "FIML")

fitMeasures(fit_g, fit.measures = print_fit)
summary(fit_g, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_g, level = .90)

model_g_fixed <- readLines(here("SEM_models","BMW3_CognitiveAbilities_fixed.sem"))
fit_g_fixed <- sem(model = model_g_fixed, data = data_cog_abilities_merged, estimator = "ML", missing = "FIML")
anova(fit_g, fit_g_fixed)

# working memory capacity

data_WMC <- data_WMC_merged %>% 
  select(BMW3_UN, BMW3_IN, BMW3_ME, SspanPartialScoreBlock1, SspanPartialScoreBlock2, OspanPartialScoreBlock1, OspanPartialScoreBlock2) 

apa.cor.table(data_WMC)

model_WMC <- readLines(here("SEM_models","BMW3_WMC.sem"))
fit_WMC <- sem(model = model_WMC, data = data_WMC_merged, estimator = "ML", missing = "FIML")

fitMeasures(fit_WMC, fit.measures = print_fit)
summary(fit_WMC, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_WMC, level = .90)

model_WMC_fixed <- readLines(here("SEM_models","BMW3_WMC_fixed.sem"))
fit_WMC_fixed <- sem(model = model_WMC_fixed, data = data_WMC_merged, estimator = "ML", missing = "FIML")

anova(fit_WMC, fit_WMC_fixed)

# WMC and TUT rates

data_WMC_HD_4 <- data_WMC_merged %>% 
  filter(id == 4) %>% 
  select(subject, SspanPartialScoreBlock1, SspanPartialScoreBlock2, OspanPartialScoreBlock1, OspanPartialScoreBlock2) %>% 
  mutate(subject = subject -41)

data_HD_4_rev <- data_HD_4 %>% 
  mutate(subject = 1:length(data_HD_4[,1])) %>% 
  left_join(data_WMC_HD_4) %>% 
  mutate(SspanPartialScoreBlock1_z = as.numeric(scale(SspanPartialScoreBlock1)),
         SspanPartialScoreBlock2_z = as.numeric( scale(SspanPartialScoreBlock2)),
         OspanPartialScoreBlock1_z = as.numeric( scale(OspanPartialScoreBlock1)),
         OspanPartialScoreBlock2_z = as.numeric( scale(OspanPartialScoreBlock2)))

model_TUT_WMC <- readLines(here("SEM_models","TUT_WMC.sem"))
fit_TUT_WMC <- sem(model = model_TUT_WMC, data = data_HD_4_rev, estimator = "ML", missing = "FIML")

fitMeasures(fit_TUT_WMC, fit.measures = print_fit)
summary(fit_TUT_WMC, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_TUT_WMC, level = .90)

# attentional control
data_ACS <- data_HD_4_ACS %>% 
  select(BMW3_UN, BMW3_IN, BMW3_ME, ACS_sum)  

apa.cor.table(data_ACS)

model_ACS <- readLines(here("SEM_models","BMW3_ACS.sem"))

fit_ACS <- sem(model = model_ACS, data = data_HD_4_ACS, estimator = "ML", missing = "FIML")

fitMeasures(fit_ACS, fit.measures = print_fit)
summary(fit_ACS,standardized = TRUE,rsquare=T)
standardizedsolution(fit_ACS, level = .90)

#################################################################################################
## Set of analyses designed to evaluate the criterion validity of the questionnaire            ##
#################################################################################################

# prediction of MW assessed with experience sampling ----
ema_model <- glmer(MW_dummy ~ Abfrage + BMW3_UN_z + BMW3_IN_z + 
              (1 |Participant) + (0 + Tag_App|Participant),   
            data= data_HD_2_exp_sampling, family=binomial(link = "logit"))
summary(ema_model) 

sjPlot::tab_model(ema_model, show.se = TRUE, show.icc	= TRUE, show.ci = 0.90)
effectsize::standardize_parameters(ema_model, method = "refit", two_sd = TRUE, exponentiate = TRUE)

confint(ema_model, parm="beta_",method="Wald")

# without time of day as a predictor
ema_model_reduced <- glmer(MW_dummy ~ BMW3_UN_z + BMW3_IN_z + 
                             (1 |Participant) + (0 + Tag_App|Participant),   
                           data= data_HD_2_exp_sampling, family=binomial(link = "logit"))
summary(ema_model_reduced) 

sjPlot::tab_model(ema_model_reduced, show.se = TRUE, show.icc	= TRUE, show.ci = 0.90)
effectsize::standardize_parameters(ema_model_reduced, method = "refit", two_sd = TRUE, exponentiate = TRUE)

confint(ema_model_reduced, parm="beta_",method="Wald")

# including meta-awareness as a predictor
ema_model_ma <- glmer(MW_dummy ~ Abfrage + BMW3_UN_z + BMW3_IN_z + BMW3_ME_z + 
                     (1 |Participant) + (0 + Tag_App|Participant),   
                   data= data_HD_2_exp_sampling, family=binomial(link = "logit"))
summary(ema_model_ma) 

sjPlot::tab_model(ema_model_ma, show.se = TRUE, show.icc	= TRUE, show.ci = 0.90)
effectsize::standardize_parameters(ema_model_ma, method = "refit", two_sd = TRUE, exponentiate = TRUE)

confint(ema_model_ma, parm="beta_",method="Wald")

# average TUT rates per day

data_HD_2_exp_sampling %>% 
  group_by(Participant) %>% 
  summarise(meanTUT = mean(MW_dummy, na.rm = T)) %>% 
  ungroup() %>% 
  summarise(avg_TUT_rate = mean(meanTUT, na.rm = T),
            sd_TUT_rate = sd(meanTUT, na.rm = T))

# prediction of TUT rates in lab studies ----

model_TUT <- readLines(here("SEM_models","BMW3_TUT.sem"))

fit_MW_TUT <- sem(model = model_TUT, data = data_HD_4, estimator = "ML", missing = "FIML")
fitMeasures(fit_MW_TUT, fit.measures = print_fit)
summary(fit_MW_TUT,standardized = TRUE,rsquare=T)
standardizedsolution(fit_MW_TUT, level = .90)

summary(fit_MW_TUT, fit.measures=TRUE,standardized = TRUE,rsquare=T)

# fixing the regression of unintentional TUT rates on the BMW3-I and of the
# intentional TUT rates on the BMW3-UI to zero

model_TUT_fixed <- readLines(here("SEM_models","BMW3_TUT_fixed.sem"))

fit_MW_TUT_fixed <- sem(model = model_TUT_fixed, data = data_HD_4, estimator = "ML", missing = "FIML")
fitMeasures(fit_MW_TUT_fixed, fit.measures = print_fit)
summary(fit_MW_TUT_fixed,standardized = TRUE,rsquare=T)
standardizedsolution(fit_MW_TUT_fixed, level = .90)

summary(fit_MW_TUT_fixed, fit.measures=TRUE,standardized = TRUE,rsquare=T)

anova(fit_MW_TUT, fit_MW_TUT_fixed)

# including meta-awareness as a predictor
model_TUT_MA <- readLines(here("SEM_models","BMW3_TUT_MA.sem"))

fit_MW_TUT_MA <- sem(model = model_TUT_MA, data = data_HD_4, estimator = "ML", missing = "FIML")
fitMeasures(fit_MW_TUT_MA, fit.measures = print_fit)
summary(fit_MW_TUT_MA,standardized = TRUE,rsquare=T)
standardizedsolution(fit_MW_TUT_MA, level = .90)

summary(fit_MW_TUT_MA, fit.measures=TRUE,standardized = TRUE,rsquare=T)

# average TUT rates

data_HD_4 %>% 
  group_by(recode) %>% 
  mutate(unint_TUT_rate = mean(c(MWsum_unint_nback_easy/8, MWsum_unint_nback_difficult/8, MWsum_unint_cmt_easy/8, MWsum_unint_cmt_difficult/8, MWsum_unint_ms_easy/8, MWsum_unint_ms_difficult/8), na.rm = T),
         int_TUT_rate = mean(c(MWsum_int_nback_easy/8, MWsum_int_nback_difficult/8, MWsum_int_cmt_easy/8, MWsum_int_cmt_difficult/8, MWsum_int_ms_easy/8, MWsum_int_ms_difficult/8), na.rm = T)) %>% 
    ungroup() %>% 
  select(unint_TUT_rate, int_TUT_rate) %>% 
    summarise(avg_unint = mean(unint_TUT_rate, na.rm = T),
              avg_int = mean(int_TUT_rate, na.rm = T),
              sd_unint = sd(unint_TUT_rate, na.rm = T),
              sd_int = sd(int_TUT_rate, na.rm = T))

  
# correlations with depressive symptoms and emotion regulation strategies ----
data_emo <- data_HD_2_emo %>% 
  select(BMW3_UN, BMW3_IN, BMW3_ME, Depression, Rumination, Neubewertung, Suppression, Rumination, Moodregulation) 
apa.cor.table(data_emo)

glm_ADS_cutoff <- glm(ADS_cutoff_binary ~ BMW3_UN + BMW3_IN + BMW3_ME, family = binomial(link = "logit"), data = data_HD_2_emo)
summary(glm_ADS_cutoff)
lreg.or <-exp(cbind(OR = coef(glm_ADS_cutoff), confint(glm_ADS_cutoff, level = .90)))
round(lreg.or, digits=2)

model_ADS <-readLines(here("SEM_models","BMW3_Depression.sem"))
fit_ADS <- sem(model = model_ADS, data = data_HD_2_emo, estimator = "MLR")

fitMeasures(fit_ADS, fit.measures = print_fit)
summary(fit_ADS, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_ADS, level = .90)

model_ER <- readLines(here("SEM_models","BMW3_EmotionRegulation.sem"))
fit_ER <- sem(model = model_ER, data = data_HD_2_emo, estimator = "ML")

fitMeasures(fit_ER, fit.measures = print_fit)
summary(fit_ER, fit.measures=TRUE,standardized = TRUE,rsquare=T)
standardizedsolution(fit_ER, level = .90)
