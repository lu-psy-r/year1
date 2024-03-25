library(tidyverse)

# TASK 1 - Exploring the data

# read the data in - Q1
data_wk9 <- read_csv("data_wk9.csv")

# compute summary statistics - Q2
summary(data_wk9)

# ggplot - Q3
data_wk9 %>% 
  ggplot(aes(x = pop_est_christian)) + # example - they can do any/all numeric cols
  geom_histogram(bins = 10) 

# TASK 2 - Visualising relationships

# ggplot - compare numeric variables Q1
data_wk9 %>% 
  ggplot(aes(x = pop_est_christian, y = pop_est_muslim)) + # example - they can do any/all numeric cols
  geom_point()

# TASK 3 - using z-scores to remove outliers

data_wk9_z <- 
  data_wk9 %>% 
  mutate(z_imm = scale(pop_est_immigrants)) # create z score of this column - Q2

# descriptives of this column - Q3
mean(data_wk9_z$z_imm)
sd(data_wk9_z$z_imm)
max(data_wk9_z$z_imm)
min(data_wk9_z$z_imm)

# histogram of z-scores - Q5
data_wk9_z %>% 
  ggplot(aes(x = z_imm)) +
  geom_histogram()

# filter out high z-scores
data_wk9_z_f <- 
  data_wk9_z %>% 
  filter(z_imm > -2.5 & z_imm < 2.5)

# TASK 4 - Unrelated samples t-test

# calculated summary stats - Q1
data_wk9_z_f %>% 
  group_by(home_location_in_UK) %>% 
  summarise(mean_est_muslim = mean(pop_est_muslim),
            sample_size = n())

# check if variances are unequal (p < .05) - Q3
var.test(pop_est_muslim ~ home_location_in_UK, data = data_wk9_z_f)

# run unrelated samples t-test
t.test(pop_est_muslim ~ home_location_in_UK, data = data_wk9_z_f, 
       paired = FALSE, 
       var.equal = TRUE)

# consider practicing these steps with the other DVs

# TASK 5 - power and effect size calculations
library(effectsize)
library(pwr)

stroop_data <- read_csv("stroop.csv") # Q2

# Q3
cohens_d(time ~ condition, 
         data = stroop_data,
         paired = TRUE)

pwr.t.test(d = 1.27, n = 20) #Q4

pwr.t.test(d = 1.27, power = .8) # Q5

pwr.t.test(n = 40, power = .9) # Q6
  