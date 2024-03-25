library(tidyverse)

# TASK 1 - Exploring the data

# read the data in - Q1
*MISSING_CODE*

# compute summary statistics - Q2
*MISSING_CODE*

# ggplot - Q3
*MISSING_data_object_name* %>% 
  ggplot(aes()) + # example - they can do any/all numeric cols
  # what type of geom? 

# TASK 2 - Visualising relationships

# ggplot - compare numeric variables Q1
*MISSING_data_object_name* %>% 
  ggplot(aes()) + # example - they can do any/all numeric cols
  # what type of geom?

# TASK 3 - using z-scores to remove outliers

*MISSING_data_object_name* <- # note you may want to save to a new object here
  *MISSING_data_object_name* %>% 
  mutate(z_imm = scale(*MISSING_DV*)) # create z score of this column - Q2

# descriptives of this column - Q3
mean() # complete...
sd()
max()
min()

# histogram of z-scores - Q5
*MISSING_data_object_name* %>% 
  ggplot(aes()) +
  # what type of geom?

# filter out high z-scores Q6
*MISSING_data_object_name* <- # note you may want to save to a new object here
  *MISSING_data_object_name* %>% 
  filter() # add expression to filter to remove large positive and negative z scores

# TASK 4 - Unrelated samples t-test

# calculated summary stats - Q1
*MISSING_data_object_name* %>% 
  group_by(*MISSING_IV*) %>% 
  summarise(*COMPLETE THIS CODE TO COMPUTE MEANS*,
            sample_size = n())

# check if variances are unequal (p < .05) - Q3
var.test(pop_est_muslim ~ home_location_in_UK, data = *MISSING_data_object_name*)

# run unrelated samples t-test
t.test(pop_est_muslim ~ home_location_in_UK, data = *MISSING_data_object_name*, 
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

pwr.t.test(d = *MISSING_value*, n = *MISSING_value*) #Q4

pwr.t.test(d = *MISSING_value*, power = *MISSING_value*) # Q5

pwr.t.test(n = *MISSING_value*, power = *MISSING_value*) # Q6
  