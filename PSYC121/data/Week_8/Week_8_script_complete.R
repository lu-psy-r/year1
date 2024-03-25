library(tidyverse)

# TASK 1 - data processing

data_w8 <- read_csv("stroop.csv")

view(data_w8) # view the data

data_w8 %>% 
  ggplot() +
  geom_density(aes(x = time, fill = condition), alpha = .8) +# you need to EDIT this for Q4
  theme_dark()

# create a new column which is the average time
data_w8 <- 
  data_w8 %>% 
  group_by(pID) %>% # for each participant...
  mutate(avg_time = mean(time)) # create a new average time from the mean of the time column

view(data_w8) # view the data

# distribution of average times
data_w8 %>% 
  ggplot() +
  geom_histogram(aes(x = avg_time), bins = 40) # you need to EDIT this for Q6

# Do we need to filter out outliers?
# add a filter to keep data within a certain range
data_w8_f <- 
  data_w8 %>% 
  filter(avg_time > 1 & avg_time < 10) # you need to EDIT this for Q7

# distribution of average times
data_w8_f %>% 
  ggplot() +
  geom_histogram(aes(x = avg_time), bins = 20) # you need to EDIT this for Q6

# TASK 2 - conducting related samples t-tests

# calculate the means for the 3 levels of the condition IV
data_w8_f %>%
  group_by(condition) %>% # you need to EDIT this for Q1
  summarise(stroop_mean = mean(time)) # you need to EDIT this for Q1

# filter is used to select two levels of the IV - Q3-5
stroop_comparison <-
  data_w8_f %>%
  filter(condition == "compatible" | condition == "incompatible")

# run the t-test comparing the means of these two levels
t.test(data = stroop_comparison, time ~ condition, paired = TRUE)



# TASK 3 - summarising and visualisation 

# calculate the means for the 3 levels of the condition IV
data_w8_summary <-# Notice we are creating/overwriting this object here 
  data_w8_f %>%
  group_by(condition) %>% # you need to EDIT this for Q1
  summarise(stroop_mean = mean(time),
            stroop_SE = sd(time)/sqrt(n())) # you need to EDIT the sd() for Q1

view(data_w8_summary) # if you've done the above step correctly, you should have different means and SEs for each condition

# let's first plot the means
data_w8_summary %>%
  ggplot(aes(x = condition, y = stroop_mean)) + # map variables to x and y for Q3
  geom_point(size = 5)

# let's do the same again, but now with error bars
data_w8_summary %>%
  ggplot(aes(x = condition, y = stroop_mean)) + # map variables to x and y
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = stroop_mean - stroop_SE, # edit this for Q5
                    ymax = stroop_mean + stroop_SE), # edit this for Q5
                width = .2) 





