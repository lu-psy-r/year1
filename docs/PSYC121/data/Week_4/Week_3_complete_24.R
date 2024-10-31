# Here's your friendly reminder that you can make noies using the comments lines (displayed in green in the script)
# Use notes to write comments to help you remember what you do and what things mean


# Make sure you run the script lines. Some students have missed the next line and the script won't play nice without it
library(tidyverse)

# What does the last line do? We can ask R to tell us more, see the next line
help(tidyverse)
help(read_csv)
help(median)

# Start by setting up a project folder for 'week 3". Then open that directory and set this as the working directory.


cows <- read_csv("penelope22.csv") # modified code to read data
cows <- read_csv(file.choose()) # Or use this to open a file dialog box

# Here is one way to check this looks like the right data

View(cows)

# so now we want to explore 2-dimension data, looking at the 'estimate' score broken down by identity

aggregate(x = cows$estimate, by = list(cows$identity), FUN = mean)

# aggregate?? What's that? OK, let's ask for help (option 2 but you can use help() as well!)

?aggregate

# This time, let's try to 'group_by() to look at 2-dimensional data. The next line will need editing!

cows %>% group_by(identity) %>% summarise(mean_estimate = mean(estimate)) # modified code for aggregate 
options(pillar.sigfig=4)  # If you don't like the look of the tibble data readout, try ask this and then repeat the above line


# Dataset 2: 
# Try to find out whether salary estimates changed with region of the UK
# Build up commands below using what you have already done, including the following (see lab sheet as well)

money <- read_csv("wages2024.csv") # example code section for wages datafile
glimpse(money)
aggregate(x = money$salary, by = list(money$uk_region), FUN = mean)
aggregate(x = money$salary, by = list(money$family_position), FUN = mean)
aggregate(x = money$salary, by = list(money$uk_region,money$family_position), FUN = mean)

# WE DO NOT ASK FOR THIS AT ALL, but if someone wanted to know how many values went in each option:
aggregate(x = money$salary, by = list(money$uk_region), FUN = length)

money %>% group_by(uk_region) %>% summarise(mean_estimate = mean(salary))
# R does not like the NAs in the data and they can be removed this way
money %>% drop_na(uk_region) %>% group_by(uk_region) %>% summarise(mean_estimate = mean(salary))

# We don't ask for the next line, but in case, we can get both region and family together by:
money %>% group_by(uk_region,family_position) %>% summarise(mean_estimate = mean(salary))
print(money %>% group_by(uk_region,family_position) %>% summarise(mean_estimate = mean(salary)), n=25) # Avoids truncation with n=)


# Dataset 3: 
# What about phone time usage as a function of phone type?
# Build up commands below using what you have already done
phones <- read_csv("screentime2024.csv") 
glimpse(phones)
phones %>% group_by(phone_type) %>% summarise(mean_estimate = mean(usage))

# To calculate the z score you can use something like this (also done by hand is fine)
mean(phones$usage)
sd(phones$usage)
z_task <- mean(phones$usage) - 1.5*(sd(phones$usage))
print(z_task)

# or
M <- mean(phones$usage)
StDev <- sd(phones$usage)
z_task_2 <- M - 1.5*StDev
print(z_task_2)


# Finally - can you produce a visualisation of the phone usage data as a function of the type of phone/

boxplot(phones$usage ~ phones$phone_type)
phones %>% ggplot(aes(x = phone_type, y = usage)) + geom_violin()


