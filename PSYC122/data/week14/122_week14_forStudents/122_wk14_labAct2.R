# PSYC122: Week 14 - Lab activity 2

# Background ------------------------------------------------------------
# What we want to do is to run a chi-square analysis to determine whether those in the RTA condition were
# more likely to remember to return the paper-clips than those in the control condition.

# Step 1: Set-up ------------------------------------------------------------------

# Run this code to empty the R environment
rm(list=ls())                            

# Run this code to load relevant libraries
library(lsr)
library(tidyverse)

# TASK: Read in the data file "RTA_study1.csv" (using the read_csv() function), have a look at the layout of
# the data and familiarise yourself with it. You can use the head() function for that.

# WRITE the code to read in data and have a look at the tables



# It is a fairly simple data file that contains four variables for 87 participants:
# 'condition': this variable indicates which condition participants were in,
# 1 = reminder-through-association condition, 2 = control condition
# 'intend': this variable indicates whether participants said they were intending to return the paper-clips,
# 1 = yes, 0 = no
# 'actualdonate': this variable indicates whether participants actually ended up returning the paper-clips and 
# therefore donating to charity, 1 = yes, 0 = no
# 'id': this variable indicates the participant ID number

# Step 2: Data-wrangling ------------------------------------------------------------------
# We need to do a little bit of wrangling to get our data into the format we need:
# First, we need to remove all the participants who said that they did not intend to return the paper-clips
# (intend = 0) as we are only interested in whether people follow through on an intention.
# Second, to make the output easier to read, we’re going to recode condition to have text labels rather than
# numerical values.

# TASK:
# 1) Use filter() to remove all participants who said that they did not intend to return the paper-clips.
# 2) Use mutate() and recode() to recode the values in condition to make 1 = rta and 2 = control and the
# values in actualdonate to 1 = donated and 0 = no_donation.
# 3) Save the output to an object named intent_recode.
#
# HINT: You can do this in two separate steps, or you can use pipes. Also, note that you will need to put
# both sides of each recode argument (i.e., 1 and rta) in quotation marks, even though 1 and 2 are numbers,
# they actually represent categories rather than numerical data.

# WRITE the code




# QUESTION: How many participants were removed because they didn't intend to return the paper-clips?
# ANSWER: 


# Step 3: Descriptive statistics ------------------------------------------------------------------
# Next you need to calculate descriptive statistics. For frequency data these are simply counts so we can use
# the function count() rather than having to use the summarise() function. We want to know how many
# participants are in each group (rta - donated, rta - didn’t donate, control - donated, control - didn’t donate) 
# so we will need to use group_by to display the results for all combinations of 'condition' and 'actualdonate'.
#
# TASK: Use group_by() and then count() to calculate the number of participants in each category. Save it to an object
# named intent_counts.
#
# HINT: To figure out which variables you need to 'group by', think about how we're using each variable.

# WRITE the code




# QUESTION: How many participants in the control condition didn’t donate? 
# ANSWER:
# QUESTION: How many participants in the control condition donated?
# ANSWER:
# QUESTION: How many participants in the rta condition didn’t donate?
# ANSWER:
# QUESTION:  How many participants in the rta condition donated?
# ANSWER:

# You may also want to calculate the percentage of children who fought in each condition.
# Run this code to calculate the percentages:
  intent_percent <- intent_recode %>%
  group_by(condition, actualdonate) %>%
  count() %>%
  ungroup() %>% # ungroups the data
  group_by(condition) %>% # then groups it again but just by condition
  mutate(percent_condition = n/sum(n) * 100)

intent_percent

# Step 4: Bar plot ------------------------------------------------------------------
# 4.1
# We want to create a simple bar plot of our count data.
# We'll use ggplot() for this in combination with the geom_bar() function. The different parts do the following
# - The first line (or layer) sets up the base of the graph: the data to use and the aesthetics (what will go on the x
# and y axis, how the plot will be grouped).
# - The aes() function can take both an x and y argument, however, with a bar plot you are just asking R to count the number of data
# points in each group so you don’t need to specify this.
# - The 'fill' argument will separate the data into each level of the grouping variable and give it a different colour.
# In this case, there is a different coloured bar for each level of 'actualdonate'.
# - The next layer adds a geom or a shape, in this case we use geom_bar() as we want to draw a bar plot.
# - position = "dodge" places the bars next to each other, rather than on top of each other. Try removing this
# argument and just running the code with geom_bar() to see what happens.

# WRITE the code to make the bar plot




# As you can see, the plot makes it much easier to visualise the data - participants in the RTA condition appear to
# have been more likely to remember to donate than those in the control condition.

# 4.2
# The ggplot2 package allows you to customise all aspects of your plots, so let’s tidy ours up a little bit.
# We’re going to do the following:
# - Edit the labels on the x-axis, y-axis and fill
# - Change the colours of the bars by using scale_fill_manual() and specifying the colours we want in the values
# argument
# - Change the theme of the plot to change how it looks

ggplot(data = intent_recode, aes(x = condition, fill = actualdonate)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(name = "Condition", labels = c("Control", "RTA")) +
  scale_y_continuous(name = "Count") +
  scale_fill_manual(name = "Behaviour", labels = c("Donated", "Did not donate"), values = c("blue", "grey"))+
  theme_classic()

# There are a few things to note about the code we just added on:
#   
# The first two lines are the same code as we used in Activity 4, what we’ve done now is add on extra layers.
# If you use simple colour names then you are restricted in the options you can choose, however, if you want more
# flexibility you can use hexadecimal colour codes, see here for more information: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/.
# There are multiple themes that you can apply. If you type theme_ the auto-complete will show you the options - 
# try a few out and see which one you prefer.
# If you want more information on any of these functions, remember you can look at the help documentation by
# typing ?function.

# Step 5: Chi-square ------------------------------------------------------------------
# So, let’s finally run that chi-square analysis to see whether our intuition from the plot holds up and there is a
# significant association between the grouping variables.

# WRITE the code to run the chi-square analysis and don't forget to print the results to the scre
# by 'calling' the object.





# Step 6: Checking assumptions ---------------------------------------------------------
# The assumptions for chi-square are as follows:
# 1) The data in the cells should be frequencies, or counts of cases rather than percentages.
# 2) The levels (or categories) of the variables are mutually exclusive. That is, a particular participant fits into
# one and only one group of each of the variables.
# 3) Each participant may contribute data to one and only one cell. If, for example, the same participants are tested
# over time such that the comparisons are of the same subjects at Time 1, Time 2, Time 3, etc., then Chi-square may
# not be used.
# 4) The study groups must be independent. This means that a different test must be used if the two groups are related.
# For example, a different test must be used if the researcher’s data consists of paired samples, such as in studies
# in which a parent is paired with his or her child.
# 5) There are 2 variables, and both are measured as categories, usually at the nominal level. While Chi-square has no
# rule about limiting the number of cells (by limiting the number of categories for each variable), a very large
# number of cells (over 20) can make it difficult to meet assumption #6 below, and to interpret the meaning of the
# results.
# 6) The expected cell frequencies should be greater than 5.

# Assumptions 1) to 5) should be evaluated by reviewing the design of the study. The only assumption that we need to
# check with R is whether all expected frequencies are greater than 5.

# WRITE the code to check that all expected frequencies are greater than 5



# Step 7: Effect size ------------------------------------------------------------------
# The last step before writing up the results is to calculate an effect size so that we have a standardised measure
# of how large the association between our grouping variables is, the effect size measure for chi-square is Cramer’s V.

# To calculate Cramer’s V we’re going to use the function cramersv() from the lsr package. 

# WRITE the code to calculate Cramer's V




# We can square the effect size (or multiple it by itself) to see how much variance in one 
# variable is accounted for by the other variable

# WRITE the code to square the effect size




# Step 8: Interpreting relationships --------------------------------------
# To help with interpreting the relationships, it is useful to look at the standardised
# residuals. These are z-scores that indicate how many standard deviations above or below
# the expected count, an observed coutn is (thus indicating how much they differ). Remember:
# +/- 1.96 p <.05, +/- 2.58 p <.01, +/-3.29 p < .001

# WRITE the code to check the standardised residuals




# Step 9: Write up -----------------------------------------------------------------
# TASK: Write up the results following APA guidelines.
#
# HINT: The Purdue writing lab website (https://owl.purdue.edu/owl/research_and_citation/apa6_style/apa_formatting_and_style_guide/statistics_in_apa.html)
# is helpful for guidance on punctuating statistics.

# Write up:
