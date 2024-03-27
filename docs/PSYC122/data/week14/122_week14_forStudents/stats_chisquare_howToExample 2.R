# PSYC122: Week 14 - Chi-square in R - Demo

# Background ------------------------------------------------------------
# What we want to do is to run a chi-square analysis to determine whether there is a relationship
# betweeen children's record for fighting in school and their preference for a violent or
# non-violent TV programme.

# Step 1: Set-up ------------------------------------------------------------------

# Run this code to empty the R environment
rm(list=ls())                            

# Run this code to load relevant libraries
library(lsr)
library(tidyverse)

# Run this code to read in data and have a look it
fighting_data <- read_csv("fighting_TV.csv")                                # Read in the data file

head(fighting_data)                                                         # Look at the table

# It is a fairly simple data file that contains 3 variables for 155 participants:
# 'ID': this variable indicates the participant ID number
# 'TV': this variable indicates which TV programme children preferred (violent = 1, non-violent = 0)
# 'Fight': this variable indicates whether children had been in a fight in school (yes = 1, 0 = no)

# Step 2: Data-wrangling ------------------------------------------------------------------
# We need to do a little bit of wrangling to get our data into the format we need:
# To make the output easier to read, we’re going to recode 'TV' and 'Fight' to have text labels
# rather than numerical values.

# TASK:
# 1) Use mutate() and recode() to recode the values in TV' to make 1 = Violent and 0 = Non-violent
# and the values in 'Fight' to 1 = Yes and 0 = No.
# 2) Save the output to an object named fighting_recode.
#
# HINT: You can do this in two separate steps, or you can use pipes. Also, note that you will need to put
# both sides of each recode argument (i.e., 1 and Violent) in quotation marks, even though 1 and 2 are numbers,
# they actually represent categories rather than numerical data.

# Run this code to recode the values
fighting_recode <- fighting_data %>%
  mutate(TV = recode(TV, "1" = "Violent", "0" = "Non-violent"),
         Fight = recode(Fight, "1" = "Yes", "0" = "No"))

head(fighting_recode)            # Look at the new table

# Step 3: Descriptive statistics ------------------------------------------------------------------
# Next you need to calculate descriptive statistics. For frequency data these are simply counts so we can use
# the function count() rather than having to use the summarise() function. We want to know how many
# participants are in each group (TV:Violent - Fight:Yes, TV:Violent - Fight:No, TV:Non-violent -
# Fight:Yes, TV:Non-violent - Fight:No) so we will need to use group_by to display the results for all combinations of 'TV' and 'Fight'.

# Run this code to do the counts and create the 'contingency' table
fighting_counts <- fighting_recode %>%
  group_by(TV, Fight) %>%
  count()

fighting_counts

# QUESTION: How many children with a preference for non-violent TV programmes got into a fight in school? 
# ANSWER: 30
# QUESTION: How many children with a preference for non-violent TV programmes did not get into a fight in school? 
# ANSWER: 70
# QUESTION: How many children with a preference for violent TV programmes got into a fight in school? 
# ANSWER: 40
# QUESTION:  How many children with a preference for violent TV programmes did not get into a fight in school? 
# ANSWER: 15

# You may also want to calculate the percentage of children who fought in each condition.

# Run this code to calculate the percentages
fighting_percent <- fighting_recode %>%
  group_by(TV, Fight) %>%
  count() %>%
  ungroup() %>% # ungroups the data
  group_by(TV) %>% # then groups it again but just by TV
  mutate(percent_TV = n/sum(n) * 100)

fighting_percent

# Step 4: Bar plot ------------------------------------------------------------------
#
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
#
# Run the code below code to create a bar plot

ggplot(data = fighting_recode, aes(x = Fight, fill = TV)) +
  geom_bar(position = "dodge")

# As you can see, the plot makes it much easier to visualise the data - children with
# a preference for violent TV programmes were more likely to get into a fight in school.


# Step 5: Chi-square ------------------------------------------------------------------
# So, let’s finally run that chi-square analysis to see whether our intuition from the plot holds up and there is a
# significant association between the grouping variables.

# Run this code to conduct the chi-square analysis
results <- chisq.test(x = fighting_recode$TV,       # the first grouping variable
                      y = fighting_recode$Fight,    # the second grouping variable
                      correct = FALSE)              # whether we want to apply the continuity correction (use if any of the expected cell frequencies < 10 in 2 x 2 table)
results

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

# Run this code to check the expected frequencies

results$expected

# Step 7: Effect size ------------------------------------------------------------------
# The last step before writing up the results is to calculate an effect size so that we have a standardised measure
# of how large the association between our grouping variables is, the effect size measure for chi-square is Cramer’s V.
# To calculate Cramer’s V we’re going to use the function cramersv() from the lsr package.

# Run this code to calculate Cramer's V:

eff_size <- cramersV(x = fighting_recode$TV,     # the first grouping variable
                     y = fighting_recode$Fight,  # the second grouping variable
                     correct = FALSE)                  # whether we want to apply the continuity correction (use if any of the expected cell frequencies < 10 in 2 x 2 table)
eff_size

# We can square the effect size (or multiple it by itself) to see how much variance in one 
# variable is accounted for by the other variable

# Run this code to square the effect size
percentageAccountedFor <- eff_size * eff_size
percentageAccountedFor

# Step 8: Interpreting relationships --------------------------------------
# To help with interpreting the relationships, it is useful to look at the standardised
# residuals. These are z-scores that indicate how many standard deviations above or below
# the expected count, an observed coutn is (thus indicating how much they differ). Remember:
# +/- 1.96 p <.05, +/- 2.58 p <.01, +/-3.29 p < .001

# Run this code to check the standardised residuals
results$residuals

# Step 9: Write up -----------------------------------------------------------------
#
# HINT: The Purdue writing lab website (https://owl.purdue.edu/owl/research_and_citation/apa6_style/apa_formatting_and_style_guide/statistics_in_apa.html)
# is helpful for guidance on punctuating statistics.

# Write up:
# There was a significant association between school fighting and TV programme preference (violent
# versus non-violent), Chi-square (1, N = 155) = 26.16, p < .001, Cramer's V = .41. Above
# expectations, 73% (40 out of 55) of children who preferred violent TV programmes, had also
# fought in school (z =3.00, p < .01), and significantly less than expected had not fought (z = 
# -2.76, p < .01). Conversely, 70% of children preferring non-violent TV programmes (70 out of 100),
# had not engaged in fighting, more than expected (z = 2.05, p < .05) and those who had fought
# were below expectations (z = -2.26, p < .05). Analysis showed 17% of the variance in school
# fighting could be accounted for by TV programme preference.
