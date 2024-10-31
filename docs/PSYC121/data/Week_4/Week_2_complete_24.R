# Remember to use comments like this one to annotate and explain what you do.

library(tidyverse)

cows <- read_csv("penelope22.csv") # Assumes the working directory and data file are one and the same
mean(cows$estimate)
sd(cows$estimate)
median(cows$estimate) # Students need to replace *MISSING* with the dataframe$variable




mean(cows$female_estimate, na.rm=TRUE) # This has been edited to account for empty cells
mean(cows$other_estimate, na.rm=TRUE) # This has been edited to account for empty cells

sd(cows$female_estimate, na.rm=TRUE) # This has been edited to account for empty cells

hist(cows$estimate)
hist(cows$estimate, breaks = 20) # This has been edited, suggestion of '20'

boxplot(cows$estimate)
boxplot(cows$female_estimate)
boxplot(cows$other_estimate)
boxplot(cows$estimate, notch=TRUE)

ggplot(cows, aes(x=identity, y=estimate),)+
geom_boxplot(fill="azure")+
theme_classic()




###  R Code Used in the David Howell book (chapter 5)
###    ____________________________________________________________________________
### Graphics for Langois and Roggman    Figure 5.1 in Fundamentals

X <- c(1.20, 1.82, 1.93, 2.04, 2.30, 2.33, 2.34, 2.47, 2.51, 2.55, 2.64,
       2.76, 2.77, 2.90, 2.91, 3.20, 3.22, 3.39, 3.59, 4.02)
Y <- c(3.13, 3.17, 3.19, 3.19, 3.20, 3.20, 3.22, 3.23, 3.25, 3.26, 3.27,
       3.29, 3.29, 3.30, 3.31, 3.31, 3.34, 3.34, 3.36, 3.38)

print(mean(X)); print(mean(Y))  # You can have multiple commands per line if you use a semi-colon
print(var(X)); print(var(Y))    #  These are needed to supply information for the legends.
print(sd(X)); print(sd(Y))
N = 20
par(mfrow = c(1,2))
hist(X, xlab = "Rated Attractiveness", main = "Composite of 4 Faces", col = "darkgreen", 
     xlim = c(1,5)  )
legend(2.5,7,"Mean = 2.64\nVariance = 0.43\nN = 20", bty = "n", cex = .75)

hist(Y, xlab = "Rated Attractiveness", main = "Composite of 32 Faces", col = "darkgreen",
     xlim = c(1,5)   )
legend(2,5,"Mean = 3.26\nVariance = .0048\nN = 20", bty = "n", cex = .75)
#  Try changing the beginning of last command from 2,5 to 4,4 to see what happens.


