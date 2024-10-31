
library(tidyverse)

phones <- read_csv("screentime2024.csv") 
money <- read_csv("wages2024.csv") 

#boxplot(phones$usage ~ phones$phone_type)
phones %>% ggplot(aes(x = phone_type, y = usage)) + geom_violin()

# Scripts can pretty hard to read and work with if we always have everything on a single line. So...
phones %>% 
  ggplot() + 
  geom_violin(aes(x = phone_type, y = usage))

# The plot doesn't look at clear and meaningful as it might, so we can add labels
boxplot(phones$usage ~ phones$phone_type, 
        xlab = "Type of phone", ylab = "Usage in minutes", main = "Fancy Schmancy graph", col = "green") # Edit the text

phones %>%
ggplot() +
geom_violin(aes(x = phone_type, y = usage), fill="yellow") +
labs(title="Title for a pretty plot", x = "Phone choice",  y= "Usage (minutes)") # Edit the text
 theme_bw()

money %>% 
  ggplot() + 
  geom_density(aes(x = salary, y = ..count.., fill = family_position), alpha=.4) +
  scale_fill_manual(values = c("darkgreen", "darkblue","red","orange")) +
  theme_classic()

# Final exercise. Convert all the phone usage data into z scores with scale. Look at the result. What is the largest and smallest z score?
usage_z <-scale(phones$usage)
usage_z_sorted <- sort(usage_z, decreasing = FALSE) # If you had to guess, what does sort() do?
print(usage_z_sorted) # This displays the object from the previous line. Does this help work out sort()?
