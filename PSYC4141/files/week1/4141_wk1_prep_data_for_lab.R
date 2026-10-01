# Script to prepare data associated with Schubert et al. (2024) on the Brief Mind Wandering Three-Factor Scale for us in 4141, week 1 lab activities.

# Load relevant libraries
library(here) # the 'here' package helps with finding your files
library(tidyverse) # the 'tidyverse' is a set of packages that handles almost everything you need to do with data

# Read in the data
data_bmw3 <- read_csv(here("PSYC4141/files/week1/mxn3v-osfstorage-archive/PrepData/", "cleanData_UNCG_personality_merged.txt"))

# Keep only what's necessary + some tidying
data_bmw3 <- data_bmw3 %>%
  select(subject:BMW3_IN) %>%
  mutate(location = as.factor(location),
         type = as.factor(type),
         gender = as.factor(gender),
         education = as.factor(education))

# Safe smaller data file
write_csv(data_bmw3, here("PSYC4141/files/week1/", "data_bmw3.csv"))
