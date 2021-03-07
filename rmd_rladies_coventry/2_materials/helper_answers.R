# R code for CASA Coding Group ----
# Data: SIMULATED VOT DATA FROM POLITZER-AHLES & PICCINIINI 2018
# https://osf.io/2n3mu/
# Article: https://www.sciencedirect.com/science/article/pii/S0095447017301407#m0005

# Load packages ----
# First install these packages (you'll only need to do this once)
# install.packages(c("tidyverse","remotes", "rmarkdown"))

# Then load the packages. You have to load packages you want to use every time you start R.
library(tidyverse)

# Load data ----
# Read in the data using the read_csv function
# If the .csv file is in the same folder as your .RProj, you can run the following:
vot <- read.csv("simulated_vot_data.csv")
# If not, you need to include the path, like this:
#vot <- read.csv("presentations/2_materials/simulated_vot_data.csv")

# Explore data ----
# Take a look at the data using head(), str(), and summary() functions
head(vot) # Take a peak at the first 6 rows
str(vot) # Take a look at the structure of each variable
names(vot) # What are the names of the variables (columns)?
summary(vot) # Summary stats for all variables
nrow(vot) # How many rows?
View(vot) # View in spreadsheet-like form

# Explore data by variables ----
head(vot$Participant) # first 6 values of the variable Participant
levels(vot$Participant) # all "levels" of the variable Participant, which is a factor
length(levels(vot$Participant)) # how many levels are there?

mean(vot$VOT)
min(vot$VOT)
max(vot$VOT)

# Prepare data ----
# Now we'll start to use some tidyverse syntax

# Create new column that converts VOT in seconds to milliseconds 
vot <- vot %>%
  mutate(VOT_ms = VOT/1000)

# Create subset with just fast data
vot_fast <- vot %>%
  filter(Condition=="Fast")

## Mean VOT ----
# Aggregated over itmes
vot_participant_means <- vot %>%
  group_by(Condition, Participant) %>%
  summarise(mean_VOT = mean(VOT),
            mean_VOT_ms = mean(VOT_ms)) %>%
  ungroup()

# Aggregated over participant means
vot_grand_means <- vot_participant_means %>%
  group_by(Condition) %>%
  #notice the input is the name of the variable in the input data frame
  summarise(mean_VOT = mean(mean_VOT), 
            mean_VOT_ms = mean(mean_VOT_ms)) %>%
  ungroup()
