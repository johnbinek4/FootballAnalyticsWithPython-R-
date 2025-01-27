# Exploring nflfastR

library(tidyverse)
library(nflfastR)

testData <- load_pbp(2024)

# 372 columns
# A lot of binary events labeled 0 or 1
# Other events labeled Null or NA

# Player Ids are different from nflId used in Databowl

# Time of day included up to the second

# NFL api id? 

# How often does this update in real time? Question to ask