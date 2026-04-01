library(dplyr)
df <- read.csv("/Users/austinra/BIOS6624/Project 3/Data Raw/frmgham2.csv")
glimpse(df)


#Data Cleaning for Primary Question

# Baseline covariates is when time = 0,
# Survival analysis is when STRKTIME is less than 10 years, if after 10 years we'll need to censor

#STRKI, STRKTIME, BASELINE COVARIATES stratified by M/F

#28 participants had a stroke
dropped <- df %>% filter((TIMESTRK == 0 & STROKE == 1))


#Filter out patients that had a stroke before the study
df <- df %>% filter(!(TIMESTRK == 0 & STROKE == 1))

#Censor patients that had a stroke after 10 years
df$STROKE_10 <- ifelse(df$TIMESTRK <= 10*365.25, df$STROKE, 0)

