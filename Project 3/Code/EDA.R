library(dplyr)
library(gtsummary)
df <- read.csv("/Users/austinra/BIOS6624/Project 3/Data Raw/frmgham2.csv")
glimpse(df)


#Data Cleaning for Primary Question

# Baseline covariates is when time = 0,
# Survival analysis is when STRKTIME is less than 10 years, if after 10 years we'll need to censor
# 
# #STRKI, STRKTIME, BASELINE COVARIATES stratified by M/F
# 
# #32 participants had a stroke
# dropped <- df %>% filter((TIMESTRK == 0 & STROKE == 1))
# length(unique(dropped$RANDID))
# 
# #Filter out patients that had a stroke before the study
# df <- df %>% filter(!(TIMESTRK == 0 & STROKE == 1))
# 
# #Censor patients who died during the study
# death <- df %>% filter(DEATH == 1 & TIMEDTH < 10*365.25)
# length(unique(death$RANDID))
# #413 died during the study
# 
# #Censor patients that had a stroke after 10 years
# df$STROKE_10 <- ifelse(df$TIMESTRK <= 10*365.25, df$STROKE, 0)
# 
# censored <- df %>% filter((TIMESTRK > 365.25*10 & STROKE ==1))
# length(unique(censored$RANDID))
# #272 patients censored
# 
# stroked <- df %>% filter(STROKE_10 == 1)
# stroked_unique <- df %>% filter(unique(RANDID))
# table(stroked$SEX)


# Define 10-year cutoff

#CHAT-GPT created this code based on above specifications 
cutoff <- 10 * 365.25

# 1. Fill missing SEX for participants who have it in any visit
df_clean <- df %>%
  group_by(RANDID) %>%
  fill(SEX, .direction = "downup") %>%
  ungroup()

# 2. Drop participants with stroke at baseline
df_clean <- df_clean %>%
  filter(!(TIMESTRK == 0 & STROKE == 1))

# 3. Compute stroke10 and time10 per participant 
df_events <- df_clean %>%
  group_by(RANDID) %>%
  summarise(
    earliest_stroke = if(any(STROKE == 1 & TIMESTRK <= cutoff)) {
      min(TIMESTRK[STROKE == 1 & TIMESTRK <= cutoff], na.rm = TRUE)
    } else { NA_real_ },
    
    earliest_death = if(any(DEATH == 1 & TIMEDTH <= cutoff)) {
      min(TIMEDTH[DEATH == 1 & TIMEDTH <= cutoff], na.rm = TRUE)
    } else { NA_real_ },
    
    # Stroke indicator
    stroke10 = if(!is.na(earliest_stroke)) 1 else 0,
    
    # Time to event or censoring
    time10 = case_when(
      !is.na(earliest_stroke) ~ earliest_stroke,               # stroke event
      !is.na(earliest_death) & is.na(earliest_stroke) ~ cutoff, # death censored
      TRUE ~ cutoff                                             # no stroke or death
    )
  ) %>%
  ungroup()

# 4. Collapse to baseline covariates (first observation)
df_baseline <- df_clean %>%
  arrange(RANDID, TIMESTRK) %>%
  group_by(RANDID) %>%
  slice(1) %>%
  ungroup() %>%
  select(RANDID, SEX, AGE, DIABETES, SYSBP, BPMEDS, CURSMOKE, TOTCHOL, BMI) %>%
  left_join(df_events, by = "RANDID")

# 5. Drop participants with missing baseline covariates
df_baseline <- df_baseline %>%
  filter(!is.na(SEX) & !is.na(AGE) & !is.na(DIABETES) &
           !is.na(SYSBP) & !is.na(BPMEDS) & !is.na(CURSMOKE) &
           !is.na(TOTCHOL) & !is.na(BMI))

# 6. Factorize SEX and stroke10
df_baseline <- df_baseline %>%
  mutate(
    SEX = factor(SEX, levels = c(1,2), labels = c("Male","Female")),
    stroke10 = factor(stroke10, levels = c(0,1), labels = c("No Stroke","Stroke"))
  )

# 7. Table 1: baseline covariates stratified by sex
table1 <- df_baseline %>%
  select(SEX, stroke10, time10, AGE, DIABETES, SYSBP, BPMEDS, CURSMOKE, TOTCHOL, BMI) %>%
  tbl_summary(
    by = SEX,
    statistic = list(
      time10 ~ "{mean} ({sd})",
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      time10 ~ "Follow-up Time (days)",
      stroke10 ~ "Stroke within 10 years",
      AGE ~ "Age",
      DIABETES ~ "Diabetes",
      SYSBP ~ "Systolic BP",
      BPMEDS ~ "BP Medications",
      CURSMOKE ~ "Current Smoker",
      TOTCHOL ~ "Total Cholesterol",
      BMI ~ "BMI"
    )
  ) %>%
  add_overall() %>%
  modify_header(label = "**Variable**")

table1

# 8. Mean ± SD of time to stroke stratified by sex (only stroke cases)
df_baseline %>%
  filter(stroke10 == "Stroke") %>%
  group_by(SEX) %>%
  summarise(
    n = n(),
    mean_time = mean(time10, na.rm = TRUE),
    sd_time = sd(time10, na.rm = TRUE)
  )
