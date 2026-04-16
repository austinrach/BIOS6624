library(dplyr)
library(gtsummary)
library(tidyr)
library(openxlsx)

df <- read.csv("/Users/austinra/BIOS6624/Project 3/Data Raw/frmgham2.csv")

#Data Cleaning for Primary Question
# Filter out patients that had a stroke before the study - 32
df <- df %>% filter(!(TIMESTRK == 0 & STROKE == 1))

# Grab patients first stroke 
stroke_info <- df %>%
  group_by(RANDID) %>%
  summarise(
    STROKE = as.integer(any(STROKE == 1 & TIMESTRK <= 10 * 365.25)),
    TIMESTRK = min(TIMESTRK)
  )

# Pull baseline covariates
baseline_covs <- df %>%
  arrange(RANDID, TIMESTRK) %>% 
  group_by(RANDID) %>%
  slice(1) %>%
  ungroup() %>%
  select(RANDID, SEX, AGE, DIABETES, SYSBP, BPMEDS, CURSMOKE, TOTCHOL, BMI)

# Join them together
df_baseline <- baseline_covs %>%
  left_join(stroke_info, by = "RANDID")


# Replace censored patients time values with 10*365.25 
df_baseline$TIME_STROKE <- ifelse(df_baseline$TIMESTRK > 365.25*10, 365.25*10, df_baseline$TIMESTRK)


# Factorize STROKE and covariates
df_baseline <- df_baseline %>%
  mutate(
    SEX = factor(SEX, levels = c(1,2), labels = c("Male","Female")),
    STROKE = factor(STROKE, levels = c(0,1), labels = c("No Stroke","Stroke")),
    DIABETES = factor(DIABETES, levels = c(0,1), labels = c("No Diabetes", "Diabetes")),
    BPMEDS = factor(BPMEDS, levels = c(0,1), labels = c("Not on Hypertension Meds", "On Hypertension Meds")),
    CURSMOKE = factor(CURSMOKE, levels = c(0,1), labels = c("Not a current smoker", "Current smoker"))
  )

# Table 1: baseline covariates stratified by sex
table1 <- df_baseline %>%
  mutate(TIME_STROKE = ifelse(STROKE == "Stroke", TIME_STROKE, NA)) %>%
  select(SEX, STROKE, TIME_STROKE, AGE, DIABETES, SYSBP, BPMEDS, CURSMOKE, TOTCHOL, BMI) %>%
  tbl_summary(
    by = SEX,
    missing = "no",
    statistic = list(
      TIME_STROKE ~ "{mean} ({sd})",
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      TIME_STROKE ~ "Time to Stroke (days)",
      STROKE ~ "Stroke within 10 years",
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

#Show numeric not character
df_baseline <- df_baseline %>%
  mutate(
    SEX = ifelse(SEX == "Male", 1, 2),
    STROKE = ifelse(STROKE == "Stroke", 1, 0),
    DIABETES = ifelse(DIABETES == "Diabetes", 1, 0),
    BPMEDS = ifelse(BPMEDS == "On Hypertension Meds", 1, 0),
    CURSMOKE = ifelse(CURSMOKE == "Current smoker", 1, 0)
  )


#Write survival dataset for primary objective analysis
write.xlsx(df_baseline,"/Users/austinra/BIOS6624/Project 3/Data Processed/survival_df.xlsx")

#Data cleaning for secondary question
df <- read.csv("/Users/austinra/BIOS6624/Project 3/Data Raw/frmgham2.csv")

# Filter out patients that had a stroke before the study - 32
df <- df %>% filter(!(TIMESTRK == 0 & STROKE == 1))


df_wide <- df %>%
  arrange(RANDID, PERIOD) %>%
  select(RANDID, PERIOD, AGE, DIABETES, SYSBP, BPMEDS, CURSMOKE, TOTCHOL, BMI) %>%
  pivot_wider(
    id_cols = RANDID,
    names_from = PERIOD,
    values_from = c(AGE, DIABETES, SYSBP, BPMEDS, CURSMOKE, TOTCHOL, BMI),
    names_glue = "{.value}{PERIOD}"  # gives you AGE1, AGE2, AGE3 etc.
  ) %>%
  left_join(
    df_baseline %>% select(RANDID, STROKE, TIME_STROKE),
    by = "RANDID"
  )

#write covariate dataset for secondary objective analysis
write.xlsx(df_wide,"/Users/austinra/BIOS6624/Project 3/Data Processed/covariate_df.xlsx")
