df <- read.csv("Project 2/dfa Raw/PrelimData.csv")
# Example variable names assumed:
# CORT_CNG3 = baseline - year1 cortical thickness
# CVLT_CNG3      = baseline - year1 CLVT
# IL_6         = baseline IL-6
# MCP_1        = baseline MCP-1

library(dplyr)
library(ggplot2)

# Descriptive Statistics
df %>%
  select(CORT_CNG3, CVLT_CNG3, IL_6, MCP_1) %>%
  summary()

sapply(df[, c("CORT_CNG3", "CVLT_CNG3", "IL_6", "MCP_1")], function(x) sum(is.na(x)))

#Visualize
vars <- c("CORT_CNG3", "CVLT_CNG3", "IL_6", "MCP_1")
for (v in vars) {
  print(
    ggplot(df, aes_string(x = v)) +
      geom_histogram(bins = 20) +
      theme_minimal() +
      ggtitle(v)
  )
}

pairs_to_plot <- list(
  c("IL_6", "CORT_CNG3"),
  c("IL_6", "CVLT_CNG3"),
  c("MCP_1", "CORT_CNG3"),
  c("MCP_1", "CVLT_CNG3"),
  c("IL_6", "MCP_1"),
  c("CORT_CNG3", "CVLT_CNG3")
)

for (p in pairs_to_plot) {
  print(
    ggplot(df, aes_string(x = p[1], y = p[2])) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      ggtitle(paste(p[2], "vs", p[1]))
  )
}

# Pearson correlations
vars_main <- df %>%
  select(CORT_CNG3, CVLT_CNG3, IL_6, MCP_1)

cor_mat_pearson <- cor(vars_main, use = "pairwise.complete.obs", method = "pearson")
cor_mat_spearman <- cor(vars_main, use = "pairwise.complete.obs", method = "spearman")

cor_mat_pearson
cor_mat_spearman

# Predictor-outcome correlations
cor.test(df$IL_6, df$CORT_CNG3, method = "pearson")
cor.test(df$IL_6, df$CVLT_CNG3, method = "pearson")
cor.test(df$MCP_1, df$CORT_CNG3, method = "pearson")
cor.test(df$MCP_1, df$CVLT_CNG3, method = "pearson")

# Predictor-predictor and outcome-outcome correlations
cor.test(df$IL_6, df$MCP_1, method = "pearson")
cor.test(df$CORT_CNG3, df$CVLT_CNG3, method = "pearson")

