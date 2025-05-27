# Cross-sectional data analysis of the data from the RYSE study.

# Package
library(dplyr)
library(ggplot2)
library(haven)
library(olsrr)

# Import master dataset
RYSE_master_dataset <- read_sav("RYSE_master_dataset_08082022.sav")

# Separation of the dataset in subdatasets
## Countries : CA or SA
df_CA <- RYSE_master_dataset[RYSE_master_dataset$Country==1,]
df_SA <- RYSE_master_dataset[RYSE_master_dataset$Country==2 & RYSE_master_dataset$Site != 4,] # we exclude Zamdela

# List of interesting variables
## Outcomes : #############

### SF-15 general health : CA -> T1, T1A, T2 / SA -> T1, T2
#T1_SF_14_PHC
#T1a_CA_SF15_14_PHC
#T2_SF_14_PHC

### WES Work Engagement : SA and CA -> T1, T2
#T1_WES_total
#T2_WES_total

### SES School Engagement : SA and CA -> T1, T2 (but not the same number of questions)
#T1_SES_total_T2 T1 School engagement overall scale (sum), for country comparisons over time (31 items (SA did not assess T1_SES_27 at T2))
#T2_SES_total T2 School engagement overall scale (sum), T2 specific (31 items since T2_SES_27 was not assessed for SA)

### BDI-II Depression : SA and CA -> T1, T1A, T2
#T1_BDI_II
#T1a_BDI_II
#T2_BDI_II

## Risks : #################

### BDI-II Depression : SA and CA -> T1, T1A, T2
#T1_BDI_II
#T1a_BDI_II
#T2_BDI_II

### CPTS Childhood Post-traumatic Stress : SA and CA -> T1, T1A, T2
#T1_CPTS
#T1a_CPTS
#T2_CPTS

### FAS Family adversity : SA and CA -> T1, T2
#T1_FAS
#T2_FAS

### PSS Perceived Stress : CA -> T2
#T2_CA_PSS

## Notes : ################
# Make a function to get the "expected" a and b of the regression line
# Do the "normal" regression line with all the points
# Do Jan's things to get rid of outliers / highleverages / influencers points for the couple of variables
# Do the regression without those points
# For each of the 3 look at the residuals :
# raw residuals, Confidence residuals, Credibility residuals, SD residuals, Quantile residuals
# Look at the 3 groups created.

## 
# First try : WES ~ CPTS for CA

# Unadjusted linear model
lm_cpts_wes_CA <- lm(T1_WES_total~1 + T1_CPTS, data = df_CA)
summary(lm_cpts_wes_CA)

# Confidence intervals
preds_cpts_wes_CA <- as.data.frame(predict(lm_cpts_wes_CA, newdata = df_CA, interval = "prediction",level=0.80))
preds_cpts_wes_CA$T1_CPTS <- df_CA$T1_CPTS  
preds_cpts_wes_CA$T1_WES_total <- df_CA$T1_WES_total


# Adjusted linear model
cooksd_cpts_wes_CA <- ols_plot_cooksd_bar(lm_cpts_wes_CA)
used_data <- model.frame(lm_cpts_wes_CA)  # This is df_CA with NAs removed
influencial_points <- which(cooks.distance(lm_cpts_wes_CA) > 4 / nrow(used_data))
used_rows <- as.numeric(rownames(used_data))
original_indices <- used_rows[influencial_points]
df_CA_clean <- df_CA[-original_indices, ]
lm_cpts_wes_CA_clean <- lm(T1_WES_total ~ 1 + T1_CPTS, data = df_CA_clean)
summary(lm_cpts_wes_CA_clean)

# Confidence intervals
preds_cpts_wes_CA_clean <- as.data.frame(predict(lm_cpts_wes_CA_clean, newdata = df_CA, interval = "prediction", level=0.80))
preds_cpts_wes_CA_clean$T1_CPTS <- df_CA$T1_CPTS
preds_cpts_wes_CA_clean$T1_WES_total <- df_CA$T1_WES_total


# Plot
# Influencer binary value to plot
df_CA_plot <- df_CA %>%
  mutate(influencer = FALSE)
df_CA_plot$influencer[original_indices] <- TRUE

lines_df <- data.frame(
  intercept = c(63 + 20 * 63 / 80,
                coef(lm_cpts_wes_CA)[1],
                coef(lm_cpts_wes_CA_clean)[1]),
  slope = c(-63 / 80,
            coef(lm_cpts_wes_CA)[2],
            coef(lm_cpts_wes_CA_clean)[2]),
  model = c("Expected", "Unadjusted", "Adjusted")
)

# ggplot
ggplot(df_CA_plot, aes(x = T1_CPTS, y = T1_WES_total)) +
  # Influencers
  geom_point(aes(shape = influencer), size = 2) +
  
  # Regression lines
  geom_abline(data = lines_df,
              aes(intercept = intercept, slope = slope, color = model),
              size = 0.8)+
  
  # Legend
  scale_color_manual(values = c("Expected" = "indianred",
                              "Unadjusted" = "skyblue",
                              "Adjusted" = "forestgreen")) +
  
  scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16),
                     labels = c("Normal", "Influencer"),
                     name = "Point type") +
  
  labs(
    x = "CPTS",
    y = "Work engagement",
    title = "Adjusted and unadjusted linear regressions for CPTS and WES"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 10)
  )


 





