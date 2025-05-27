# Cross-sectional data analysis of the data from the RYSE study.

## Packages : ######
library(dplyr)
library(ggplot2)
library(haven)
library(olsrr)
library(rstanarm)

## Import master dataset : #####
RYSE_master_dataset <- read_sav("RYSE_master_dataset_08082022.sav")

## Separation of the dataset : Canada (CA) and South Africa (SA) : ####
df_CA <- RYSE_master_dataset[RYSE_master_dataset$Country==1,]
df_SA <- RYSE_master_dataset[RYSE_master_dataset$Country==2 
                             & RYSE_master_dataset$Site != 4,] # we exclude Zamdela


## List of interesting variables : #########
## Outcomes : #####

### SF-15 general health : CA -> T1, T1A, T2 / SA -> T1, T2
# Values : 0 to 100
#T1_SF_14_PHC
#T1a_CA_SF15_14_PHC
#T2_SF_14_PHC

### WES Work Engagement : SA and CA -> T1, T2
# Values : 9 to 63
#T1_WES_total
#T2_WES_total

### SES School Engagement : SA and CA -> T1, T2
# Values : 31 to 155
#T1_SES_total_T2 T1 School engagement overall scale (sum), for country comparisons over time (31 items (SA did not assess T1_SES_27 at T2))
#T2_SES_total T2 School engagement overall scale (sum), T2 specific (31 items since T2_SES_27 was not assessed for SA)

### BDI-II Depression : SA and CA -> T1, T1A, T2
# Values : 0 to 63
#T1_BDI_II
#T1a_BDI_II
#T2_BDI_II

## Risks : ##

### BDI-II Depression : SA and CA -> T1, T1A, T2
# Values : 0 to 63
#T1_BDI_II
#T1a_BDI_II
#T2_BDI_II

### CPTS Childhood Post-traumatic Stress : SA and CA -> T1, T1A, T2
# Values : 20 to 100
#T1_CPTS
#T1a_CPTS
#T2_CPTS

### FAS Family adversity : SA and CA -> T1, T2
# Values : 0 to 9
#T1_FAS
#T2_FAS

### PSS Perceived Stress : CA -> T2
# Values : 10 to 50
#T2_CA_PSS

### VbC Community victimisation : SA and CA
# Values : 4 to 20
#T1_VbC
#T2_VbC

### PoNS Perception of neighborhood : SA and CA
# Values : 8 to 32
#T1_PoNS
#T2_PoNS


## Get influencers and the corres. adjusted linear regression : ########

adjusted_fit <- function(df,adversity,outcome,main="Adjusted and unadjusted linear regression",xlab="Adversity",ylab="Outcome"){
  
  # Unadjusted linear model
  lm_unadjusted <- lm(as.formula(paste(outcome, "~", adversity)), data = df)
  print("--- Unadjusted model :")
  print(summary(lm_unadjusted))
  
  # Identification of influencial points using Cook's D.
  used_data <- model.frame(lm_unadjusted)
  influencial_points <- which(cooks.distance(lm_unadjusted) > 4 / nrow(used_data))
  used_rows <- as.numeric(rownames(used_data))
  original_indices <- used_rows[influencial_points]
  
  # Cleaned data for the LM
  df_clean <- df[-original_indices, ]
  
  # Adjusted linear model
  lm_adjusted <- lm(as.formula(paste(outcome, "~", adversity)), data = df_clean)
  print("--- Adjusted model :")
  print(summary(lm_adjusted))
  
  # Residuals of the adjusted linear model
  predicted_all <- predict(lm_adjusted, newdata = df)
  residuals_all <- df[[outcome]] - predicted_all
  
  # Bayesian glm for credibility intervals
  lm_adjusted_cred <- stan_glm(as.formula(paste(outcome, "~", adversity)), data = df)
  
  # Plot
  # Influencer binary value to plot
  df_plot <- df %>%
    mutate(influencer = FALSE)
  df_plot$influencer[original_indices] <- TRUE
  
  # Lines
  lines_df <- data.frame(
    intercept = c(coef(lm_unadjusted)[1],
                  coef(lm_adjusted)[1]),
    slope = c(coef(lm_unadjusted)[2],
              coef(lm_adjusted)[2]),
    model = c("Unadjusted", "Adjusted")
  )
  
  
  # ggplot
  plot <- ggplot(df_plot, aes_string(x = adversity, y = outcome)) +
    
    # Influencers
    geom_point(aes(shape = influencer), size = 2) +
    
    # Regression lines
    geom_abline(data = lines_df,
                aes(intercept = intercept, slope = slope, color = model),
                size = 0.8)+
    
    # Legend
    scale_color_manual(values = c("Unadjusted" = "skyblue",
                                  "Adjusted" = "forestgreen"),
                       name="Model") +
    
    scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16),
                       labels = c("Normal", "Influencer"),
                       name = "Point type") +
    
    labs(
      x = xlab,
      y = ylab,
      title = main
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 10)
    )
  
  return(list(plot=plot,
         influencers_indices=original_indices,
         lm_adjusted=lm_adjusted,
         lm_adjusted_cred=lm_adjusted_cred,
         residuals_adjusted=residuals_all))
}

## Results : ###########

# CPTS explaining WES
adjusted_fit(df=df_CA,adversity="T1_CPTS",outcome="T1_WES_total",main="Adjusted and unadjusted linear regression for CPTS and WES for Canada at T1",xlab="CPTS",ylab="WES")
adjusted_fit(df=df_CA,adversity="T2_CPTS",outcome="T2_WES_total",main="Adjusted and unadjusted linear regression for CPTS and WES for Canada at T2",xlab="CPTS",ylab="WES")

# BDI explaining SES
adjusted_fit(df=df_SA,adversity="T1_BDI_II",outcome="T1_SES_total_T2",main="Adjusted and unadjusted linear regression for BDI and SES for SA at T1",xlab="BDI-II",ylab="SES")
adjusted_fit(df=df_CA,adversity="T1_BDI_II",outcome="T1_SES_total_T2",main="Adjusted and unadjusted linear regression for BDI and SES for Canada at T1",xlab="BDI-II",ylab="SES")
adjusted_fit(df=df_SA,adversity="T2_BDI_II",outcome="T2_SES_total",main="Adjusted and unadjusted linear regression for BDI and SES for SA at T2",xlab="BDI-II",ylab="SES")
adjusted_fit(df=df_CA,adversity="T2_BDI_II",outcome="T2_SES_total",main="Adjusted and unadjusted linear regression for BDI and SES for Canada at T2",xlab="BDI-II",ylab="SES")

adjusted_fit(df=df_CA,adversity="T1_FAS",outcome="T1_BDI_II",main="Adjusted and unadjusted linear regression for FAS and BDI for Canada at T1",xlab="FAS",ylab="BDI-II")

# CPTS explaining BDI
adjusted_fit(df=df_CA,adversity="T1_CPTS",outcome="T1_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for Canada at T1",xlab="CPTS",ylab="BDI-II")
adjusted_fit(df=df_SA,adversity="T1_CPTS",outcome="T1_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for SA at T1",xlab="CPTS",ylab="BDI-II")
adjusted_fit(df=df_CA,adversity="T2_CPTS",outcome="T2_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for Canada at T2",xlab="CPTS",ylab="BDI-II")
adjusted_fit(df=df_SA,adversity="T2_CPTS",outcome="T2_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for SA at T2",xlab="CPTS",ylab="BDI-II")

# Many more possibilities of crossing...

## Groups : Resilient, average, vulnerable ########
# Get a result
res <- adjusted_fit(df=df_CA,adversity="T1_CPTS",outcome="T1_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for Canada at T1",xlab="CPTS",ylab="BDI-II")

lm_adjusted <- res$lm_adjusted
lm_adjusted_cred <- res$lm_adjusted_cred
residuals <- res$residuals_adjusted
plot <- res$plot

## Raw residuals : #######
df_groups <- data.frame(resilient = sum(residuals<0, na.rm=TRUE), average = sum(residuals==0, na.rm=TRUE), vulnerable = sum(residuals>0, na.rm=TRUE), row.names=c("raw residuals"))

## Confidence residuals : ######

# Predictions to get the lower and upper bounds for confidence intervals
# Two possibilities : confidence (x% sure for the mean value) or prediction (x% sure for an observation)

groups_from_prediction <- function(df_groups, actual, preds, names){
  for(i in 1:length(preds)){
    pred <- preds[[i]]
    
    resilient_conf  <- actual < pred$lwr
    vulnerable_conf <- actual > pred$upr
    average_conf    <- actual >= pred$lwr & actual <= pred$upr
    
    df_groups <- rbind(df_groups,
                       data.frame(
                         resilient  = sum(resilient_conf, na.rm = TRUE),
                         average    = sum(average_conf, na.rm = TRUE),
                         vulnerable = sum(vulnerable_conf, na.rm = TRUE),
                         row.names  = names[[i]]
                       ))
  }
  return(df_groups)
}


actual <- df_CA$T1_BDI_II
preds <- list(
  
  as.data.frame(predict(lm_adjusted, newdata = df_CA, interval = "prediction", level = 0.75)),
  as.data.frame(predict(lm_adjusted, newdata = df_CA, interval = "prediction", level = 0.6)),
  as.data.frame(predict(lm_adjusted, newdata = df_CA, interval = "prediction", level = 0.5)),
  as.data.frame(predict(lm_adjusted, newdata = df_CA, interval = "confidence", level = 0.99)),
  as.data.frame(predict(lm_adjusted, newdata = df_CA, interval = "confidence", level = 0.95))
)
names <- list(
  "pred. residuals (75%)",
  "pred. residuals (60%)",
  "pred. residuals (50%)",
  "conf. residuals (99%)",
  "conf. residuals (95%)"
)

df_groups <- groups_from_prediction(df_groups, actual, preds, names)

# Visualization of the intervals
visualisation_confidence_intervals <- function(df, adversity, outcome, adjusted_lm, preds,labels){
  # Coefficients of the adjusted liner model
  intercept <- coef(adjusted_lm)[1]
  slope     <- coef(adjusted_lm)[2]
  
  # Points and regression line
  plot <- ggplot(df, aes(x = .data[[adversity]], y = .data[[outcome]])) +
    geom_point() +
    geom_abline(intercept = intercept, slope = slope, color = "grey", linetype = "solid")+
    labs(
      x = adversity,
      y = outcome,
      title = "Predictions intervals"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 10)
    )
  
  colors <- list("indianred4","indianred","skyblue4","skyblue3","skyblue1")
  # Ribbons for prediction
  for(i in seq_along(preds)){
    pred <- preds[[i]]
    pred[[outcome]]   <- df[[outcome]]
    pred[[adversity]] <- df[[adversity]]
    
    plot <- plot + 
      geom_ribbon(data = pred, 
                  aes(x = .data[[adversity]], ymin = lwr, ymax = upr), 
                  fill = colors[i], alpha = 0.4, inherit.aes = FALSE)
  }
  
  return(plot)
}

visualisation_confidence_intervals(df=df_CA,adversity="T1_CPTS",outcome="T1_BDI_II",adjusted_lm =lm_adjusted,preds,names)



## Credibility residuals : ##########
# We can do the same with a bayesian approach with a credibility interval (for the mean and for predicted values)
###### NOT DONE YET - DON'T RUN #######

preds <- list(
  predict(lm_adjusted_cred, newdata = df_CA, probs = c(0.125, 0.875)),  # ≈ 75% interval
  predict(lm_adjusted_cred, newdata = df_CA, probs = c(0.2, 0.8)),      # ≈ 60%
  predict(lm_adjusted_cred, newdata = df_CA, probs = c(0.25, 0.75)),    # ≈ 50%
  fitted(lm_adjusted_cred, newdata = df_CA, probs = c(0.005, 0.995)),   # ≈ 99% credible for mean
  fitted(lm_adjusted_cred, newdata = df_CA, probs = c(0.025, 0.975))    # ≈ 95% credible for mean
)

# Rename to use the function that already exists
preds <- lapply(preds, function(df) {
  names(df)[grepl("^Q[0-9.]+", names(df))] <- c("lwr", "upr")
  df
})

names <- list(
  "posterior pred. (75%)",
  "posterior pred. (60%)",
  "posterior pred. (50%)",
  "posterior mean (99%)",
  "posterior mean (95%)"
)

df_groups <- groups_from_prediction(df_groups, actual, preds, names)


## Standard Deviation residuals : ####

## Quantile residuals : ####




