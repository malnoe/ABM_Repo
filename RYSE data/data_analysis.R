# Cross-sectional data analysis of the data from the RYSE study.

## Packages : ######
library(car)
library(dplyr)
library(ggplot2)
library(haven)
library(lmtest)
library(olsrr)
library(rstanarm)
library(sandwich)

## Import master dataset : #####
setwd("~/Ecole/M1/Stage/Internship_repo/RYSE data")
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
  
  # Tests of the residuals
  print(shapiro.test(residuals_all)) # Normality of residuals
  print(durbinWatsonTest(lm_adjusted)) # Auto-correlation of residuals
  print(bptest(lm_adjusted)) # Homoscedasticity
  
  # Bayesian glm for credibility intervals
  lm_adjusted_cred <- stan_glm(as.formula(paste(outcome, "~", adversity)), data = df,refresh=0)
  
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
  
  if(lines_df$slope[[1]] < 0){
    plot <-plot + geom_text(x=max(na.omit(df[[adversity]]))-5,y=max(na.omit(df[[outcome]]))-5,label="Resilient",alpha=0.2,color="grey") + geom_text(x=min(na.omit(df[[adversity]]))+5,y=min(na.omit(df[[outcome]]))+5,label="Vulnerable",alpha=0.2,color="grey")
  }
  else{
    plot <- plot + geom_text(x=min(na.omit(df[[adversity]]))+5,y=max(na.omit(df[[outcome]]))-5,label="Vulnerable",alpha=0.2,color="grey") + geom_text(x=max(na.omit(df[[adversity]]))-5,y=min(na.omit(df[[outcome]]))+5,label="Resilient",alpha=0.2,color="grey")
  }
  
  return(list(plot=plot,
         influencers_indices=original_indices,
         lm_adjusted=lm_adjusted,
         lm_adjusted_cred=lm_adjusted_cred,
         residuals_adjusted=residuals_all))
}

## Results : ###########

# CPTS explaining WES
adjusted_fit(df=df_CA,adversity="T1_CPTS",outcome="T1_WES_total",main="Adjusted and unadjusted linear regression for CPTS and WES for Canada at T1",xlab="CPTS",ylab="WES")
#adjusted_fit(df=df_CA,adversity="T2_CPTS",outcome="T2_WES_total",main="Adjusted and unadjusted linear regression for CPTS and WES for Canada at T2",xlab="CPTS",ylab="WES")

# BDI explaining SES
#adjusted_fit(df=df_SA,adversity="T1_BDI_II",outcome="T1_SES_total_T2",main="Adjusted and unadjusted linear regression for BDI and SES for SA at T1",xlab="BDI-II",ylab="SES")
#adjusted_fit(df=df_CA,adversity="T1_BDI_II",outcome="T1_SES_total_T2",main="Adjusted and unadjusted linear regression for BDI and SES for Canada at T1",xlab="BDI-II",ylab="SES")
#adjusted_fit(df=df_SA,adversity="T2_BDI_II",outcome="T2_SES_total",main="Adjusted and unadjusted linear regression for BDI and SES for SA at T2",xlab="BDI-II",ylab="SES")
#adjusted_fit(df=df_CA,adversity="T2_BDI_II",outcome="T2_SES_total",main="Adjusted and unadjusted linear regression for BDI and SES for Canada at T2",xlab="BDI-II",ylab="SES")

#adjusted_fit(df=df_CA,adversity="T1_FAS",outcome="T1_BDI_II",main="Adjusted and unadjusted linear regression for FAS and BDI for Canada at T1",xlab="FAS",ylab="BDI-II")

# CPTS explaining BDI
adjusted_fit(df=df_CA,adversity="T1_CPTS",outcome="T1_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for Canada at T1",xlab="CPTS",ylab="BDI-II")
#adjusted_fit(df=df_SA,adversity="T1_CPTS",outcome="T1_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for SA at T1",xlab="CPTS",ylab="BDI-II")
#adjusted_fit(df=df_CA,adversity="T2_CPTS",outcome="T2_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for Canada at T2",xlab="CPTS",ylab="BDI-II")
#adjusted_fit(df=df_SA,adversity="T2_CPTS",outcome="T2_BDI_II",main="Adjusted and unadjusted linear regression for CPTS and BDI for SA at T2",xlab="CPTS",ylab="BDI-II")

# Many more possibilities of crossing...

## Groups : Resilient, average, vulnerable ########

## Raw residuals : #######
get_groups_residuals <- function(residuals,is_resilience_positive=FALSE){
  res_list <- list()
  for(i in 1:length(residuals)){
    if(is_resilience_positive){
      res_list[i] <- if(is.na(residuals[i])){NA}else{if(residuals[i]>0){"resilient"}else{if(residuals[i]==0){"average"}else{"vulnerable"}}}
    }
    else{
      res_list[i] <- if(is.na(residuals[i])){NA}else{if(residuals[i]<0){"resilient"}else{if(residuals[i]==0){"average"}else{"vulnerable"}}}
    }
  }
  return(res_list)
}

## Confidence residuals : ######

# Predictions to get the lower and upper bounds for confidence intervals
# Two possibilities : confidence (x% sure for the mean value) or prediction (x% sure for an observation)

# Function to return groups based on confidence intervals
get_groups_confidence <- function(actual, pred,is_resilience_positive=FALSE) {
  groups <- vector("character", length(actual))
  for (i in 1:length(actual)) {
    # Take car of any NA values
    if (is.na(actual[i]) || is.na(pred$lwr[i]) || is.na(pred$upr[i])) {
      groups[i] <- NA
    }else if(is_resilience_positive){
      if (actual[i] < pred$lwr[i]) {
        groups[i] <- "vulnerable"
      } else if (actual[i] > pred$upr[i]) {
        groups[i] <- "resilient"
      } else {
        groups[i] <- "average"
      }
    }
    else{
      if (actual[i] < pred$lwr[i]) {
        groups[i] <- "resilient"
      } else if (actual[i] > pred$upr[i]) {
        groups[i] <- "vulnerable"
      } else {
        groups[i] <- "average"
      }
    }
  }
  return(groups)
}

# Visualization of the intervals
visualisation_confidence_intervals <- function(df, adversity, outcome, adjusted_lm, preds, labels, main = "Intervals") {
  # Adjusted linear model coefficients
  intercept <- coef(adjusted_lm)[1]
  slope     <- coef(adjusted_lm)[2]
  
  # Base graph with points and regression line
  plot <- ggplot(df, aes(x = .data[[adversity]], y = .data[[outcome]])) +
    geom_point() +
    geom_abline(intercept = intercept, slope = slope, color = "grey", linetype = "solid") +
    labs(
      x = adversity,
      y = outcome,
      title = main,
      fill = "Interval"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10))
  
  # Combine all intervals in one dataframe
  all_preds <- do.call(rbind, lapply(seq_along(preds), function(i) {
    pred <- preds[[i]]
    pred[[outcome]]   <- df[[outcome]]
    pred[[adversity]] <- df[[adversity]]
    pred$label <- labels[i]
    pred
  }))
  all_preds$label <- factor(all_preds$label, levels = labels)
  
  # Ribbons with legend for each interval
  plot <- plot +
    geom_ribbon(
      data = all_preds,
      aes(
        x = .data[[adversity]],
        ymin = lwr,
        ymax = upr,
        fill = label,
        group = label
      ),
      alpha = 0.4,
      inherit.aes = FALSE
    )
  
  if(slope < 0){
    plot <-plot + geom_text(x=max(na.omit(df[[adversity]]))-5,y=max(na.omit(df[[outcome]]))-5,label="Resilient",alpha=0.2,color="grey") + geom_text(x=min(na.omit(df[[adversity]]))+5,y=min(na.omit(df[[outcome]]))+5,label="Vulnerable",alpha=0.2,color="grey")
  }
  else{
    plot <- plot + geom_text(x=min(na.omit(df[[adversity]]))+5,y=max(na.omit(df[[outcome]]))-5,label="Vulnerable",alpha=0.2,color="grey") + geom_text(x=max(na.omit(df[[adversity]]))-5,y=min(na.omit(df[[outcome]]))+5,label="Resilient",alpha=0.2,color="grey")
  }
  
  return(plot)
}


## Quantile residuals : ####
# We want the quantile_sub lowest residuals and quantile_sup biggest residuals
get_groups_quantile <- function(residuals,quantile_sub,quantile_sup,is_resilience_positive){
  n <- sum(!is.na(residuals))
  n_sub <- trunc(n*quantile_sub)
  n_sup <- trunc(n*quantile_sup)
  index_ordered_residuals <- order(residuals)
  
  res <- rep(NA, length(residuals))
  res[!is.na(residuals)] <- "average"
  if(is_resilience_positive){
    # Lowest residuals -> vulnerable
    for(i in 1:n_sub){
      index <- index_ordered_residuals[i]
      res[[index]] <- "vulnerable"
    }
    # Biggest residuals -> resilient
    for(i in n:(n-n_sup)){
      index <- index_ordered_residuals[i]
      res[[index]] <- "resilient"
    }
  }
  else{
    # Lowest residuals -> resilient
    for(i in 1:n_sub){
      index <- index_ordered_residuals[i]
      res[[index]] <- "resilient"
    }
    # Biggest residuals -> vulnerable
    for(i in n:(n-n_sup)){
      index <- index_ordered_residuals[i]
      res[[index]] <- "vulnerable"
    }
  }
  
  return(res)
}

## Credibility residuals : ##########
# We can do the same with a bayesian approach with a credibility interval (for the mean and for predicted values)

# Function to build the credibility intervals from the bayesian adjusted lm
get_credibility_intervals <- function(lm_adjusted_cred,newdata,lwr=0.025,upr=0.975){
  complete_rows <- complete.cases(newdata)
  
  # Posterior linear prediction on complete cases
  preds <- posterior_linpred(lm_adjusted_cred,
                             newdata = newdata[!is.na(newdata[,c(adversity_string)]),],
                             draws = 1000,
                             transform = TRUE)
  
  # Ensure preds is a matrix with rows = draws, cols = observations
  if (is.null(dim(preds))) {
    preds <- matrix(preds, nrow = 1000)
  }
  
  # Compute credible intervals per observation (apply over columns)
  intervals <- t(apply(preds, 2, quantile, probs = c(lwr, upr)))
  # Formating the result
  res <- data.frame(lwr=c(), upr=c())
  counter <- 1
  # Adding NA
  for(i in 1:nrow(newdata)){
    if(!is.na(newdata[i,1])){
      res[i,"lwr"] <- intervals[counter,1]
      res[i,"upr"] <- intervals[counter,2]
      counter <- counter + 1
    }
    else{
      res[i,"lwr"] <- NA
      res[i,"upr"] <- NA
    }
  }
  return(res)
}


## Presentation ##########
# Example 1
df <- df_CA
adversity_string <- "T1_CPTS"
outcome_string <- "T1_BDI_II"
outcome <- df_CA$T1_BDI_II
res <- adjusted_fit(df=df,adversity=adversity_string,outcome=outcome_string)

# Example 2
df <- df_CA
adversity_string <- "T1_CPTS"
outcome_string <- "T1_WES_total"
outcome <- df_CA$T1_WES_total
res <- adjusted_fit(df=df_CA,adversity="T1_CPTS",outcome="T1_WES_total",main="Adjusted and unadjusted linear regression for CPTS and WES for Canada at T1",xlab="CPTS",ylab="WES")

# Get the info
lm_adjusted <- res$lm_adjusted
lm_adjusted_cred <- res$lm_adjusted_cred
residuals <- res$residuals_adjusted
plot <- res$plot
resilience_sign <- lm_adjusted$coefficients[2]<0

# Raw residuals
groups_raw <- get_groups_residuals(residuals,is_resilience_positive=resilience_sign)
df_n_groups <- data.frame(resilient = sum(groups_raw=="resilient", na.rm=TRUE), average = sum(groups_raw=="average", na.rm=TRUE), vulnerable = sum(groups_raw=="vulnerable", na.rm=TRUE), row.names=c("raw residuals"))

# Confidence intervals
# Prediction
preds_conf <- list(
  as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.75)),
  as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.6)),
  as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.5)),
  as.data.frame(predict(lm_adjusted, newdata = df, interval = "confidence", level = 0.99)),
  as.data.frame(predict(lm_adjusted, newdata = df, interval = "confidence", level = 0.95))
)

# Names for the predictions
names_conf <- list(
  "pred. residuals (75%)",
  "pred. residuals (60%)",
  "pred. residuals (50%)",
  "conf. residuals (99%)",
  "conf. residuals (95%)"
)

# Get groups and update df_n_groups
groups_confidence <- list()
for(i in 1:length(preds_conf)){
  groups <- get_groups_confidence(outcome, preds_conf[[i]],is_resilience_positive=resilience_sign)
  groups_confidence[[ names_conf[[i]] ]] <- groups
  df_n_groups <- rbind(df_n_groups,
                       data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names_conf[[i]])))
}


# Quantiles
# Quantile values -> Can be asymetric (looking at the QQ-plot ?)
list_quantile_sub <- list(0.05,0.1,0.15,0.25)
list_quantile_sup <- list(0.05,0.1,0.15,0.25)

# Names
names <- list(
  "quantiles (5%)",
  "quantiles (10%)",
  "quantiles (15%)",
  "quantiles (25%)"
)

# Get groups and update df_n_groups
groups_quantile <- list()
for(i in 1:length(list_quantile_sub)){
  groups <- get_groups_quantile(residuals,list_quantile_sub[[i]],list_quantile_sup[[i]],is_resilience_positive=resilience_sign)
  groups_quantile[[ names[[i]] ]] <- groups
  df_n_groups <- rbind(df_n_groups,
                       data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names[[i]])))
}


# Credibility intervals
# Predictions
preds_cred <- list(
  get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.0005,upr=0.9995),
  get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.005,upr=0.995),
  get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.025,upr=0.975),
  get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.05,upr=0.95),
  get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.125,upr=0.875),
  get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.25,upr=0.75)
)


# Names for the predictions
names_cred <- list(
  "cred. 99.9%",
  "cred. 99%",
  "cred. 95%",
  "cred. 90%",
  "cred. 75%",
  "cred. 50%"
)

# Get groups and update df_n_groups
groups_credibility <- list()
for(i in 1:length(preds_cred)){
  groups <- get_groups_confidence(outcome, preds_cred[[i]],is_resilience_positive=resilience_sign)
  groups_credibility[[ names_cred[[i]] ]] <- groups
  df_n_groups <- rbind(df_n_groups,
                       data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names_cred[[i]])))
}

## Results
# Cardinal of each group
View(df_n_groups)

# Groups
groups_raw
groups_confidence
groups_quantile
groups_credibility

# Visualization
visualisation_confidence_intervals(df=df,adversity=adversity_string,outcome=outcome_string,adjusted_lm =lm_adjusted,preds_conf,names_conf,main="Confidence intervals")
qqnorm(residuals)
qqline(residuals)
visualisation_confidence_intervals(df=df,adversity=adversity_string,outcome=outcome_string,adjusted_lm =lm_adjusted,preds_cred,names_cred,main="Credibility intervals")



###### NOT DONE YET - DON'T RUN #######


## Standard Deviation residuals : ####
CPTS_bins <- c(20, 32, 45, 60, 80, 100)
res_SD <- c()
bin_labels <- rep(NA, nrow(df))  # To stock the bin of each line.

# Calculate the SD for each bin
for (i in 1:(length(CPTS_bins) - 1)) {
  in_bin <- df[[adversity_string]] > CPTS_bins[i] & df[[adversity_string]] < CPTS_bins[i + 1]
  
  bin_labels[in_bin] <- i #We save the i indice of the bin for each line that's in the bin
  
  residuals_bin <- residuals[in_bin]
  res_SD[i] <- sd(residuals_bin, na.rm = TRUE)
}

groups_sd <- rep(NA, nrow(df))

for (i in seq_along(residuals)) {
  bin_i <- bin_labels[i]
  
  # Skip if bin or residual is NA
  if (is.na(bin_i) || is.na(residuals[i])) next
  
  sd_i <- res_SD[bin_i]
  res <- residuals[i]
  
  if (abs(res) <= sd_i) {
    groups_sd[i] <- "average"
  } else if (res > sd_i) {
    groups_sd[i] <- "resilient"
  } else if (res < -sd_i) {
    groups_sd[i] <- "vulnerable"
  }
}

groups_sd
df_n_groups <- rbind(df_n_groups,
                     data.frame(resilient = sum(groups_sd=="resilient", na.rm=TRUE), average = sum(groups_sd=="average", na.rm=TRUE), vulnerable = sum(groups_sd=="vulnerable", na.rm=TRUE), row.names=c("SD 1")))

