# Cross-sectional data analysis of the data from the RYSE study.

## Packages ######
library(car) # lm testing
library(dplyr) 
library(ggplot2) # ploting
library(haven) # read sav data
library(lmtest) # lm testing
library(olsrr) # influence statistic
library(rstanarm) # bayesian lm
library(sandwich) # lm testing

## Import master dataset #####
setwd("~/Ecole/M1/Stage/Internship_repo/RYSE data")
RYSE_master_dataset <- read_sav("RYSE_master_dataset_08082022.sav")

## Separation of the dataset : Canada (CA) and South Africa (SA) ####
df_CA <- RYSE_master_dataset[RYSE_master_dataset$Country==1,]
df_SA <- RYSE_master_dataset[RYSE_master_dataset$Country==2 
                             & RYSE_master_dataset$Site != 4,] # we exclude Zamdela


## List of interesting variables #########
## Outcome ##

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

## Risks ###

### BDI-II Depression : SA and CA -> T1, T1A, T2
# Values : 0 to 63
bins_BDI_II <- c(0,14,20,26,63)
#T1_BDI_II
#T1a_BDI_II
#T2_BDI_II

### CPTS Childhood Post-traumatic Stress : SA and CA -> T1, T1A, T2
# Values : 20 to 100
bins_CPTS <- c(20, 32, 45, 60, 80,100)
#T1_CPTS
#T1a_CPTS
#T2_CPTS

### FAS Family adversity : SA and CA -> T1, T2
# Values : 0 to 9
bins_FAS <- c(0,4,9)
#T1_FAS
#T2_FAS

### PSS Perceived Stress : CA -> T2
# Values : 10 to 50
bins_PSS <- c(10,24,37,50)
#T2_CA_PSS

### VbC Community victimisation : SA and CA
# Values : 4 to 20
bins_VbC <- c(4,8,12,16,20)
#T1_VbC
#T2_VbC

### PoNS Perception of neighborhood : SA and CA
# Values : 8 to 32
bins_PoNS <- c(8,16,24,32)
#T1_PoNS
#T2_PoNS



## Get influencers and the corres. adjusted linear regression ########

# Function for adjusted linear regression using influencial statistics
# Return : list containing :
# plot : plot with data points, standard regression, adjusted regression and bayesian regression
# influencers_indices : index of points from the original df that are considered influencial and removed to get the adjusted linear regression
# lm_adjusted : adjusted linear regression
# lm_ajusted_cred : adjusted linear regression using bayesian regression
# residuals_adjusted : residuals of the adjusted linear regression
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
                  coef(lm_adjusted)[1],
                  coef(lm_adjusted_cred)[1]),
    slope = c(coef(lm_unadjusted)[2],
              coef(lm_adjusted)[2],
              coef(lm_adjusted_cred)[2]),
    model = c("Unadjusted", "Adjusted","Adjusted Bayes")
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
    scale_color_manual(values = c("Unadjusted" = "deepskyblue3",
                                  "Adjusted" = "aquamarine3",
                                  "Adjusted Bayes" = "slateblue2"),
                       name="Model") +
    
    scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16),
                       labels = c("Normal", "Influencer"),
                       name = "Point type") +
    
    # Title, xlab, ylab, theme
    labs(
      x = xlab,
      y = ylab,
      title = main
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 10)
    )
  
  # Resilient/Vulnerable anotations
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


## Adjusted fit results ###########

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


## Raw residuals functions #######

# Functions to get the groups from the raw residuals
get_groups_raw_residuals <- function(residuals,is_resilience_positive=FALSE){
  res_list <- list()
  for(i in 1:length(residuals)){
    
    # Result depends of the slope sign
    if(is_resilience_positive){
      res_list[i] <- if(is.na(residuals[i])){NA}
                      else if(residuals[i]>0){"resilient"}
                      else if(residuals[i]==0){"average"}
                      else{"vulnerable"}
    }
    else{
      res_list[i] <- if(is.na(residuals[i])){NA}
                    else if(residuals[i]<0){"resilient"}
                    else if(residuals[i]==0){"average"}
                    else{"vulnerable"}
    }
  }
  return(res_list)
}

# Visualization function for raw residuals
visualization_raw_residuals <- function(df, adversity, outcome, adjusted_lm, groups, main = "Groups using raw residuals") {
  
  # Adjusted linear regression coefficient for the plot
  intercept <- coef(adjusted_lm)[1]
  slope     <- coef(adjusted_lm)[2]
  
  # Add groups to the temporary df to color the points
  df$group <- factor(groups, levels = c("resilient", "average", "vulnerable"))
  
  # Viz
  plot <- ggplot(df, aes(x = .data[[adversity]], y = .data[[outcome]], color = group)) +
    geom_point(shape=1,size=0.8) +
    geom_abline(intercept = intercept, slope = slope, color = "grey", linetype = "solid") +
    labs(
      x = adversity,
      y = outcome,
      title = main,
      color = "Group"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10))+
    scale_color_manual(values=c("resilient"="skyblue","vulnerable"="coral","average"="grey"))
  
  # Add resilient and vulnerable text
  if(slope < 0){
    plot <-plot + geom_text(x=max(na.omit(df[[adversity]]))-5,y=max(na.omit(df[[outcome]]))-5,label="Resilient",alpha=0.2,color="grey") + geom_text(x=min(na.omit(df[[adversity]]))+5,y=min(na.omit(df[[outcome]]))+5,label="Vulnerable",alpha=0.2,color="grey")
  }
  else{
    plot <- plot + geom_text(x=min(na.omit(df[[adversity]]))+5,y=max(na.omit(df[[outcome]]))-5,label="Vulnerable",alpha=0.2,color="grey") + geom_text(x=max(na.omit(df[[adversity]]))-5,y=min(na.omit(df[[outcome]]))+5,label="Resilient",alpha=0.2,color="grey")
  }
  
  return(plot)
}

## Confidence residuals functions ######

# Predictions to get the lower and upper bounds for confidence intervals
# Two possibilities : confidence (x% sure for the mean value) or prediction (x% sure for an observation)

# Function to return groups based on confidence/credibility intervals
get_groups_intervals <- function(actual, pred,is_resilience_positive=FALSE) {
  
  # Initialization of the result vector
  groups <- vector("character", length(actual))
  
  # For each point
  for (i in 1:length(actual)) {
    # Look if there are NA values
    if (is.na(actual[i]) || is.na(pred$lwr[i]) || is.na(pred$upr[i])) {
      groups[i] <- NA
    }
    # Grouping depending if the value is under / over / within the bounds of the intervals
    # Condition on the slope sign
    else if(is_resilience_positive){
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

# Visualization function for intervals
visualization_intervals <- function(df, adversity, outcome, adjusted_lm, preds, labels, 
                                               main = "Intervals", 
                                               colors = c("#deebf7", "#9ecae1","skyblue2", "#6baed6", "#3182bd", "#08519c")) {
  # Coefficients of the linear regression
  intercept <- coef(adjusted_lm)[1]
  slope     <- coef(adjusted_lm)[2]
  
  # Base graph with points and linear regression 
  plot <- ggplot(df, aes(x = .data[[adversity]], y = .data[[outcome]])) +
    geom_point(shape = 1, size = 0.8) +
    geom_abline(intercept = intercept, slope = slope, color = "grey", linetype = "solid") +
    labs(
      x = adversity,
      y = outcome,
      title = main,
      fill = "Interval"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10))
  
  # Combine all intervals parsed in the function
  all_preds <- do.call(rbind, lapply(seq_along(preds), function(i) {
    pred <- preds[[i]]
    pred[[outcome]]   <- df[[outcome]]
    pred[[adversity]] <- df[[adversity]]
    pred$label <- labels[i]
    pred
  }))
  all_preds$label <- factor(all_preds$label, levels = labels)
  
  # Add ribbons for every intervals
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
    ) +
    scale_fill_manual(values = colors)
  
  # Resilient / Vulnerable annotations
  if (slope < 0) {
    plot <- plot +
      geom_text(x = max(na.omit(df[[adversity]])) - 5,
                y = max(na.omit(df[[outcome]])) - 5,
                label = "Resilient", alpha = 0.2, color = "grey") +
      geom_text(x = min(na.omit(df[[adversity]])) + 5,
                y = min(na.omit(df[[outcome]])) + 5,
                label = "Vulnerable", alpha = 0.2, color = "grey")
  } else {
    plot <- plot +
      geom_text(x = min(na.omit(df[[adversity]])) + 5,
                y = max(na.omit(df[[outcome]])) - 5,
                label = "Vulnerable", alpha = 0.2, color = "grey") +
      geom_text(x = max(na.omit(df[[adversity]])) - 5,
                y = min(na.omit(df[[outcome]])) + 5,
                label = "Resilient", alpha = 0.2, color = "grey")
  }
  
  return(plot)
}


## Quantile residuals functions ####
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

## Credibility residuals functions ##########
# We can do the same with a bayesian approach with a credibility interval (for the mean and for predicted values)

# Function to build the credibility intervals from the bayesian adjusted lm
get_credibility_intervals <- function(lm_adjusted_cred,newdata,lwr=0.025,upr=0.975){
  
  # Posterior linear prediction
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
  
  # Adding NA or computed value for every point of the dataset
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

## Standard deviation functions ########
get_groups_sd <- function(df, residuals, bins, adversity_string, is_resilience_positive, sd_multiplicator=1){
  res_SD <- c()
  bin_labels <- rep(NA, nrow(df))  # To stock the bin of each line.
  
  # Calculate the SD for each bin
  for (i in 1:(length(bins) - 1)) {
    in_bin <- df[[adversity_string]] > bins[i] & df[[adversity_string]] < bins[i + 1]
    
    bin_labels[in_bin] <- i #We save the i indice of the bin for each line that's in the bin
    
    residuals_bin <- residuals[in_bin]
    res_SD[i] <- sd(residuals_bin, na.rm = TRUE)
  }
  
  # Flag each residual as resilient, average or vulnerable
  groups_sd <- rep(NA, nrow(df))
  
  for (i in seq_along(residuals)) {
    bin_i <- bin_labels[i]
    
    # Skip if bin or residual is NA
    if (is.na(bin_i) || is.na(residuals[i])) next
    
    # Recuperate the SD for the bin and the residual of the current point
    sd_i <- res_SD[bin_i]*sd_multiplicator
    res <- residuals[i]
    
    # Look at the value of the residuals with respect to the SD
    if (abs(res) <= sd_i) {
      groups_sd[i] <- "average"
    }
    # Bigger residual -> resilient
    else if(is_resilience_positive){
      if (res > sd_i) {
        groups_sd[i] <- "resilient"
      } else if (res < -sd_i) {
        groups_sd[i] <- "vulnerable"
      }
    }
    # Bigger residual -> vulnerable
    else{
      if (res > sd_i) {
        groups_sd[i] <- "vulnerable"
      } 
      else if (res < -sd_i) {
        groups_sd[i] <- "resilient"
      }
    }
  }
  return(list(groups_sd=groups_sd,res_SD=res_SD*sd_multiplicator))
}

# Visualization function for the SD intervals
visualization_sd_intervals <- function(df,adversity,outcome,adjusted_lm,bins,res,main="SD Intervals"){
  # Adjusted linear model coefficients
  intercept <- coef(adjusted_lm)[1]
  slope     <- coef(adjusted_lm)[2]
  
  # Base graph with points and regression line
  plot <- ggplot(df, aes(x = .data[[adversity]], y = .data[[outcome]])) +
    geom_point(shape=1,size=0.8) +
    geom_abline(intercept = intercept, slope = slope, color = "grey", linetype = "solid") +
    labs(
      x = adversity,
      y = outcome,
      title = main,
      fill = "Interval"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10))
  
  # Color the area for each result
  all_polygons <- data.frame()
  
  for (i in seq_along(res)){
    res_SD <- res[[i]]$res_SD
    for(j in 1:(length(bins)-1)){
      point_j <- bins[[j]]
      point_j1 <- bins[[j+1]]
      fit_j <- intercept + slope * point_j
      fit_j1 <- intercept + slope * point_j1
      sd_j <- res_SD[[j]]
      
      
      df_area <- data.frame(
        x = c(point_j, point_j, point_j1, point_j1),
        y = c(fit_j - sd_j, fit_j + sd_j, fit_j1 + sd_j, fit_j1 - sd_j),
        group = paste0("poly_", i, "_", j),
        fill_factor = names_sd[[i]]
      )
      
      all_polygons <- rbind(all_polygons, df_area)
    }
  }
  
  # Add polygons to the main plot
  plot <- plot +
    geom_polygon(data = all_polygons, aes(x = x, y = y, group = group, fill = fill_factor), alpha = 0.3, color = NA) +
    scale_fill_manual(values = c("2SD" = "lightblue", "1SD" = "skyblue2", "0.5SD" = "deepskyblue3"))
  
  
  # Add resilient and vulnerable text
  if(slope < 0){
    plot <-plot + geom_text(x=max(na.omit(df[[adversity]]))-5,y=max(na.omit(df[[outcome]]))-5,label="Resilient",alpha=0.2,color="grey") + geom_text(x=min(na.omit(df[[adversity]]))+5,y=min(na.omit(df[[outcome]]))+5,label="Vulnerable",alpha=0.2,color="grey")
  }
  else{
    plot <- plot + geom_text(x=min(na.omit(df[[adversity]]))+5,y=max(na.omit(df[[outcome]]))-5,label="Vulnerable",alpha=0.2,color="grey") + geom_text(x=max(na.omit(df[[adversity]]))-5,y=min(na.omit(df[[outcome]]))+5,label="Resilient",alpha=0.2,color="grey")
  }
  
  return(plot)
  
}


## K-means functions ######
get_groups_kmeans <- function(df, residuals, is_resilience_positive, data_for_kmeans="residuals_only", outcome_string="", adversity_string="") {
  
  # Initialize result vector
  groups <- rep(NA_character_, nrow(df))
  
  # Create dataframe for clustering
  if(data_for_kmeans=="residuals_only"){
    non_na_indices <- which(!is.na(residuals))
    clean_residuals <- residuals[non_na_indices]
    df <- data.frame(residual = clean_residuals)
  }
  else{
    df <- df[,c(adversity_string,outcome_string)]
    non_na_indices <- which(!is.na(residuals))
    clean_residuals <- residuals[non_na_indices]
    clean_adversity <- df[non_na_indices,adversity_string]
    clean_outcome <- df[non_na_indices,outcome_string]
    df <- data.frame(residual = clean_residuals, adversity=clean_adversity,outcome=clean_outcome)
  }
  
  # Run K-means clustering with 3 centers
  clust <- kmeans(df, centers = 3)
  df$cluster <- as.factor(clust$cluster)
  
  # Calculate mean residual for each cluster
  cluster_means <- tapply(df$residual, df$cluster, mean)
  
  # Identify cluster corresponding to min, average, and max
  ordered_clusters <- names(sort(cluster_means))
  min_group <- ordered_clusters[1]
  average_group <- ordered_clusters[2]
  max_group <- ordered_clusters[3]
  
  # Assign labels based on cluster and resilience sign
  for (i in seq_along(non_na_indices)) {
    idx <- non_na_indices[i]
    cluster_label <- as.character(df$cluster[i])
    if (is_resilience_positive) {
      if (cluster_label == max_group) {
        groups[idx] <- "resilient"
      } else if (cluster_label == min_group) {
        groups[idx] <- "vulnerable"
      } else {
        groups[idx] <- "average"
      }
    } else {
      if (cluster_label == max_group) {
        groups[idx] <- "vulnerable"
      } else if (cluster_label == min_group) {
        groups[idx] <- "resilient"
      } else {
        groups[idx] <- "average"
      }
    }
  }
  
  return(groups)
}

visualization_groups <- function(df,adversity,outcome,adjusted_lm,groups,main="Clusturing results"){
  # Adjusted linear regression coefficient for the plot
  intercept <- coef(adjusted_lm)[1]
  slope     <- coef(adjusted_lm)[2]
  
  # Add groups to the temporary df to color the points
  df$group <- factor(groups, levels = c("resilient", "average", "vulnerable",NA))
  
  
  # Get the limit values of the residuals to visualize the separation of the groups
  df$residuals <- df[[outcome]] - (intercept + slope*df[[adversity]])
  # Resilient = positive residuals, vulnerable = negative residuals
  if(slope<0){
    resilient_limit <- min(df[df$group=="resilient",]$residuals, na.rm = TRUE)
    vulnerable_limit <- max(df[df$group=="vulnerable",]$residuals, na.rm = TRUE)
  }
  # Resilient = negative residuals, vulnerable = positive residuals
  else{
    resilient_limit <- max(df[df$group=="resilient",]$residuals, na.rm = TRUE)
    vulnerable_limit <- min(df[df$group=="vulnerable",]$residuals, na.rm = TRUE)
  }
  
  # Viz
  plot <- ggplot(df, aes(x = .data[[adversity]], y = .data[[outcome]], color = group)) +
    geom_point(shape=1,size=0.8) +
    geom_abline(intercept = intercept, slope = slope, color = "grey", linetype = "solid") +
    labs(
      x = adversity,
      y = outcome,
      title = main,
      color = "Group"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10))+
    scale_color_manual(values = c("resilient" = "skyblue", "average" = "grey", "vulnerable" = "coral"))
  
  # Add resilient and vulnerable text + lines for the seperation of the groups
  if(slope < 0){
    plot <-plot + geom_text(x=max(na.omit(df[[adversity]]))-5,y=max(na.omit(df[[outcome]]))-5,label="Resilient",alpha=0.2,color="grey") + geom_text(x=min(na.omit(df[[adversity]]))+5,y=min(na.omit(df[[outcome]]))+5,label="Vulnerable",alpha=0.2,color="grey")
    plot <- plot + 
      geom_abline(intercept = intercept+resilient_limit, slope = slope, color = "grey", linetype = "dashed")+
      geom_abline(intercept = intercept+vulnerable_limit, slope = slope, color = "grey", linetype = "dashed")
  }
  else{
    plot <- plot + geom_text(x=min(na.omit(df[[adversity]]))+5,y=max(na.omit(df[[outcome]]))-5,label="Vulnerable",alpha=0.2,color="grey") + geom_text(x=max(na.omit(df[[adversity]]))-5,y=min(na.omit(df[[outcome]]))+5,label="Resilient",alpha=0.2,color="grey")
    plot <- plot + 
      geom_abline(intercept = intercept+resilient_limit, slope = slope, color = "grey", linetype = "dashed")+
      geom_abline(intercept = intercept+vulnerable_limit, slope = slope, color = "grey", linetype = "dashed")
    }
  
  return(plot)
}


## Hierarchical clusturing functions ####
get_groups_hclust <- function(df, residuals, is_resilience_positive, data_for_hclust="residuals_only", outcome_string="", adversity_string="",distance="euclidean",method="average") {
  
  # Initialize result vector
  groups <- rep(NA_character_, nrow(df))
  
  # Create dataframe for clustering
  if(data_for_hclust=="residuals_only"){
    non_na_indices <- which(!is.na(residuals))
    clean_residuals <- residuals[non_na_indices]
    df <- data.frame(residual = clean_residuals)
  }
  else{
    df <- df[,c(adversity_string,outcome_string)]
    non_na_indices <- which(!is.na(residuals))
    clean_residuals <- residuals[non_na_indices]
    clean_adversity <- df[non_na_indices,adversity_string]
    clean_outcome <- df[non_na_indices,outcome_string]
    df <- data.frame(residual = clean_residuals, adversity=clean_adversity,outcome=clean_outcome)
  }
  
  # Run Hclust clustering with 3 centers
  dist <- dist(df,method=distance)
  clust <- hclust(d = dist, method=method)
  df$cluster <- as.factor(cutree(clust,k=3))
  
  # Calculate mean residual for each cluster
  cluster_means <- tapply(df$residual, df$cluster, mean)
  
  # Identify cluster corresponding to min, average, and max
  ordered_clusters <- names(sort(cluster_means))
  min_group <- ordered_clusters[1]
  average_group <- ordered_clusters[2]
  max_group <- ordered_clusters[3]
  
  # Assign labels based on cluster and resilience sign
  for (i in seq_along(non_na_indices)) {
    idx <- non_na_indices[i]
    cluster_label <- as.character(df$cluster[i])
    if (is_resilience_positive) {
      if (cluster_label == max_group) {
        groups[idx] <- "resilient"
      } else if (cluster_label == min_group) {
        groups[idx] <- "vulnerable"
      } else {
        groups[idx] <- "average"
      }
    } else {
      if (cluster_label == max_group) {
        groups[idx] <- "vulnerable"
      } else if (cluster_label == min_group) {
        groups[idx] <- "resilient"
      } else {
        groups[idx] <- "average"
      }
    }
  }
  
  return(groups)
}
## Results presentation commands ######

# Example 3
df <- df_CA
adversity_string <- "T1_FAS"
outcome_string <- "T1_BDI_II"
outcome <- df_CA$T1_BDI_II
bins <- bins_FAS
res <- adjusted_fit(df=df,adversity=adversity_string,outcome=outcome_string)


# Example 2
df <- df_CA
adversity_string <- "T1_CPTS"
outcome_string <- "T1_WES_total"
outcome <- df_CA$T1_WES_total
bins <- bins_CPTS
res <- adjusted_fit(df=df_CA,adversity="T1_CPTS",outcome="T1_WES_total",main="Adjusted and unadjusted linear regression for CPTS and WES for Canada at T1",xlab="CPTS",ylab="WES")

# Example 1
df <- df_CA
adversity_string <- "T1_CPTS"
outcome_string <- "T1_BDI_II"
outcome <- df_CA$T1_BDI_II
bins <- bins_CPTS
res <- adjusted_fit(df=df,adversity=adversity_string,outcome=outcome_string)


# Get the info
lm_adjusted <- res$lm_adjusted
lm_adjusted_cred <- res$lm_adjusted_cred
residuals <- res$residuals_adjusted
plot <- res$plot
resilience_sign <- lm_adjusted$coefficients[2]<0

# Raw residuals
groups_raw <- get_groups_raw_residuals(residuals,is_resilience_positive=resilience_sign)
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
  groups <- get_groups_intervals(outcome, preds_conf[[i]],is_resilience_positive=resilience_sign)
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
  groups <- get_groups_intervals(outcome, preds_cred[[i]],is_resilience_positive=resilience_sign)
  groups_credibility[[ names_cred[[i]] ]] <- groups
  df_n_groups <- rbind(df_n_groups,
                       data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names_cred[[i]])))
}

# Standard deviation
list_sd_multiplicator <- list(2,1,0.5)
names_sd <- list("2SD","1SD","0.5SD")

groups_sd <- list()
res <- list()
for(i in 1:length(list_sd_multiplicator)){
  res[[i]] <- get_groups_sd(df, residuals, bins, adversity_string, resilience_sign, sd_multiplicator=list_sd_multiplicator[[i]])
  groups <- res[[i]]$groups_sd
  groups_sd[[ names_sd[[i]] ]] <- groups
  df_n_groups <- rbind(df_n_groups,
                       data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names_sd[[i]])))
}

# Kmeans (only residuals, adversity and outcome, residuals + adversity +outcome)
groups_kmeans_residuals_only <- get_groups_kmeans(df, residuals,resilience_sign,outcome_string = outcome_string,adversity_string = adversity_string)
groups_kmeans_all1 <- get_groups_kmeans(df, residuals,resilience_sign,data_for_kmeans = "all",outcome_string = outcome_string,adversity_string = adversity_string)
groups_kmeans_all2 <- get_groups_kmeans(df, residuals,resilience_sign,data_for_kmeans = "all",outcome_string = outcome_string,adversity_string = adversity_string)
groups_kmeans_all3 <- get_groups_kmeans(df, residuals,resilience_sign,data_for_kmeans = "all",outcome_string = outcome_string,adversity_string = adversity_string)
groups_kmeans_all4 <- get_groups_kmeans(df, residuals,resilience_sign,data_for_kmeans = "all",outcome_string = outcome_string,adversity_string = adversity_string)

list_results_kmeans <- list(groups_kmeans_residuals_only,groups_kmeans_all1,groups_kmeans_all2,groups_kmeans_all3,groups_kmeans_all4)
list_names_kmeans <- list("kmeans residuals only","kmeans all 1","kmeans all 2","kmeans all 3","kmeans all 4")

for(i in 1:length(list_results_kmeans)){
  df_n_groups <- rbind(df_n_groups,
                       data.frame(resilient = sum(list_results_kmeans[[i]]=="resilient", na.rm=TRUE), average = sum(list_results_kmeans[[i]]=="average", na.rm=TRUE), vulnerable = sum(list_results_kmeans[[i]]=="vulnerable", na.rm=TRUE), row.names=c(list_names_kmeans[[i]])))
}

# Hierarchical clusturing (only residuals, adversity and outcome, residuals + adversity +outcome)
methodes <- list("average","single","complete","ward.D","ward.D2")
distances <- list("euclidean","manhattan","canberra")
groups_hclust <- list()
for(i in 1:length(methodes)){
  for(j in 1:length(distances)){
    groups <- get_groups_hclust(df, residuals,resilience_sign,outcome_string = outcome_string,adversity_string = adversity_string,distance=distances[j],method=methodes[i])
    groups_hclust[[ paste("Hclust",methodes[i],distances[j]) ]] <- groups
    df_n_groups <- rbind(df_n_groups,
                         data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(paste("Hclust",distances[j],methodes[i]))))
  }
}


## Results #########
# Cardinal of each group
View(df_n_groups)

# Groups
groups_raw
groups_confidence
groups_quantile
groups_credibility
groups_sd
groups_kmeans_residuals_only
groups_hclust

# Visualizations
visualization_raw_residuals(df,adversity_string,outcome_string,lm_adjusted,groups_raw)
visualization_intervals(df=df,adversity=adversity_string,outcome=outcome_string,adjusted_lm =lm_adjusted,preds_conf,names_conf,main="Confidence intervals")
visualization_intervals(df=df,adversity=adversity_string,outcome=outcome_string,adjusted_lm =lm_adjusted_cred,preds_cred,names_cred,main="Credibility intervals")
visualization_sd_intervals(df,adversity=adversity_string,outcome=outcome_string,adjusted_lm=lm_adjusted,bins=bins,res=res,main="SD Intervals")
qqnorm(residuals)
qqline(residuals)
visualization_groups(df,adversity_string,outcome_string,lm_adjusted,groups_kmeans_residuals_only,main="Kmeans residuals only")
visualization_groups(df,adversity_string,outcome_string,lm_adjusted,groups_kmeans_all1,main="Kmeans with adversity + outcome + residuals 1")
visualization_groups(df,adversity_string,outcome_string,lm_adjusted,groups_kmeans_all2,main="Kmeans with adversity + outcome + residuals 2")
visualization_groups(df,adversity_string,outcome_string,lm_adjusted,groups_hclust$`Hclust ward.D2 euclidean`,main="Hierachical clusturing residuals only")

## Comparison of 2 clusterings #####
# Function to get the cross-table, the number of elements in common and the corresponding proportion
number_equal_predictions <- function(groups1,groups2,name1="Group1",name2="Group2"){
  table <- table(groups1,groups2,dnn=list(name1,name2))
  n_total <- sum(!is.na(groups1))
  n_common <- sum(groups1==groups2,na.rm=TRUE)
  return(list(table=table,n_common=n_common,proportion=n_common/n_total))
}

# Function to visualize 2 clusturing
visualization_comparison <- function(df, adversity_string, outcome_string, adjusted_lm, groups1, groups2, name1 = "Groups1", name2 = "Groups2") {
  # Adjusted linear regression coefficient
  intercept <- coef(adjusted_lm)[1]
  slope     <- coef(adjusted_lm)[2]
  
  df1 <- df
  df2 <- df
  
  df1$group <- factor(groups1, levels = c("resilient", "average", "vulnerable", NA))
  df1$method <- factor(rep(name1, nrow(df)), levels = c(name1, name2))
  df2$group <- factor(groups2, levels = c("resilient", "average", "vulnerable", NA))
  df2$method <- factor(rep(name2, nrow(df)), levels = c(name1, name2))
  
  df <- rbind(df1, df2)
  df$method <- factor(df$method, levels = c(name1, name2))  # important de le refaire ici
  df$group <- factor(df$group,levels=c("resilient", "average", "vulnerable", NA))
  
  # Combine into a plot
  plot <- ggplot(df, aes(x = .data[[adversity_string]], y = .data[[outcome_string]])) +
    # Groups1: hollow circles
    geom_point(aes(color = group, shape = method), size = 2, alpha = 0.8)+
    geom_abline(intercept = intercept, slope = slope, color = "grey", linetype = "dashed") +
    labs(
      x = adversity_string,
      y = outcome_string,
      title = paste(name1, "vs", name2, "clustering"),
      color = "Group",
      shape = "Method"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(size = 10)) +
    scale_color_manual(values = c("resilient" = "skyblue", "average" = "grey", "vulnerable" = "coral")) +
    scale_shape_manual(values = setNames(c(1, 3), c(name1, name2)))
  
      
  # Add contextual labels
  if (slope < 0) {
    plot <- plot +
      geom_text(x = max(na.omit(df[[adversity_string]])) - 5, y = max(na.omit(df[[outcome_string]])) - 5, label = "Resilient", alpha = 0.2, color = "grey") +
      geom_text(x = min(na.omit(df[[adversity_string]])) + 5, y = min(na.omit(df[[outcome_string]])) + 5, label = "Vulnerable", alpha = 0.2, color = "grey")
  } else {
    plot <- plot +
      geom_text(x = min(na.omit(df[[adversity_string]])) + 5, y = max(na.omit(df[[outcome_string]])) - 5, label = "Vulnerable", alpha = 0.2, color = "grey") +
      geom_text(x = max(na.omit(df[[adversity_string]])) - 5, y = min(na.omit(df[[outcome_string]])) + 5, label = "Resilient", alpha = 0.2, color = "grey")
  }
  
  return(plot)
}


number_equal_predictions(groups_kmeans_residuals_only,groups_quantile[[4]],name1="Kmeans",name2="Quantile 25%")
visualization_comparison(df_CA,adversity_string,outcome_string,lm_adjusted,groups_kmeans_residuals_only,groups_quantile[[4]],name1="Kmeans",name2="Quantile 25%")
visualization_comparison(df_CA,adversity_string,outcome_string,lm_adjusted,groups_kmeans_residuals_only,groups_hclust$`Hclust ward.D2 euclidean`,name1="Kmeans",name2="Hclust ward.D2 euclidean")



## LPA and LCA preparation ####
# 1 risk -> CPTS
# 3 outcomes -> depression, SES and SF-15

# Look at the normality of the residuals

# For each combination take 4-5 intervals and compare them : 1SD, 0.5SD, quantile 25%, quantile 10%, conf 95%
# -> look more at different distributions of groupings than really the method itself but is ok.
# -> jusitify the utilization of big or small intervals.



## LPA ####
# Take the residuals * adversity with residuals =0 if group=average

## LCA ####
# Take the class and not the residuals.