## Utils
## Packages ######
library(car) # lm testing
library(caret) # confusion matrix
library(corrr) # correlation
library(corrplot) # plot
library(dplyr) # dataframe managment
library(e1071) #NaiveBayes
library(factoextra) # PCA
library(FactoMineR) # PCA
library(Factoshiny) # PCA
library(ggplot2) # ploting
library(generics)
library(gridExtra) # plotinh
library(haven) # read sav data
library(lmtest) # lm testing
library(MASS) # for stepAIC + lda + qda
library(missForest) # to impute data
library(olsrr) # influence statistic
library(poLCA) # Latent Class Analysis
library(rpart) # Classification trees
library(rstanarm) # bayesian lm
library(sandwich) # lm testing
library(tidyLPA) # LPA
library(tidymodels) # multiclass classification regression

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
                  coef(lm_adjusted)[1]),
    slope = c(coef(lm_unadjusted)[2],
              coef(lm_adjusted)[2]),
    model = c("Unadjusted", 
              "Adjusted")
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
                                  "Adjusted" = "aquamarine3"),
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
    in_bin <- df[[adversity_string]] >= bins[i] & df[[adversity_string]] < bins[i + 1]
    
    bin_labels[in_bin] <- i #We save the i indice of the bin for each line that's in the bin
    
    residuals_bin <- residuals[in_bin]
    if (sum(in_bin) == 1) {
      res_SD[i] <- 0
    } else {
      res_SD[i] <- sd(residuals[in_bin])
    }
    
  }
  
  # Flag each residual as resilient, average or vulnerable
  groups_sd <- rep(NA, nrow(df))
  
  for (i in seq_along(residuals)) {
    bin_i <- bin_labels[i]
    
    # Skip if bin or residual is NA
    if (is.na(bin_i) || is.na(residuals[i])) next
    
    # Recuperate the SD for the bin and the residual of the current point
    sd_i <- res_SD[bin_i]*sd_multiplicator
    res <- residuals[[i]]
    
    # Look at the value of the residuals with respect to the SD
    if(abs(res) <= sd_i) {
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
visualization_sd_intervals <- function(df,adversity,outcome,adjusted_lm,bins,res,names_sd,main="SD Intervals"){
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

## Functions : Get all groups + transformation of residuals ####

transform_residuals<-function(residuals,adversity,is_resilience_positive,method){
  res <- c()
  for(i in 1:length(residuals)){
    if(method=="multiply"){
      res[i] <- residuals[i]*adversity[i]
    }
    else if(method=="log_multiply"){
      res[i] <- residuals[i]*log(1+adversity[i])
    }
    else if(method=="multiply_divide"){
      if(is_resilience_positive){
        res_i <- residuals[i]
        if(res_i>=0){
          res[i] <- res_i*adversity[i]
        }
        else{
          res[i] <- res_i/adversity[i]
        }
      }
      else{
        res_i <- residuals[i]
        if(res_i>=0){
          res[i] <- res_i/adversity[i]
        }
        else{
          res[i] <- res_i*adversity[i]
        }
      }
    }
    else if(method=="log_multiply_divide"){
      if(is_resilience_positive){
        res_i <- residuals[i]
        if(res_i>=0){
          res[i] <- res_i*log(1+adversity[i])
        }
        else{
          res[i] <- res_i/log(1+adversity[i])
        }
      }
      else{
        res_i <- residuals[i]
        if(res_i>=0){
          res[i] <- res_i/log(1+adversity[i])
        }
        else{
          res[i] <- res_i*log(1+adversity[i])
        }
      }
    }
  }
  print(head(res,10))
  return(res)
}

# Function to get a dataframe with all of the grouping methods result and the dataframe with the sizes of each group for each method
get_all_groups <- function(df,adversity_string,outcome_string,bins,res,modification="nothing",visualization=TRUE){
  
  outcome <- df[[outcome_string]]
  adversity <- df[[adversity_string]]
  
  # Initialize the result data_frames : one with the grouping for each person and each method and one with the number of people in each group for each method
  df_n_groups <- data.frame(resilient=c(),average=c(),vulnerable=c())
  df_result <- data.frame(residuals=res$residuals_adjusted,adversity=adversity)
  
  # Get the info
  lm_adjusted <- res$lm_adjusted
  lm_adjusted_cred <- res$lm_adjusted_cred
  plot <- res$plot
  resilience_sign <- lm_adjusted$coefficients[2]<0
  if(modification!="nothing"){
    residuals <- transform_residuals(res$residuals_adjusted,adversity,resilience_sign,method=modification)
  }
  else{
    residuals <- res$residuals_adjusted
  }
  
  
  # Raw residuals
  groups_raw <- get_groups_raw_residuals(residuals,is_resilience_positive=resilience_sign)
  df_n_groups <- rbind(df_n_groups,data.frame(resilient = sum(groups_raw=="resilient", na.rm=TRUE), average = sum(groups_raw=="average", na.rm=TRUE), vulnerable = sum(groups_raw=="vulnerable", na.rm=TRUE), row.names=c("raw")))
  df_result[["raw"]] <- groups_raw
  
  if(visualization){
    print(visualization_raw_residuals(df,adversity_string,outcome_string,lm_adjusted,groups_raw))
  }
  
  # Prediction and confidence intervals
  preds_conf <- list(
    as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.75)),
    as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.6)),
    as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.5)),
    as.data.frame(predict(lm_adjusted, newdata = df, interval = "confidence", level = 0.99)),
    as.data.frame(predict(lm_adjusted, newdata = df, interval = "confidence", level = 0.95))
  )
  names_conf <- list(
    "pred. residuals (75%)",
    "pred. residuals (60%)",
    "pred. residuals (50%)",
    "conf. residuals (99%)",
    "conf. residuals (95%)"
  )
  for(i in 1:length(preds_conf)){
    groups <- get_groups_intervals(outcome, preds_conf[[i]],is_resilience_positive=resilience_sign)
    df_n_groups <- rbind(df_n_groups,
                         data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names_conf[[i]])))
    df_result[[names_conf[[i]]]] <- groups
  }
  
  if(visualization){
    print(visualization_intervals(df=df,adversity=adversity_string,outcome=outcome_string,adjusted_lm =lm_adjusted,preds_conf,names_conf,main="Confidence and prediction intervals"))
  }
  
  
  
  # Quantiles
  list_quantile_sub <- list(0.05,0.1,0.15,0.2,0.25,0.3,0.35)
  list_quantile_sup <- list(0.05,0.1,0.15,0.2,0.25,0.3,0.35)
  names_quant <- list(
    "quantiles (5%)",
    "quantiles (10%)",
    "quantiles (15%)",
    "quantiles (20%)",
    "quantiles (25%)",
    "quantiles (30%)",
    "quantiles (35%)"
  )
  for(i in 1:length(list_quantile_sub)){
    groups <- get_groups_quantile(residuals,list_quantile_sub[[i]],list_quantile_sup[[i]],is_resilience_positive=resilience_sign)
    df_n_groups <- rbind(df_n_groups,
                         data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names_quant[[i]])))
    df_result[[names_quant[[i]]]] <- groups
  }
  
  # Credibility intervals
  preds_cred <- list(
    get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.0005,upr=0.9995),
    get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.005,upr=0.995),
    get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.025,upr=0.975),
    get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.05,upr=0.95),
    get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.125,upr=0.875),
    get_credibility_intervals(lm_adjusted_cred,newdata=df[c(adversity_string)],lwr=0.25,upr=0.75)
  )
  names_cred <- list(
    "cred. 99.9%",
    "cred. 99%",
    "cred. 95%",
    "cred. 90%",
    "cred. 75%",
    "cred. 50%"
  )
  for(i in 1:length(preds_cred)){
    groups <- get_groups_intervals(outcome, preds_cred[[i]],is_resilience_positive=resilience_sign)
    df_n_groups <- rbind(df_n_groups,
                         data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names_cred[[i]])))
    df_result[[names_cred[[i]]]] <- groups
  }
  
  if(visualization){
    print(visualization_intervals(df=df,adversity=adversity_string,outcome=outcome_string,adjusted_lm =lm_adjusted_cred,preds_cred,names_cred,main="Credibility intervals"))
  }
  
  # Standard deviation
  list_sd_multiplicator <- list(2,1,0.5)
  names_sd <- list("2SD","1SD","0.5SD")
  res_sd <- list()
  for(i in 1:length(list_sd_multiplicator)){
    res_sd[[i]] <- get_groups_sd(df, residuals, bins, adversity_string, resilience_sign, sd_multiplicator=list_sd_multiplicator[[i]])
    groups <- res_sd[[i]]$groups_sd
    df_n_groups <- rbind(df_n_groups,
                         data.frame(resilient = sum(groups=="resilient", na.rm=TRUE), average = sum(groups=="average", na.rm=TRUE), vulnerable = sum(groups=="vulnerable", na.rm=TRUE), row.names=c(names_sd[[i]])))
    df_result[[names_sd[[i]]]] <- groups
  }
  if(visualization){
    print(visualization_sd_intervals(df,adversity=adversity_string,outcome=outcome_string,adjusted_lm=lm_adjusted,bins=bins,res=res_sd,names_sd=names_sd,main="SD Intervals"))
  }
  
  # Kmeans (only residuals)
  groups_kmeans <- get_groups_kmeans(df, residuals,resilience_sign,outcome_string = outcome_string,adversity_string = adversity_string)
  df_n_groups <- rbind(df_n_groups,
                       data.frame(resilient = sum(groups_kmeans=="resilient", na.rm=TRUE), average = sum(groups_kmeans=="average", na.rm=TRUE), vulnerable = sum(groups_kmeans=="vulnerable", na.rm=TRUE), row.names=c("Kmeans")))
  df_result[["Kmeans"]] <- groups_kmeans
  if(visualization){
    print(visualization_groups(df,adversity_string,outcome_string,lm_adjusted,groups_kmeans,main="Groups using k-means algorithm"))
  }
  
  return(list(df_result=df_result,df_n_groups=df_n_groups))
}

get_all_groups_small <- function(df,residuals,adversity_string,outcome_string,bins,lm_adjusted){
  
  outcome <- df[[outcome_string]]
  adversity <- df[[adversity_string]]
  resilience_sign <- lm_adjusted$coefficients[[2]]<0
  
  
  # Initialize the result data_frames : one with the grouping for each person and each method and one with the number of people in each group for each method
  df_result <- data.frame(residuals=residuals)
  
  # Prediction and confidence intervals
  preds_conf <- list(
    as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.75)),
    as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.6)),
    as.data.frame(predict(lm_adjusted, newdata = df, interval = "prediction", level = 0.5))
  )
  names_conf <- list(
    "pred. residuals (75%)",
    "pred. residuals (60%)",
    "pred. residuals (50%)"
  )
  for(i in 1:length(preds_conf)){
    groups <- get_groups_intervals(outcome, preds_conf[[i]],is_resilience_positive=resilience_sign)
    df_result[[names_conf[[i]]]] <- groups
  }
  
  # Quantiles
  list_quantile_sub <- list(0.15,0.2,0.25,0.3)
  list_quantile_sup <- list(0.15,0.2,0.25,0.3)
  names_quant <- list(
    "quantiles (15%)",
    "quantiles (20%)",
    "quantiles (25%)",
    "quantiles (30%)"
  )
  for(i in 1:length(list_quantile_sub)){
    groups <- get_groups_quantile(residuals,list_quantile_sub[[i]],list_quantile_sup[[i]],is_resilience_positive=resilience_sign)
    df_result[[names_quant[[i]]]] <- groups
  }
  
  # Standard deviation
  list_sd_multiplicator <- list(1,0.5)
  names_sd <- list("1SD","0.5SD")
  res_sd <- list()
  for(i in 1:length(list_sd_multiplicator)){
    res_sd[[i]] <- get_groups_sd(df, residuals, bins, adversity_string, resilience_sign, sd_multiplicator=list_sd_multiplicator[[i]])
    groups <- res_sd[[i]]$groups_sd
    df_result[[names_sd[[i]]]] <- groups
  }
  
  # Kmeans (only residuals)
  groups_kmeans <- get_groups_kmeans(df, residuals,resilience_sign,outcome_string = outcome_string,adversity_string = adversity_string)
  df_result[["Kmeans"]] <- groups_kmeans

  return(df_result)
}

## Functions : classification ####
classification_metrics <- function(true_labels, predicted_labels) {
  # Convert to factors with same levels
  levels <- c("resilient", "average", "vulnerable")
  true_labels <- factor(true_labels, levels = levels)
  predicted_labels <- factor(predicted_labels, levels = levels)
  
  # Confusion Matrix
  cm <- caret::confusionMatrix(predicted_labels, true_labels)
  
  # Extract raw table
  cm_table <- cm$table
  n_classes <- length(levels)
  
  # Print
  print(cm_table)
  
  # Initialize results list
  results <- list()
  
  # Overall accuracy
  results$accuracy <- cm$overall["Accuracy"]
  
  # Balanced accuracy = mean(recall per class)
  recalls <- numeric(n_classes)
  precisions <- numeric(n_classes)
  f1s <- numeric(n_classes)
  fns <- numeric(n_classes)
  fps <- numeric(n_classes)
  supports <- numeric(n_classes)
  
  for (i in 1:n_classes) {
    class <- levels[i]
    
    TP <- cm_table[i, i]
    FN <- sum(cm_table[, i]) - TP
    FP <- sum(cm_table[i, ]) - TP
    TN <- sum(cm_table) - TP - FP - FN
    
    recall <- if ((TP + FN) == 0) NA else TP / (TP + FN)
    precision <- if ((TP + FP) == 0) NA else TP / (TP + FP)
    f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA else 2 * (precision * recall) / (precision + recall)
    
    recalls[i] <- recall
    precisions[i] <- precision
    f1s[i] <- f1
    fns[i] <- FN
    fps[i] <- FP
    supports[i] <- sum(cm_table[, i])
  }
  
  names(recalls) <- levels
  names(precisions) <- levels
  names(f1s) <- levels
  names(fns) <- paste0("FN_", levels)
  names(fps) <- paste0("FP_", levels)
  names(supports) <- levels
  
  results$macro_recall <- mean(recalls, na.rm = TRUE)
  results$macro_precision <- mean(precisions,na.rm=TRUE)
  results$f1_per_class <- f1s
  results$precision_per_class <- precisions
  results$recall_per_class <- recalls
  results$macro_f1 <- mean(f1s, na.rm = TRUE)
  results$false_negatives <- fns
  results$false_positives <- fps
  results$support <- supports
  
  return(results)
}

estimation_classification <- function(df,df_result,item_name,list_group_names,method="classification_tree",predictors=c("T1_Sex", "T1_Age", paste0("T1_CYRM_", 1:28))){
  set.seed(1) # For reproductibility
  res <- data.frame()
  
  for(i in 1:length(list_group_names)){
    # Get the grouping
    group_name <- list_group_names[[i]]
    print(group_name)
    
    # We get the groups for the chosen grouping for all 3 items
    df[["groups"]] <- df_result[[group_name]]
    y <- df[["groups"]]
    X <- df[, predictors]
    
    # We choose the classification method and look at the results
    if(method=="classification_tree"){
      formula <- as.formula(paste("groups ~", paste(predictors, collapse = " + ")))
      arbre <- rpart(formula, data = df, method = "class")
      predictions <- predict(arbre, type = "class")
    }
    else if(method=="LDA"){
      model_lda <- lda(X,y)
      predictions <- predict(model_lda, X)$class
    }
    else if(method=="Naive_Bayes"){
      formula <- as.formula(paste("groups ~", paste(predictors, collapse = " + ")))
      model_nb <- naiveBayes(formula, data = df)
      predictions <- predict(model_nb,newdata=df,type="class")
    }
    else if(method=="Logistic_Regression"){
      df[["groups"]] <- factor(df[["groups"]], levels = c("resilient", "average", "vulnerable"))
      formula <- as.formula(paste("groups ~", paste(predictors, collapse = " + ")))
      model_fit <- multinom_reg() |> fit(formula, data = df)
      preds <- model_fit |> augment(new_data = df)
      predictions <- preds$.pred_class
    }
    
    # Metrics
    metrics <- classification_metrics(df[["groups"]],predictions)
    null_model <- max(metrics$support[["average"]],metrics$support[["resilient"]],metrics$support[["vulnerable"]]) / (metrics$support[["resilient"]]+metrics$support[["average"]]+metrics$support[["vulnerable"]])
    
    res <- rbind(res, data.frame(
      group_name = group_name,
      accuracy = metrics$accuracy[[1]],
      null_model = null_model,
      difference = metrics$accuracy[[1]]-null_model,
      macro_precision = metrics$macro_precision[[1]],
      macro_recall = metrics$macro_recall[[1]],
      macro_f1 = metrics$macro_f1[[1]],
      precision_resilient = metrics$precision_per_class[["resilient"]],
      recall_resilient = metrics$recall_per_class[["resilient"]],
      f1score_resilient = metrics$f1_per_class[["resilient"]],
      precision_average = metrics$precision_per_class[["average"]],
      recall_average = metrics$recall_per_class[["average"]],
      f1score_average = metrics$f1_per_class[["average"]],
      precision_vulnerable = metrics$precision_per_class[["vulnerable"]],
      recall_vulnerable = metrics$recall_per_class[["vulnerable"]],
      f1score_vulnerable = metrics$f1_per_class[["vulnerable"]],
      support_resilient = metrics$support[["resilient"]],
      support_average = metrics$support[["average"]],
      support_vulnerable = metrics$support[["vulnerable"]]
    ))
  }
  return(res)
}

estimation_classification_tree_with_test <- function(df, df_result, item_name, list_group_names, n_perm = 100, predictors=c("T1_Sex", "T1_Age", paste0("T1_CYRM_", 1:28))) {
  set.seed(1) # For reproducibility
  res <- data.frame()
  
  for (i in seq_along(list_group_names)) {
    group_name <- list_group_names[[i]]
    cat("Processing:", group_name, "\n")
    
    df[["groups"]] <- df_result[[group_name]]
    y <- df[["groups"]]
    X <- df[, predictors]
    
    # Build and evaluate model on real data
    formula <- as.formula(paste("groups ~", paste(predictors, collapse = " + ")))
    arbre <- rpart(formula, data = df, method = "class")
    predictions <- predict(arbre, type = "class")
    
    metrics <- classification_metrics(df[["groups"]], predictions)
    acc_obs <- metrics$accuracy[[1]]
    
    # Permutation test
    perm_accuracies <- numeric(n_perm)
    for (j in 1:n_perm) {
      df$groups_perm <- sample(df[["groups"]])  # shuffle labels
      arbre_perm <- rpart(groups_perm ~ ., data = cbind(df[predictors], groups_perm = df$groups_perm), method = "class")
      preds_perm <- predict(arbre_perm, type = "class")
      perm_accuracies[j] <- mean(preds_perm == df$groups_perm)
    }
    p_value <- mean(perm_accuracies >= acc_obs)
    
    
    null_model <- max(metrics$support[["average"]],metrics$support[["resilient"]],metrics$support[["vulnerable"]]) / sum(unlist(metrics$support))
    
    # Add the result to the global dataframe
    res <- rbind(res, data.frame(
      group_name = group_name,
      accuracy = acc_obs,
      null_model = null_model,
      difference = acc_obs-null_model,
      p_value_permutation = p_value,
      macro_precision = metrics$macro_precision[[1]],
      macro_recall = metrics$macro_recall[[1]],
      macro_f1 = metrics$macro_f1[[1]],
      precision_resilient = metrics$precision_per_class[["resilient"]],
      recall_resilient = metrics$recall_per_class[["resilient"]],
      f1score_resilient = metrics$f1_per_class[["resilient"]],
      precision_average = metrics$precision_per_class[["average"]],
      recall_average = metrics$recall_per_class[["average"]],
      f1score_average = metrics$f1_per_class[["average"]],
      precision_vulnerable = metrics$precision_per_class[["vulnerable"]],
      recall_vulnerable = metrics$recall_per_class[["vulnerable"]],
      f1score_vulnerable = metrics$f1_per_class[["vulnerable"]],
      support_resilient = metrics$support[["resilient"]],
      support_average = metrics$support[["average"]],
      support_vulnerable = metrics$support[["vulnerable"]]
    ))
  }
  return(res)
}

# Function to modify the residuals depending on the group
transformed_residuals <- function(df_result,group_name,method="nothing"){
  res <- c()
  for(i in 1:nrow(df_result)){
    group <- df_result[i,group_name,drop=TRUE]
    
    # If the person is in the average group then the residual is set to 0.
    if(group=='average'){
      res <- c(res,0)
    }
    # Else we do a transformation
    else{
      if(method=="nothing"){
        res <- c(res,df_result[i,"residuals"])
      }
      else if(method=="log_multiply_divide"){
        # Multiply/Divide (depending on the group) the residuals by log(1+adversity)
        if(group=="resilient"){
          res <- c(res,df_result[i,"residuals"]*log1p(1+df_result[i,"adversity"]))
        }
        else{
          res <- c(res,df_result[i,"residuals"]/log1p(1+df_result[i,"adversity"]))
        }
      }
      else if(method=="multiply_divide"){
        # Multiply/Divide (depending on the group) the residual by the adversity
        if(group=="resilient"){
          res <- c(res,df_result[i,"residuals"]*(1+df_result[i,"adversity"]))
        }
        else{
          res <- c(res,df_result[i,"residuals"]/(1+df_result[i,"adversity"]))
        }
      }
      else if(method=="log_multiply"){
        # Multiply the residual by log(1+adversity)
        res <- c(res,df_result[i,"residuals"]*log1p(1+df_result[i,"adversity"]))
      }
      else{
        # Multiply the residual by the adversity
        res <- c(res,df_result[i,"residuals"]*df_result[i,"adversity"])
      }
    }
  }
  return(res)
}

# Function for regression trees
regression_tree <- function(df,df_result,list_group_names,predictors=explication_vars,method="nothing"){
  set.seed(1) # For reproductibility
  res <- data.frame()
  residuals <- df_result[["residuals"]]
  
  for(i in 1:length(list_group_names)){
    # Get the grouping
    group_name <- list_group_names[[i]]
    print(group_name)
    
    # We transform the residuals according to the groups
    df[[paste0("residuals",group_name)]] <- transformed_residuals(df_result,group_name,method=method)
    
    # We choose the classification method and look at the results
    response_var <- paste0("residuals", group_name)
    response_var <- paste0("`", response_var, "`")
    f <- paste0(response_var, " ~ ", paste(predictors, collapse = " + "))
    formula <- as.formula(f)
    tree <- rpart(formula, data = df)
    prediction <- predict(tree)
    
    # We calculate metrics
    true <- df[[paste0("residuals",group_name)]]
    n <- nrow(df_result)
    MSE <- mean((true-prediction)^2)
    RMSE <- sqrt(MSE)
    MAE <- mean(abs(true-prediction))
    R.squared <- 1 - sum((true-prediction)^2)/sum((true-mean(true))^2)
    R.squared.adjusted <- 1 - (1-R.squared)*(n-1)/(n-length(predictors)-1)
    
    
    res <- rbind(res, data.frame(
      group_name = group_name,
      average_group_size=sum(df_result[[group_name]]=="average")/nrow(df_result),
      MSE =MSE,
      RMSE = RMSE,
      MAE=MAE,
      R.squared=R.squared,
      R.squared.adjusted=R.squared.adjusted))
  }
  return(res)
}

