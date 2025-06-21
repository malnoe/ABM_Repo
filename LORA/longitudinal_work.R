# Longitudinal study

## Packages ####
library(ggplot2)
library(dplyr)
library(missForest)
library(purrr)
library(rjags)
library(SSranef)



## Import the data and the functions ####
df <- readRDS("C:/Users/garan/Documents/Ecole/M1/Stage/Internship_repo/LORA/ds_forJan.rds")
source("~/Ecole/M1/Stage/Internship_repo/LORA/utils.R")

load("~/Ecole/M1/Stage/Internship_repo/LORA/longitudinal_work_data.RData")

## Select, recode and sum variables and clean dataframe ####
  # Select relevant variables and lines and create the week variable
dh_variables <- paste0("dh_",c(1:28,44:58))
ghq_variables <- paste0("ghq_", 1:28)
variables <- c("id","age","gender",ghq_variables,paste0("pss_",1:10),dh_variables)
df <- df[1:30966,variables]
df[["week"]] <- rep(0:25,1191)
df <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))

  # Create a variable indicating if the participant was there or not
df$present <- !apply(is.na(df[, ghq_variables]), 1, all)


  # Replace negative values by NAs
for(variable in variables){
  df[[variable]] <- ifelse(df[[variable]]>=0, df[[variable]], NA)
}

  # Recode PSS
variables_to_recode <- paste0("pss_", c(4, 5, 7, 8))
for (variable in variables_to_recode) {
  inverse_var <- paste0(variable, "_inverse")
  df[[inverse_var]] <- ifelse(df[[variable]] %in% 0:4, 4 - df[[variable]], NA)
}

  # Build total scores
pss_variables <- c(paste0("pss_",c(1:3,6,9,10)),paste0("pss_",c(4, 5, 7, 8),"_inverse"))


df[["pss_sum"]] <- rowSums(df[,pss_variables],na.rm = TRUE)
df[["ghq_sum"]] <- rowSums(df[,ghq_variables],,na.rm = TRUE)
df[["dh_sum"]] <- rowSums(df[,dh_variables],,na.rm = TRUE)

  # Select relevant lines and final variables
final_variables <- c("id","week","present","age","gender","pss_sum","ghq_sum","dh_sum")
df <- df[,final_variables]


## Diagnostic of how many people participated how many times ####
df_participation <- df %>%
  filter(present) %>%
  count(week, name = "number_participants")

presence_counts <- df %>%
  filter(present) %>%
  group_by(id) %>%
  summarise(weeks_present = list(week), .groups = "drop")

df_full_attendance <- tibble()

for (week_number in 1:25) {
  weeks_required <- 1:week_number
  
  n_fully_present <- sum(sapply(presence_counts$weeks_present, function(weeks) {
    all(weeks_required %in% weeks)
  }))
  
  df_full_attendance <- bind_rows(df_full_attendance, tibble(
    week = week_number,
    fully_present = n_fully_present
  ))
}
df_participation <- df_participation %>%
  left_join(df_full_attendance, by = "week")

## Selection of participants ####

# If we want over 500 people without interruption -> week 12 -> 515 people
participants_full_12 <- df %>%
  filter(week <= 12) %>%
  group_by(id) %>%
  summarise(n_weeks_present = sum(present), .groups = "drop") %>%
  filter(n_weeks_present == 12)

df_present_full_12 <- df %>%
  filter(id %in% participants_full_12$id,week>0,week<=12)

d <- df_present_full_12


## Residualization + grouping ####
d[["residuals_ghq_pss"]] <- NA
d[["residuals_ghq_dh"]] <- NA


for(adversity in c("pss_sum","dh_sum")){
  for(week_number in 1:25){
    # Get the people present that week
    index_present <- d$week==week_number&d$present
    # Check if there are enough people (ie > 2) to do the regression.
    if(sum(index_present)>2){
      data_regression <- d[index_present,c(adversity,"ghq_sum")]
      
      # Unadjusted linear model
      lm_unadjusted <- lm(as.formula(paste("ghq_sum", "~", adversity)), data = data_regression)
      
      # Identification of influencial points using Cook's D.
      used_data <- model.frame(lm_unadjusted)
      influencial_points <- which(cooks.distance(lm_unadjusted) > 4 / nrow(used_data))
      used_rows <- as.numeric(rownames(used_data))
      original_indices <- used_rows[influencial_points]
      
      # Cleaned data for the LM
      df_clean <- data_regression[-original_indices, ]
      
      # Verify that there are at least two points for the adjusted regression
      if(nrow(df_clean)>2){
        # Adjusted linear model
        lm_adjusted <- lm(as.formula(paste("ghq_sum", "~", adversity)), data = df_clean)

        # Residuals of the adjusted linear model
        predicted_all <- predict(lm_adjusted, newdata = data_regression)
        residuals_all <- data_regression[["ghq_sum"]] - predicted_all
        
        # Put the residual in the result df
        if(adversity=="pss_sum"){
          d[index_present,"residuals_ghq_pss"] <- residuals_all
        }
        else{
          d[index_present,"residuals_ghq_dh"] <- residuals_all
        }
        # Get the grouping
        if(adversity=="pss_sum"){
          bins <- c(0,14,26,40)
        }
        else{
          bins <- c(0,76,151,226,302)
        }
        df_result <- get_all_groups_small(data_regression,residuals_all,adversity,"ghq_sum",bins,lm_adjusted)
        list_groups <- list("quantiles (15%)",
                            "quantiles (20%)",
                            "quantiles (25%)",
                            "pred. residuals (75%)",
                            "pred. residuals (60%)",
                            "pred. residuals (50%)",
                            "1SD",
                            "0.5SD",
                            "Kmeans")
        for(group_name in list_groups){
          d[index_present,paste0("tr_residuals_",group_name,"_",adversity)] <- transformed_residuals(df_result,group_name,method="nothing")
        }
      }
    }
  }
}


## SaS : Intercept only -> alpha ####

# GHQ~PSS
alpha_pss <- ss_ranef_alpha(y=d$residuals_ghq_pss, unit=d$id)
posterior_summary(alpha_pss, ci = 0.90, digits = 2)
ranef_summary(alpha_pss, ci = 0.95, digits = 2)
caterpillar_plot(alpha_pss, col_id = FALSE)
pip_plot(alpha_pss, col_id = FALSE)


# GHQ~DH
alpha_dh <- ss_ranef_alpha(y=d$residuals_ghq_dh, unit=d$id)
posterior_summary(alpha_dh, ci = 0.90, digits = 2)
ranef_summary(alpha_dh, ci = 0.95, digits = 2)
caterpillar_plot(alpha_dh,col_id = FALSE)
pip_plot(alpha_dh,col_id = FALSE)


  # Function to get the number of people that are categorized as non-average depending on the threshold for the PIP
pct_PIP <- function(alpha_res,percentages){
  res <- data.frame(PIP=c(),pct_non_average=c(),pct_resilient=c(),pct_vulnerable=c())
  for(percentage in percentages){
    ranef_sum <- ranef_summary(alpha_res, ci = 0.95, digits = 2)
    pct_resilient <- sum(ranef_sum$PIP>percentage&ranef_sum$Post.mean<0,na.rm=TRUE)/515*100
    pct_vulnerable <- sum(ranef_sum$PIP>percentage&ranef_sum$Post.mean>0,na.rm=TRUE)/515*100
    res <- rbind(res,data.frame(PIP=c(percentage),pct_non_average=c(pct_resilient+pct_vulnerable),pct_resilient=c(pct_resilient),pct_vulnerable=c(pct_vulnerable)))
  }
  return(res)
}

pct_PIP_alpha_pss<- pct_PIP(alpha_pss,seq(from=0.5,to=1,by=0.05))
pct_PIP_alpha_dh<- pct_PIP (alpha_dh,seq(from=0.5,to=1,by=0.05))
View(pct_PIP_alpha_pss)
View(pct_PIP_alpha_dh)

## SaS : Intercept and slope -> beta ####
## Week but the spike and slab is only on the slope
# GHQ~PSS
beta_pss <- ss_ranef_beta(y=d$residuals_ghq_pss, X=d$week, unit=d$id)
posterior_summary(beta_pss, ci = 0.90, digits = 2)
ranef_summary(beta_pss, ci = 0.95, digits = 2)
caterpillar_plot(beta_pss,col_id = FALSE)
pip_plot(beta_pss,col_id = FALSE)

# GHQ~DH
beta_dh <- ss_ranef_beta(y=d$residuals_ghq_dh, X=d$week, unit=d$id)
posterior_summary(beta_dh, ci = 0.90, digits = 2)
ranef_summary(beta_dh, ci = 0.95, digits = 2)
caterpillar_plot(beta_dh,col_id = FALSE)
pip_plot(beta_dh,col_id = FALSE)

pct_PIP_beta_pss<- pct_PIP(beta_pss,seq(from=0.5,to=1,by=0.05))
pct_PIP_beta_dh<- pct_PIP(beta_dh,seq(from=0.5,to=1,by=0.05))

View(pct_PIP_beta_pss)
View(pct_PIP_beta_dh)


## SaS : Intercept on transformed residuals ####
list_groups <- list("quantiles (15%)",
                    "quantiles (20%)",
                    "quantiles (25%)",
                    "pred. residuals (75%)",
                    "pred. residuals (60%)",
                    "pred. residuals (50%)",
                    "1SD",
                    "0.5SD",
                    "Kmeans")
# GHQ~PSS
list_pct_PIP_PSS_tr_res <-list()
for(group_name in list_groups){
  alpha <- ss_ranef_alpha(y=d[[paste0("tr_residuals_",group_name,"_pss_sum")]], unit=d$id)
  list_pct_PIP_PSS_tr_res[[paste0("tr_residuals_",group_name,"_pss_sum")]] <- pct_PIP(alpha,seq(from=0.5,to=1,by=0.05))
}

# GHQ~DH
list_pct_PIP_PSS_tr_res <-list()
for(group_name in list_groups){
  alpha <- ss_ranef_alpha(y=d[[paste0("tr_residuals_",group_name,"_dh_sum")]], unit=d$id)
  list_pct_PIP_PSS_tr_res[[paste0("tr_residuals_",group_name,"_dh_sum")]] <- pct_PIP(alpha,seq(from=0.5,to=1,by=0.05))
}