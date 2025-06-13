# Longitudinal study

## Packages ####
library(ggplot2)
library(dplyr)
library(missForest)
library(purrr)


## Import the data ####
df <- readRDS("C:/Users/garan/Documents/Ecole/M1/Stage/Internship_repo/LORA/ds_forJan.rds")

## Select, recode and sum variables and clean dataframe ####
  # Select relevant variables and lines and creat the week variable
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

## Residualization ####
df[["residuals_ghq_pss"]] <- NA
df[["residuals_ghq_dh"]] <- NA

for(adversity in c("pss_sum","dh_sum")){
  for(week_number in 1:25){
    # Get the people present that week
    index_present <- df$week==week_number&df$present
    # Check if there are enough people (ie > 2) to do the regression.
    if(sum(index_present)>2){
      data_regression <- df[index_present,c(adversity,"ghq_sum")]
      
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
        
        if(adversity=="pss_sum"){
          df[index_present,"residuals_ghq_pss"] <- residuals_all
        }
        else{
          df[index_present,"residuals_ghq_dh"] <- residuals_all
        } 
      }
    }
  }
}

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


# If we want over 500 people
# With interuption -> week 14
# Without interuption -> week 12





