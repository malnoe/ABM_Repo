# TidyLPA test
library(tidyLPA)
library(dplyr)

# Model with 3 profiles
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(3)

# Plotting profiles
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  scale() %>%
  estimate_profiles(3) %>% 
  plot_profiles()

# Comparing solutions
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(1:3) %>%
  compare_solutions(statistics = c("AIC", "BIC"))

# The model specification
# It's based en multinomial gaussian modelling so there are 2 arguments to specify
# Variance : equal or varying
# Covariance : zero, equal or varying.
# You can mix-n-match every combination

# The number of classes
# One of the key points of LPA is to choose the number of groups -> 1, 2, 3, 4, ...
# You test each possibility and see what's the best result regarding the AIC,BIC or other indicators

pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(1:5, 
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))



