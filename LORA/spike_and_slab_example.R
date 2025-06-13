# install.packages("devtools")
library(devtools)
library(rjags)
devtools::install_github("josue-rodriguez/SSranef")
library(SSranef)

d <- lme4::sleepstudy
d$y <- c(scale(d$Reaction))

#Alpha model
alpha <- ss_ranef_alpha(y = d$y, unit = d$Subject)
posterior_summary(alpha, ci = 0.90, digits = 2)
ranef_summary(alpha, ci = 0.95, digits = 2)
caterpillar_plot(alpha)
pip_plot(alpha)

#Beta model
beta <- ss_ranef_beta(y = d$y, X = d$Days, unit = d$Subject)
posterior_summary(beta, digits = 2)
ranef_summary(beta, digits = 2)
caterpillar_plot(beta)
pip_plot(beta)

#Multivariate
mv_data <- gen_mv_data(5, 5)
str(mv_data)
mv_model <- ss_ranef_mv(Y = cbind(mv_data$y1, mv_data$y2),
                        X = mv_data$x,
                        unit = mv_data$id,
                        burnin = 100,
                        iter = 500,
                        chains = 4)
posterior_summary(mv_model)

#Priors
# change prior for mean intercept
priors <- list(alpha = "alpha ~ dt(0, 1, 3)",
               # for each jth unit, change prior probability of inclusion
               gamma = "gamma[j] ~ dbern(0.75)") 

fit <- ss_ranef_alpha(y = d$y, unit = d$Subject, priors = priors)
ranef_summary(fit)

# On top
jags_model_text <- fit$model_text
cat(jags_model_text)
