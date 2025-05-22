library(ggplot2)
library(glue)

theme_set(theme_minimal())

# Parameters
b1 <- 1.5
sigma2 <- 1.2

# Simulation of data
n <- 100
x <- rnorm(n)
y <- rnorm(n, mean = b1 * x, sd = sqrt(sigma2))

canine_behavior_df <- data.frame(excitability = x, sepanx = y)

# Viz
ggplot(canine_behavior_df, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = b1, col = "steelblue", lwd = 1) +
  geom_vline(xintercept = 0.5, lty = "dashed", col = "gray50") +
  geom_hline(yintercept = b1 * 0.5, lty = "dashed", col = "gray50") +
  labs(x = "Excitability", y = "Separation Anxiety")

# Liner model fit
exc_sepanx_fit <- lm(sepanx ~ excitability, data = canine_behavior_df)
summary(exc_sepanx_fit)

# CI for the point 0.5
x0_df <- data.frame(excitability = 0.5)
predict(exc_sepanx_fit, newdata = x0_df, interval = "prediction", level = 0.95)

# Generalized CI
beta_hats <- as.vector(coef(exc_sepanx_fit))
sigma_hat <- sigma(exc_sepanx_fit)
xrange <- seq(min(x), max(x), length.out = n)

pred_lwr_bounds <- sapply(xrange, \(x) qnorm(0.025, mean = beta_hats[1] + beta_hats[2] * x, sd = sigma_hat))
pred_upr_bounds <- sapply(xrange, \(x) qnorm(0.975, mean = beta_hats[1] + beta_hats[2] * x, sd = sigma_hat))
pred_bounds <- data.frame(x = xrange, lwr = pred_lwr_bounds, upr = pred_upr_bounds)

preds <- predict(exc_sepanx_fit, newdata = canine_behavior_df, interval = "prediction")

ggplot(canine_behavior_df, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = b1, col = "steelblue", lwd = 1) +
  geom_ribbon(data = pred_bounds, aes(y = lwr, ymin = lwr, ymax = upr), alpha = 0.2, col = "gray50",  fill = "plum", lty = "dashed") + 
  geom_ribbon(data = preds, aes(ymin = lwr, ymax = upr), alpha = 0.2, col = "gray50", fill = "orange") + 
  labs(x = "Excitability", y = "Separation Anxiety")










