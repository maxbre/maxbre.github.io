# Load the packages
library(fitdistrplus)
library(tidyverse)


# Generate random data from a Weibull distribution
set.seed(123)
weibull_data <- rweibull(n = 1000, shape = 1.5, scale = 2)


dat <- data.frame(weibull_data)

# Plot histogram with density
dat %>% 
ggplot(aes(x = weibull_data)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "skyblue") +
  stat_function(fun = dweibull, args = list(shape = 1.5, scale = 2), color = "red", size = 1) +
  labs(title = "Weibull Distribution", x = "Values", y = "Density") +
  theme_minimal()


# Fit a Weibull distribution to the generated data
fit <- fitdist(weibull_data, "weibull")

# Display the estimated parameters
print(fit)

fitdist(weibull_data, "gamma")
