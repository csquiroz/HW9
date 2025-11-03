### HW 9 - Zoo 800 ###
### Cody Quiroz ###
### 02-Nov-25 ###

## Part 1a ##
library(tidyverse)
set.seed(123) #reproducability

#constants
alpha <- 10 #intercept
beta <- 5 #slope
n <- 100 #number of obs

#generate x
x <- runif(n, 0, 10)

#function to sim y given sig (error)
simulate_y <- function(sigma) {
  epsilon <- rnorm(n, mean = 0, sd = sigma)
  y <- alpha + beta * x + epsilon
  tibble(x = x, y = y, sigma = sigma)
}

#simulate for 3 lvls of error
dat <- bind_rows(
  simulate_y(1),
  simulate_y(10),
  simulate_y(25)
)


## Part 1b ##
ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~sigma, nrow = 1) +
  labs(
    x = "x",
    y = "y"
  ) +
  theme_bw()

#save plot
ggsave("figures/regression_facets.png", width = 10, height = 4)


## Part 2a & 2b combined ##

#define sim param
p_values <- c(0.55, 0.6, 0.65) #will do all at once
n_flips <- 1:20
n_sim <- 100

#run simulation
results <- expand.grid(p = p_values, n = n_flips) %>%
  mutate(power = NA_real_)

for (i in 1:nrow(results)) {
  p <- results$p[i]
  n <- results$n[i]
  
  #repeat 100 times
  significant <- replicate(n_sim, {
    flips <- rbinom(1, n, p)
    test <- binom.test(flips, n, p = 0.5)
    test$p.value < 0.05
  })
  
  results$power[i] <- mean(significant)
}

#plot
ggplot(results, aes(x = n, y = power, color = as.factor(p))) +
  geom_line() +
  geom_point() +
  labs(
    x = "Number of Flips",
    y = "Probability of Detecting Bias (α < 0.05)",
    color = "True Coin Probability"
  ) +
  theme_bw()

#save plot
ggsave("figures/coin_flip_power.png", width = 7, height = 5)
