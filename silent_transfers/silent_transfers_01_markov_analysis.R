#
# 04_silent_transfer_markov_analysis.R
#
# Reed Sorensen
# June 2017
#

library(mstate)
library(msm)
library(dplyr)

states <- c(
  "ref_hospital" = 1, 
  "dist_hospital" = 2, 
  "health_center" = 3, 
  "ltfu" = 4
)

covars <- c("sex", "age", "pregnant", "tb")



n_subjects <- 1000
n_obs_periods <- 5
total_n <- n_subjects * n_obs_periods

sims <- data.frame(
  subject = rep(1:100, each = n_obs_periods),
  time = sample(1:10, size = total_n, replace = TRUE),
  sex = rep(rbinom(n = n_subjects, prob = 0.5, size = 1), each = n_obs_periods)
)

sims2 <- sims %>%
  arrange(subject, time)


q_matrix <- matrix(nrow = length(states), byrow = TRUE, c(
  c(1,0.2,0.2,0.2),
  c(0.2,1,0.2,0.2),
  c(0.2,0.2,1,0.2),
  c(0.2,0.2,0.2,1)
))


df <- simmulti.msm(
  data = sims2,
  qmatrix = q_matrix
)

df2 <- df %>%
  left_join(data.frame(state = states, name = names(states)))

fit1 <- msm(state ~ time, 
  subject = subject, 
  data = df2,
  qmatrix = q_matrix,
  covariates = ~ sex
)










