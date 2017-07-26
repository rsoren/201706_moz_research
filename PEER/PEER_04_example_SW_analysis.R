
library(samplingDataCRT)
library(lme4)
library(dplyr)

K <- 4 #number of time points
J <- 30 #number of subjects, each cluster and timepoint
#variances of each level
sigma.1 <- 0.1
sigma.2 <- 0.4
sigma.3 <- 0.9
#regression paramters
mu.0 <- 0
theta <- 1.5
betas <- rep(0, K-1)
parameters <- c(mu.0, betas, theta)

##### for parallel study #####
I <- 6 #number of cluster
sw <- 2 #number of cluster switches
# create a design matrix
X <- designMatrix(nC=I, nT=K, nSw=sw)
# create the corresponding complete data design matrix
D <- completeDataDesignMatrix(J, X)
# performe covariance-Variance matrix for longitudinal design
V <- CovMat.Design(K, J, I, sigma.1.q=sigma.1, sigma.2.q=sigma.2, sigma.3.q=sigma.3)
# sample data within the design
sample.data <- sampleData(
  type = "long", 
  K=K, J=J, I=I, D=D, V=V, 
  parameters=parameters 
)


# convert variable names to match CIOB analysis
df <- sample.data %>%
  select(
    subject, val, 
    coorte = measurement, 
    US = cluster, 
    intervencao = intervention
  )

# look at difference in distribution of 'val' between levels of 'intervencao'
boxplot(val~intervencao, data = df)

#####
# run stepped-wedge analysis (continuous outcome)
fit <- lmer(val ~ intervencao + coorte + (intervencao|US), data=df)
summary(fit)

# look at random effects
ranef(fit)



#####
# convert the data set to dichotomous outcome,
#   to demonstrate how to analyze a dichotomous outcome
mean_val <- mean(sample.data$val)

df2 <- df %>%
  mutate(val2 = ifelse(val > mean_val, 1, 0))

# run stepped-wedge analysis (dichotomous outcome)
fit2 <- glmer(
  val2 ~ intervencao + coorte + (intervencao|US), 
  data = df2,
  family = "binomial"
)
summary(fit2)



#####
# convert the data, aggregate by cluster
# 

df3 <- df2 %>%
  group_by(US, coorte, intervencao) %>%
  summarize(
    sim = sum(val2),
    total = n() ) %>%
  mutate(nao = total - sim)


fit3 <- glmer(
  cbind(df3$sim, df3$nao) ~ intervencao + coorte + (intervencao|US),
  data = df3,
  family = binomial(logit)
)

summary(fit3)






