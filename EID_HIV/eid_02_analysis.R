#
# eid_02_analysis.R
#
# Reed Sorensen
# August 2017
#

library(dplyr)
library(gmodels)
library(descr)

rm(list = ls())

dir <- "C:/Users/rsoren/Documents/prog/projects/201706_moz_research/"
df <- readRDS(paste0(dir, "_intermediate_files/DB_EID_v3.RDS"))


# distribution of dates of birth
hist(df$DatadeNascimento_2, breaks = 30)


# Describe the sociodemographic profile of children attending EID services and tested for HIV in Maputo city;

# demographic variables: 
# -- sex: Sexo
freq(df$Sexo, plot=F)


# -- age: Calculate as date of collection (colheitaUS1) minus date of birth (Data de Nacimento)
df$idade <- as.numeric(df$colheitaUS1_2 - df$DatadeNascimento_2)
hist(df$idade, breaks = 30)

# -- Above and below 6 weeks

# distribution of age at collection, by age group
# 0-6 weeks; 7-12 weeks; 13-18 weeks; 19-24 weeks; 25-30 weeks; 31-36 weeks; 37 + 
# Note: I changed the cutoffs slightly, so the first number increases by 6 each time

df$idade_semana <- df$idade / 7
df$idade_semana_grupo <- cut(df$idade_semana, breaks = c(0,6,12,18,24,30,36,999), right = FALSE)
freq(df$idade_semana_grupo, plot=F)


# 49 entries didn't have a date of birth
with(df, crosstab(is.na(colheitaUS1_2), is.na(DatadeNascimento_2), plot=F))


# Check why some colletion dates before the date of birth
# NOTE: fixed this; don't need to switch day and month from Esmeralda's file after all
#
# tmp1 <- subset(df, idade < 0,
#   select = c("DatadeNascimento", "colheitaUS1", "DatadeNascimento_2", "colheitaUS1_2"))
#


# -- provenance (health facility): Proveniencia
freq(df$Proveniencia, plot=F)


# Determine the proportion of mothers with known HIV status at delivery moment;
# No data for this at the moment


# Determine the proportion of exposed children who took the first test;
# No data for this at the moment


# Determine the proportion of children tested for HIV using PCR DNA at 4 to 6 weeks at the
# EID services in Maputo city;
# Among all children, what proportion had a PCR test within 6 weeks
# -- Define no test as whether 'colheitaUS1' is missing, or if collection >6 weeks after birth
df2 <- df %>%
  mutate(
    test_after_6wk = ifelse(idade > 42, 1, 0),
    test_none = ifelse(is.na(colheitaUS1_2), 1, 0),
    test_within_6wk = ifelse(test_after_6wk | test_none, 1, 0) )

# with(df2, freq(test_after_6wk, plot=F))
# with(df2, freq(test_none, plot=F))
with(df2, freq(test_within_6wk, plot=F))


# Determine the proportion of children with positive HIV who had a confirmatory test;
# Among children who took the first PCR test and were positive 
#   (If ResultadoLab1 is 1, or if missing use ResultadoUS1),
# how many had a second test (how many not missing, ResultadoLAB2 | ResultadoUS2)


df3 <- df2 %>%
  mutate(
    first_test_result = ifelse(is.na(ResultadoLAB1), ResultadoUS1, ResultadoLAB1),
    first_test_positive = ifelse(first_test_result == 1, 1, 0),
    second_test_result = ifelse(is.na(ResultadoLAB2), ResultadoUS2, ResultadoLAB2),
    second_test_positive = ifelse(second_test_result == 1, 1, 0),
    had_second_test = ifelse(is.na(second_test_result), 0, 1) )

# how many missing first lab result
with(df3, table(is.na(ResultadoLAB1)) )

# among those with positive first test, how many took second test (133)
with(subset(df3, first_test_positive == 1),
  freq(had_second_test, plot=F)
)


# Determine the proportion of children with discordant results who had a third confirmatory test;

df4 <- df3 %>%
  mutate(
    discordant_posneg = ifelse(first_test_result == 1 & second_test_result == 0, 1, 0),
    discordant_negpos = ifelse(first_test_result == 0 & second_test_result == 1, 1, 0),
    is_discordant = ifelse(discordant_posneg | discordant_negpos, 1, 0),
    third_test_result = ifelse(is.na(ResultadoLAB3), ResultadoUS3, ResultadoLAB3),
    had_third_test = ifelse(is.na(third_test_result), 0, 1)
  )

table(df4$first_test_result, df4$second_test_result, exclude = NULL)
with(df4, freq(is_discordant, plot=F))
with(subset(df4, is_discordant == 1), freq(had_third_test, plot=F))


# Determine the proportion of children with the appropriate management
#   through the age of 18 months (as per the algorithm) who had a positive result;
# 
# Definition of appropriate management:
# 1. Positive PCR DNA test at 4-6 weeks
# 2. ART initiation [variable for this?] # DatadeiniciodoTARV
# 3a. Pos. confirmatory test --> Lifelong ART
# 3b. Pos. confirmatory test --> Neg. --> Pos. --> Lifelong ART
# 3c. Pos. confirmatory test --> Neg. --> Neg. --> Referral to clinician
#


# --Variables of interest:
# test_within_6wk
# first_test_positive
# had_second_test
# second_test_positive
# is_discordant
# had_third_test
# third_test_positive

# see "testing_cascade_logic.docs" for a visual representation
#   of the Boolean logic

df5 <- df4 %>%
  # remove children with negative first test, then no 2nd or 3rd test
  # -- must be an error, because there's no way to diagnose positive case
  filter(
    !(first_test_positive == 0 & had_second_test == 0 & had_third_test == 0) ) %>%
  mutate(
    c0 = as.integer(NA),
    # not compliant if didn't take test within 6 weeks
    c1 = ifelse(test_within_6wk == 0, 0, c0),
    
    # compliant if first test was negative, then took second or third tests
    c2 = ifelse(is.na(c1) & first_test_positive == 0 & 
        (had_second_test | had_third_test), 1, c1),
    
    # not compliant if first test was negative, then didn't take second or third test
    c3 = ifelse(is.na(c2) & first_test_positive == 0 & 
        !(had_second_test | had_third_test), 1, c2),
    
    # among people who took test within 6 weeks and had positive first test,
    #   not compliant if didn't start TARV
    c4 = ifelse(is.na(c3) & is.na(DatadeiniciodoTARV_2), 0, c3),
    
    # among people who took test within 6 weeks, had positive first test, and
    #   started TARV, not compliant if didn't have second test
    c5 = ifelse(is.na(c4) & had_second_test == 0, 0, c4),
    
    # among people on ART who took second test,
    # compliant if the second test is positive (end of algorithm)
    c6 = ifelse(is.na(c5) & second_test_positive == 1, 1, c5),
    
    # among people on ART who took second test and it was negative,
    # compliant if they had a third test
    c7 = ifelse(is.na(c6) & had_third_test == 1, 1, c6),

    # among people on ART who took second test and it was negative,
    # non-compliant if they didn't take a third test
    c8 = ifelse(is.na(c7) & had_third_test == 0, 1, c7)  ) %>%
  mutate(
    started_tarv = ifelse(is.na(DatadeiniciodoTARV_2), 0, 1),
    ProvUS_factor = factor(ProvUS)
  )
  

# write.csv(df5, paste0(dir, "EID_HIV/eid_hiv_processed_data.csv"), row.names = FALSE)


#-- check coding for appropriate management variable
# lapply(paste0("c", 1:7), function(x) print(table(df5[, x])))


# get results for selected variables by proveniencia 

results_by_prov <- df5 %>%
  group_by(Proveniencia) %>%
  dplyr::summarize(
    appropriate_management = sum(c8, na.rm=T),
    started_tarv = sum(started_tarv, na.rm=T),
    median_age_days = median(idade, na.rm=T),
    first_test_positive = sum(first_test_result == 1, na.rm=T),
    first_test_negative = sum(first_test_result == 0, na.rm=T),
    had_second_test = sum(had_second_test, na.rm=T),
    second_test_positive = sum(second_test_result == 1, na.rm=T),
    second_test_negative = sum(second_test_result == 0, na.rm=T),
    had_third_test = sum(had_third_test, na.rm=T),
    third_test_positive = sum(third_test_result == 1, na.rm=T),
    third_test_negative = sum(third_test_result == 0, na.rm=T),
    facility_receive_1 = sum(!is.na(DataderecepcaonaCCR1), na.rm=T),
    facility_receive_2 = sum(!is.na(DataderecepcaonaCCR2), na.rm=T),
    facility_receive_3 = sum(!is.na(DataderecepcaonaCCR3), na.rm=T),
    # not sure if these 'mother_receive' variables measure the same thing,
    #   but there's no alternative
    # Note that only 1 and 3 have "oucuidador" in the variable name
    mother_receive_1 = sum(!is.na(Datadeentregaamaeoucuidador1_2), na.rm=T),
    mother_receive_2 = sum(!is.na(Datadeentregaamae2_2), na.rm=T),
    mother_receive_3 = sum(!is.na(Datadeentregaamaeoucuidador3_2), na.rm=T),
    total = n() )


pct_names <- names(results_by_prov)[!names(results_by_prov) %in% c("Proveniencia", "total")]

for (nm in pct_names) {
  results_by_prov[, paste0("pct_", nm)] <- 
    results_by_prov[, nm] / results_by_prov$total
}

write.csv(results_by_prov, paste0(dir, "EID_HIV/results_by_prov.csv"))

#####
# Bivariate analysis, comparing how many children had appropriate management, versus:
# - how many children had test within 6 weeks
crosstab(df5$c8, df5$test_within_6wk, plot=F)
# - how many started TARV
crosstab(df5$c8, df5$started_tarv, plot=F)
# - how many mothers comes to take the first result # what's the variable for this?
# - how many have turn-around time before 30 days # what's the variable for this?






# overall frequency of following protocol ('appropriate management')
freq(df5$c8, plot=F)


# split by provenance (health facility)
df6 <- df5 %>%
  group_by(Proveniencia) %>%
  dplyr::summarize(
    count = sum(c8),
    total = n()) %>%
  mutate(proportion = count / total)

print(df6)


# Identify the factors relating to the non-compliance with the 
#   algorithm for EID for PCR DNA HIV first positive test.
# -- Logistic regression

# What are the factors we want to use to predict non-compliance?
# -- Waiting on data about maternal age and site of delivery
# -- Child age at enrollment in CCR (same thing as age at first collection)
# -- Time it takes for the laboratory to get a result back to the health facility


# Among people who took test within 6 weeks and 
#   had positive first test [designated as is.na(c3)],
# is age at child's first test associated with with whether they started TARV?

dat_fit1 <- df5 %>%
  filter(is.na(c3))

fit1 <- glm(
  formula = started_tarv ~ idade,
  data = dat_fit1,
  family = binomial(link = "logit")
)

summary(fit1)

# check for evidence of clustering
# rho=0 means no clustering; rho=1 means complete clustering

library("Hmisc")
deff(as.logical(dat_fit1$started_tarv), cluster = dat_fit1$Proveniencia)


# random effects model that accounts for clustering
library(lme4)

dat_fit1_re <- dat_fit1 %>%
  # have to drop facilities with low numbers, otherwise this model doesn't converge
  group_by(Proveniencia) %>%
  filter(n() >= 10) %>% # keep only facilities with n >= 10; lowest number that still converges
  as.data.frame(.) %>%
  mutate(Proveniencia = droplevels(Proveniencia))

table(dat_fit1_re$Proveniencia)

fit1_re <- glmer(
  formula = started_tarv ~ idade + (1 | Proveniencia),
  data = dat_fit1_re,
  family = binomial(link = "logit")
)

summary(fit1_re)

ranef1 <- ranef(fit1_re)[[1]][,1]


# Among people who took test within 6 weeks and 
#   had positive first test and started TARV [designated as is.na(c4)],
# is age at child's first test associated with with whether they had second test?

dat_fit2 <- subset(df5, is.na(c4))

# check for evidence of clustering in having a second test (no clustering)
deff(as.logical(dat_fit2$had_second_test), cluster = dat_fit2$Proveniencia)

table(dat_fit2$Proveniencia)

fit2 <- glm(
  formula = had_second_test ~ idade, 
  data = dat_fit2,
  family = binomial(link = "logit")
)

summary(fit2)


# Data for turnaround time analyses

dat_fit3 <- df5 %>%
  mutate(
    turnaround_time_1 = as.numeric(processamento1_2 - colheitaUS1_2),
    turnaround_time_2 = as.numeric(processamento2_2 - colheitaUS2_2),
    tt_30 = turnaround_time_1 >= 30,
    tt_30_2 = turnaround_time_2 >= 30
  )

# check for clustering in turnaround time (not enough to need accounting for it)
table(dat_fit3$Proveniencia)
deff(dat_fit3$turnaround_time_1, cluster = dat_fit3$Proveniencia)
deff(dat_fit3$turnaround_time_2, cluster = dat_fit3$Proveniencia)

# Among people who had a FIRST sample taken, 
# what is the effect of faster turnaround time ('processamento1_2' minus 'colheitaUS1_2')

# check for clustering in appropriate management (rho=0.0051)
deff(as.logical(dat_fit3$c8), cluster = dat_fit3$Proveniencia)

# outcome: appropriate management
# -- predictor: turnaround time at least 30 days for first test
fit3 <- glm(
  formula = c8 ~ tt_30,
  data = dat_fit3,
  family = binomial(link = "logit")
)

summary(fit3)

# -- predictor: turnaround time (continuous) for first test

fit4 <- glm(
  formula = c8 ~ turnaround_time_1,
  data = dat_fit3,
  family = binomial(link = "logit")
)
summary(fit4)


# outcome: started TARV
# -- predictor: turnaround time at least 30 days

fit5 <- glm(
  formula = started_tarv ~ tt_30,
  data = dat_fit3,
  family = binomial(link = "logit")
)

summary(fit5)

# -- predictor: turnaround time (continuous)

fit6 <- glm(
  formula = started_tarv ~ turnaround_time_1,
  data = dat_fit3,
  family = binomial(link = "logit")
)

summary(fit6)


# outcome: appropriate management
# -- predictor: turnaround time at least 30 days for second test
fit7 <- glm(
  formula = c8 ~ tt_30_2,
  data = dat_fit3,
  family = binomial(link = "logit")
)

summary(fit7)

# -- predictor: turnaround time (continuous) for second test

fit8 <- glm(
  formula = c8 ~ turnaround_time_2,
  data = dat_fit3,
  family = binomial(link = "logit")
)
summary(fit8)

# interesting that dichotomous <>30 days is significant, but 
#   a continuous measure of time is not
# -- checking a 'generalized additive model' (GAM) to visualize 
#    the non-linear effect of turnaround time 

library(mgcv)
fit8_gam <- gam(
  formula = c8 ~ s(turnaround_time_2),
  data = dat_fit3,
  family = binomial(link = "logit")
)

summary(fit8_gam)
plot(fit8_gam)


# Among people with discordant results for the 
# first two tests, what factors influence 
# whether they got a third test? 
# -- Not enough data to answer this (n=23, with 2 people getting third test)


# Motive de nao TARV
# 1 = abandono
# 2 = nao encaminhado para o TARV
# 3 = transferido
# 4 = recusa
# 5 = sem proceso TARV
# 6 = obito
# 7 = endereco / contacto falso
# 8 = nao inicio
# 9 = sem resultado
# 10 = sem ficha de no proceso TARV
# 11 = dados nao conferem (not the same)

# Report as: This number of children don't start TARV
#            If not start TARV, frequency giving each reason

with(subset(df5, started_tarv != 1), freq(MotivodenaoTARV, plot=F))


# FALSE = did not start TARV
# TRUE = started TARV
table(!is.na(df5$DatadeiniciodoTARV_2))

# among people who didn't start TARV, why not?
with(subset(df5, is.na(DatadeiniciodoTARV_2)), 
  table(MotivodenaoTARV)
)


# save as R Markdown document
# library("rmarkdown")
# setwd(paste0(dir, "EID_HIV/"))
# 
# rmarkdown::render(
#   input = "eid_02_analysis.R",
#   output_format = "pdf_document"
# )
# 
# setwd(dir)


