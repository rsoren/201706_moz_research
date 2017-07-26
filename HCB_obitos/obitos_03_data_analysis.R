#
# obitos_analysis.R
#
# Reed Sorensen
# June 2017
#


library(dplyr)
library(survival)
library(lubridate)

rm(list = ls())

df3 <- read.csv(
  file = "_intermediate_files/obitos_data_symptoms_and_diseases.csv", as.is = TRUE )  %>%
  mutate(y_tumor_and_kaposi = ifelse(y_tumors | y_sarcoma_de_kaposi, TRUE, FALSE) )

standard_symptom_dichot_vars <- readRDS("_intermediate_files/standard_symptom_list.RDS")
symptom_dichot_vars <- names(df3)[grepl("x_", names(df3))]
disease_dichot_vars <- names(df3)[substr(names(df3),1,2) == "y_"]

disease_dichot_vars <- disease_dichot_vars[
  !disease_dichot_vars %in% c("y_tumors", "y_sarcoma_de_kaposi")]

analysis_vars <- c(
  "time_days",
  "died",
  "AntecedentedeTB",
  "Sexo",
  # "estadoTARV",
  "onTARV",
  # "DatadeinicioTARVantesdointernamento",
  # "DatadeinicioTARVnointernamento",
  "oms_score",
  "ward",
  "WBC_2",
  "Hb_2",
  standard_symptom_dichot_vars,
  symptom_dichot_vars,
  disease_dichot_vars
)

# check that all variables exist
tmp <- analysis_vars[!analysis_vars %in% names(df3)]
if (length(tmp) > 0) {
  stop(paste("Variables not in 'df3':", paste(tmp, collapse = ', '))) 
}


# survival analysis with random forest algorithm

library("randomForestSRC")
library("ggRandomForests")

# df4 <- df3[, analysis_vars]
# df4 <- df3[, c("time_days", "died", symptom_dichot_vars)]
df4 <- df3[, c("time_days", "died", disease_dichot_vars, symptom_dichot_vars)]

var_family = "surv"
var_importance = "none"

# takes a while to run...
fit <- rfsrc(
  formula = Surv(time_days, died) ~ .,
  ntree = 10000,
  forest = TRUE,
  importance = TRUE,
  data = df4
)

options(scipen = 999)

df_imp1 <- data.frame(
  variable = names(fit$importance),
  importance = as.vector(fit$importance) ) %>%
  arrange(desc(importance))


write.csv(df_imp1, "HCB_obitos/obitos_randomforest_variable_importance.csv", row.names = FALSE)

rf <- read.csv("HCB_obitos/obitos_randomforest_variable_importance.csv")
tmp_vimp <- vimp(fit)


library(polycor)
library(corrplot)

jpeg("HCB_obitos/obitos_correlations_symptoms.jpg")
tmp1 <- df3[, standard_symptom_dichot_vars]
tmp_corr1 <- polycor::hetcor(tmp1)
corrplot(tmp_corr1$correlations)
dev.off()

jpeg("HCB_obitos/obitos_correlations_disease.jpg",
  height = 1200, width = 1200)
tmp3 <- df4[, disease_dichot_vars]
tmp_corr3 <- polycor::hetcor(tmp3)
corrplot(tmp_corr3$correlations)
dev.off()

# Cox proportional hazards
# fit <- coxph(Surv(time_days, died) ~ idade + sexo + oms_score, data = df)
# fit <- coxph(Surv(time_days, event_var) ~ oms_score, data = df3)
# fit <- coxph(Surv(time_days, event_var) ~ Hb, data = df3)
fit <- coxph(Surv(time_days, died) ~ Sexo, data = df)
summary(fit)

basehaz(fit)

# Kaplan-Meier estimator
#-- no strata
fit2 <- survfit(Surv(time_days, died) ~ 1, data = df)
plot(fit2)

fit3 <- survfit(Surv(time_days, died) ~ onTARV, data = df)
plot(fit3)
  
median(df$time_days)
summary(df$time_days)



################################
# NOTES

# Patient profile of HIV-positive patients hospitalized in wards of 
# the central hospital of Beira
# -- Clinical mortality outcome from January to June 2015
# -- Sample only includes patients who died --> how to interpret survival analysis??
#    We can't claim to know the future at time 0
#
# Data collection during november/december 2016
#

# Results of interest:
#
# 1. Frequency of principal complaint ('Queixaprincipal'), symptoms and comorbidities
#    Other complaints are in other variables 
#    -- Tosse, Vomito, Dispneia, Diareia, Sudorese nocturna, Anorexia, Confusao, Febre, Cefaleia,
#       Perda de peso, Regidez na nuca (neck stiffness), astenia
# 
# 2. Percent on TARV at admisison ('EstadoTARV')
#
# 3. History of TB ('AntecedentedeTB'), separated by whether they
#    had it in the last 2 years ('DataAno') compared to date of admission
#
# 4. OMS stage at admission ('Estadio clinico da OMS')
#    -- Definition includes diagnosis of TB
#    -- Check on this; only includes 2 and 3 (not 4, what we expect most often)
# 
# 5. Hemoglobin levels ('Hb')
#    >11       not anemic
#    9 to 11   mild
#    6 to 9    moderate 
#    <6        severe
#
# 6. CD4 count, and how much it contributes to mortality
#    Distribution among >500, 200-500 (moderate) and <200 (severe)
#    -- This variable is miscoded ('CD4'); it's dichotomous, not continuous
#
# 7. Frequency of all opportunistic infections
#    -- Lucia will do
#
# 8. In the wards, determine if the medications given are same as those prescribed
#    -- We don't have data for this right now
#
# 9. Location of HIV test ('Local de teste de HIV')
#    Testing in central hospital 'HCB' means they don't know their status
#
# 10. First or second line drugs ('Linha de TARV usada')
#     -- 0 is first-line; 1 is second-line
#
# 11. Vital signs
#     -- 'Frequencia cardiaca', heart rate per minute (72 is median; normal range is 60-99)
#     -- 'Frequencia respiratoria', 12 is median (20 is high part of the normal range)
#     -- 'Tensao arterial'; blood pressure (normal is <140 systolic and <90 diastolic)
# 
# 12. Frequency of co-morbidities ('Doencaas associated')
#
# 13. For anybody who had a respiratory problem, did they have an x-ray?
#     X-ray, Rx Torax, those who have TB, if they have an X-ray
#



# Survival analysis notes:
# 
# For survival, prediction error is measured by 1-C, where C is 
# Harrell's (Harrell et al., 1982) concordance index. 
# Prediction error is between 0 and 1, and measures how well the 
# predictor correctly ranks (classifies) two random individuals in 
# terms of survival. A value of 0.5 is no better than random guessing. 
# A value of 0 is perfect.
#
#
# For survival forests (Ishwaran et al. 2008), the censoring variable 
# must be coded as a non-negative integer with 0 reserved for censoring 
# and (usually) 1=death (event). The event time must be non-negative.





