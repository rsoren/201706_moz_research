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

df <- readRDS("_intermediate_files/DB_EID_v3.RDS")


# Describe the sociodemographic profile of children attending EID services and tested for HIV in Maputo city;

# demographic variables: 
# -- sex: Sexo
freq(df$Sexo, plot=F)


# -- age: Calculate as date of collection (colheitaUS1) minus date of birth (Data de Nacimento)
df$idade <- df$colheitaUS1_2 - df$DatadeNascimento_2

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


# Determine the proportion of children tested for HIV using PCR DNA at 4 to 6 weeks at the EID services in Maputo city;
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
    had_second_test = ifelse(is.na(second_test_result), 0, 1) )

# how many missing first lab result
with(df3, table(is.na(ResultadoLAB1)) )

# among those with positive first test, how many took second test (133)
with(subset(df3, first_test_positive == 1),
  freq(had_second_test, plot=F)
)


# Determine the proportion of children with discordant results who had a third confirmatory test;
# There are no subject with discordant results for tests 1 and 2


df4 <- df3 %>%
  mutate(
    discordant_posneg = ifelse(first_test_result == 1 & second_test_result == 0, 1, 0),
    discordant_negpos = ifelse(first_test_result == 0 & second_test_result == 1, 1, 0),
    is_discordant = ifelse(discordant_posneg | discordant_negpos, 1, 0),
    third_test_result = ifelse(is.na(ResultadoLAB3), ResultadoUS3, ResultadoLAB3),
    had_third_test = ifelse(is.na(third_test_result), 0, 1)
  )

table(df4$first_test_result, df4$second_test_result, useNA = "always")
with(df4, freq(is_discordant, plot=F))
with(subset(df4, is_discordant == 1), freq(had_third_test, plot=F))


# Determine the proportion of children with the appropriate management
#   through the age of 18 months (as per the algorithm) who had a positive result;
#
# -- Make the time window smaller, try 9 months and 3 months
# -- Decide which threshold to use based on the remaining sample size
#




# Identify the factors relating to the non-compliance with the algorithm for EID for PCR DNA HIV first positive test.
# -- Logistic regression

# What are the factors we want to use to predict non-compliance?
# -- Still waiting on data about maternal age and site of delivery
# -- Child age at enrollment in CCR
# -- Time it takes for the laboratory to get a result back to the health facility

# -- Whether or not mother comes to take the result
# -- Missed appointment


# Exclude children for whom we don't have 3 months of data, starting from date of first collection
# 

# How does child age at enrollment influence:
# -- Maternal age
# -- Site of delivery
# -- Turn around time
# -- Whether the mother comes to take the result

# Does taking the second test means that 
# people are more likely to be on TARV

# Lab staff said there were no problems
#   with PCR machine breaking during the study period

# Among people with discordant results for the 
# first two tests, what factors influence 
# whether they got a third test? 

# factors --> non-compliance

# can't use the word in the definition (dictionary)

# taking more time for the result to return to the health facility
# --> non-compliance

# statistically, logistic regression with a categorical predictor is the same as a chi-square test

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




# RESULTADO LAB 2 -- result of second test
# 0 = negative
# 1 = positive
# 2 = indeterminate

library(gmodels)
CrossTable(
  x = factor(dat$Resultado_Lab_1_v2), 
  y = factor(dat$Resultado_Lab_2_v2)
)


# Among children with a positive first PCR test, 
#   proportion that had a second test

# Denominator: RESULTADO LAB 1

# if laboratory result is missing, use the result from the 
# health facility (if it exists)
# 

#  # . ( ) [space] / ,

names(dat) <- gsub(
  pattern = "[^a-zA-Z0-9]",
  replacement = "",
  x = names(dat)
)

write.csv(
  x = dat,
  file = "_intermediate_files/DB_EID_25012017_v2.csv",
  row.names = FALSE
)









# denominator = all children with HIV 
              = 
# numerator = number of children who had two tests




CrossTable(dat$Resultado_LAB_1)
CrossTable(dat$Resultado_LAB_2)
CrossTable(dat$Resultado_LAB_1, dat$Resultado_LAB_2)

table(dat$`Resultado LAB 1`)
table(dat$Resultado_LAB_2)
