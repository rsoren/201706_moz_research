#
# 01_tmps_data_prep.R
#
# Reed Sorensen
# July 2017
# 

# Compare levels of disability by sex, age, and province
# -- Disability defined as simple scoring (WHODAS)
# 1 = NENHUMA
# 2 = PEQUENA
# 3 = ALGUMA
# 4 = MUITA
# 5 = GRAVE (OU NAO PODE FAZER)
#

library(haven)
library(dplyr)
library(reshape2)

# read in data
df <- read_dta("_data/TMPS2016BASEFINAL.dta")


# keep only year 2015 and the disability variables
df2 <- df %>%
  filter(yrsurvey == 2015) %>%
  select(provincia, yrsurvey, sexo, idade_gt7, anos,
    dif_fica_pe, dif_casa, 
    dif_tarefa_nova, dif_comunidade, dif_concentrar, dif_andar, 
    dif_banho, dif_vestir, dif_socializar, dif_amizade, 
    dif_trabalho, dif_emocional) %>%
  as.data.frame(.)

# keep only rows with data for all disability questions
# -- few are missing responses for some (rather than all or none) disability questions
df2[] <- lapply(df2, function(x) ifelse(x == "", NA, x)) # first convert "" to NA
df2 <- filter(df2, complete.cases(df2))

saveRDS(df2, "_intermediate_files/tmps_data_processed.RDS")


