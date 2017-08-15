#
# incomas_02_data_prep_dropbox.R
#
# Reed Sorensen
# July 2017
#


library(dplyr)
library(foreign)
library(readstata13)


df <- readstata13::read.dta13("_data/INCOMAS Child and mother long.dta")
df2 <- readstata13::read.dta13("_data/INCOMAS Household and injury only 2017-06-08.dta")

df2_mergevars <- df2[, c("odkuri", "province", "urbanorrural")]

df_new <- df %>%
  select(-province, -urbanorrural) %>% # get rid of variables with same name, but missing info
  left_join(df2_mergevars, by = "odkuri")

# check whether merge worked
table(df_new$province, exclude = FALSE)
table(df_new$urbanorrural, exclude = FALSE)


# individual-level data
# -- mother is 15-49 years old
# -- legal guardian
#
# household data
# -- ID to link with individual-level data
#
# documentation about what's different?
#



vars <- c(
  diarrhea_2wk = "select306", # diarrhea in last 2 weeks
  fever_2wk = "select320",    # fever in last 2 weeks
  born_2011 = "birth2011",    # child born January 2011 or later
  check_2011 = "check2011",   # asked a second time if child was born 2011 or later
  num_births_2011 = "birthafter2011_integer", # number births Jan 2011 or later
  age_months = "month_integer",
  year_at_birth = "year_integer",
  age = "age1_integer",
  cat1_mother1549 = "category___1",
  cat2_legal = "category___2",
  cat3_mother_50plus = "category___3",
  cat4_men_15plus = "category___4",
  where_first_treatment = "select329",
  fever_treatment_took = "select330",
  # fever_treatment_type = "select331",  # this contains information about other diseases
                                         # we should re-code to include only diseases we want
  
  # fever variables
  fever1 ="select328___1",   # *** # publico: unidade sanitaria
  fever2 = "select328___2",  # *** # publico: brigrada movel    
  fever3 = "select328___3",  # *** # publico: outro publico
  fever4 = "select328___4",  # *** # privado: clinica
  fever5 = "select328___5",  # --- # privado: farmacia  # this one is ambiguous; maybe didn't see a doctor
  fever6 = "select328___6",  #       privado: medico
  fever7 = "select328___7",  # *** # privado: outro
  fever8 = "select328___8",  #       outro: mercado
  fever9 = "select328___9",  #       outro: medicina tradicional
  fever10 = "select328___10", # --- # outro: pessoal de saude do bairro # this one is ambiguous; remove
  fever11 = "select328___11", # --- # outro: outro
  
  # *** means 'went to a health facility'
  # --- means remove because it's ambiguous whether going here is good
  
  # 'select309' is where got treatment for diarrhea
  d1 = "select309___1",   # public: unidade sanitaria
  d2 = "select309___2",   # public: brigada movel
  d3 = "select309___3",   # public: outro publico
  d4 = "select309___4",   # private: clinica
  d5 = "select309___5",   # private: farmacia
  d6 = "select309___6",   # private: medico
  d7 ="select309___7",   # private: outro
  d8 = "select309___8",   # other: mercado
  d9 = "select309___9",   # other: traditional healer
  d10 = "select309___10",  # other: 'pessoal de saude do bairro'
  d11 = "select309___11"  # other: other
  
)


