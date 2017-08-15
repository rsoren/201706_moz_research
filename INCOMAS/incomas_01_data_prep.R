#
# incomas_01_data_prep.R
#
# Reed Sorensen
# July 2017
#

library(dplyr)
library(data.table)
library(gmodels)


df_in <- fread("_data/INCOMAS_DATA_2017-07-24_0836.csv") %>%
  as.data.frame(.)

check_var <- function(x, dat = df_in) {
  names(dat)[grepl(x, names(dat), ignore.case = TRUE)]
}

tmp <- df_in[, check_var("inj")] %>%
  filter(injure131 > 0)

varnames <- names(df_in)[grepl("select309", names(df_in))]
tmp <- df_in[, varnames]

incomas3 <- df_in

incomas4 <- incomas3 %>%
  mutate(
    newvar = (select309___1 + select309___2 + select309___3) > 0,
    newvar2 = 10,
    newvar3 = newvar2 + 5
  )

table(incomas4$newvar)

table(incomas4$select306)

incomas5 <- incomas4 %>%
  mutate(
    newvar = NA,
    newvar = ifelse(select306 == 1, 1, newvar),
    newvar = ifelse(select306 == 2, 0, newvar)
  )


table(incomas5$newvar)


fit1 <- glm(newvar ~ age1_integer, family = "binomial", data = incomas5)
summary(fit1)

my_coefs <- coef(fit1)
my_confints <- confint(fit1)
my_results <- cbind(my_coefs, my_confints)
my_odds_ratios <- exp(my_results)


# first part, household survey
# -- section "inquerito agregado"
# -- 'nmember' and 'name' are still part of inquerito agregado,
#     although the section heading says 'individual'
# -- ask the head of household about things like bed nets, water, etc.
# -- the head of household 
#
# second part, inquerito individual (starts at variable 116: indentification_integer)
# -- almost completely independent from the first part
# -- interviewed individuals only
# -- smaller sample (?)
# -- women ages 15 to 49
# -- use the variable 'result2' to know which lines correspond to the 
#    second survey (inquerito individual)
# 

# What type of analysis to do?

# Simple frequencies by age and location
# -- Create a variable that tells whether it's a woman 15-49 and/or guardian
#      Use dichotomous (is or isn't woman 15-49 AND legal guardian)
# -- Age groups (<18, 18+)
# -- Rural and urban
# -- Province
# -- Everything stratified by province (split other variables by province)
#
# logistic regression analysis
# 
# Outcomes of interest
# -- diarrhea in the last two weeks
#    * mother's age
#    * child's age
#    * urban or rural
#    * province
# -- fever in the last two weeks
#    * mother's age
#    * child's age
#    * urban or rural
#    * province
# 

# Where did you get treatment?
# select329 is where did you go FIRST for treatment ** we want this 
# select328 is where are ALL of the places you went for treatment
# For fevers, use select330
# -- During the period in which the child was sick, did you take treatment?
# -- variable select331 is which type treatment

# -- among mothers with sick children, whether or not goes to 
#    tradtional healer or market for treatment
#
# For diarrhea, use select308: Did you seek a consultation
# If so, where (select309)



# Think about including PCR breaking as a covariate
# Look in Brad on the conversation with James




household_vars <- c(
  province = "province",      # 1 = Sofala, 2 = Manica
  urban = "urbanorrural"      # 1 = urban, 2 = rural
)
# TO DO: merge these onto the individual-level data


tmp <- df_in %>%
  filter(!is.na(province))

gmodels::CrossTable(tmp$province, tmp$urbanorrural)


tmp2 <- tmp %>%
  group_by(urbanorrural) %>%
  summarize(count = n()) %>%
  mutate(pct = count / sum(count))


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




df1 <- df_in %>%
  select_(.dots = c(vars)) %>%
  
  # keep only mothers who are a legal guardian of a child <5 years old
  filter(cat1_mother1549 == 1 & cat2_legal == 1) %>%
  
  # keep only people who said twice that birth was 2011 or later
  filter(born_2011 == 1 & check_2011 == 1) %>% 
  mutate(
    mom_18plus = NA,
    mom_18plus = ifelse(age >= 18, 1, mom_18plus),
    mom_18plus = ifelse(age < 18, 0, mom_18plus),
    age = ifelse(age > 80 | age < 10, NA, age),
    diarrhea_2wk_bin = NA,
    diarrhea_2wk_bin = ifelse(diarrhea_2wk == 1, 1, diarrhea_2wk_bin),
    diarrhea_2wk_bin = ifelse(diarrhea_2wk == 2, 0, diarrhea_2wk_bin),
    fever_2wk_bin = NA,
    fever_2wk_bin = ifelse(fever_2wk == 1, 1, fever_2wk_bin),
    fever_2wk_bin = ifelse(fever_2wk == 2, 0, fever_2wk_bin)
  )




fit1 <- glm(diarrhea_2wk_bin ~ age, data = df1, family = "binomial")
summary(fit1)
exp(coef(fit1))
confint(fit1)

names(df1)

fit2<- glm(fever_2wk_bin ~ age, data = df1, family = "binomial")
summary(fit2)
exp(coef(fit2))
confint(fit2)
exp(coef(fit2))


df1$catvar <- sample(c("a", "b", "c", "d"), size = nrow(df1), replace = TRUE)
df1$catvar2 <- factor(df1$catvar, levels = c("a", "b", "c", "d"))

fit3 <- glm(diarrhea_2wk_bin ~ age + catvar2, data = df1, family = "binomial")
summary(fit3)




table(df1$mom_18plus)
table(df1$diarrhea_2wk)
table(df1$fever_2wk)
table(df1$cat1_mother1549)
table(df1$cat1_mother1549, df1$cat2_legal)
table(df1$born_2011)



# Link maternal age with category from variable: “category”
# -	Use categories 1 and 2, which represent 
# o	1 = mother between 15 and 49 years
# o	2 = Legal guardian from children less than 5 years

# Other variables
# -	Demographic characteristics (within “Inquerito ao Agregado”)
# -	Years of education (levelschool)
# -	Number of living children (select221)




