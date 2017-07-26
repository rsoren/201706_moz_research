#
# incomas_01_data_prep.R
#
# Reed Sorensen
# July 2017
#

library(dplyr)
library(data.table)

df_in <- fread("_data/INCOMAS_DATA_2017-07-24_0836.csv") %>%
  as.data.frame(.)


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
household_vars <- c(
  province = "province",      # 1 = Sofala, 2 = Manica
  urban = "urbanorrural"      # 1 = urban, 2 = rural
)
# TO DO: merge these onto the individual-level data


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
  cat4_men_15plus = "category___4"
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
    mom_18plus = ifelse(age < 18, 0, mom_18plus) )


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




