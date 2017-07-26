#
# 02_PEER_analysis_individual_data.R
#
# Reed Sorensen
# June 2017
#

rm(list = ls())


# install.packages("dplyr")

library(dplyr)
library(lme4)
library(lubridate)

dat_in <- read.csv("_data/seguimento_c_exposta.csv")

# make sure all names are alphanumeric
names(dat_in) <- gsub("\\.", "", names(dat_in))
names(dat_in)[grepl("Data_derefer", names(dat_in))] <- "Data_dereferenciadeCPPaCCR"
names(dat_in)[grepl("Teste_diagn", names(dat_in))] <- "Teste_diagnostico"
names(dat_in)[grepl("Observa", names(dat_in))] <- "Observacoes"



US_1 <- c("CS Dondo-Sede", "CS 1 de Maio")
US_2 <- c("HD Gondola", "CS Munhava")
US_3 <- c("CS Nhamaonha", "CS Macurungo")

coorte_levels <- data.frame(
  coorte_tmp = as.integer(0:3),
  coorte = as.factor(c("00", "1a", "2a", "3a"))
)

df <- dat_in %>%
  mutate(
    US = as.factor(US),
    idade = Idade_paciente,
    inicio_TARV = as.character(Data_inicio_TARV),
    lev1 = ymd(as.character(Data_1o_Lev)),
    lev2 = ymd(as.character(Data_2o_Lev)),
    lev3 = ymd(as.character(Data_3o_Lev)),
    lev4 = ymd(as.character(Data_4o_Lev)) ) %>%
  rename(coorte_tmp = coorte) %>%
  left_join(coorte_levels, by = "coorte_tmp") %>%
  mutate(
    lev1_dichot = ifelse(is.na(lev1), 0, 1),
    lev2_dichot = ifelse(is.na(lev2), 0, 1),
    lev3_dichot = ifelse(is.na(lev3), 0, 1),
    lev4_dichot = ifelse(is.na(lev4), 0, 1),
    lev_count = lev1_dichot + lev2_dichot + lev3_dichot + lev4_dichot ) %>%
  mutate(
    coorte_num = as.numeric(substr(coorte, 1, 1)),
    intervencao = 0,
    intervencao = ifelse(US %in% US_1 & coorte_num >= 1, 1, intervencao),
    intervencao = ifelse(US %in% US_2 & coorte_num >= 2, 1, intervencao),
    intervencao = ifelse(US %in% US_3 & coorte_num >= 3, 1, intervencao) ) %>%
  arrange(US, coorte)  %>%
  filter(ymd(as.character(Data_entrega_1o_PCR)) > ymd("2015-10-01"))



# create new variable for aggregate data analysis

# Baseline dates: September 21, 2015 to December 20, 2015
# Intervention dates: December 21, 2015 to November 20, 2016
# -- Get total number of children with PCR 
#    during the baseline + intervention period
# -- Exclude kids with TDR test


df_subset <- df %>%
  mutate(pcr1 = ymd(as.character(Data_entrega_1o_PCR)) ) %>%
  filter(
    # exclude people outside the study period
    # from beginning of baseline to end of intervention
    pcr1 >= ymd("2015-09-21") & pcr1 <= ymd("2016-11-20"), 
    # exclude people who had a TDR test
    is.na(Data_TDR)
  )

# next week, we'll look at individual-level data
# to see if we can exclude the children that got two tests (PCR and TDR)
# Some children started TARV because they came into the facility already sick.

new_dat <- df_subset %>%
  group_by(US, coorte) %>%
  summarize(PCR = sum(PCR)) %>%
  as.data.frame(.)

write.csv(new_dat, 
  file = "_data/newdata_aggregated_from_individual.csv",
  row.names = FALSE
)


# -- Outcome: Time to receive PCR result after collection
# Data_entrega_1o_PCR - Data_1a_colheita_PCR  
# Data_entrega_resultado_2o_PCR - Data_2a_colheita_PCR

# -- Outcome: Time to start TARV after receiving PCR result
# Data_inicio_TARV - Data_entrega_1o_PCR

# -- Outcome: Time of first CPP visit after birth
# Data_1a_CPP - Data_Nascimento

# -- Outcome: Time to start CCR after finishing CPP consult
# Data_Inscriacao_CCR - Data_de.referência.de.CPP.a.CCR




df2 <- df %>%
  mutate(
    dias_colheita_entrega_pcr1 = as.numeric(
      ymd(as.character(Data_entrega_1o_PCR)) -  
      ymd(as.character(Data_1a_colheita_PCR)) ),
    
    dias_colheita_entrega_pcr2 = as.numeric(
      ymd(as.character(Data_entrega_resultado_2o_PCR)) - 
      ymd(as.character(Data_2a_colheita_PCR)) ),
    
    dias_entrega_TARV = as.numeric(
      ymd(as.character(Data_inicio_TARV)) - 
      ymd(as.character(Data_entrega_1o_PCR)) ),
    
    dias_nascimento_cpp = as.numeric(
      ymd(as.character(Data_1a_CPP)) - 
      ymd(as.character(Data_Nascimento)) ),
    
    dias_cpp_ccr = as.numeric(
      ymd(as.character(Data_Inscriacao_CCR)) - 
      ymd(as.character(Data_1a_CPP)) ),
    
    # among people start Tarv, how many started on the same day they received the PCR result
    day_get_PCR = as.numeric(ymd(as.character(Data_entrega_1o_PCR))),
    day_start_TARV = as.numeric(ymd(as.character(Data_inicio_TARV))),
    same_day = ifelse(day_get_PCR == day_start_TARV, 1, 0),
    age_diff_CCR_TARV = as.numeric(
      ymd(as.character(Data_inicio_TARV)) - 
      ymd(as.character(Data_Inscriacao_CCR)) ) / 30,
    idade_start_TARV = idade + age_diff_CCR_TARV,
    
    # check consistency of dates
    birth_before_CCR = ymd(as.character(Data_Nascimento)) <= ymd(as.character(Data_Inscriacao_CCR)),
    CCR_before_PCRtake = ymd(as.character(Data_Inscriacao_CCR)) <= ymd(as.character(Data_1a_colheita_PCR)),
    PCRtake_before_PCRresult = ymd(as.character(Data_1a_colheita_PCR)) <= ymd(as.character(Data_entrega_1o_PCR))
    
)
    
with(df2, table(birth_before_CCR))
with(df2, table(CCR_before_PCRtake))
with(df2, table(PCRtake_before_PCRresult))



# check some variables
df3 <- df2 %>%
  dplyr::select(
    US, coorte, 
    Data_dereferenciadeCPPaCCR, Data_Inscriacao_CCR, Data_entrega_1o_PCR, 
    Data_1o_Lev, Data_2o_Lev, Data_3o_Lev, Data_4o_Lev )
    
# Data_entrega_1o_PCR - Data_1a_colheita_PCR
#
# library(MASS)
#
# tmp_fit2 <- MASS::glm.nb(
#   dias_colheita_entrega_pcr1 ~ intervencao + coorte + Prov,
#   # dias_colheita_entrega_pcr1 ~ intervencao + coorte, 
#   data = filter(df2, dias_colheita_entrega_pcr1 > 3 & dias_colheita_entrega_pcr1 < 180)
# )
# 
# summary(tmp_fit2)


get_ci <- function(model, variable, exponentiate = FALSE) {
  
  # model <- fit1; variable <- "coorte1a"; exponentiate = FALSE # dev variables
  
  txt <- paste0("Beta coefficient of '", variable, "': ")
  
  output <- list(
    tmp_point <- fixef(model)[variable],
    tmp_confint <- confint(
      model, parm = "beta_", method = "Wald")[variable,]
  )
  
  if (exponentiate) {
    txt <- paste0("Exponentiated '", variable, "': ")
    output <- lapply(output, exp)
  }
  
  output <- lapply(output, function(x) signif(x, digits = 3))
  cat("\n", paste0(
    txt, output[[1]], " (95% CI: ", paste0(output[[2]], collapse = ", "), ")"
  ))
  
}




# colheita --> entrega
# For this one, check out 
#   zero-inflated negative binomial random effects regression
#   to account for structural zeros (Munhava as on-site PCR machine)
#   Link: https://stats.stackexchange.com/questions/38195/zero-inflated-negative-binomial-mixed-effects-model-in-r
# 


time_fit1 <- glmer(
  dias_colheita_entrega_pcr1 ~ intervencao + coorte + idade + (1|US),
  data = filter(df2, dias_colheita_entrega_pcr1 > 3),  # do this with zero-inflated model
  family = poisson(link = "log")
)
summary(time_fit1)


time_fit2 <- glmer.nb(
  # formula = dias_colheita_entrega_pcr1 ~ intervencao + coorte + (1|US),
  formula = dias_colheita_entrega_pcr1 ~ intervencao + coorte + idade + (1|US),
  data = filter(df2, dias_colheita_entrega_pcr1 > 3 & dias_colheita_entrega_pcr1 < 180)
)
summary(time_fit2)
get_ci(time_fit2, variable = "intervencao", exponentiate = TRUE)


# nascimento --> cpp
time_fit3 <- glmer(
  dias_nascimento_cpp ~ intervencao + coorte + idade + (1|US),
  # data = df2,
  data = filter(df2, dias_nascimento_cpp >= 0 & dias_nascimento_cpp < 90),
  family = poisson(link = "log")
)
summary(time_fit3)


time_fit4 <- glmer.nb(
  formula = dias_nascimento_cpp ~ intervencao + coorte + idade + (1|US),
  data = filter(df2, dias_nascimento_cpp >= 0 & dias_nascimento_cpp < 90)
)
summary(time_fit4)
get_ci(time_fit4, variable = "intervencao", exponentiate = TRUE)


# cpp --> ccr

time_fit5 <- glmer(
  dias_cpp_ccr ~ intervencao + coorte + idade + (1|US),
  data = filter(df2, dias_cpp_ccr >= 0 & dias_cpp_ccr < 60),
  family = poisson(link = "log")
)
summary(time_fit5)


time_fit6 <- glmer.nb(
  formula = dias_cpp_ccr ~ intervencao + coorte + idade + (1|US),
  data = filter(df2, dias_cpp_ccr >= 0 & dias_cpp_ccr < 60)
)
summary(time_fit6)
get_ci(time_fit6, variable = "intervencao", exponentiate = TRUE)



# outcome: getting PCR result and start TARV on the same day
# -- exclude TDR
# Result: Compared to the control group, children in CCR receiving the intervention were
#         more likely to start TARV on the same day that they received their 
#         PCR result (OR = 4.65 [95% CI: 1.48, 14.6]). 

tmp <- df2[, 
  c("PCR", "TDR", "idade", "idade_start_TARV", "Data_Nascimento", "Data_Inscriacao_CCR", "Data_TDR", 
    "Data_1a_colheita_PCR", "Data_entrega_1o_PCR", "day_get_PCR", 
    "Data_inicio_TARV",  "day_start_TARV", "same_day")]


df2_no_tdr <- filter(df2, TDR != 1)
df2_all_pcr <- filter(df2, 
  PCR == 1,
  dias_colheita_entrega_pcr1 != 0,
  birth_before_CCR,
  CCR_before_PCRtake )
  

tmp2 <- tmp %>%
  filter(TDR == 1)

# with age as covariate
time_fit7 <- glmer(
  same_day ~ intervencao + coorte + idade + (1|US),
  data = df2_all_pcr,
  # data = df2_no_tdr,
  family = binomial(link = "logit")
)
summary(time_fit7)
get_ci(time_fit7, variable = "intervencao", exponentiate = TRUE)


# without age as covariate
time_fit8 <- glmer(
  same_day ~ intervencao + coorte + (1|US),
  data = df2_no_tdr,
  family = binomial(link = "logit")
)
summary(time_fit8)
get_ci(time_fit8, variable = "intervencao", exponentiate = TRUE)



# stepped wedge analysis

# logistic regression, whether or not did fourth pick-up
fit1 <- glmer(
  formula = lev2_dichot~intervencao + coorte + idade + (1|US),
  # formula = lev4_dichot~intervencao + coorte + (1|US),
  # formula = lev3_dichot~intervencao + idade + coorte + (1|US),
  # formula = lev3_dichot~intervencao + coorte + (1|US),
  # data = df,
  data = df2,
  family = binomial(link = "logit")
)
summary(fit1)
get_ci(fit1, variable = "intervencao", exponentiate = TRUE)


# Poisson regression
# NOTE: negative binomial version doesn't converge

fit2 <- glmer(
  formula = lev_count~intervencao + idade + coorte + (1|US),
  data = df,
  family = poisson(link = "log"))
summary(fit2)
get_ci(fit2, variable = "intervencao", exponentiate = TRUE)








