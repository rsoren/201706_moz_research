#
# 02_stepped_wedge_analysis.R
#
# Reed Sorensen
# June 2017
#


library(dplyr)
library(lme4)
library(lubridate)

rm(list = ls())

# prep data

dat1 <- read.csv("_data/ficha_de_verificacao_de_registro.csv", as.is = TRUE)

names(dat1) <- gsub("\\.", "", names(dat1))

names(dat1)[grepl("total_cca_iniciaram_TARV_", names(dat1))] <- c(
  "total_cca_iniciaram_TARV_ate_45dias_apos_recep_resultado", 
  "total_cca_iniciaram_TARV_ate_45dias_apos_recep_do_PCR", 
  "total_cca_iniciaram_TARV_ate_45dias_apos_recep_do_TDR", 
  "total_cca_iniciaram_TARV_ate_45dias_sem_diagnostico"
)


var_name_tmp <- "numero_cca_expost_HIV_PCR"
names(dat1)[names(dat1) == var_name_tmp] <- paste0(var_name_tmp, "_positivo")


US_1 <- c("CS Dondo-Sede", "CS 1 de Maio")
US_2 <- c("HD Gondola", "CS Munhava")
US_3 <- c("CS Nhamaonha", "CS Macurungo")


# facility-level data
df <- dat1 %>%
  filter(!is.na(Prov)) %>%
  dplyr::select(-ID, -Prov, -Mes_dados) %>%
  group_by(US, coorte) %>% # aggregate the three months within each US/coorte
  summarize_all(sum, na.rm = TRUE) %>% # assumes all NA mean zero
  mutate(
    coorte_num = as.numeric(substr(coorte, 1, 1)),
    intervencao = 0,
    intervencao = ifelse(US %in% US_1 & coorte_num >= 1, 1, intervencao),
    intervencao = ifelse(US %in% US_2 & coorte_num >= 2, 1, intervencao),
    intervencao = ifelse(US %in% US_3 & coorte_num >= 3, 1, intervencao) ) %>%
  as.data.frame(.) %>%
  mutate(
    US = as.factor(US),
    coorte = as.factor(coorte),
    cca_expost_HIV = numero_mulheres_HIV_1a_CPP + 
      numero_cca_expost_HIV_identificada_CCS_referida_CCR )
  


df[df$US == "CS Munhava" & 
    df$coorte == "00", "numero_cca_expost_HIV_ref_CPP_CCR_inscrt"] <- 136


saveRDS(df, "_intermediate_files/PEER_processed_data.RDS")


run_sw_analysis <- function(numerator, denominator) {
  
  # numerator <- "numero_mulheres_HIV_mais_de_duas_CPP" # dev variable
  # denominator <- "numero_mulheres_HIV_1a_CPP1" # dev variable
  
  require(lme4)
  
  tmp <- df[, c("US", "coorte", "intervencao", numerator, denominator)]
  tmp$sim <- tmp[, numerator]
  tmp$nao <- tmp[, denominator] - tmp$sim
  
  tmp2 <- tmp %>%
    group_by(intervencao) %>%
    summarize(
      numerator = sum(sim),
      denominator = sum(nao + sim)) %>%
    mutate(pct = round(numerator / denominator, digits = 3))
  
  if (any(tmp$nao < 0 )) stop(paste0(
    "Analysis requires all numbers to be positive",
    "\n--", numerator,
    "\n--", denominator))
  
  outcome_var <- cbind(tmp$sim, tmp$nao)
  
  fit <- glmer(
    outcome_var ~ intervencao + coorte + (1|US),
    data = tmp,
    family = binomial(link = "logit") # logistic regression
  )
  
  out <- list(
    vars = c(numerator, denominator), 
    fit = fit, 
    count_pct = tmp2
  )
  
  return(out)
  
}



get_result <- function(model, variable, exponentiate = FALSE) {
  
  # model <- fit1; variable <- "coorte1a"; exponentiate = FALSE # dev variables
  
  txt <- paste0("Beta coefficient of '", variable, "': ")
  
  output <- list(
    tmp_point <- fixef(model)[variable],
    tmp_confint <- confint(
      model, parm = "beta_", method = "Wald")[variable,]
  )
  
  if (exponentiate) {
    txt <- paste0("Odds ratio of '", variable, "': ")
    output <- lapply(output, exp)
  }
  
  output <- lapply(output, function(x) round(x, digits = 2))
  c(output[[1]], output[[2]][1], output[[2]][2])
}


get_output <- function(description, num_var, denom_var) {
  # num_var <- "numero_mulheres_HIV_mais_de_duas_CPP" # dev variable
  # denom_var <- "numero_mulheres_HIV_1a_CPP1" # dev variable
  # description <- "This is a description" # dev variable
  
  tmp_run <- run_sw_analysis(num_var, denom_var)
  
  result <- get_result(
    model = tmp_run[["fit"]],
    variable = "intervencao", 
    exponentiate = TRUE
  )
  
  out <- c(description, result, 
    tmp_run[["count_pct"]][1, "pct"], 
    tmp_run[["count_pct"]][2, "pct"], 
    num_var, denom_var)
  
  names(out) <- c(
    "description", "odds_ratio", "lower_ci", "upper_ci", 
    "pct_control_unadjusted", "pct_intervencao_unadjusted",
    "numerator", "denominator")
  
  return(out)
  
}


# STEPPED WEDGE ANALYSIS

results <- do.call("rbind", list(
  
  get_output(
    "Among HIV positive mothers - how many did more than 2 CPP consults",
    num_var = "numero_mulheres_HIV_mais_de_duas_CPP",
    denom_var = "numero_mulheres_HIV_1a_CPP" ),
  
  get_output(
    "Among women who did first CPP consult - how many were referred to CCR",
    num_var = "numero_cca_expost_HIV_refer_CPP_para_CCR",
    denom_var = "numero_mulheres_HIV_1a_CPP" ),
  
  get_output(
    "Among women who did first CPP consult - how many enrolled in CCR",
    num_var = "numero_cca_expost_HIV_ref_CPP_CCR_inscrt",
    denom_var = "numero_mulheres_HIV_1a_CPP" ),

  
  # check the variable counts
  # tmp <- df[, c("US", "coorte",
  #   "numero_cca_expost_HIV_ref_CPP_CCR_inscrt",
  #   "numero_mulheres_HIV_1a_CPP")]
  # -- numerator too big because they can come from other sources
  # -- solution: create new variable that includes CCS and people who come from home
  #              and use it as the denominator
  
  # -- negative numbers problem; data quality
  # get_output(
  #   "Among children enrolled in CCR - how many have a PCR test",
  #   num_var = "numero_cca_expost_HIV_1o_PCR",
  #   denom_var = "numero_cca_1a_CCR" ),
  # 
  
  # -- negative numbers problem; data quality
  # get_output(
  #   "Among all the PCR tests taken - how many went to the reference laboratory?", 
  #   num_var = "numero_amostras_PCR_enviados_Lab",
  #   denom_var = "numero_cca_expost_HIV_1o_PCR" ),
  
  get_output(
    paste0(
      "Among children that began CCR - ",
      "how many began TARV within 45 days of getting the PCR result"),
    num_var = "total_cca_iniciaram_TARV_ate_45dias_apos_recep_do_PCR",
    denom_var = "numero_cca_1a_CCR" ),
  
  get_output(
    paste0(
      "Among PCR tests that went to the laboratory - ",
      "how many test results came back to the health facility"),
    num_var = "numero_PCR_recebidosdolab_ref",
    denom_var = "numero_amostras_PCR_enviados_Lab" ),
  
  get_output(
    paste0(
      "Among tests that came back to the health facility - ",
      "how many came within 28 days"), 
    num_var = "numero_PCR_recebido_ate_28dias",
    denom_var = "numero_PCR_recebidosdolab_ref" ),
  
  get_output(
    paste0(
      "Among samples returned to the health facility - ", 
      "how many were received by the patient" ), 
    num_var = "numero_PCR_entregue",
    denom_var = "numero_PCR_recebidosdolab_ref" ),
  
  get_output(
    paste0(
      "Among children enrolled in CCR - ", 
      "how many got the PCR test within 8 weeks" ), 
    num_var = "numero_cca_expost_HIV_fez_1o_PCR_Idade_48_semanas_de_vida",
    denom_var = "numero_cca_expost_HIV_1o_PCR" ),
  
  # -- negative numbers problem
  get_output( # check on this; within 16 weeks, or between 8 and 16 weeks
    paste0(
      "Among the children that got a PCR test - ",
      "how many receive the test result within 4 months" ),
    num_var = "numero_cca_expost_HIV_recebeu_PCR_com_idade_816_semanas_de_vida",
    # denom_var = "numero_PCR_entregue"
    denom_var = "numero_PCR_recebidosdolab_ref" )
  # 
  # tmp <- df[, c("US", "coorte", "numero_cca_expost_HIV_recebeu_PCR_com_idade_816_semanas_de_vida",
  #   "numero_PCR_entregue")]
  
  # # still need to resolve these
  # Among children in CCR, 
  #   how many have been referred to TARV based on PCR result?
  # Numerator: total_cca_iniciaram_TARV_ate_45dias_apos_recep_do_PCR
  # Denominator: new variable 'PCR' created in 'df7' 
  # -- number of PCR tests in the specified time frame
  #
  # Numerator: new variable
  # Denominator: numero_cca_expost_HIV_PCR_positivo
  # 
  # In the time period specified (September 21, 2015 to November 20, 2016) 
  #   among children exposed to HIV with a positive PCR test, how many 
  #   children that have positive PCR result started TARV
  #   -- Starting TARV defined as receiving PCR within the time period
  #
  
))

write.csv(results, "PEER/PEER_stepped_wedge_oddsratios.csv", row.names = FALSE)

# # OTHER ANALYSES


# Among total women in CPP (Total_mulher_1a_cpp), 
# how many have HIV (numero_mulheres_HIV_1a_CPP1)
# -- Just report the percent with positive HIV result

df %>%
  group_by(.) %>%
  summarize(
    num = sum(numero_mulheres_HIV_1a_CPP),
    denom = sum(Total_mulher_1a_cpp)) %>%
  mutate(pct = num / denom)



# Among PCR tests the health facility received, what percent were positive?
# Numerator: numero_cca_expost_HIV_PCR_positivo
# Denominator: numero_amostras_PCR_enviados_Lab
# -- Just report the prevalence (%)

df %>%
  group_by(.) %>%
  summarize(
    num = sum(numero_cca_expost_HIV_PCR_positivo),
    denom = sum(numero_amostras_PCR_enviados_Lab)) %>%
  mutate(pct = num / denom)




