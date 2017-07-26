#
# obitos_02_frequencies.R
#
# Reed Sorensen
# July 2017
#

library(dplyr)


rm(list = ls())



##### 
# SYMPTOM FREQUENCIES

standard_symptom_list <- readRDS("_intermediate_files/standard_symptom_list.RDS")

df3 <- read.csv("_data/obitos_data_combined_symptoms.csv", as.is = TRUE)

# function for getting frequency information from a dichotomous variable
# -- most applicable to the standard symptom list

get_freq <- function(var, data, multiple_levels = FALSE) {
  
  # var <- "N1Tosse"; data <- df3
  # var <- "Hb_categories"; data <- df4 
  
  n_total <- nrow(data)
  n_nonmissing <- sum(!is.na(data[, var]))
  tmp_levels <- names(table(data[, var]))
  
  result <- do.call("rbind", lapply(tmp_levels, function(x) {
    # x <- tmp_levels[1] # dev variable
    tmp_level <- x
    
    tmp <- data.frame(
      variable = var,
      level = tmp_level,
      count = table(data[, var])[[tmp_level]],
      n_nonmissing = n_nonmissing,
      n_total = n_total )
    
    tmp2 <- tmp %>%
      mutate(
        pct_among_nonmissing = round(count / n_nonmissing, digits = 4) * 100,
        pct_among_total = round(count / n_total, digits = 4) * 100 )
    
    return(tmp2)
    
  }))
  
  return(result)
  
}

symptom_results <- lapply(standard_symptom_list, get_freq, data = df3)

symptom_results2 <- do.call("rbind", symptom_results) %>%
  filter(level != "FALSE") %>%
  arrange(desc(count))

write.csv(
  x = symptom_results2, 
  file = "HCB_obitos/obitos_frequencies_symptoms.csv",
  row.names = FALSE
)


#####
# FREQUENCIES OF OTHER VARIABLES

df4 <- df3 %>%
  mutate(
    # principal complaint
    on_TARV = as.logical(NA),
    on_TARV = ifelse(EstadoTARV == 0, TRUE, on_TARV),
    on_TARV = ifelse(EstadoTARV %in% c(1,2), FALSE, on_TARV),
    had_TB = as.logical(NA),
    had_TB = ifelse(AntecedentedeTB == 1, TRUE, had_TB),
    Hb = abs(Hb), 
    Hb_categories = cut(Hb, breaks = c(0, 6, 9, 11, 999), right = FALSE),
    LocaldetestedeHIV = ifelse(LocaldetestedeHIV == "", NA, LocaldetestedeHIV),
    freq_card_cat = cut(FrequenciaCardiaca, breaks = c(0, 60, 100, 999), right = FALSE),
    freq_resp_cat = cut(FrequenciaRespiratoria, breaks = c(0, 12, 20, 999), right = FALSE),
    age_cat = cut(Idadeanos, breaks = c(0, 25, 35, 45, 999), right = FALSE)
  )


# process blood pressure variables (super weird format)

#-- systolic

table(df4$TensaoArterial) # start again
df4$TensaoArterial <- gsub("\\\\", "/", df4$TensaoArterial)

df4 <- df4 %>%
  mutate(
    TensaoArterial = ifelse(TensaoArterial == "12\0370-80", "", TensaoArterial),
    TensaoArterial = ifelse(TensaoArterial == "", NA, TensaoArterial) )

df4$TensaoArterial_systolic <- sapply(df4$TensaoArterial, function(x) {
  # x <- "100/50" # dev variable
  as.numeric(strsplit(x, split = "/")[[1]][1])
})

df4$tensao_systolic_cat <- cut(
  df4$TensaoArterial_systolic, breaks = c(0, 60,  139, 999), right = FALSE )


#-- diastolic

df4$TensaoArterial_diastolic <- sapply(df4$TensaoArterial, function(x) {
  # x <- "100/50" # dev variable
  as.numeric(strsplit(x, split = "/")[[1]][2])
})

df4$tensao_diastolic_cat <- cut(
  df4$TensaoArterial_diastolic, breaks = c(0, 60, 90, 999), right = FALSE )


other_var_list <- c(
  "on_TARV",
  "Hb_categories",
  "oms_score",
  "EstadoTARV",
  "AntecedentedeTB",
  "LocaldetestedeHIV",
  "LinhadeTARVusada",
  "freq_card_cat",
  "freq_resp_cat",
  "tensao_systolic_cat",
  "tensao_diastolic_cat",
  "had_TB",
  "age_cat",
  "Sexo"
)


other_results <- lapply(other_var_list, get_freq, data = df4)
other_results2 <- do.call("rbind", other_results)

write.csv(
  x = symptom_results2, 
  file = "HCB_obitos/obitos_frequencies_other.csv",
  row.names = FALSE
)


# percent with RxTorax among those with at least 1 sinais_pulmonar
df5 <- df4 %>%
  dplyr::select(sinais_pulmonar, RxTorax) %>%
  filter(sinais_pulmonar > 0)

get_freq("RxTorax", data = df5)




#####
# DISEASES
# get frequencies of disease variables

df6 <- read.csv("_intermediate_files/obitos_data_symptoms_and_diseases.csv", 
  as.is = TRUE)


disease_vars <- names(df6)[substr(names(df6),1,2) == "y_"]

disease_results <- lapply(disease_vars, get_freq, data = df6)
disease_results2 <- do.call("rbind", disease_results) %>%
  filter(level != FALSE) %>%
  arrange(desc(count))


write.csv(disease_results2, "HCB_obitos/obitos_frequencies_diseases.csv", row.names = FALSE)
