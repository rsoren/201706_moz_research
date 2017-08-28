#
# TO DO:
# By month and aggregating all months
#
# By district (at district level, use monthly report (RMUS) as the gold standard, 
#   and compare monthly report from district, NEP_FISICO and SIS-MA/MB (modulo basico)
# - RMUS and NEP_FISICO
# - RMUS and SIS-MA/MB (change the name to include "MB")
#
# 
# By province
# -- Aggregate facility-level results to the province
# -- Aggregate district-level results to the province
# 
# According to Global Fund
# <=10% margin of error is high quality
# <=20% and >10% margin of error is medium quality
# >20% margin of error is low quality
# http://www.who.int/tb/advisory_bodies/impact_measurement_taskforce/meetings/tf_17march10_pres_1_3_puvimanasinghe.pdf?ua=1
#
# Make additional variables for medium and low quality, 
#   plus a single variable with "low", "medium" and "high" are levels


library(dplyr)

df <- read.csv("DQA/BD_SOFALA_E_MANICA.csv")

# define location groups
province_manica <- c(
  "CS Vila Nova", "Eduardo Mondlane", "CS Messica", "HD Manica", 
  "HD Gondola", "CS Inchope"
)

d_manica <- c("HD Manica", "CS Messica")
d_chimoio <- c("Eduardo Mondlane", "CS Vila Nova")
d_gondola <- c("HD Gondola", "CS Inchope")
d_muanza <- c("CS Muanza Sede", "CS Muanza Baixo")
d_nhamatanda <- c("HR Nhamatanda", "CS Cheadeia")
d_dondo <- c("CS Dondo", "CS Mafambisse")
d_beira <- c("CS Munhava", "CS Mascarenhas")


locations <- data.frame(US = unique(df$Unidade_Sanitaria)) %>%
  mutate(
    province = ifelse(US %in% province_manica, "Manica", "Sofala"),
    district = "",
    district = ifelse(US %in% d_manica, "Manica", district),
    district = ifelse(US %in% d_chimoio, "Chimoio", district),
    district = ifelse(US %in% d_gondola, "Gondola", district),
    district = ifelse(US %in% d_muanza, "Muanza", district),
    district = ifelse(US %in% d_nhamatanda, "Nhamatanda", district),
    district = ifelse(US %in% d_dondo, "Dondo", district),
    district = ifelse(US %in% d_beira, "Beira", district)
  )



poisson_stats <- function(dat, lr_var2, other_var2) {
  # dat <- tmp2; lr_var2 <- "Testados_LR"; other_var2 <- "Testados_RMUS" # dev
  
  do.call("rbind", lapply(1:nrow(dat), function(i) {
    # i <- 1 # dev
    res1 <- poisson.test(c(dat[i, other_var2], dat[i, lr_var2]))
    res2 <- unlist(res1[c("estimate", "conf.int")])
    names(res2) <- gsub("[^a-z0-9]", "", names(res2))
    return(res2)
  }))
}


# Used Boolean logic from this source: 
# https://stackoverflow.com/questions/3269434/whats-the-most-efficient-way-to-test-two-integer-ranges-for-overlap
check_overlap <- Vectorize(function(x1, x2, y1, y2) x1 <= y2 && y1 <= x2)



get_concordance <- function(indicator, instrument1, instrument2, 
  exclude_months_with_missing = TRUE, 
  agg_months = FALSE, agg_loc_var = NULL) {
  
  # indicator = "Testados"; instrument1 = "LR"; instrument2 = "FB" # dev
  # exclude_months_with_missing = TRUE # dev
  # agg_months = FALSE; agg_loc_var = "district"
  
  require(dplyr)
  
  # identify analytic variables
  lr_var <- paste0(indicator, '_', instrument1)
  other_var <- paste0(indicator, '_', instrument2)
  
  tmp <- df[, c("Unidade_Sanitaria", "Mes", other_var, lr_var)]
  
  # if 'exclude_months_with_missing' is TRUE, remove months that do not have data
  #   from both instruments
  if (exclude_months_with_missing) {
    tmp <- subset(tmp, complete.cases(tmp[,c(lr_var, other_var)]))
  }
  
  
  # if 'agg_months' is TRUE, add together the cases from all months
  if (agg_months) {
    tmp2 <- tmp %>%
      select(-Mes) %>%
      group_by(Unidade_Sanitaria) %>%
      summarize_all(sum, na.rm = TRUE) %>%
      as.data.frame(.)
  } else { tmp2 <- tmp }
  
  
  # prepare for aggregating the facilities by month and/or geography
  groupvars <- "Unidade_Sanitaria"
  
  # if 'agg_loc_var' is not NULL, then aggregate the data by the location specified
  if (!is.null(agg_loc_var)) {
    groupvars <- agg_loc_var
    tmp2 <- tmp2 %>%
      left_join(locations[, c("US", agg_loc_var)], by = c("Unidade_Sanitaria" = "US")) %>%
      select(-Unidade_Sanitaria)
  }
  
  if ("Mes" %in% names(tmp2)) groupvars <- append(groupvars, "Mes")
  
  tmp2 <- tmp2 %>%
    group_by_(.dots = groupvars) %>%
    summarize_all(sum, na.rm = TRUE) %>%
    as.data.frame(.)
  
  # identify the analysis
  tmp2$indicator <- indicator
  tmp2$instrument1 <- instrument1
  tmp2$instrument2 <- instrument2
  
  # rename the variables for the counts
  tmp2$num1 <- tmp2[, lr_var]
  tmp2$num2 <- tmp2[, other_var]
  # tmp2 <- tmp2[, !names(tmp2) %in% c(lr_var, other_var)]
  
  # calculate incidence rate ratio
  tmp2[, c("rate_ratio", "lo", "hi")] <- poisson_stats(
    dat = tmp2, lr_var2 = "num1", other_var2 = "num2"
  )
  
  tmp2[, "ci95_overlaps_10pct"] <- check_overlap(
    x1 = 0.9,  x2 = 1.1,  y1 = tmp2[, "lo"], y2 = tmp2[, "hi"]
  )
  
  tmp2[, "ci95_overlaps_20pct"] <- check_overlap(
    x1 = 0.8,  x2 = 1.2,  y1 = tmp2[, "lo"], y2 = tmp2[, "hi"]
  )
  
  tmp2 <- tmp2 %>%
    mutate(
      data_quality = "",
      data_quality = ifelse(ci95_overlaps_10pct, "high", data_quality),
      data_quality = ifelse(ci95_overlaps_20pct & !ci95_overlaps_10pct, "medium", data_quality),
      data_quality = ifelse(!ci95_overlaps_20pct & !ci95_overlaps_10pct, "low", data_quality)
    )
  
  # calculate concordance the old way
  tmp2$concordance_oldversion <- (tmp2[,"num1"] - tmp2[,"num2"]) / tmp2[,"num1"]
  
  out <- tmp2[, c(groupvars, "indicator", "instrument1", "instrument2", 
    "num1", "num2", "rate_ratio", "lo", "hi", "data_quality", "concordance_oldversion")]
  
  return(out)
}


#####
# RESULTS

inputs1 <- as.matrix(expand.grid(
  indicator = c("Testados", "Positivos", "Tratados_ACTs", "Obitos"),
  instrument1 = "LR",
  instrument2 = c("FB", "RMUS", "NEP_FISICO", "SIS_MA"),
  exclude_months_with_missing = "TRUE"
))

# there's no variable for 'Obitos' as measured by 'FB'
inputs1 <- inputs1[!(inputs1[,1] == "Obitos" & inputs1[,3] == "FB"),]


for (agg in c("facility", "district", "province")) {
  for (aggregate_months in c(FALSE, TRUE)) {
    
    month_txt <- "bymonth"
    if (aggregate_months) month_txt <- "allmonths"
    
    results1 <- do.call("rbind", lapply(1:nrow(inputs1), function(i) {
      # i <- 1 # dev
      agg2 <- agg
      if (agg == "facility") agg2 <- NULL
      
      print(paste0(inputs1[i,]))
      tmp_indicator <- inputs1[i,1]
      tmp_instrument1 <- inputs1[i,2]
      tmp_instrument2 <- inputs1[i,3]
      tmp_exclude <- ifelse(inputs1[i,4] == "TRUE", TRUE, FALSE)
      
      get_concordance(
        indicator = tmp_indicator, 
        instrument1 = tmp_instrument1, 
        instrument2 = tmp_instrument2,
        exclude_months_with_missing = tmp_exclude,
        agg_loc_var = agg2, 
        agg_months = aggregate_months
      )
    
    }))
    
    write.csv(results1, paste0(
      "DQA/results_LR_", month_txt, "_by", agg, "_excludemissing.csv"), row.names = FALSE)
    
  }
}



###

inputs2 <- as.matrix(expand.grid(
  indicator = c("Testados", "Positivos", "Tratados_ACTs", "Obitos"),
  instrument1 = "RMUS",
  instrument2 = c("NEP_FISICO", "SIS_MA"),
  exclude_months_with_missing = "TRUE"
))

for (agg in c("facility", "district", "province")) {
  for (aggregate_months in c(FALSE, TRUE)) {
    
    month_txt <- "bymonth"
    if (aggregate_months) month_txt <- "allmonths"
    
    results1 <- do.call("rbind", lapply(1:nrow(inputs2), function(i) {
      # i <- 1 # dev
      agg2 <- agg
      if (agg == "facility") agg2 <- NULL
      
    
      print(paste0(inputs2[i,]))
      tmp_indicator <- inputs2[i,1]
      tmp_instrument1 <- inputs2[i,2]
      tmp_instrument2 <- inputs2[i,3]
      tmp_exclude <- ifelse(inputs2[i,4] == "TRUE", TRUE, FALSE)
      
      get_concordance(
        indicator = tmp_indicator, 
        instrument1 = tmp_instrument1, 
        instrument2 = tmp_instrument2,
        exclude_months_with_missing = tmp_exclude,
        agg_loc_var = agg2, 
        agg_months = aggregate_months
      )
    
    }))
    
    write.csv(results1, paste0(
      "DQA/results_RMUS_", month_txt, "_by", agg, "_excludemissing.csv"), row.names = FALSE)
    
  }
}


