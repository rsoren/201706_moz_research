

library(dplyr)

df <- read.csv("DQA/BD_SOFALA_E_MANICA.csv")



calculate_stats <- function(dat, lr_var2, other_var2) {
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
  exclude_months_with_missing = TRUE) {
  
  # indicator = "Testados"; instrument1 = "LR"; instrument2 = "FB" # dev
  # exclude_months_with_missing = TRUE # dev
  
  require(dplyr)
  
  # identify analytic variables
  if (instrument1 != "LR") stop("The argument 'instrument1' must be 'LR'")
  lr_var <- paste0(indicator, '_', instrument1)
  other_var <- paste0(indicator, '_', instrument2)
  
  tmp <- df[, c("Unidade_Sanitaria", "Mes", other_var, lr_var)]
  
  # if 'complete_cases' is TRUE, remove months that do not have data
  #   from both instruments
  if (exclude_months_with_missing) {
    tmp <- subset(tmp, complete.cases(tmp[,c(lr_var, other_var)]))
  }
  
  # add together the cases from all months
  tmp2 <- tmp %>%
    select(-Mes) %>%
    group_by(Unidade_Sanitaria) %>%
    summarize_all(sum, na.rm = TRUE) %>%
    as.data.frame(.)
  
  # identify the analysis
  tmp2$indicator <- indicator
  tmp2$instrument1 <- instrument1
  tmp2$instrument2 <- instrument2
  
  # rename the variables for the counts
  tmp2$num1 <- tmp2[, lr_var]
  tmp2$num2 <- tmp2[, other_var]
  tmp2 <- tmp2[, !names(tmp2) %in% c(lr_var, other_var)]
  
  # choose numerator and denominator
  tmp2$numerator <- apply(tmp2[, c("num1", "num2")], MARGIN = 1, FUN = min)
  tmp2$denominator <- apply(tmp2[, c("num1", "num2")], MARGIN = 1, FUN = max)
  
  # calculate concordance
  tmp2$concordance <- (tmp2[,"num1"] - tmp2[,"num2"]) / tmp2[,"num1"]
  tmp2$concordance2 <- tmp2$numerator / tmp2$denominator
  
  # calculate incidence rate ratio
  tmp2[, c("ratio", "lo", "hi")] <- calculate_stats(
    dat = tmp2, lr_var2 = "num1", other_var2 = "num2"
  )
  
  tmp2[, "ratio_ci95_overlaps_10pct"] <- check_overlap(
    x1 = 0.9,  x2 = 1.1,  y1 = tmp2[, "lo"], y2 = tmp2[, "hi"]
  )
  
  return(tmp2)
}

# # dev example
# df_tmp <- get_concordance(
#   indicator = "Testados", instrument1 = "LR", instrument2 = "FB",
#   exclude_months_with_missing = TRUE
# )

inputs <- as.matrix(expand.grid(
  indicator = c("Testados", "Positivos", "Tratados_ACTs", "Obitos"),
  instrument1 = "LR",
  instrument2 = c("FB", "RMUS", "NEP_FISICO", "SIS_MA"),
  exclude_months_with_missing = "TRUE"
))

# there's no variable for 'Obitos' as measured by 'FB'
inputs <- inputs[!(inputs[,1] == "Obitos" & inputs[,3] == "FB"),]

results <- do.call("rbind", lapply(1:nrow(inputs), function(i) {
  # i <- 1 # dev
  print(paste0(inputs[i,]))
  tmp_indicator <- inputs[i,1]
  tmp_instrument1 <- inputs[i,2]
  tmp_instrument2 <- inputs[i,3]
  tmp_exclude <- ifelse(inputs[i,4] == "TRUE", TRUE, FALSE)
  
  get_concordance(
    indicator = tmp_indicator, 
    instrument1 = tmp_instrument1, 
    instrument2 = tmp_instrument2,
    exclude_months_with_missing = tmp_exclude
  )

}))

# TODO: do this by month, 
#       then calculate what percent of months are within 10% margin of error


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



results2 <- results %>%
  left_join(locations, by = c("Unidade_Sanitaria" = "US")) %>%
  mutate(province = ifelse(Unidade_Sanitaria %in% in_manica, "Manica", "Sofala")) %>%
  group_by(province, indicator, instrument2) %>%
  summarize(
    num1 = sum(num1),
    num2 = sum(num2) ) %>%
  mutate(
    ratio = num2 / num1,
    concordance = (num1 - num2) / num1 ) %>%
  filter(province == "Sofala" & instrument2 == "RMUS")


# report the "accuracy" of instruments as a rate ratio -- excluding months with missing info
# report the "availability" of instruments as percent of months with a missing instrument


