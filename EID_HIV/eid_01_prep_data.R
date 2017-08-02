#
# eid_01_prep_data.R
#
# Reed Sorensen
# August 2017
#


library("readxl")
library("dplyr")
library("lubridate")

rm(list = ls())


# dat <- read_xls("_data/esmeralda/DB_EID_25012017.xls")
# 
# # change variables names to be R compatible
# names(dat) <- gsub(
#   pattern = "[^a-zA-Z0-9]",
#   replacement = "",
#   x = names(dat)
# )
# 
# write.csv(dat, "_intermediate_files/DB_EID_25012017_v2.csv", row.names = FALSE)



df_in <- read.csv("_intermediate_files/DB_EID_25012017_v2.csv")

df <- df_in

# Esmeralda tried to fix the order of day/month manually and 
#   now the dates render in different formats
# The correct date is what appears when you click on the cell in Excel
#
# -- Write a function that converts the date variables to a common format
#

fix_date <- function(j, switch_monthday = TRUE) {
  # j <- "DatadeNascimento"; switch_monthday <- FALSE # dev variables
  tmpvar <- as.character(df[, j])
  
  tmpvar2 <- as.vector(sapply(tmpvar, function(i) {
    ifelse(grepl("/", i), dmy(i), dmy("1/1/1900") + days(as.integer(i)))
  }))
  
  tmp_df <- data.frame(x_orig = tmpvar, x_tmp = tmpvar2) %>%
    mutate(
      x = as_date(x_tmp),
      x2 = x - 2,
      x_day = as.character(day(x2)),
      x_month = as.character(month(x2)),
      x_year = as.character(year(x2)),
      x_switch_daymonth = gsub(' ', '/', paste(x_month, x_day, x_year)),
      x3 = as_date(ifelse(grepl("/", x_orig), x, dmy(x_switch_daymonth))),
      x3_noswitch = as_date(ifelse(grepl("/", x_orig), x, x2))
    )
  
  out <- tmp_df$x3
  if (switch_monthday == FALSE) out <- tmp_df$x3_noswitch
  
  return(out)
}


# Check that the fix_date function works as expected
# -- Row 1 should be July 5, 2014 (switch day/month from original)
# -- Row 2 should be June 26, 2014 (unchanged for original)

tmp <- subset(df, select = "DatadeNascimento")

# tmp[, "dob"] <- fix_date("DatadeNascimento")
tmp[, "dob"] <- fix_date("DatadeNascimento", switch_monthday = FALSE)


# Convert all funky dates to common format
date_names <- c(
  names(df)[grepl("Data", names(df))],
  "colheitaUS1", "entradalab1", "processamento1", 
  "colheitaUS2", "entradalab2", "processamento2", 
  "colheitaUS3", "entradalab3", "processamento3"
)

# df[, paste0(date_names, "_2")] <- lapply(date_names, fix_date)
df[, paste0(date_names, "_2")] <- lapply(date_names, fix_date, switch_monthday = FALSE)


# Check all vars
date_names_2 <- paste0(rep(date_names, each = 2), c("", "_2"))
tmp2 <- df[, date_names_2]


saveRDS(df, "_intermediate_files/DB_EID_v3.RDS")


