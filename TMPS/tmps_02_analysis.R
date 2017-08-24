#
# tmps_02_analysis.R
#
# Reed Sorensen
# July 2017
#
# 1. Get frequencies for each disability variable
# 2. Convert responses to numbers according to WHODAS simple scoring
# 3. Sum the disability scores to get an aggregated score
# 4. Describe the distribution of disability by age, sex, and province
# 5. Show results graphically
#
#
# # Save as R Markdown file:
#
# rm(list = ls())
# dir <- "C:/Users/rsoren/Documents/prog/projects/201706_moz_research/"
# 
# library("rmarkdown")
# setwd(paste0(dir, "TMPS/"))
# 
# rmarkdown::render(
#   input = "tmps_02_analysis.R",
#   output_format = "pdf_document"
# )
# 
# setwd(dir)


library(dplyr)
library(ggplot2)
library(knitr)

knitr::opts_chunk$set(echo = FALSE, message = FALSE)

#+



df2 <- readRDS(paste0(dir, "_intermediate_files/tmps_data_processed.RDS"))


#####
# 1. Get frequencies for each disability variable

df3 <- df2 %>%
  select(-yrsurvey, -anos) %>%
  reshape2::melt(id.vars = c("provincia", "sexo", "idade_gt7")) %>%
  group_by(provincia, sexo, idade_gt7, variable, value) %>%
  summarize(count = n() ) %>%
  mutate(percent = as.character(round(count / sum(count), digits = 3) * 100) )

df3$sortvar <- factor(df3$value,  levels = c(
  "NENHUMA", "PEQUENA", "ALGUMA", "MUITA", 
  "GRAVE", "GRAVE OU NAO PODE FAZER", "NN"
))

df3 <- df3 %>%
  arrange(provincia, sexo, idade_gt7, variable, sortvar) %>%
  select(-sortvar)

write.csv(df3, paste0(dir, "TMPS/disability_question_frequencies.csv"), row.names = FALSE)


#####
# 2. Convert responses to numbers according to WHODAS simple scoring

# function for converting disability questions to numbers
convert2num <- function(var, 
  suffix = "_num", 
  txt_grave = "GRAVE OU NAO PODE FAZER" ) {
  # var <- "dif_fica_pe"; suffix <- "_num" # dev variable
  # txt_grave <- "GRAVE OU NAO PODE FAZER" # dev variable
  
  tmp <- as.data.frame(df2)
  tmp$tmpvar <- tmp[, var]
  
  tmp2 <- tmp %>%
    mutate(
      tmpvar_num = NA,
      tmpvar_num = ifelse(tmpvar == "NENHUMA", 1, tmpvar_num),
      tmpvar_num = ifelse(tmpvar == "PEQUENA", 2, tmpvar_num),
      tmpvar_num = ifelse(tmpvar == "ALGUMA", 3, tmpvar_num),
      tmpvar_num = ifelse(tmpvar == "MUITA", 4, tmpvar_num),
      tmpvar_num = ifelse(tmpvar == txt_grave, 5, tmpvar_num)
    )
  
  df2[, paste0(var, suffix)] <<- tmp2$tmpvar_num
  
}


df2 <- as.data.frame(df2) # convert to data frame to avoid 'tibble' error

# 
dif_names <- names(df2)[substr(names(df2),1,4) == "dif_"]
dif_names2 <- dif_names[dif_names != "dif_emocional"]

for (i in dif_names2) convert2num(i)
convert2num("dif_emocional", txt_grave = "GRAVE")


# check that the numeric variable has same info as character variable
num_names <- names(df2)[grepl("_num", names(df2))]

# for (name_i in num_names) {
#   # name_i <- num_names[1] # dev variable
#   cat("\n", name_i, ": ")
#   name2 <- substr(name_i, 1, nchar(name_i) - 4)
#   table_i <- table(df2[, name_i], df2[, name2])
#   res <- all(apply(table_i, MARGIN = 1, FUN = function(x) sum(x != 0)) == 1 )
#   cat(res, "\n")
# }


#####
# 3. Sum the disability scores to get an aggregated score

df2$disabilidade_num <- apply(df2[, num_names], MARGIN = 1, sum)



#####
# 4. Describe the distribution of disability by age, sex, and province

#-- fit regression models
fit1 <- lm(disabilidade_num ~ poly(anos, 3) + sexo + provincia, data = df2)
summary(fit1)

#-- make predictions
age_range <- min(df2$anos):max(df2$anos)

dat_pred <- expand.grid(
  anos = age_range,
  sexo = names(table(df2$sexo)),
  provincia = names(table(df2$provincia))
)

dat_pred[, c("pred", "lo", "hi")] <- predict(
  object = fit1, newdata = dat_pred, interval = "confidence")


#+ 

# plot the results from fit 1 
# -- covariates: age (3rd degree polynomial), sex and province

plot(NULL, NULL, 
  xlim = range(age_range), 
  ylim = c(0, max(dat_pred$pred)),
  xlab = "Age",
  ylab = "Predicted disability",
  main = "Model includes age (3rd degree poly.), sex and province"
)

with(subset(dat_pred, sexo == "FEMININO" & provincia == "MANICA"),
  lines(anos, pred, col = "navyblue"))

with(subset(dat_pred, sexo == "MASCULINO" & provincia == "MANICA"),
  lines(anos, pred, lty = 2, col = "navyblue"))

with(subset(dat_pred, sexo == "FEMININO" & provincia == "SOFALA"),
  lines(anos, pred, col = "orange"))

with(subset(dat_pred, sexo == "MASCULINO" & provincia == "SOFALA"),
  lines(anos, pred, lty = 2, col = "orange"))

legend("topleft",
  legend = c("Manica females", "Manica males", "Sofala females", "Sofala males"),
  lty = c(1,2,1,2), col = c(rep("navyblue", 2), rep("orange", 2)), cex = 0.8
)




fit2 <- lm(disabilidade_num ~ poly(anos, 4), data = df2)
summary(fit2)

dat_pred2 <- data.frame(anos = age_range)

dat_pred2[, c("pred", "lo", "hi")] <- predict(
  object = fit2, newdata = dat_pred2, interval = "confidence" )


library(ggplot2)

ggplot(data = dat_pred2, aes(anos, pred)) + 
  geom_ribbon(aes(ymin=lo, ymax=hi), alpha = 0.2, fill = "darkblue") +
  expand_limits(x = 0, y = 0) + 
  geom_line() +
  labs(title = "Model includes age (4th degree poly.)") +
  labs(x = "Age (years)", y = "Predicted disability")


# GAM model

library(mgcv)
fit3 <- gam(disabilidade_num ~ s(anos), data = df2)
summary(fit3)

dat_pred2[, c("pred_gam", "se_gam")] <- predict(
  object = fit3, newdata = dat_pred2, se.fit = TRUE)

dat_pred2 <- dat_pred2 %>%
  mutate(
    lo_gam = pred_gam - 1.96 * se_gam,
    hi_gam = pred_gam + 1.96 * se_gam )


ggplot(data = dat_pred2, aes(anos, pred_gam)) + 
  geom_ribbon(aes(ymin=lo_gam, ymax=hi_gam), alpha = 0.2, fill = "darkred") +
  expand_limits(x = 0, y = 0) + 
  geom_line() +
  labs(title = "Model includes age (GAM smoothing)") +
  labs(x = "Age (years)", y = "Predicted disability")



