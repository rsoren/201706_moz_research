#
# incomas_03_rti_prep_data_tmp.R
#
# Reed Sorensen
# August 2017
#
# Save as R Markdown and HTML files:
#
# source("save_as_markdown.R")
# save_as_markdown("INCOMAS/incomas_03_rti_prep_data.R")
#


#+ include=FALSE

library(dplyr)
library(readstata13)
library(ggplot2)
library(FactoMineR)
library(data.table)

dir <- "C:/Users/rsoren/Documents/prog/projects/201706_moz_research/"

df_in <- read.dta13(paste0(dir, 
  "_data/INCOMAS Household and injury only 2017-06-08.dta")) %>%
  filter(!duplicated(geopoint_test_lat, geopoint_test_lng))

df_orig <- fread(paste0(dir, "_data/INCOMAS_DATA_2017-07-24_0836.csv")) %>%
  as.data.frame(.)

check_var <- function(x, dat = df_in) {
  names(dat)[grepl(x, names(dat), ignore.case = TRUE)]
}


check_var("inj")
check_var("inj", dat = df_orig)


df <- df_in %>%
  dplyr::select(
    lat = geopoint_test_lat, long = geopoint_test_lng,
    injmva_any, # how many total household members injured by MV in last 6 months
    injmva_adults,   # how many adults injured by MV in last 6 months
    injmva_children, # how many children injured by MV in last 6 months
    
    # Ask Kristjana where this variable is (called 'injure' in the original data)
    # injmva_severity = injure, # After the accident, what happened to the family member (see below for coding)
    
    province,  urbanorrural, 
    assets_car,  assets_motorcycle,
    householdn, child0_4, child5_14, women15, men15,
    date_test ) %>%
  mutate(
    
    # it looks like the 'injmva_children' and 'injmva_adults' variables
    # are missing unless one or the other is true
    # -- to salvage the variables, I'll convert all NA into 0
    # -- assumes that the above is true, and that there would otherwise be no missing data
    #    (not a good assumption)
    
    injmva_children = ifelse(is.na(injmva_children), 0, injmva_children),
    injmva_adults = ifelse(is.na(injmva_adults), 0, injmva_adults),
    
    n_kids = child0_4 + child5_14,
    n_adults = women15 + men15,
    
    injury2_all_yes = injmva_any > 0,
    injury2_all_no = householdn - injury2_all_yes,
    
    injury2_kid_yes = injmva_children > 0,
    injury2_kid_no = n_kids - injury2_kid_yes,
    
    injury2_adult_yes = injmva_adults > 0,
    injury2_adult_no = n_adults - injury2_adult_yes,
    
    urban = urbanorrural == "Urbano",
    has_motorcycle = assets_motorcycle == "Checked",
    has_car = assets_car == "Checked",
    
    vehicles = NA,
    vehicles = ifelse(!has_car & !has_motorcycle, "None", vehicles),
    vehicles = ifelse(!has_car & has_motorcycle, "Moto only", vehicles),
    vehicles = ifelse(has_car & !has_motorcycle, "Car only", vehicles),
    vehicles = ifelse(has_car & has_motorcycle, "Both", vehicles),
    
    vehicles2 = factor(vehicles, 
      levels = c("None", "Moto only", "Car only", "Both")),
    
    outcome2 = injmva_adults / n_adults )


# use 'assets' variables to construct to SES index
# -- dimensionality reduction techniques for categorical/heterogeneous variables
# https://stats.stackexchange.com/questions/215404/is-there-factor-analysis-or-pca-for-ordinal-or-binary-data?noredirect=1&lq=1
# https://stats.stackexchange.com/questions/5774/can-principal-component-analysis-be-applied-to-datasets-containing-a-mix-of-cont

asset_vars <- names(df_in)[grepl("assets_", names(df_in))]

asset_vars2 <- asset_vars[
  !(asset_vars %in% c("assets_car", "assets_motorcycle"))]

df_ses <- df_in[, asset_vars2]
df_ses[] <- lapply(df_ses, function(x) {
  vec <- x == "Checked"
  vec2 <- rep(NA, length(vec))
  vec2 <- ifelse(vec == TRUE, 1, vec2)
  vec2 <- ifelse(vec == FALSE, 0, vec2)
  return(vec2)
})

tmp3 <- factanal(
  x = df_ses, 
  factors = 1, 
  rotation = "promax", 
  scores = "regression"
)

factor_scores <- tmp3$scores
df$ses_index <- tmp3$scores[,1]



# look at distribution of household size by urbanicity

df %>%
  group_by(urban) %>%
  summarize(
    mean = mean(householdn, na.rm = TRUE),
    sd = sd(householdn, na.rm = TRUE) ) %>%
  filter(complete.cases(.))

# ggplot(subset(df, !is.na(urban) & !is.na(householdn)), aes(x = householdn, fill = urban)) +
#   geom_density(alpha = 0.2)


# check for missing data

vars1 <- c(
  "injmva_any", "injmva_children", "injmva_adults", 
  "urban", "ses_index", "has_car", 
  "has_motorcycle", "householdn", "n_kids", "n_adults"
)

vars1[!(vars1 %in% names(df))]


lapply(df[, vars1], function(x) {
  table(is.na(x))
})


#+ echo=FALSE
# all ages

df1 <- subset(df, !is.na(injmva_any) & householdn > 0)

fit1 <- glm(
  injmva_any ~ urban + has_car + has_motorcycle + 
    ses_index + offset(log(householdn)),
  data = df1, 
  family = poisson()
)

summary(fit1)



# kids only

df2 <- subset(df, !is.na(injmva_children) & n_kids > 0)

fit2 <- glm(
  injmva_children ~ urban + has_car + has_motorcycle + 
    ses_index + offset(log(n_kids)),
  data = df2, 
  family = poisson()
)

summary(fit2)


# adults only

df3 <- subset(df, !is.na(injmva_adults) & n_adults > 0)

fit3 <- glm(
  injmva_adults ~ urban + has_car + has_motorcycle + 
    ses_index + offset(log(n_adults)),
  data = df3, 
  family = poisson()
)

summary(fit3)


dat_pred3 <- expand.grid(
  urban = unique(df$urban),
  has_motorcycle = unique(df$has_motorcycle),
  has_car = unique(df$has_car),
  n_adults = 1:8,
  ses_index = mean(df3$ses_index, na.rm = TRUE)
) %>%
  filter(complete.cases(.)) %>%
  arrange(urban)


dat_pred3$phat <- predict(fit3, 
  newdata = dat_pred3,
  type = "response"
)


# adults only; alternative specification for vehicles
fit4 <- glm(
  formula = injmva_adults ~ urban + vehicles2 + ses_index, 
  data = df3, 
  family = poisson()
)

summary(fit4)


dat_spatial <- sp::SpatialPointsDataFrame(
  coords = subset(df3, select = c("lat", "long")),
  data = df3
)

xrange1 <- c(min(df$long, na.rm=T), max(df$long,na.rm=T))
yrange1 <- c(min(df$lat, na.rm=T), max(df$lat,na.rm=T))

fit5 <- geostatsp::glgm(
  formula = injmva_adults ~ urban + vehicles2 + ses_index + offset(log(n_adults)),
  family = "poisson",
  grid = 400,
  data = dat_spatial
)


str(fit5)
result5 <- fit5$raster[["random.mean"]]
plot(result5, col = colors(12), xlim = xrange1, ylim = yrange1)


library(spatstat)

outcome_ppp <- ppp(
  x = df3$long, 
  y = df3$lat, 
  window = owin(
    xrange = c(min(df3$long, na.rm=T), max(df3$long,na.rm=T)),
    yrange = c(min(df3$lat, na.rm=T), max(df3$lat,na.rm=T)) ) )

fit_spatstat <- ppm(
  # Q = outcome_ppp ~ urban + vehicles2 + ses_index + offset(log(n_adults)),
  Q = outcome_ppp ~ 1, 
  data = df3
)



#  # figure out new link
# fit6 <- spatstat::slrm(
#   formula = outcome_ppp ~ urban + vehicles2 + ses_index,
#   link = "log",
#   data = df
# )


library(geoR)
library(geoRglm)

dat_spatial2 <- as.geodata(dat_spatial)

geoRglm::glsm.mcmc(
  geodata = dat_spatial2, )


library(McSpatial)


library(hglm)

fit_hglm <- hglm2(
  
  injmva_adults ~ urban + has_car + has_motorcycle + 
    ses_index + offset(log(n_adults)),
)



sp::SpatialGridDataFrame(grid = NULL, data = expand.grid(
  x = min(xrange1):max(xrange1),
  y = min(yrange1):max(yrange1)
))

fit8 <- autoKrige(
  formula = outcome2 ~ urban + vehicles2 + ses_index,
  input_data = dat_spatial,
  new_data = c()
)

# 0	Ferido apenas
# 1	Hospitalizado
# 2	Incapacitados Permanentemente
# 3	Morte no local do acidence
# 4	Morte no hospital
# 5	Morte depois da alta





# injure131
# Quantos membros do agregado familiar foram feridos 
#   como resultado de qualquer acidente de veiculo em 
#   estrada nos ultimos 6 meses (tanto como um ocupante 
#   de carro ou pedestre)?
#
# injure1311
# NUMERO DE ADULTOS
#
# injure1312
# NUMERO DE CRIANCAS
# 


