#
# incomas_03_rti_prep_data.R
#
# Reed Sorensen
# August 2017
#
# Analysis of road traffic injuries from the INCOMAS study
# Preliminary regression results and geospatial analysis
# 
#+ include=FALSE
# Save as R Markdown and HTML files:
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
  "_data/INCOMAS Household and injury only 2017-06-08.dta"))

df_orig <- fread(paste0(dir, "_data/INCOMAS_DATA_2017-07-24_0836.csv")) %>%
  as.data.frame(.)

check_var <- function(x, dat = df_in) {
  names(dat)[grepl(x, names(dat), ignore.case = TRUE)]
}


check_var("inj")
check_var("inj", dat = df_orig)


df <- df_in %>%
  select(
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
    
    outcome2 = injmva_adults / n_adults
  )


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


#+ include=FALSE

df %>%
  group_by(urban) %>%
  summarize(
    mean = mean(householdn, na.rm = TRUE),
    sd = sd(householdn, na.rm = TRUE) ) %>%
  filter(complete.cases(.))

# ggplot(subset(df, !is.na(urban) & !is.na(householdn)), aes(x = householdn, fill = urban)) +
#   geom_density(alpha = 0.2)

 
#+ include=FALSE

# check for missing data

vars1 <- c(
  "injmva_any", "injmva_children", "injmva_adults", 
  "urban", "ses_index", "has_car", 
  "has_motorcycle", "householdn", "n_kids", "n_adults"
)

# vars1[!(vars1 %in% names(df))]


lapply(df[, vars1], function(x) {
  table(is.na(x))
})


#+ echo=FALSE

#' #### Model 1 - All ages
#' - Family: Poisson
#' - Outcome: Number of RTI among all ages
#' - Household predictors: urbanicity, having a car, having a motorcycle,
#'             SES index
#' - Other: Offset is number of people in the household
#' - 'ses\_index' uses all asset indicators except car and motorcycle 
#' (i.e. electricity, radio, tv, mobile phone, phone landline, refrigerator, 
#' watch, bicycle, cart and motorboat), 
#' because those are used as independent variables in the model. It is calculated
#' with _multiple correspondence analysis_. The method collapses all of the 
#' asset indicators into a single number while retaining as much information as possible.
#' 

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



#' #### Model 2 - Children only
#' - Family: Poisson
#' - Outcome: Number of RTI among children
#' - Household predictors: urbanicity, having a car, having a motorcycle,
#'             SES index
#' - Other: Offset is number of children in the household

#+ echo=FALSE

# kids only

df2 <- subset(df, !is.na(injmva_children) & n_kids > 0)

fit2 <- glm(
  injmva_children ~ urban + has_car + has_motorcycle + 
    ses_index + offset(log(n_kids)),
  data = df2, 
  family = poisson()
)

summary(fit2)



#' #### Model 3 - Adults only
#' - Family: Poisson
#' - Outcome: Number of RTI among adults
#' - Household predictors: urbanicity, having a car, having a motorcycle,
#'             SES index
#' - Other: Offset is number of adults in the household

#+ echo=FALSE

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



#' #### Model 4 - Vehicles as single categorical variable
#' - Family: Poisson
#' - Outcome: Number of RTI among adults
#' - Household predictors: urbanicity, 
#'   categorical vehicle variable (see below), SES index
#' - Other: Offset is number of adults in the household  

#' </br>

#' Vehicle variable:
#' - 1. No car or motorcycle (reference group)
#' - 2. Motorcycle only
#' - 3. Car only
#' - 4. Car and motorcycle

#+ echo=FALSE

# adults only; alternative specification for vehicles
fit4 <- glm(
  formula = injmva_adults ~ urban + vehicles2 + ses_index, 
  data = df3, 
  family = poisson()
)

summary(fit4)



#' #### Model 5 - using spatial coordinates a predictor

#+ echo=FALSE

dat_spatial <- sp::SpatialPointsDataFrame(
  coords = subset(df3, select = c("long", "lat")),
  data = df3
)


library(INLA)

# fit5 <- geostatsp::glgm(
#   formula = injmva_adults ~ urban + vehicles2 + ses_index + offset(log(n_adults)),
#   family = "poisson",
#   grid = 40,
#   data = dat_spatial
# )

saveRDS(fit5, paste0(dir, "_intermediate_files/inla_spatial_fit5.RDS"))
fit5 <- readRDS(paste0(dir, "_intermediate_files/inla_spatial_fit5.RDS"))


result5 <- exp(fit5$raster[["random.mean"]])
colors1 <- colorRampPalette(colors = c("white", "darkblue"))


#' Map shows incidence rate ratio (IRR) for each pixel
#' - Darkers areas are over Beira and Chimoio  
#'   


#' **[Click here](https://drive.google.com/open?id=1F5nMjt8ExAg0UiTRu8LCvDKIZtw&usp=sharing)** to see this location on a Google map

#+ echo=FALSE

save_result <- FALSE

if (save_result) jpeg(paste0(dir, "INCOMAS/rti_map_fit5_grid40_includeurbancovariate.jpg"))
plot(result5, col = colors1(12))
if (save_result) dev.off()

# regression results, adjusting for location
res5 <- fit5$inla$summary.fixed

#' **Spatial regression results:**  
#' Note: 'hi' and 'lo' indicate the upper and lower bounds for 
#' the Bayesian credible interval (not a confidence interval)

#+ echo=FALSE

# display results
matrix(cbind(res5$mean, res5$`0.025quant`, res5$`0.975quant`),
  dimnames = list(row.names(res5), c("mean", "lo", "hi")),
  nrow = nrow(res5)
)


# for showing the map extent of Google maps
my_coords <- expand.grid(long = xrange1, lat = yrange1)


