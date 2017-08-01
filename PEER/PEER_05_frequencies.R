
library(dplyr)

dat_in <- readRDS("_intermediate_files/PEER_processed_data.RDS")


# numerator: numero_mulheres_HIV_mais_de_duas_CPP
# denominator: numero_mulheres_HIV_1a_CPP

dat <- dat_in %>%
  select(coorte_num == 1) %>%
  mutate(
    prop1 = numero_mulheres_HIV_mais_de_duas_CPP / numero_mulheres_HIV_1a_CPP
  )

