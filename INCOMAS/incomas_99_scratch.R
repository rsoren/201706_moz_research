#
# incomas_99_scratch.R
#
# Reed Sorensen
# July 2017
#

library(dplyr)
library(data.table)

df_in <- fread("_data/INCOMAS_DATA_2017-07-24_0836.csv") %>%
  as.data.frame(.)


#####
# Variables for road traffic study
# Latitude is 'geopoint_test_lat'
# Longitude is 'geopoint_test_lng'
#




# full list of variables of interest

# also add variable about province

# what maternal factors affect whether a child has diarrhea in the last 2 weeks
diarrhea_2wk ~ maternal_age + education + province + urbanicity
# -- use the variable 'levelschool'

# among women who have a child with diarrhea, where did they go for treatment


# does urbanicity or education affect where women go for treatment

# we're interested in what factors affect where women go in general
# -- but better to have a question about a specific facility type


# What factors affect whether a mother goes to the market
#   or a traditional healder for diarrhea treatment?
# -- separate or compbine the two places; check both

# worst are mercado or traditional healer
# middle is farmacia or private medico
# best are unidade sanitaria, brigada movel, outro publico and private clinica


outcome_vars <- c(
  ccr_facility = "select305_text",  # facility will use for CCR
  
  # DIARRHEA VARIABLES
  
  diarrhea_2wk = "select306", # diarrhea in last 2 weeks
  blood_in_feces = "select307", # blood in feces
  diarrhea_treatment = "select308", # counseling or treatment for diarrhea
  
  # 'select309' is where got treatment for diarrhea
  "select309___1",   # public: unidade sanitaria
  "select309___2",   # public: brigada movel
  "select309___3",   # public: outro publico
  "select309___4",   # private: clinica
  "select309___5",   # private: farmacia
  "select309___6",   # private: medico
  "select309___7",   # private: outro
  "select309___8",   # other: mercado
  "select309___9",   # other: traditional healer
  "select309___10",  # other: 'pessoal de saude do bairro'
  "select309___11",  # other: other
  "select309_text",  # "specify"
  "select3091_text", # name of the place
  
  # 'select310' is type of treatment
  "select310a",      # ORS from packet
  "select310b",      # home-made ORS
  "select310c",      # rice water
  "select311",       # gave something else for diarrhea
  
  # 'select312' is what type of treatment given
  "select312___1",   # comprimidos/xarope
  "select312___2",   # injeccoes 
  "select312___3",   # soros intranosos
  "select312___4",   # remedia caseiro
  "select312___5",   # other
  "select312_text",  # text for other
  
  
  # variables about treatment at home are not so important
  
  # FEVER VARIABLES
  
  "select320",       # fever in last 2 weeks
  "select321",       # 
  "select322_a",
  "select322_b",
  "select323",       # maybe not useful
  "select324",
  "select324_text",
  "select325",       # not so important
  "select326",       # not so important
  "select327",      # yes, is important
  
  "select328___1",  # publico: unidade sanitaria
  "select328___2",  # publico: brigrada movel
  "select328___3",  # publico: outro publico
  "select328___4",  # privado: clinica
  "select328___5",  # privado: farmacia
  "select328___6",  # privado: medico
  "select328___7",  # privado: outro
  "select328___8",  # outro: mercado
  "select328___9",  # outro: medicina tradicional
  "select328___10", # outro: pessoal de saude do bairro
  "select328___11", # outro: outro
  
  "select328_text",
  "select3281_text",
  "select329",
  "select329_text",
  "select3291_text",
  "select330",
  "select331___1",
  "select331___2",
  "select331___3",
  "select331___4",
  "select331___5",
  "select331___6",
  "select331___7",
  "select331___8",
  "select331___9",
  "select331___10",
  "select331___11",
  "select331___12",
  "select331___13",
  "select331___14",
  "select331_text"
)




