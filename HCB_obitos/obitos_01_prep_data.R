#
# obitos_01_prep_data.R
#
# Reed Sorensen
# July 2017
#

library(dplyr)
library(stringr)

rm(list = ls())

df_in <- read.csv("_data/LuciaProjectMestrado_v2.csv", as.is = TRUE)


#####
# PREPARE PRIMARY ANALYTIC VARIABLES

# convert admission date and death date to a numeric variable
df <- df_in

convert_to_date <- function(x) {
  x2 <- strsplit(as.character(x), split = ' ')[[1]][1]
  x3 <- gsub(pattern = "/", replacement = "-", x2)
  ifelse(x3 == "", NA, parse_date_time(x3, "mdY"))
}

df$hosp_start_date <- sapply(df$DatadeInternamento, convert_to_date)
df$mortality_date <- sapply(df$Datadeobito, convert_to_date)
df$time_var <- df$mortality_date - df$hosp_start_date
df$time_days = df$time_var / (60 * 60 * 24)


# other variables
df2 <- df %>%
  mutate(
    died = ifelse(is.na(Datadeobito), 0, 1),
    oms_score = factor(EstadioclnicodaOMS),
    onTARV = ifelse(EstadoTARV == 0, 1, 0),
    ward = factor(ifelse(Localdeinternamento == 0, NA, Localdeinternamento)),
    Hb_2 = abs(Hb),
    WBC_2 = abs(WBC) ) %>%
  filter(!is.na(time_var)) %>%
  filter(time_var >= 0) %>%
  filter(!is.na(DatadeInternamento)) # 1 missing date of admission


# check the missing variables
tmp2 <- df %>%
  dplyr::select(
    DatadeInternamento, Datadeobito, 
    hosp_start_date, mortality_date, time_days )

#####
# SYMPTOMS

# list of all variables that contain responses about a patient's symptoms
symptom_vars <- c("Queixaprincipal", 
  names(df_in)[grepl("Outrossinaises", names(df_in))]
)

#-- rename some responses to avoid forbidden characters (e.g. +)
remove_char_df2 <- function(x, vars) {
  for (var in vars) df2[, var] <<- gsub(x, "", df2[, var])
}

remove_char_df2("\\+", vars = symptom_vars)

  
# get vector of unique symptoms
symptoms <- sort(unique(do.call("c", df2[symptom_vars])))


# define which alternative spellings should be grouped together
symp_list <- list(
  abcesso = c("Abcesso", "Abcesso multiplos", "Abcessos multiplos"),
  adenopatia = c("Adenopatia indolora", "Linfadenopatia indolora"),
  agitacao = c(
    "Agitacao", 
    "Agitacao motora", 
    "Agitacao pscomotora", 
    "Alteracao pscomotora", 
    "Agitacao psicomotora" ),
  ascite = c("Ascite", "Ascite "),
  atralgia = c("Atralgia", "Atralgia ", "Artralgia"),
  distensao_abdominal = c(
    "Aumento de vplume abdominal",
    "Aumento do volume abdominal",
    "Aumento do volume adbominal a 2 semanas",
    "Distensao abdominal",
    "Abdomen distendido",
    "Destensao abdominal",
    "Inchaco da barriga a 1 semana"),
  cefaleia = c("Cefaleia", "Cefaleia  a 2 dias", "Cefaleia intensa ha 1 semana" ),
  convulcao = c("Convulcao", "Convulsao"),
  enlarged_thorax = c("Abaulamento do hemitorax direito"), # pulmonary signal
  random_pulmonary_signs = c(
    "Diminuicao de murmurio vesicular",
    "Hemoptise",
    "Taquipneia",
    "Tiragem costal"),
  crepitacao_pulmonar = c( # pulmonary signal (not symptoms)
    "Crepitacao",
    "Crepitacao pulmonar",
    "Crepitacoes",
    "Crepitacoes oulmonares",
    "Crepitacoes pelmonares", 
    "Crepitacoes pulmonares", 
    "Crepitacoes Pulmonares", 
    "Crepitacoes pulmonaresPlacas esbranquicadas",
    "Crepitantes pulmonares", 
    "Crepitaoes pulmonares", 
    "Crepitracoes pulmonares",
    "Creptacoes pulmonares",
    "Dor no hemitorax; Crepitacoes pulmonares " ),
  disfagia = c("Disfagia"),
  desidratado = c(
    "Desidratacao",
    "desidratada",
    "Desidratada", 
    "Desidratado", 
    "Desidratado ", 
    "Desitracao", 
    "Desitratacao",
    "Desitratado",
    "Prega cutanea" ),
  diareia = c(
    "Diareia",
    "Diareia 2 semanas, tosse 1 mes",
    "Diareia a 2 dias",
    "Diareia a 3 dias", 
    "Diareia a 4 dias", 
    "Diareia com sangue", 
    "Diareia com sangue ", 
    "Diareia, fraqueza e febre a uma semana", 
    "Diareia, tosse", 
    "Diareia, vomito", 
    "Diareia, vomito, tosse,dores de cabeca, falta de ar",
    "Diarreia", 
    "Diarreia sanguinolenta" ), 
  hemmorragia_digestiva_baixa = c("Hematoquesia", "Melena"),
  vomito = c(
    "Diareia, vomito", 
    "Diareia, vomito, tosse,dores de cabeca, falta de ar",
    "Dor abdominalVomito"), 
  febre = c("Calafrios"),
  perda_de_peso = c("Caquetico", "Caquexia", "Perda de peso"),
  cianose = c("Cianose"),
  astenia = c(
    "Dificuldade de andar",
    "Dificuldade de marcha",
    "Difilculdade em caminhar",
    "Fraqueza a mais de 1 semana", 
    "Diareia, fraqueza e febre a uma semana", 
    "Fraqueza, dificuldade para andar",
    "Astenia a 2 semanas"),
  diminuicao_consciencia = c( # neurological symptoms
    "Diminuicao da consciencia",
    "Diminuicao de consciencia",
    "Diminuicao do nivel de conscienca",
    "Confusao mental",
    "Desmaio",
    "Perda de consciencia" ),
  amnesia = c("Amnesia"), # neurological symptoms
  formigueiro = c("Formigueiro"), # neurological symptoms
  sinais_de_hemorragia = c(
    "Petequeias", 
    "Equimosis", 
    "Epistase", 
    "Epixtase", 
    "Epixtase a 1 mes",
    "Sangramento nasal a 2 semanas"),
  anuria = c("Anuria a 5 dias", "Disuria"), # urological signs
  desnutricao = c("Desnutricao"),
  massa_abdominal = c("Massa abdominal"),
  diminuicao_forca = c(
    "Diminuicao da forca",
    "Diminuicao da forca MIE", 
    "Diminuicao da forca muscular",
    "Diminuicao de forca no MIE",
    "Falta de forca; Falta de apetite" ),
  anorexia = c("Falta de forca; Falta de apetite" ),
  arritmia_cardiaca = c("Arritmia cardiaca"), # cardiac signs
  dispneia = c(
    "Dispneia",
    "Tosse com sangue, dispneia",
    "Dificuldade de repirar",
    "Dificuldade de respirar",
    "Dificuldade de respirar a 3 semanas", 
    "Dificuldade respiratoria",
    "Falta de ar",
    "Diareia, vomito, tosse,dores de cabeca, falta de ar",
    "Adejo nasal" ),
  dor_abdominal = c(
    "Dor abdomianl", 
    "Dor abdominal", 
    "Dor abdominal ", 
    "Dor abdominal a 1 semana", 
    "Dor abdominal a 5 dias", 
    "Dor abdominal a mais de 1 mes", 
    "Dor abdominal de 6 meses", 
    "Dor abdominal e fraqueza a 1 semana", 
    "Dor abdominal e tosse", 
    "Dor abdominalVomito" ),
  dor_cabeca = c(
    "Dor de cabeca", 
    "Diareia, vomito, tosse,dores de cabeca, falta de ar",
    "Dor de cabeca e dor lombar a 3 semanas" ),
  dor_musculoesqueletica = c("Dor lombar", "Mialgia"),
  dor_lombar = c("Dor lombar", "Dor nos menbros inferiores"),
  dor_peito = c(
    "Dor de peito", 
    "Dor no peito", 
    "Dor toraxica", 
    "Dor no hemitorax; Crepitacoes pulmonares " ),
  edema = c(
    "Edema", 
    "Edema ", 
    "Edema MIE", 
    "Edema nos membros inferiores", 
    "Edema papebral", 
    "Edemas",
    "Inchaco nos pes",
    "Tumefacao na face "),
  esplenomegalia = c("esplenomegalia", "Esplenomegalia", "Hepatoesplenomegalia"),
  hepatomegalia = c("Hepatomegalia"),
  random_cardiac_signs = c("Ortopneia", "Taquicardia"),
  ouvido_sinsis_e_sinomas = c("Hiperacusia", "Hipoacusia", "Otalgia", "Perda de audicao"),
  random_urological_signs = c("Oliguria"),
  random_neurological_signs = c(
    "Psicose",
    "Hipertonia", 
    "Kerning", 
    "Movimentos involuntarios",
    "Obnubilado",
    "Odinofagia",
    "Sonolencia",
    "Sialoreia",
    "Tonturas",
    "Parestesia"),
  anemia = c("Hipocorado"),
  ferida = c(
    "Ferida limpa",
    "Ferida na boca e fraqueza", 
    "Ferida na cabeca", 
    "Ferida na perna", 
    "Ferida na perna esquerda", 
    "Ferida no pescoco", 
    "Feridas na boca" ), 
  outros = c("Muita sede", "Nao deambula", "Nauseas", "Nistagmo", 
    "Polidipsia", "Poliuria",
    "Prurido", "Saciedade precoce", "Secrecao essbranquicada", 
    "Sequedade da boca", "Veruga vaginal", "Alteracao visual",
    "Alergia no abdomem", "Gemido", "Indigestao" ),
  paralisia = c(
    "Hemiplegia", 
    "Hemiplegia a esquerda", 
    "Hemiplegia D", 
    "HemiplegiaClonus posetivo",
    "Dificuldade de falar",
    "Perda de fala"),
  ictericia = c(
    "Ictericia", 
    "Icterico" ),
  inconsciente = c("Inconsciente", "Incosciente" ),
  lesoes_da_pele = c(
    "Lesoes descamativas no couro cabeludo e tronco", 
    "Dermatite" ),
  candiase_oral = c("Lesoes esbranquicadas", "Descamacao da pele"),  # put this in candiase oral
  lesoes_nodulares = c(
    "Manchas hiperpigmentadas",
    "Lesoes na pele", 
    "Lesoes na pele de 2 semanas", 
    "Lesoes na pele hiperpigmentadas", 
    "Lesoes nodulares no palato", 
    "Lesoes violetas no MI", 
    "Nodulos hiperpigmentados",
    "Placas violaceas"),
  letargia = c("Letargia", "Letargico"),
  mal_estar = c(
    "Mal estar",
    "Mal estar  geral", 
    "Mal estar geral", 
    "Mal estra geral" ),
  mv = c( # this is a pulmonary signal
    "MV", 
    "MV ausente nas bases", 
    "MV dimenuido", 
    "MV diminuido", 
    "MV diminuido nas bases" ),
  oroteia_purulenta = c("Otoreia purulenta", "Otoreia Purulenta" ),
  palidez = c("palidez", "Palidez", "Palidez ", "Palido"),
  palpitacao = c("Palpitacao", "Palpitacao ", "Palpitacoes"),
  placas_esbranquivadas = c(
    "Placas esbranquicadas", 
    "Crepitacoes pulmonaresPlacas esbranquicadas",
    "Placas esbraquicadas", 
    "Placas essbranquicadas" ),
  polipneia = c("Polipneia", "Polipneico"),
  prostracao = c(
    "Prostacao; Prostacao; Caquexia; Caquexia",
    "Prostracao", 
    "Prostrado" ),
  quelite_angular = c(
    "quelite angular", 
    "Quelite angular" ),
  solucos = c("Solucos", "Solusos" ),
  tosse = c(
    "Tosse", 
    "Tosse a 2 semanas", 
    "Tosse a 3 meses", 
    "Tosse a 3 semanas", 
    "Tosse a 4 meses", 
    "Tosse a 4 semanas", 
    "Tosse a 5 dias", 
    "Diareia, vomito, tosse,dores de cabeca, falta de ar",
    "Tosse com sangue, dispneia", 
    "Tosse e diareia", 
    "Tosse e dor toraxica a 1 mes", 
    "Tosse e fraqueza a 1 mes", 
    "Tosse hemoptoica", 
    "Tosse, dor em todo  corpo, dor de cabeca" ),
  ulcera = c("Ulcera", "Ulcera na regiao inguinal"),
  vertige = c(
    "Vertige", 
    "Vertigem", 
    "Vertigem " )
)

symptoms_categorized <- as.vector(do.call("c", symp_list))
symptoms_uncategorized <- symptoms[!(symptoms %in% symptoms_categorized)]


# create variable that combines all symptoms variables into a single string
df2$all_symp <- apply(df2[, symptom_vars], MARGIN = 1, FUN = paste, collapse = "-")


# create a new dichotomous variable for each symptom group
# -- e.g. for patients with a cought, their value of 'x_tosse' is TRUE, otherwise FALSE

for (symp in names(symp_list)) {
  
  possible_responses <- symp_list[[symp]]
  
  # check if the possible responses are in an individual's list of symptoms
  df2[, paste0("x_", symp)] <-  apply(
    sapply(possible_responses, FUN = grepl, x = df2$all_symp), MARGIN = 1, any )
  
}


write.csv(df2, "_data/obitos_data_prepared.csv")


standard_symptom_list <- c(
  "N1Tosse", "N2Dispneia", "N3Sudoresenocturna", "N4Confusaomental",
  "N5Cefaleia", "N6Regideznanuca", "N7Vomito", "N8Diareia",
  "N9Anorexia", "N10Febre", "N11Perdadepeso", "N12Astenia"
)

saveRDS(standard_symptom_list, "_intermediate_files/standard_symptom_list.RDS")


for (var in standard_symptom_list) {
  df2[, var] <- ifelse(df2[, var] == "True", TRUE, FALSE)
}
  

# update the standard symptoms
# -- if the write-in answer matches something in the standard symptom list,
#    ensure that the standard symptom is set to TRUE
df3 <- df2 %>%
  mutate(
    sinais_neurologico = N4Confusaomental + x_convulcao + x_diminuicao_consciencia + 
      x_amnesia + x_formigueiro + x_random_neurological_signs +
      x_inconsciente + x_paralisia,
    sinais_pulmonar = x_enlarged_thorax + x_random_pulmonary_signs +
      x_crepitacao_pulmonar + x_tosse ) %>%
  mutate(
    N1Tosse = ifelse(x_tosse, TRUE, N1Tosse),
    N2Dispneia = ifelse(x_dispneia, TRUE, N2Dispneia),
    N4Confusaomental = ifelse(x_diminuicao_consciencia | x_amnesia, TRUE, N4Confusaomental),
    N5Cefaleia = ifelse(x_cefaleia, TRUE, N5Cefaleia), 
    N7Vomito = ifelse(x_vomito, TRUE, N7Vomito),
    N8Diareia = ifelse(x_diareia, TRUE, N8Diareia),
    N9Anorexia = ifelse(x_anorexia, TRUE, N9Anorexia),
    N10Febre = ifelse(x_febre, TRUE, N10Febre),
    N11Perdadepeso = ifelse(x_perda_de_peso, TRUE, N11Perdadepeso),
    N12Astenia = ifelse(x_astenia, TRUE, N12Astenia)
  )

write.csv(df3, "_data/obitos_data_combined_symptoms.csv", row.names = FALSE)


#####
# DISEASES


# remove '+' sign from the disease responses
remove_char_df3 <- function(x, vars) {
  for (var in vars) df3[, var] <<- gsub(x, "", df3[, var])
}

remove_char_df3("\\+", vars = "DoenasAssociadas")


# get vector of unique disease responses
diseases_tmp <- as.vector(do.call("c", sapply(df3$DoenasAssociadas, function(x) {
  tmp <- trimws(strsplit(x, split = ";")[[1]])
  
})))

diseases <- sort(unique(diseases_tmp))


# group together responses that refer to the same disease
disease_list <- list(
  anemia = c(
    "Anemia", 
    "Anemia cronica ligeira", 
    "Anemia cronica severa", 
    "Anemia descompesada", 
    "Anemia grave", 
    "Anemia Grave", 
    "Anemia leve", 
    "Anemia ligeira", 
    "Anemia moderada", 
    "Anemia Moderada", 
    "anemia moderada descompesada", 
    "Anemia severa", 
    "ANemia severa",
    "Anemia severa cronica" ),
  avc = c(
    "AVC", 
    "AVC com hemiplegia", 
    "AVC com hemiplegia direita", 
    "AVC com hemiplegia esquerda" ),
  broncopneumonia = c(
    "Broncopnemonia",
    "Broncopulmonia tuberculosa",
    "BPN",
    "BPN aspirativa",
    "BPN com derame pleural",
    "BPN com insuficiencia respiratoria",
    "BPN encefalopatia por HIV",
    "BPN grave",
    "BPN Grave",
    "BPN nosocomial grave",
    "BPN/ TB",
    "BPN/ Tp",
    "BPN/ TP",
    "BPN/PCP",
    "BPN/TB",
    "BPN/TB disseminado",
    "BPN/TP" ),
  candiase = c( # disease
    "Candiase oral",
    "Candiase oroesofagica",
    "Candidiase esofagica",
    "Candidiase oral",
    "Candidiase Oral",
    "Candidiase oroesofagica",
    "Candidiase orofaringea",
    "Candidiasis oral",
    "Candisisase oroesofagica",
    "Caandidiase oroesofagica"),
  celulite = c( # disease
    "Celulite do MID",
    "Celulite do MIE" ),
  desequilibrio = c( # disease
    "Desequilibrio hidroeletrolitico",
    "Desequilibrio hormonal" ),
  desidratacao = c( # both
    "Desidratacao grave", 
    "Diareia aguda com desidratacao",
    "Gastroenterite aguda com desidratacao grave",
    "GEA com desidratacao",
    "GEA com desidratacao grave",
    "GEA com desidratacao leve",
    "GEA com desidratacao moderada",
    "GEA com desidratacao severa" ),
  desnutricao = c( # disease
    "Desnutricao grave",
    "Desnutricao proteica",
    "Desnutricao proteico calorica severa",
    "Malnutricao severa"),
  diareia = c( # both
    "Diareia aguda com desidratacao",
    "Diareia cronica", #-- disease
    "Diarreia aguda",
    "Diarreia cronica",
    "Gastroenterite aguda",
    "Gastroenterite aguda com desidratacao grave",
    "Gastroenterite agudo",
    "Gastroenterite cronica",
    "GEA",
    "GEA com desidratacao",
    "GEA com desidratacao grave",
    "GEA com desidratacao leve",
    "GEA com desidratacao moderada",
    "GEA com desidratacao severa",
    "Desenteria" ),
  encefalopatia_not_due_to_HIV = c( # d
    "Encefalopatia hepatica",
    "Encefalopatia uremica" ),
  encefalopatia_due_to_HIV = c( # d
    "ENcefalite",
    "Encefalite aguda",
    "Encefalopatia",
    "Encefalite por HIV",
    "Encefalite tipo HIV",
    "Encefalopatia por HIV",
    "ENcefalopatia por HIV" ),
  hemorragia_gastrica = c(
    "Hemoragia gastrica",
    "Hemorrragia digestiva alta" ),
  hiv_sem_sida = c(
    "HIV em TARV",
    "HIV sem TARV",
    "SP em TARv",
    "SP em TARV",
    "SP OMS sem TARV",
    "SP sem TARV",
    "SP sem TARV III OMS" ),
  sida = c(
    "Caquexia",# if person has this 'disease', make sure their symptom is TRUE
    "Caquexia grave",
    "Sindrome caquetico",
    "Sindrome Caquetico",
    "HIV-Sida",
    "HIV-SIDA",
    "HIV-SIDA OMS IV",
    "HIV OMS IV",
    "HIV OMS IV em TARV",
    "HIV OMS IV sem TARV",
    "HIV Sida",
    "SIDA em TARV",
    "Sida OMS IV",
    "SIDA OMS IV",
    "Sida OMS IV em TARV",
    "SIDA OMS IV em TARV",
    "Sida OMS IV sem TARV",
    "SIDA OMS IV sem TARV",
    "Sida ou MS4",
    "ID OMS IV",
    "Imunodepresao",
    "IMunodepresao -OMS IV",
    "Imunodepresao OMS IV",
    "Imunodepresao por HIV",
    "Imunodepressao",
    "Imunodepressao em TARV OMS IV",
    "Imunodepressao IV OMS",
    "imunodepressao OMS IV",
    "SO sem TARV OMS IV",
    "SP em TARV OMS IV",
    "SP IV OMS sem TARV",
    "SP OMS IV em TARV",
    "SP sem TARV IV OMS",
    "Sp sem TARV OMS IV",
    "SP sem TARV OMS IV",
    "HIV Sida OMS IV",
    "Sida avancado" ),
  hta = c(
    "HTA",
    "HTA controlada",
    "Emergencia hipertensiva" ),
  insuficiencia_cardiaca = c(
    "ICC",
    "ICC/CMD",
    "Insuficiencia cardiaca congestiva",
    "Insuficiencia cardiaca D",
    "Miocardiopatia dilatada por HIV",
    "Miocardiopatia por  HIV",
    "Cardiopatia por HIV" ),
  insuficiencia_hepatica = c(
    "Insuficiencia hepatica",
    "Insufiencia hepatica" ),
  insuficiencia_renal = c(
    "Insuficiencia renal",
    "Insuficiencia renal aguda",
    "Insuficiencia renal cronica",
    "IRC grau 1" ),
  insuficiencia_resp = c(
    "Insuficiencia respiratoria",
    "Insuficiencia respiratoria aguda",
    "Insuficiencia respiratoria cronica",
    "edema agudo do pulmao"),
  malaria = c(
    "Malaria",
    "Malaria complicada",
    "Malaria Complicada",
    "Malaria grave com trombocitopenia",
    "Malaria PF" ),
  meningocefalite = c(
    "Meningiencefalite",
    "Meningite tuberculosa",
    "meningoencefalite",
    "Meningoencefalite",
    "Meningoencefalite bacteriana" ),
  pancitopenia = c(
    "Pancitopenia",
    "Pancitopenia severa" ),
  pcp = c(
    "PCP" ),
  sarcoma_de_kaposi = c(
    "S. Kaposi em quimioterapia",
    "Sarcoma de  Kaposi disseminado",
    "Sarcoma de Kaposi",
    "Sarkoma de Kapose",
    "SK disseminado" ),
  sepsis = c(
    "Sepse",
    "Sepse com CID",
    "Sepse do sistema nervoso central",
    "Sepse grave",
    "Sepse Grave",
    "Sepse grave pela ulcera infectada",
    "Sepse intra abdominal",
    "Sepse pulmonar",
    "Sepse repiratoria grave",
    "Sepse respiratoria",
    "Sepsi respiratoria pos aborto",
    "Sepsis",
    "Sepsis do SNC",
    "Sepsis grave",
    "Sepsis por aborto",
    "Falencia Multiorganica"),
  tb = c(
    "TB abdominal",
    "TB Abdominal",
    "TB abdominal em tratamento",
    "TB desseminada",
    "TB disseminada",
    "TB em tratamento",
    "TB em tratamento fase de manutencao",
    "TB miliar",
    "TB peritoneal",
    "TB pleural",
    "TB pulmonar",
    "TB pulmonar em tratamento",
    "TB pulmonar em tratamento/ PCP",
    "TB pulmonar miliar",
    "TbP com derame pleural",
    "TBP em tratamento",
    "TP",
    "Meningite tuberculosa",
    "Recaida de TB pulmonar"),
  trombocitopenia = c(
    "Trombocitopenia",
    "Trombocitopenia grave" ),
  other_tumors = c(
    "Cancer de figado",
    "Carcinoma hepatocelular",
    "Tumor abdominal",
    "Tumor cerebral provavel" ),
  outros = c(
    "Choque Hipovolemico",
    "Falencia terapeutica",
    "Hipoglicemia",
    "Epistase",
    "Hipoproteinemia",
    "Hipotensao",
    "LOE",
    "Otite media purulenta",
    "Sindrome adenico",
    "Sindrome de desgaste",
    "SIR a TARV",
    "Neuropatia periferica",
    "Traumatismo craneoencefalico" ),
  outras_doencas_dermatologicas = c(
    "Pitiriase vesicolor",
    "Sindrome de steven Johnson",
    "Veruga vaginal" ),
  outras_doencas_abdominais = c(
    "Obstipacao intestinal",
    "DIP",
    "Hepatopatia cronica",
    "Sindrome icterico",
    "Dor abdominal para estudo",
    "Refluxo gastroesofagico",
    "Sindrome ascitico" ),
  outras_doencas_urologicais = c(
    "ITU",
    "Litiase renal" )
)


diseases_categorized <- as.vector(do.call("c", disease_list))
diseases_uncategorized <- diseases[!(diseases %in% diseases_categorized)]

# opportunistic infections

# TODO: make a new dichotomous variable indicating whether
#       the patient had an opportunistic infection at admission
opportunistic_infections <- c(
  "broncopneumonia", "candiase", "diareia", "encefalopatia_due_to_HIV",
  "malaria", "meningocefalite", "pcp", "sepsis", "tb"
)


# create a new dichotomous variable for each symptom group
# -- e.g. for patients with a cought, their value of 'x_tosse' is TRUE, otherwise FALSE

for (dis in names(disease_list)) {
  
  possible_responses <- disease_list[[dis]]
  
  # check if the possible responses are in an individual's list of symptoms
  df3[, paste0("y_", dis)] <-  apply(
    sapply(possible_responses, FUN = grepl, x = df3$DoenasAssociadas), MARGIN = 1, any )
  
}


write.csv(df3, "_intermediate_files/obitos_data_symptoms_and_diseases.csv")










