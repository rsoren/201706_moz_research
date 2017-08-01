#
# 01_get_data.R
#
# Reed Sorensen
# June 19, 2017
#

library("RODBC")
library("readxl")
library("foreign")


# # get 'swamp' package
# pkgs <- c("foreach", "doParallel", "iterators", "parallel", "Matrix","lme4","INLA","Rtools","devtools")
# repos <- c("https://cran.rstudio.com", "https://www.math.ntnu.no/inla/R/stable") 
# install.packages(pkgs,repos=repos,dependencies = "Depends")

dir <- "C:/Users/rsoren/Documents/CIOB analyses/"

df_in <- RODBC::odbcConnectAccess2007(paste0(dir, "peer/DB PEER_6-May-17.accdb"))
tables <- RODBC::sqlTables(df_in)

# PEER
df <- RODBC::sqlFetch(df_in, "Ficha de verificação de registo")
write.csv(df, "data/ficha_de_verificacao_de_registro.csv", row.names = FALSE)

df2 <- RODBC::sqlFetch(df_in, "Seguimento C_Exposta")
write.csv(df2, "data/seguimento_c_exposta.csv", row.names = FALSE)


sheets <- readxl::excel_sheets(
  paste0(dir, "peer/Base_dados_quali_prof_ CPP 02-Jun-17.xlsx")
)



# Qualitative study
df3 <- readxl::read_excel(
  path = paste0(dir, "peer/Base_dados_quali_prof_ CPP 02-Jun-17.xlsx"),
  sheet = "EI Prof CPP"
)

write.csv(df3, "data/base_dados_quali_prof_CPP.csv", row.names = FALSE)


# Epi Info
path_suffix1 <- "Obitos HCB/Obitos HCB/Obitos HCB/"
path_suffix2 <- "LuciaProjectoMestrado"


df5_in <- RODBC::odbcConnectAccess2007(
  paste0(dir, path_suffix1, "LuciaProjectoMestrado (CORRIGIDO).mdb")
)
tables5 <- RODBC::sqlTables(df5_in)



df6_in <- RODBC::odbcConnectAccess2007(
  paste0(dir, path_suffix2, "LuciaProjectoMestrado (CORRIGIDO).mdb")
)
tables6 <- RODBC::sqlTables(df6_in)










