#Correction for Brazil
library(readxl)
library(xlsx)

db <- read_excel("SABE_brazil_full.xlsx")
db <- as.data.frame(db)

db$FM_CardioMetab_Diabetes_c05 <- ifelse(test = db$FM_CardioMetab_Diabetes_c05 == 0, yes = 2, no = 1)
db$FS_Aislamiento_ViveSolo_g2 <- ifelse(test = db$FS_Aislamiento_ViveSolo_g2 == 0, yes = 2, no = 1)
db$FM_CardioMetab_Hiperten_c04 <- ifelse(test = db$FM_CardioMetab_Hiperten_c04 == 0, yes = 2, no = 1)
db$FM_CardioMetab_IAM_c08 <- ifelse(test = db$FM_CardioMetab_IAM_c08 == 0, yes = 2, no = 1)
db$FM_EstiloVida_ActividadFis_c25a <- ifelse(test = db$FM_EstiloVida_ActividadFis_c25a == 0, yes = 2, no = 1)
db$FM_EstiloVida_Caida12Mes_c11 <- ifelse(test = db$FM_EstiloVida_Caida12Mes_c11 == 0, yes = 2, no = 1)
db$FM_SaludMental_ProbNervDiagnost_c20 <- ifelse(test = db$FM_SaludMental_ProbNervDiagnost_c20 == 0, yes = 2, no = 1)

write.xlsx2(x = db[,-1], file = "SABE_brazil_full.xlsx", row.names = TRUE,
            characterNA = "")

