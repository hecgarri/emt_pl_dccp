# Carga de paquetes
library(shiny)
library(RODBC)
library(openxlsx)
library(lubridate)

# Establece conexiones a los diferentes servidores
con3 <- RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse")


instituciones_disponibles <- sqlQuery(con3, "SELECT DISTINCT
L.Region
,UPPER([NombreInstitucion]) [NombreInstitucion]
FROM [DM_Transaccional].[dbo].[DimInstitucion] as D
INNER JOIN [DM_Transaccional].[dbo].[DimComprador] as C ON D.entCode=C.entCode
INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L ON C.IDLocalidadUnidaddeCompra = L.IDLocalidad")


instituciones_disponibles <- saveRDS(instituciones_disponibles, file = paste0(gsub("-","",Sys.Date()),"_institucion_disponibles.rds"))