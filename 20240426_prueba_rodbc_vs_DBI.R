library(odbc)
library(DBI)
library(RODBC)
library(microbenchmark)

con3 <- RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse")

con4 <- dbConnect(odbc(), Driver = "ODBC Driver 17 for SQL Server", Server = "10.34.71.202", UID = "datawarehouse", PWD = "datawarehouse")

orgCodes <- dbGetQuery(con4, "SELECT DISTINCT OC.IDUnidaddeCompra FROM DM_Transaccional.dbo.ThOrdenesCompra as OC")




mbm <- microbenchmark(
    rodbc = sqlQuery(
      con3
      ,paste0("SELECT  
      T.Year
      ,I.NombreInstitucion
      ,COUNT(DISTINCT OC.CodigoOC) [Cantidad OC]
      ,SUM(OC.MontoCLP+OC.ImpuestoCLP) [MONTO CLP]
      FROM DM_Transaccional.dbo.ThOrdenesCompra as OC
      INNER JOIN DM_Transaccional.dbo.DimTiempo as T ON OC.IDFechaEnvioOC=T.DateKey
      INNER JOIN DM_Transaccional.dbo.DimComprador as C ON OC.IDUnidaddeCompra=C.IDUnidaddeCompra
      INNER JOIN DM_Transaccional.dbo.DimInstitucion as I ON C.entCode=I.entCode
      WHERE T.Year = 2023
      AND OC.IDUnidaddeCompra = ",orgCodes[round(runif(1, min =1, max = 8355)),],"
      GROUP BY T.Year
      ,I.NombreInstitucion")
    )
    ,dbi = dbGetQuery(
      con4
      ,paste0("SELECT  
      T.Year
      ,I.NombreInstitucion
      ,COUNT(DISTINCT OC.CodigoOC) [Cantidad OC]
      ,SUM(OC.MontoCLP+OC.ImpuestoCLP) [MONTO CLP]
      FROM DM_Transaccional.dbo.ThOrdenesCompra as OC
      INNER JOIN DM_Transaccional.dbo.DimTiempo as T ON OC.IDFechaEnvioOC=T.DateKey
      INNER JOIN DM_Transaccional.dbo.DimComprador as C ON OC.IDUnidaddeCompra=C.IDUnidaddeCompra
      INNER JOIN DM_Transaccional.dbo.DimInstitucion as I ON C.entCode=I.entCode
      WHERE T.Year = 2023
      AND OC.IDUnidaddeCompra = ",orgCodes[round(runif(1, min =1, max = 8355)),],"
      GROUP BY T.Year
      ,I.NombreInstitucion")
    )
  ,times = 100
)

resumen <- summary(mbm)

