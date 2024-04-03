#Quita archivos del WorkSpace ===========================================================
#
rm(list = ls())

#Fija el directorio de trabajo ==========================================================



#Carga de paquetes necesarios para el análisis ==========================================
#
load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

packages = c("tidyverse" #Conjunto integral de paquetes para manipular y analizar datos de manera coherente y eficiente.
             , "RODBC" #facilita la conexión y manipulación de bases de datos a través de ODBC (Open Database Connectivity).
             , "plotly" #proporciona herramientas interactivas para la creación de gráficos dinámicos y visualizaciones interactivas
             , "data.table" #Paquete optimizado para manipulación eficiente de grandes conjuntos de datos, destacando por su velocidad y funcionalidades avanzadas.
             , "formattable"
             , "hutils"
             , "readr"
             , "VennDiagram"
             , "RColorBrewer"
             ,"openxlsx")

load_pkg(packages)


# #Establece conexiones a los diferentes servidores =======================================
# 
# #con = RODBC::odbcConnect("aquiles", uid = "datawarehouse", pwd = "datawarehouse") #TIVIT

con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles

con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse

con4 = RODBC::odbcConnect("dwh", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse


datos <- sqlQuery(con2, 
                  "
                  select distinct
u.usrFirstName+u.usrLastName [Nombre]
,u.usrTaxID
,u.usrPhone	
,u.usrPosition
,u.usremail
,u.usrLastLogin
,E.entName
,o.orgTaxID
,COUNT(OC.CodigoOC) [Total OC]
,SUM(OC.MontoCLP+OC.ImpuestoCLP) [MontoCLP]
,SUM(OC.MontoUSD+OC.ImpuestoUSD) [MontoUSD]
from DCCPPlatform.dbo.gblOrganization as O 
inner join estudios_procesamientodatosabiertos.dbo.THOrdenesCompra as OC ON OC.IDSucursal =O.orgCode 
inner join estudios_procesamientodatosabiertos.dbo.DimTiempo as T ON OC.IDFechaEnvioOC=T.DateKey
inner join DCCPProcurement.dbo.prcPOHeader as PO ON OC.CodigoOC = PO.porCode
inner join DCCPPlatform.dbo.gblUser as U ON U.usrID = PO.porSellerUser
inner join DCCPPlatform.dbo.gblEnterprise as E ON O.orgEnterprise =E.entCode 
where E.entCode collate Modern_Spanish_CI_AI  in (SELECT distinct s.EntCode
                            FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                            WHERE EntCode NOT IN ('N/A') AND
                            ((s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                            (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= 2024) and
                            (year(s.fechacreacion)<= 2024)))
and o.orgClass=1
and T.Year = 2023
group by 
u.usrFirstName+u.usrLastName
,u.usrTaxID
,u.usrPhone	
,u.usrPosition
,u.usremail
,u.usrLastLogin
,E.entName
,o.orgTaxID          
                  ")

usuarios_empresas <- datos %>% 
  group_by(orgTaxID) %>% 
  filter(usrLastLogin == max(usrLastLogin)) %>% 
  mutate(`Total OC` = sum(`Total OC`),
         MontoCLP = sum(MontoCLP),
         MontoUSD =sum(MontoUSD)) %>% 
  arrange(desc(MontoCLP))


wb <- createWorkbook()

addWorksheet(wb, sheetName = 'Datos usuarios proveedores')

writeData(wb, sheet = 'Datos usuarios proveedores', x = usuarios_empresas)

saveWorkbook(wb, "20240403_consulta_proveedores_con_sello.xlsx", overwrite = TRUE)
