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


data <- sqlQuery(con4, 
                 "
     SELECT  DISTINCT
        T.Year
		,U.Nombres+' '+U.Apellidos [Nombre completo]
		,U.RUT [Rut usuario]
		,U.Sexo [Sexo usuario]
		,U.eMail
		,C.RUTUnidaddeCompra [Rut unidad de compra]
		,C.RazonSocialUnidaddeCompra [Razon social unidad de compra]
		,I.NombreInstitucion [Institucion]
		,L.Region
		,S.Sector [Sector Institucion]
		,CASE OC.porisintegrated 
            WHEN 3 THEN 'Compra Agil'
            ELSE (CASE  OC.IDProcedenciaOC
                    WHEN 703 THEN 'Convenio Marco'
                    WHEN 701 THEN 'Licitación Pública'
                    WHEN 1401 THEN 'Licitación Pública'
                    WHEN 702 THEN 'Licitación Privada'
                    ELSE 'Trato Directo' 
                END)
        END AS Procedencia
		,MAX(OC.IDFechaEnvioOC) [Fecha última OC]
		,COUNT(DISTINCT OC.CodigoOC) [Cantidad de OC]
		,SUM(OC.MontoCLP+OC.ImpuestoCLP) [Monto Transado (pesos)]
		,SUM(OC.MontoCLF+OC.ImpuestoCLF) [Monto Transado (UF)]
		,SUM(OC.MontoUSD+OC.ImpuestoUSD) [Monto Transado (USD)]
        FROM [DM_Transaccional_dev].[dbo].[THOrdenesCompra] as OC 
        LEFT JOIN [DM_Transaccional_dev].[dbo].DimUsuario as U ON OC.usrID = U.usrID 
    		INNER JOIN [DM_Transaccional_dev].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
        LEFT JOIN [DM_Transaccional_dev].[dbo].[DimComprador] as C ON OC.IDUnidaddeCompra = C.IDUnidaddeCompra
        LEFT JOIN [DM_Transaccional_dev].[dbo].[DimLocalidad] as L ON L.IDLocalidad = C.IDLocalidadUnidaddeCompra
        LEFT JOIN [DM_Transaccional_dev].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
        LEFT JOIN [DM_Transaccional_dev].[dbo].[DimSector] as S ON I.IdSector = S.IdSector
		--------------------------------------------------------------------------------------------------------------------------------------------------
		--------------------------------------------------------------------------------------------------------------------------------------------------
		--------------------------------------------------------------------------------------------------------------------------------------------------
		--------------------------------------------------------------------------------------------------------------------------------------------------

	  WHERE T.Year = 2023
       -- AND OC.IDEstadoOC IN  (4,5,6,7,12)
	  GROUP BY 
	  T.Year
		,U.Nombres+' '+U.Apellidos 
		,U.RUT 
		,U.Sexo 
		,U.eMail
		,C.RUTUnidaddeCompra 
		,C.RazonSocialUnidaddeCompra 
		,I.NombreInstitucion 
		,L.Region
		,S.Sector 
		,CASE OC.porisintegrated 
            WHEN 3 THEN 'Compra Agil'
            ELSE (CASE  OC.IDProcedenciaOC
                    WHEN 703 THEN 'Convenio Marco'
                    WHEN 701 THEN 'Licitación Pública'
                    WHEN 1401 THEN 'Licitación Pública'
                    WHEN 702 THEN 'Licitación Privada'
                    ELSE 'Trato Directo' 
                END)
        END 
                 ")





write.csv2(data, file = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/datos/20240313 consulta usuarios.csv"
           ,fileEncoding = "latin1")