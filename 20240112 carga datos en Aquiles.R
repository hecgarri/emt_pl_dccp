#Quita archivos del WorkSpace ===========================================================
#
rm(list = ls())

#Fija el directorio de trabajo ==========================================================
#

wd_path = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/datos"


setwd(wd_path)

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
             ,"openxlsx"
             ,"odbc")

load_pkg(packages)


con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles
con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse


con4 <- dbConnect(odbc(),
                  Driver = "SQL Server",
                  Server = "10.34.71.146",
                  Database = "Estudios",
                  UID = "datawarehouse",
                  PWD = "datawarehouse")

sii <- sqlQuery(con3, 
                "SELECT *
                FROM [Estudios].[dbo].[sii_atrib_2021]")

#dbRemoveTable(con, "RUTs_genero_validados")
#cargar maestra diaria en BD Estudios del servidor AquilesConsulta 
dbWriteTable(conn = con4, 
             name = "sii_atrib_2021", 
             value = sii,
             row.names= FALSE)   