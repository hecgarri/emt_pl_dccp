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
             , "shiny"
             ,"ggplot2"
             ,"DT"
             ,"shinyjs")


load_pkg(packages)

detalles = function(path = wd_path, pattern = "*.rds"){
  require(dplyr)
  
  details = file.info(path = paste0(wd_path), list.files(pattern=pattern))
  
  details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ] %>% 
    filter(isdir==FALSE)
  
  details$files = rownames(details)
  
  rownames(details) = NULL
  
  return(details)
}

datasets <- detalles()

# Plan de análisis: 
# 1. cruzar las tablas gblEnterpriseAddress con prcPOHeader, a través de los campos eadCode y porShipAddress
# 2. Verificar en qué proporción coinciden los campos porShipAddress y porInVoiceAddress
# 3. Probar la conjetura respecto de qué el campo porShipAddress se modifica 
# siempre y cuándo la dirección de despacho seaa distinta de la que se provee
#  por default, ya que en dicho caso bastaría con modificar el formulario 
#  4. Cruzar la información sobre dirección de envío desde diferentes fuentes, 
#  por ejemplo, comparar la información de las órdenes de compra con las
#  solicitudes de cotización del módulo de compra ágil y la informacción contenida
#  en licitaciones

up_to_date <- FALSE # ¿Los datos están actualizados? 

if (!up_to_date){
  
  # #Establece conexiones a los diferentes servidores =======================================
  # 
  # #con = RODBC::odbcConnect("aquiles", uid = "datawarehouse", pwd = "datawarehouse") #TIVIT
  
  con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles
  
  con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse
  
  
  
  #############################################################################################
  # pserSellerCity es llenado a mano, no me sirve. Duplica innecesariamente los datos
  #############################################################################################  
  
  start <- Sys.time()
  
  datos <- sqlQuery(con2, "
		  WITH TABLA AS (SELECT 
	       SC.CodigoSolicitudCotizacion
		  ,SC.Descripcion
		  ,SC.FechaPublicacion
		  ,CP.CodigoProducto
		  ,GS.productoname
		  ,CP.Descripcion [Descripcion 2]
		  ,GS.level1
		  ,GS.level2
		  ,GS.level3
		  ,CP.Cantidad
		  ,CP.Precio
		  ,CP.MontoTotalProducto
		  ,O1.orgLegalName [Razón Social Proveedor]
		  ,E.entName
		  ,E.entCode
		  ,CO.Id [CodigoCotizacion]
		  ,CO.EsProveedorSeleccionado
		  ,CO.EsOfertaParcial
		  ,O1.orgActivity [Actividad Proveedor]
		  ,O2.orgLegalName [Razón Social Comprador]
		  ,SC.CodigoRegion [Región de Despacho]
		,(CASE  
		WHEN SII.TRAMOS_13 IS NULL THEN 'Sin categoría' -- 
		WHEN SII.TRAMOS_13 IN (1,2,3,4,5,6,7,8,9) THEN 'MiPyme'
		ELSE 'Grande' 
		END) AS [Size]
		,(CASE 
				WHEN SII.[DESCRIPCION_REGION] = 'I REGION DE TARAPACA' THEN 1
				WHEN SII.[DESCRIPCION_REGION] = 'II REGION DE ANTOFAGASTA' THEN 2
				WHEN SII.[DESCRIPCION_REGION] = 'III REGION DE ATACAMA' THEN 3
				WHEN SII.[DESCRIPCION_REGION] = 'IV REGION COQUIMBO' THEN 4
				WHEN SII.[DESCRIPCION_REGION] = 'V REGION VALPARAISO' THEN 5
				WHEN SII.[DESCRIPCION_REGION] = 'VII REGION DEL MAULE' THEN 7
				WHEN SII.[DESCRIPCION_REGION] = 'VIII REGION DEL BIO BIO' THEN 8
				WHEN SII.[DESCRIPCION_REGION] = 'IX REGION DE LA ARAUCANIA' THEN 9
				WHEN SII.[DESCRIPCION_REGION] = 'X REGION LOS LAGOS' THEN 10
				WHEN SII.[DESCRIPCION_REGION] = 'XI REGION AYSEN DEL GENERAL CARLOS IBAÑEZ DEL CAMPO' THEN 11
				WHEN SII.[DESCRIPCION_REGION] = 'XII REGION DE MAGALLANES Y LA ANTARTICA CHILENA' THEN 12
				WHEN SII.[DESCRIPCION_REGION] = 'XIII REGION METROPOLITANA' THEN 13
				WHEN SII.[DESCRIPCION_REGION] = 'XIV REGION DE LOS RIOS' THEN 14
				WHEN SII.[DESCRIPCION_REGION] = 'XV REGION ARICA Y PARINACOTA' THEN 15
				WHEN SII.[DESCRIPCION_REGION] = 'XVI REGION DE ÑUBLE' THEN 16
				WHEN SII.[DESCRIPCION_REGION] = 'Sin Información' THEN 0
				WHEN SII.[DESCRIPCION_REGION] IS NULL THEN 'Sin categoría'
				ELSE 6 
				END) AS id_regionProv
		FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC --TABLESAMPLE (1000 ROWS) REPEATABLE (123)
	  LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] as co ON co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
	  INNER JOIN [DCCPCotizacion].[dbo].[CotizacionProducto] as CP ON CO.Id = CP.CotizacionId
	  INNER JOIN [DCCPProcurement].[dbo].[prcGoodAndService] as GS ON CP.CodigoProducto = GS.ProductoCode 
	  INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O1 ON CO.CodigoSucursalEmpresa COLLATE Modern_Spanish_CI_AI = O1.orgCode COLLATE Modern_Spanish_CI_AI
	  INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] as E ON CO.CodigoEmpresa = E.entCode COLLATE Modern_Spanish_CI_AI
	  INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O2 ON SC.CodigoOrganismo  COLLATE Modern_Spanish_CI_AI = O2.orgCode COLLATE Modern_Spanish_CI_AI
	  INNER JOIN [Estudios].[dbo].[sii_atrib_2021] as SII ON CAST(LEFT(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-',''))
        			,LEN(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-','')))-1) as VARCHAR(50)) = CAST(SII.RUT AS VARCHAR(50))  
	  INNER JOIN [Estudios].[dbo].[THTamanoProveedor] as TP ON TP.entcode = E.entCode AND TP.[AñoTributario]=2022 
	  ---------------------------------------------------
	  WHERE YEAR(SC.FechaPublicacion) = 2023
	  ) 
	  SELECT 
		TABLA.*,
		(CASE 
			WHEN [id_regionProv] = [Región de Despacho] AND Size = 'MiPyme' THEN 'Local' 
  				WHEN [id_regionProv] != [Región de Despacho] AND Size = 'MiPyme' THEN 'No Local'
			WHEN size = 'Sin categoría' THEN 'sin categoría'
			ELSE 'Grande' END) AS [Proveedor Local 2]
	FROM TABLA;
")
  
  
  end <- Sys.time()
  
  print(difftime(end, start, units = "mins"))
  
  saveRDS(datos, file = paste0(gsub("-", "", today()), " proveedores locales compra ágil", ".rds"))
  
} else {
  datos <- readRDS(file = file.path(wd_path, datasets$files[1]))
}


datos <- setDT(datos)


regiones <- tibble(
  `Región de Despacho` = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  ,NombreRegion = c('Tarapacá', 'Antofagasta','Atacama'
                   ,'Coquimbo','Valparaíso','O`Higgins'
                   ,'Maule','Biobío','Araucanía'
                   ,'Los Lagos','Aysén','Magallanes'
                   ,'Metropolitana','Los Ríos','Arica y Parinacota','Ñuble'))

datos_rubros_region <- datos %>% 
  mutate(FechaPublicacion = as.Date(FechaPublicacion)) %>% 
  group_by(FechaPublicacion, `Región de Despacho`,level1) %>% 
  summarise(ofertas = n_distinct(CodigoCotizacion)
            ,solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,proveedores = NA) %>% 
  arrange(desc(ofertas)) %>% 
  inner_join(regiones, by = 'Región de Despacho')  
  # setDT() %>% 
  # melt(id.vars = c("FechaPublicacion", "Región de Despacho", "level1", "ofertas", "NombreRegion")
  #      ,measure.vars = c("ofertas", "solicitudes", "proveedores"))




#total_por_tipo <- aggregate(ofertas ~ level1 + `Región de Despacho`, data = datos_rubros_region, sum)