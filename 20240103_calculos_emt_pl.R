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
             ,"openxlsx")

load_pkg(packages)


# #Establece conexiones a los diferentes servidores =======================================
# 
# #con = RODBC::odbcConnect("aquiles", uid = "datawarehouse", pwd = "datawarehouse") #TIVIT

con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles

con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse



detalles = function(path = wd_path, pattern = "*.rds"){
  require(dplyr)
  
  details = file.info(path = paste0(wd_path), list.files(pattern=pattern))
  
  details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ] %>% 
    filter(isdir==FALSE)
  
  details$files = rownames(details)
  
  rownames(details) = NULL
  
  return(details)
}



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

up_to_date <- TRUE

if (!up_to_date){

  start <- Sys.time()
  
  datos <- sqlQuery(con2, "
    WITH TABLA AS (SELECT 
	  SC.Descripcion
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
	  ,CO.EsProveedorSeleccionado
	  ,O1.orgActivity [Actividad Proveedor]
	  ,O2.orgLegalName [Razón Social Comprador]
	  ,SC.CodigoRegion [Región de Despacho]
--	  ,O2.orgActivity [Actividad Comprador]
	  ,(CASE  
            WHEN SII.TRAMOS_13 = 1 THEN 'MiPyme'
            WHEN SII.TRAMOS_13 IN (2,3,4,5,6,7,8,9) THEN 'MiPyme'
            ELSE 'Grande' END
            ) [Size]
	  ,(CASE SII.[DESCRIPCION_REGION] 
		WHEN 'I REGION DE TARAPACA' THEN 1
		WHEN 'II REGION DE ANTOFAGASTA' THEN 2
		WHEN 'III REGION DE ATACAMA' THEN 3
		WHEN 'IV REGION COQUIMBO' THEN 4
		WHEN 'V REGION VALPARAISO' THEN 5
		WHEN 'VII REGION DEL MAULE' THEN 7
		WHEN 'VIII REGION DEL BIO BIO' THEN 8
		WHEN 'IX REGION DE LA ARAUCANIA' THEN 9
		WHEN 'X REGION LOS LAGOS' THEN 10
		WHEN 'XI REGION AYSEN DEL GENERAL CARLOS IBAÑEZ DEL CAMPO' THEN 11
		WHEN 'XII REGION DE MAGALLANES Y LA ANTARTICA CHILENA' THEN 12
		WHEN 'XIII REGION METROPOLITANA' THEN 13
		WHEN 'XIV REGION DE LOS RIOS' THEN 14
		WHEN 'XV REGION ARICA Y PARINACOTA' THEN 15
		WHEN 'XVI REGION DE ÑUBLE' THEN 16
		WHEN 'Sin Información' THEN 0
		ELSE 6 END) AS id_regionProv
  FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC --TABLESAMPLE (1000 ROWS) REPEATABLE (123)
  LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] as co ON co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
  INNER JOIN [DCCPCotizacion].[dbo].[CotizacionProducto] as CP ON CO.Id = CP.CotizacionId
  INNER JOIN [DCCPProcurement].[dbo].[prcGoodAndService] as GS ON CP.CodigoProducto = GS.ProductoCode 
  INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O1 ON CO.CodigoSucursalEmpresa COLLATE Modern_Spanish_CI_AI = O1.orgCode COLLATE Modern_Spanish_CI_AI
  INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O2 ON SC.CodigoOrganismo  COLLATE Modern_Spanish_CI_AI = O2.orgCode COLLATE Modern_Spanish_CI_AI
  INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON CAST(LEFT(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-',''))
        		,LEN(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-','')))-1) as VARCHAR(50))  = CAST(SII.RUT AS VARCHAR(50))
  WHERE YEAR(SC.FechaPublicacion) = 2023
  ) 
  SELECT 
    TABLA.*,
    (CASE 
        WHEN [id_regionProv] = [Región de Despacho] AND Size = 'MiPyme' 
		THEN 'Local' 
        ELSE 
		(CASE
			WHEN [id_regionProv] != [Región de Despacho] AND Size = 'MiPyme'
			THEN 'No Local'
			ELSE 'Grande' END
		)
    END) AS [Proveedor Local 2]
FROM TABLA;
")
  
  
  end <- Sys.time()
  
  difftime(end, start, units = "mins")
  
  saveRDS(datos, file = paste0(gsub("-", "", today()), " proveedores locales compra ágil", ".rds"))
  
} else {
  datos <- readRDS(file = '20240111 proveedores locales compra ágil.rds')
}


wb <- createWorkbook()

# Crea una hoja de índice con enlaces a otras hojas
# 

addWorksheet(wb, sheetName = 'Indice')

indice <- data.frame(
  Hoja = c("Proveedor Local - Rubro"
           , "Proveedores inter - Rubro"),
  Enlace = c("Proveedor Local - Rubro!A1"
             , "Proveedores inter - Rubro!A1")
)

# Escribe la tabla en la hoja de índice
writeDataTable(wb, sheet = "Indice", x = indice, startCol = 1, startRow = 1, tableName = "Indice", tableStyle = "TableStyleLight9")


addWorksheet(wb, sheetName = 'Proveedor Local - Rubro')

n_oferta_rubros <- datos %>%
  group_by(level1, `Región de Despacho`, `Proveedor Local 2`) %>% 
  summarise(n_ofertas = n(),  
            n_solicitudes = sum(EsProveedorSeleccionado),
            n_proveedores = n_distinct(`Razón Social Comprador`)) %>% 
  setDT() %>% 
  data.table::dcast(level1+`Región de Despacho`~`Proveedor Local 2`, value.var = c('n_ofertas', 'n_solicitudes', 'n_proveedores'))

writeData(wb, sheet = "Proveedor Local - Rubro", x = n_oferta_rubros)


addWorksheet(wb, sheetName = 'Proveedores inter - Rubro')

n_regiones <- datos %>% 
  group_by(level1, `Región de Despacho`, `id_regionProv`) %>% 
  summarise(n_ofertas = n(),
            n_solicitudes = sum(EsProveedorSeleccionado),
            n_proveedores = n_distinct(`Razón Social Comprador`)) %>% 
  setDT() %>% 
  data.table::dcast(level1+`Región de Despacho`~id_regionProv, value.var = c('n_ofertas', 'n_solicitudes', 'n_proveedores'))
  

writeData(wb, sheet = "Proveedores inter - Rubro", x = n_regiones)





# Agrega fórmulas para crear hipervínculos en cada hoja
for (i in 1:length(wb$sheet_names)) {
  sheet_name <- wb$sheet_names[i]
  if (sheet_name != "Indice") {
    writeFormula(wb, sheet = sheet_name, x = paste0('HYPERLINK(', indice$Enlace[i], 'Volver al índice'), startCol = 1, startRow = 1)
  }
}


saveWorkbook(wb, paste0(gsub("-", "", today()), "_estadisticas_proveedores_locales.xlsx"), overwrite = TRUE)





# 
# 
# datos <- sqlQuery(con2, "
#     SELECT
#     TABLA2.*
#     ,(CASE 
#     WHEN TABLA2.[Proveedor Local 2]='1' AND TABLA2.Size='MiPyme' THEN 'MiPyme Local' 
#     WHEN TABLA2.Size = 'Grande' THEN 'Grande'
#     WHEN TABLA2.Size = 'MiPyme' AND TABLA2.[Proveedor Local 2] = '0' THEN 'MiPyme No Local'
#     ELSE 'Otro Caso' END
#     ) [Proveedor Local Ley]
#     ,GS.level1 [Rubro ONU 1]
#     ,GS.level2 [Rubro ONU 2]
#     ,GS.level3 [Rubro ONU 3]
#     ,GS.productoname
#     ,PI.poiQuantity
#     ,PI.poiNetPrice
#     ,PI.poiTotalAmount
#     ,PI.poiName
#     ,PI.poiDescription
#           FROM  
#           (
#           SELECT 
#            TABLA.*
#             ,(CASE 
#             WHEN TABLA.[id_region_sr] = TABLA.[Región  Unidad de Compra] THEN 1
#             ELSE 0 END) [Proveedor Local 1]
#             ,(CASE 
#             WHEN TABLA.[id_region_sr] = TABLA.[Región Despacho (SolicitudCotizacion)] THEN 1
#             ELSE 0 END) [Proveedor Local 2]
#             ,SII.DESCRIPCION_ACTIVIDAD_ECONOMICA
#             ,(CASE  
#             WHEN SII.TRAMOS_13 = 1 THEN 'Sin Ventas'
#             WHEN SII.TRAMOS_13 IN (2,3,4,5,6,7,8,9) THEN 'MiPyme'
#             ELSE 'Grande' END
#             ) [Size]
#           
#                     FROM (
#                     SELECT --TOP 10000 
#                     	SC.FechaPublicacion -- Fecha de publicación de solicitd de cotización
#                       ,PO.porID
#                       ,SC.CodigoSolicitudCotizacion
#                     	,SC.Descripcion [Descripción Cotización]
#                       ,PO.porDescription [Descripción orden de compra]
#                     	,PO.[porBuyerOrganization]
#                     	,LOWER(REPLACE(REPLACE(SR.psrBuyerTaxID,'.',''),'-','')) [Rut Comprador] 
#                     	,SR.psrBuyerOrganizationLegalName  [Nombre Organismo Comprador]
#                     	,SR.psrBuyerActivity [Actividad del Comprador] 
#                     	,SR.psrBuyerAddress [Dirección  Unidad de Compra]
#                     	,SR.psrBuyerCity [Región  Unidad de Compra]
#                     	,SC.CodigoRegion [Región Despacho (SolicitudCotizacion)]
#                     	,SR.psrBuyerDistrict [Comuna  Unidad de Compra]
#                       --,CO.MontoTotal
#                     	,PO.[porSellerOrganization]
#                     	,SR2.psrSellerOrganizationLegalName  [Nombre Proveedor (sucursal)]
#                     	,SR2.psrSellerActivity [Actividad Proveedor]
#                     	,CO.EsProveedorSeleccionado [Ganadora]
#                     	,LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')) [Rut Proveedor] 
#                     	,LEFT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-',''))
#                     		,LEN(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')))-1) [Rut_numero]
#                     	,RIGHT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')),1) [Rut_dv]
#                     	,SR2.psrSellerOrganizationLegalName  [Razón Social Proveedor]
#                       ,O.orgCreationDate
#                     	,SR2.psrSellerActivity [Actividad del Proveedor  (prcPOStaticRecipient)] 
#                     	,SR2.psrSellerAddress [Dirección  Proveedor  (prcPOStaticRecipient)]
#                     	,SR2.psrSellerCity [Región  Proveedor  (prcPOStaticRecipient)]
#                     	,SR2.psrSellerDistrict [Comuna  Proveedor  (prcPOStaticRecipient)]
#                     	
#                       ,(CASE SR2.psrSellerCity 
#                     	WHEN 'DEL BIO BIO' THEN 8 
#                     	WHEN 'ñUBLE' THEN 16
#                     	WHEN 'Región Aysén del General Carlos Ibáñez del Campo' THEN 11
#                     	WHEN 'Región de Arica y Parinacota' THEN 15
#                     	WHEN 'Región de Coquimbo' THEN 4
#                     	WHEN 'Región de los Lagos' THEN 10
#                     	WHEN 'Región de Magallanes y de la Antártica' THEN 12
#                     	WHEN 'Región de Valparaíso' THEN 5
#                     	WHEN 'Región del Libertador General Bernardo O´Higgins' THEN 6
#                     	WHEN 'Región del Ñuble' THEN 16
#                     	WHEN 'Bio - Bio' THEN 8
#                     	WHEN 'Ñuble' THEN 16
#                     	WHEN 'Región de Antofagasta' THEN 2
#                     	WHEN 'Región de Atacama' THEN 3
#                     	WHEN 'Región de la Araucanía' THEN 9
#                     	WHEN 'Región de Los Ríos' THEN 14
#                     	WHEN 'Región de Tarapacá' THEN 1
#                     	WHEN 'Región del Biobío' THEN 8
#                     	WHEN 'Región del Maule' THEN 7
#                     	WHEN 'Región Metropolitana de Santiago' THEN 13 
#                     	ELSE 0 END) [id_region_sr]
#                       ,COUNT(DISTINCT PO.porCode) [Cantidad OC]
#                       ,SUM(
#                 			CASE 
#                 			WHEN CO.EsProveedorSeleccionado = 1 THEN PO.porTotalAmount
#                 			ELSE 0 END
#                 			) [Montos]
#                                   
#                   	FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC TABLESAMPLE (1000 ROWS) REPEATABLE (123)
#                   	LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] as co ON co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
#                   	INNER JOIN [DCCPCotizacion].[dbo].[RelacionOC] as ROC ON ROC.IdSolicitudCotizacion = SC.Id
#                   	INNER JOIN [DCCPProcurement].[dbo].[prcPOHeader] as PO ON PO.porID=ROC.porId
#                   	INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] as SR ON PO.porID =SR.psrOrder 
#                   	INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] as SR2 ON PO.porID =SR2.psrOrder
#                     INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O ON PO.porSellerOrganization = O.orgCode
#                   	WHERE porIsIntegrated = 3
#                   		AND (YEAR(SC.FechaPublicacion) = 2023 
#                   		--AND (SR.psrBuyerActivity != 'UNIVERSIDADES')
#                   		AND (SC.idEstado>=2)
#                   		--AND MONTH([porSendDate]) = 11
#                   		) 
#                   		AND ([porBuyerStatus] IN (4, 5, 6, 7, 12))
#                     --        AND SR.psrBuyerCity = 16
#                   	-- ORDER BY NEWID() -- Asegura la selección aleatoria
#                     GROUP BY 
#                   	SC.FechaPublicacion
#               			,PO.porID
#               			,SC.CodigoSolicitudCotizacion
#               			,PO.porDescription
#               			,SC.Descripcion
#               			,PO.porBuyerOrganization
#               			,SR.psrBuyerTaxID
#               			,SR.psrBuyerOrganizationLegalName
#               			,SR.psrBuyerActivity
#               			,SR.psrBuyerAddress
#               			,SR.psrBuyerCity
#               			,SC.CodigoRegion
#               			,SR.psrBuyerDistrict
#               			,PO.porSellerOrganization
#               			,SR2.psrSellerOrganizationLegalName
#                     ,O.orgCreationDate
#               			,SR2.psrSellerActivity
#               			,CO.EsProveedorSeleccionado
#               			,SR2.psrSellerTaxID
#               			,SR2.psrSellerAddress
#               			,SR2.psrSellerCity
#               			,SR2.psrSellerDistrict
#                           
#                         	)  TABLA 
#             INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON TABLA.Rut_numero = SII.RUT
#           ) TABLA2
#     INNER JOIN [DCCPProcurement].[dbo].[prcPOItem] as PI ON TABLA2.porID=PI.poiOrder
#     INNER JOIN [DCCPProcurement].[dbo].[prcGoodAndService] as GS ON PI.poiGoodAndService = GS.ProductoCode
#                  ")
# 
# 
# 
# 
# 
# 
# datos <- sqlQuery(con2,
# "
# SELECT 
# 	  OL.[poiID]
#       ,OL.[porID]
#       ,OL.[CodigoOC]
#       ,OL.[NombreItem]
# 	  ,PR.NombreProducto
# 	  ,RU.RubroN1
# 	  ,RU.RubroN2
# 	  ,RU.RubroN3
#       ,OL.[DescripcionItem]
#       ,OL.[UnidaddeMedida]
#       ,OL.[CantidadItem]
#       ,OL.[MonedaOC]
#       ,OL.[Monto]
#       ,OL.[MontoUSD]
#       ,OL.[MontoCLP]
#       ,OL.[MontoCLF]
#       ,OL.[MontoUTM]
# 	  ,P.RazonSocialSucursal
# 	  ,P.RUTSucursal
# 	  ,P.ActividadSucursal
# 	  ,E.NombreEmpresa
# 	  ,DT.Tamano
# 	  ,C.RazonSocialUnidaddeCompra
# 	  ,C.RUTUnidaddeCompra
# 	  ,C.ActividadUnidaddeCompra
# 	  ,I.NombreInstitucion
# 	  ,S.Sector
#   FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC TABLESAMPLE (1000 ROWS) REPEATABLE (123)
#   LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] as co ON co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
#   INNER JOIN [DCCPCotizacion].[dbo].[RelacionOC] as ROC ON ROC.IdSolicitudCotizacion = SC.Id
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[THOrdenesCompra] as OC  ON OC.porID=ROC.porId 
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[THOrdenesCompraLinea] as OL ON OC.CodigoOC=OL.CodigoOC 
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimProducto] as PR ON OL.IDProducto=PR.IDProducto
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimRubro] as RU ON PR.IdRubro = RU.IdRubro
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimProveedor] as P ON OC.IDSucursal=P.IDSucursal 
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimComprador] as C ON OC.IDUnidaddeCompra = C.IDUnidaddeCompra
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimEmpresa] as E ON P.entCode = E.entCode 
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimSector] as S ON I.IdSector = S.IdSector
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[THTamanoProveedor] as TP ON TP.entcode = E.entCode AND TP.[AñoTributario]=2022 
#   INNER JOIN [10.34.71.202].[DM_Transaccional].[dbo].[DimTamanoProveedor] as DT ON TP.idTamano = DT.IdTamano
#   WHERE T.Year = 2023 AND 
#   OC.IDEstadoOC IN  (4,6,12,13) AND
#   OC.porIsIntegrated = 3 AND 
#   YEAR(SC.FechaPublicacion) = 2023;
# "
# )
# 
# 
# datos <- sqlQuery(con2, "
#           WITH TABLA AS (SELECT 
#     	  SC.Descripcion
#     	  ,CP.CodigoProducto
#     	  ,GS.productoname
#     	  ,CP.Descripcion [Descripcion 2]
#     	  ,GS.level1
#     	  ,GS.level2
#     	  ,GS.level3
#     	  ,CP.Cantidad
#     	  ,CP.Precio
#     	  ,CP.MontoTotalProducto
#     	  ,SC.CodigoRegion [Región de Despacho]
#     	  ,O1.orgLegalName [Razón Social Proveedor]
#     	  ,CO.EsProveedorSeleccionado
#     	  ,O1.orgActivity [Actividad Proveedor]
#     	  ,O2.orgLegalName [Razón Social Comprador]
#     --	  ,O2.orgActivity [Actividad Comprador]
#     	  ,(CASE  
#                 WHEN SII.TRAMOS_13 = 1 THEN 'Sin Ventas'
#                 WHEN SII.TRAMOS_13 IN (2,3,4,5,6,7,8,9) THEN 'MiPyme'
#                 ELSE 'Grande' END
#                 ) [Size]
#     	  ,(CASE SII.[DESCRIPCION_REGION] 
#     		WHEN 'I REGION DE TARAPACA' THEN 1
#     		WHEN 'II REGION DE ANTOFAGASTA' THEN 2
#     		WHEN 'III REGION DE ATACAMA' THEN 3
#     		WHEN 'IV REGION COQUIMBO' THEN 4
#     		WHEN 'V REGION VALPARAISO' THEN 5
#     		WHEN 'VII REGION DEL MAULE' THEN 7
#     		WHEN 'VIII REGION DEL BIO BIO' THEN 8
#     		WHEN 'IX REGION DE LA ARAUCANIA' THEN 9
#     		WHEN 'X REGION LOS LAGOS' THEN 10
#     		WHEN 'XI REGION AYSEN DEL GENERAL CARLOS IBAÑEZ DEL CAMPO' THEN 11
#     		WHEN 'XII REGION DE MAGALLANES Y LA ANTARTICA CHILENA' THEN 12
#     		WHEN 'XIII REGION METROPOLITANA' THEN 13
#     		WHEN 'XIV REGION DE LOS RIOS' THEN 14
#     		WHEN 'XV REGION ARICA Y PARINACOTA' THEN 15
#     		WHEN 'XVI REGION DE ÑUBLE' THEN 16
#     		WHEN 'Sin Información' THEN 0
#     		ELSE 6 END) AS id_regionProv
#       FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC --TABLESAMPLE (1000 ROWS) REPEATABLE (123)
#       LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] as co ON co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
#       INNER JOIN [DCCPCotizacion].[dbo].[CotizacionProducto] as CP ON CO.Id = CP.CotizacionId
#       INNER JOIN [DCCPProcurement].[dbo].[prcGoodAndService] as GS ON CP.CodigoProducto = GS.ProductoCode 
#       INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O1 ON SC.CodigoEmpresa COLLATE Modern_Spanish_CI_AI = O1.orgCode COLLATE Modern_Spanish_CI_AI
#       INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O2 ON SC.CodigoOrganismo  COLLATE Modern_Spanish_CI_AI = O2.orgCode COLLATE Modern_Spanish_CI_AI
#       INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON CAST(LEFT(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-',''))
#             		,LEN(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-','')))-1) as VARCHAR(50))  = CAST(SII.RUT AS VARCHAR(50))
#       WHERE YEAR(SC.FechaPublicacion) = 2023
#       ) 
#       SELECT 
#         *,
#         CASE 
#             WHEN [id_regionProv] = [Región de Despacho] THEN 1
#             ELSE 0 
#         END AS [Proveedor Local 2]
#     FROM TABLA;
#                   ")
# 
# 
# #####################
# # Análisis ==========
# #####################
# #####################
# 
# 
# rubros3 <- datos %>% 
#   filter(Ganadora == 1) %>% 
#   group_by(`Rubro ONU 1`, `Proveedor Local Ley`) %>% count() %>% 
#   data.table::setDT() %>% 
#   data.table::dcast(`Rubro ONU 1`~`Proveedor Local Ley`, value.var = 'n') %>% 
#   mutate(n_var = round((((`Proveedor Local`/(`Proveedor Local`+`Otro Caso`))-1))*100,2),
#          n_oc = (`Proveedor Local`+`Otro Caso`)) %>%
#   arrange(desc(n_var)) %>% 
#   arrange(desc(n_oc)) 
# 
# (
# productos <- datos %>% 
#   filter(Ganadora == 1, `Rubro ONU 1` == rubros3$`Rubro ONU 1`[2]) %>% 
#   group_by(`Rubro ONU 1`,productoname, `Proveedor Local Ley`) %>% count() %>% 
#     data.table::setDT() %>% 
#     data.table::dcast(`productoname`~`Proveedor Local Ley`, value.var = 'n') %>% 
#     mutate(n_var = round((((`Proveedor Local`/(`Proveedor Local`+`Otro Caso`))-1))*100,2),
#            n_oc = (`Proveedor Local`+`Otro Caso`)) %>%
#     arrange(desc(n_var)) %>% 
#     arrange(desc(n_oc)) 
# )
# 
# (
#   productos_especificos <- datos %>%
#     filter(Ganadora == 1, `productoname` == productos$productoname[5]) %>% 
#     group_by(productoname,poiName, poiDescription, `Descripción Cotización`, `Proveedor Local Ley`) %>%
#     select(productoname
#            ,poiName
#            , poiDescription
#            , `Descripción Cotización`
#            , `Proveedor Local Ley`
#            , poiQuantity
#            , poiNetPrice
#            ,`Nombre Organismo Comprador`)  
#     
#   )
# 
# 
# datos %>% 
#   filter(productoname == "Clavo-tornillo") %>% View()
# 
# data %>% 
#   filter(`Rubro ONU` == "Equipamiento y suministros médicos") %>% 
#   View()
# 
# data %>% 
#   filter(DESCRIPCION_ACTIVIDAD_ECONOMICA == "VENTA AL POR MENOR DE OTROS PRODUCTOS EN COMERCIOS ESPECIALIZADOS N.C.P.") %>% 
#   View()
# 
# data %>% 
#   filter(DESCRIPCION_ACTIVIDAD_ECONOMICA == "INSTALACIONES DE GASFITERIA, CALEFACCION Y AIRE ACONDICIONADO") %>% 
#   group_by(`Nombre Organismo Comprador`) %>% 
#   count() %>% 
#   View()
# 
# data %>% 
#   group_by(`Actividad Proveedor`) %>% 
#   summarise(n_oc = n(), 
#             monto_transado = sum(porTotalAmount)/1000000
#             ,oc_promedio = median(porTotalAmount)/1000000
#   ) %>%  
#   arrange(desc(n_oc)) %>% View()
# 
# 
# data %>% 
#   filter(Rut_numero == "2706385") %>%
#   group_by(porCode) %>% 
#   summarise(rubros = n()) %>% 
#   View()
# 
# 
# data %>% 
#   filter(`Actividad Proveedor` == "ACTIVIDADES DE CONSULTORIA DE GESTION") %>% 
#   View()
# # 
# # 
# # start <- Sys.time()
# # data <- sqlQuery(con2, "
# #                  SELECT DISTINCT
# #                  
# #                  Subquery.porCode
# #                   ,Subquery.porDescription
# #                   ,Subquery.[porBuyerOrganization]
# #                   ,Subquery.entCode
# #                   ,Subquery.[Nombre Unidad de Compra]
# #                   ,Subquery.[Nombre Organismo Comprador]
# #                   ,Subquery.[Nombre dirección Unidad de Compra]
# #                   ,Subquery.[Dirección  Unidad de Compra]
# #                   ,Subquery.[Región  Unidad de Compra]
# #                   ,Subquery.[Comuna  Unidad de Compra]
# # 
# #                   ,PI.poiName
# #                   ,PI.poiDescription
# #                   ,O2.orgLegalName [Nombre Proveedor (sucursal)]
# #                   , E2.entName [Nombre Proveedor (matriz)]
# #                   --, (CASE WHEN C.citCode = C2.citCode THEN 1 ELSE 0 END) [Proveedor Local]
# #                   , EA2.eadName [Nombre dirección sucursal]
# #                   , EA2.eadAddress [Dirección  Sucursal]
# #                   , C2.citName [Región  Sucursal]
# #                   , D2.disName [Comuna  Sucursal]
# #                   /*, ST.pstDescription
# #                   ,[porShipmentType] -- Tipo de Despacho
# #                   ,[porInvoiceAddress] -- Dirección de facturación 
# #                   ,[porShipAddress] -- Dirección de despacho
# #                   ,[porShipInstructions]
# #                   ,[porHandlingInstructions]
# #                   ,[porSpecialInstructions]
# #                   ,[porDeliveryInstructions]*/
# #                  FROM(
# #                  
# #                   SELECT TOP 1000 
# #                   porCode
# #                   ,porDescription
# #                   ,[porBuyerOrganization]
# #                   ,E.entCode
# #                   ,O.orgLegalName [Nombre Unidad de Compra]
# #                   ,E.entName [Nombre Organismo Comprador]
# #                   , EA.eadName [Nombre dirección Unidad de Compra]
# #                   , EA.eadAddress [Dirección  Unidad de Compra]
# #                   , C.citName [Región  Unidad de Compra]
# #                   , D.disName [Comuna  Unidad de Compra]
# #                   FROM [DCCPProcurement].[dbo].[prcPOHeader] PO
# #                    INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] O ON PO.porBuyerOrganization = O.orgCode -- Identifica unidad de compra
# #                    INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] E ON O.orgEnterprise = E.entCode      -- Identifica organismo comprador           
# #                    INNER JOIN [DCCPPlatform].[dbo].[gblOrganizationAddress] OA ON O.orgCode = OA.oraOrganization
# #                    INNER JOIN [DCCPPlatform].[dbo].[gblEnterpriseAddress] EA ON OA.oraAddress = EA.eadCode
# #           				 INNER JOIN [DCCPPlatform].[dbo].[gblCity] C ON EA.eadCity = C.citCode
# #           				 INNER JOIN [DCCPPlatform].[dbo].[gblDistrict] D ON EA.eadDistrict = D.disCode
# #           				 INNER JOIN [DCCPPlatform].[dbo].[gblAddressType] AT ON OA.oraType = AT.atyCode
# #                   WHERE porIsIntegrated = 3
# #                       AND (YEAR([porSendDate]) = 2023 AND MONTH([porSendDate]) = 11) 
# #                       AND ([porBuyerStatus] IN (4, 5, 6, 7, 12))
# #           				    AND C.citCode = 16
# #                   ORDER BY NEWID() -- Asegura la selección aleatoria
# #                   
# #           				 ) AS Subquery
# #         				 INNER JOIN [DCCPProcurement].[dbo].[prcPOHeader] PO ON Subquery.[porCode] = PO.[porCode]
# #         				 INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] O2 ON PO.porSellerOrganization = O2.orgCode -- Identifica sucursal prov.
# #                  INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] E2 ON O2.orgEnterprise = E2.entCode -- Identifica Proveedor
# #         				 INNER JOIN [DCCPPlatform].[dbo].[gblOrganizationAddress] OA2 ON O2.orgCode = OA2.oraOrganization 
# #         				 INNER JOIN [DCCPPlatform].[dbo].[gblEnterpriseAddress] EA2 ON OA2.oraAddress = EA2.eadCode
# #         				 INNER JOIN [DCCPPlatform].[dbo].[gblCity] C2 ON EA2.eadCity = C2.citCode
# #         				 INNER JOIN [DCCPPlatform].[dbo].[gblDistrict] D2 ON EA2.eadDistrict = D2.disCode
# #                  INNER JOIN [DCCPProcurement].[dbo].[prcPOShipmentType] ST ON PO.porShipmentType = ST.pstCode 
# #         				 INNER JOIN [DCCPProcurement].[dbo].[prcPOItem] PI ON PO.[porID] = PI.poiOrder
# # 
# #                  ")
# # end <- Sys.time()
# # difftime(end, start, units = "mins")
# # 
# # 
# # data %>% 
# #   group_by(poiDescription) %>% 
# #   summarise(n_ = n()) %>%  
# #   order_by(desc(n_))
# # 
# # 
# # 
# # 
# # 
# # data <- sqlQuery(con2, "
# # SELECT A.*
# #   ,SII.CODIGO_ACTIVIDAD_ECONOMICA
# # ,SII.DESCRIPCION_ACTIVIDAD_ECONOMICA
# # ,(CASE 
# #   WHEN  A.[Región  Proveedor  (prcPOStaticRecipient)] = A.[Región  Unidad de Compra] THEN 1 
# #   ELSE 0 END) [Proveedor Local 1]
# # FROM (
# #   SELECT DISTINCT
# #   
# #   Subquery.*
# #     
# #     ,PO.porName
# #   --,PO.porDescription
# #   ,PO.porTotalAmount
# #   --,PI.poiName
# #   --,PI.poiDescription
# #   ,SR2.psrSellerOrganizationLegalName  [Nombre Proveedor (sucursal)]
# #   ,SR2.psrSellerActivity [Actividad Proveedor]
# #   ,[porSellerOrganization]
# #   ,LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')) [Rut Proveedor] 
# #   ,LEFT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-',''))
# #         ,LEN(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')))-1) [Rut_numero]
# #   ,RIGHT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')),1) [Rut_dv]
# #   ,SR2.psrSellerOrganizationLegalName  [Razón Social Proveedor]
# #   ,SR2.psrSellerActivity [Actividad del Proveedor  (prcPOStaticRecipient)] 
# #   ,SR2.psrSellerAddress [Dirección  Proveedor  (prcPOStaticRecipient)]
# #   ,SR2.psrSellerCity [Región  Proveedor  (prcPOStaticRecipient)]
# #   ,SR2.psrSellerDistrict [Comuna  Proveedor  (prcPOStaticRecipient)]
# #   
# #   
# #   
# #   
# #   
# #   /*
# #     , EA2.eadName [Nombre dirección sucursal]
# #   , EA2.eadAddress [Dirección  Sucursal]
# #   , C2.citName [Región  Sucursal]
# #   , D2.disName [Comuna  Sucursal]
# #   , ST.pstDescription
# #   ,[porShipmentType] -- Tipo de Despacho
# #   ,[porInvoiceAddress] -- Dirección de facturación 
# #   ,[porShipAddress] -- Dirección de despacho
# #   ,[porShipInstructions]
# #   ,[porHandlingInstructions]
# #   ,[porSpecialInstructions]
# #   ,[porDeliveryInstructions]
# #   
# #   */
# #     
# #     FROM (
# #       SELECT  DISTINCT --TOP 100000 
# #       PO.porCode
# #       ,PO.porDescription [Descripción orden de compra]
# #       ,SC.Descripcion [Descripción Cotización]
# #       ,PO.[porBuyerOrganization]
# #       ,LOWER(REPLACE(REPLACE(SR.psrBuyerTaxID,'.',''),'-','')) [Rut Comprador] 
# #       ,SR.psrBuyerOrganizationLegalName  [Nombre Organismo Comprador]
# #       ,SR.psrBuyerActivity [Actividad del Comprador] 
# #       ,SR.psrBuyerAddress [Dirección  Unidad de Compra]
# #       ,SR.psrBuyerCity [Región  Unidad de Compra]
# #       ,SR.psrBuyerDistrict [Comuna  Unidad de Compra]
# #       
# #       FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC
# #       LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] co on co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
# #       INNER JOIN [DCCPCotizacion].[dbo].[RelacionOC] as ROC ON ROC.IdSolicitudCotizacion = SC.Id
# #       INNER JOIN DCCPProcurement.dbo.prcPOHeader as PO ON PO.porID=ROC.porId
# #       INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR ON PO.porID =SR.psrOrder 
# #       INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR2 ON PO.porID =SR2.psrOrder
# #       WHERE porIsIntegrated = 3
# #       AND (YEAR([porSendDate]) = 2023 
# #            AND (psrBuyerActivity != 'UNIVERSIDADES')
# #            AND (SC.idEstado>=2)
# #            --AND MONTH([porSendDate]) = 11
# #       ) 
# #       AND ([porBuyerStatus] IN (4, 5, 6, 7, 12))
# #       AND SR.psrBuyerCity = 16
# #       -- ORDER BY NEWID() -- Asegura la selección aleatoria
# #     ) AS Subquery
# #   INNER JOIN [DCCPProcurement].[dbo].[prcPOHeader] PO ON Subquery.[porCode] = PO.[porCode]
# #   INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR2 ON PO.porID =SR2.psrOrder
# #   
# #   /* 
# #     INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] O2 ON PO.porSellerOrganization = O2.orgCode -- Identifica sucursal prov.
# #   INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] E2 ON O2.orgEnterprise = E2.entCode -- Identifica Proveedor
# #   INNER JOIN [DCCPPlatform].[dbo].[gblOrganizationAddress] OA2 ON O2.orgCode = OA2.oraOrganization 
# #   INNER JOIN [DCCPPlatform].[dbo].[gblEnterpriseAddress] EA2 ON OA2.oraAddress = EA2.eadCode
# #   INNER JOIN [DCCPPlatform].[dbo].[gblCity] C2 ON EA2.eadCity = C2.citCode
# #   INNER JOIN [DCCPPlatform].[dbo].[gblDistrict] D2 ON EA2.eadDistrict = D2.disCode
# #   
# #   INNER JOIN [DCCPProcurement].[dbo].[prcPOShipmentType] ST ON PO.porShipmentType = ST.pstCode 
# #   INNER JOIN [DCCPProcurement].[dbo].[prcPOItem] PI ON PO.[porID] = PI.poiOrder -- Para analizar por tipo de producto 
# #   */
# # ) A
# # INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON A.Rut_numero = SII.RUT -- De aquí voy a sacar el tamaño
# # 
# # 
# # 
# # ")
#
