#Quita archivos del WorkSpace ===========================================================

rm(list = ls())

#Fija el directorio de trabajo ==========================================================

wd_path = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/datos/"

data_path = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/datos"

setwd(wd_path)

#Carga de paquetes necesarios para el análisis ==========================================

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
             , "openxlsx"
             , "shiny"
             , "ggplot2"
             , "DT"
             , "shinyjs"
             , "sqldf")

load_pkg(packages)

consultar_y_guardar <- function(x,y, window = -11
                                ,wd_path = data_path
                                ,tipoConsulta = "cotizaciones compra ágil"
                                ,depurar = TRUE
                                ,updated = TRUE) {
  
  detalles = function(path = wd_path, pattern = "*.rds"){
    require(dplyr)
    
    details = file.info(path = paste0(wd_path), list.files(pattern=pattern))
    
    details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ] %>% 
      filter(isdir==FALSE)
    
    details$files = rownames(details)
    
    rownames(details) = NULL
    
    return(details)
  }
  
  datasets <- detalles(path = wd_path)
  
  
  
  if (!updated){
    
    con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles
    
    con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse
    
    if (length(years) == 0 | !tipoConsulta %in% c('cotizaciones compra ágil'
                                                  , 'órdenes de compra'
                                                  ,'usuarios proveedores')) {
      mensaje <- "Parámetros inválidos. Asegúrate de proporcionar años y/o tipo de consulta válidos."
      return(mensaje)
    }
    
    
    require(lubridate)
    
    # Es importante notar que la función switch es sensible al uso del operador de asignación
    # porque al usar <- en lugar de = arroja un error. OJO 
    
    ejecutarConsulta <- switch (tipoConsulta,
                                'cotizaciones compra ágil' = function(x = x, y = y, window = window) {
                                  sqlQuery(con2, sprintf(
                                    "
		  DECLARE @MONTH AS INT;
      DECLARE @YEAR AS INT;
                  
      SET @MONTH = %s;
      SET @YEAR = %s;
                  
      DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
      DECLARE @startDate datetime = dateadd(month,%s, @currentMonth)
      , @endDate datetime = dateadd(month, 1, @currentMonth);
		---------------------------------------------------------------------------------------------------------  
     WITH TABLA AS (SELECT 
	     SC.CodigoSolicitudCotizacion
		  ,YEAR(SC.FechaPublicacion) [year]
		  ,MONTH(SC.FechaPublicacion) [month]
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
		  ,O2.orgLegalName [Razón Social Unidad de Compra]
		  ,E2.entName [Razón Social Comprador]
		  ,O2.orgActivity
		  ,O2.orgParentOrganization
		  ,O2.orgTaxID
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
		---------------------------------------------------------------------------------------------------------
		FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC --TABLESAMPLE (1000 ROWS) REPEATABLE (123)
	  LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] as co ON co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
	  INNER JOIN [DCCPCotizacion].[dbo].[CotizacionProducto] as CP ON CO.Id = CP.CotizacionId
	  INNER JOIN [DCCPProcurement].[dbo].[prcGoodAndService] as GS ON CP.CodigoProducto = GS.ProductoCode 
	  INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O1 ON CO.CodigoSucursalEmpresa COLLATE Modern_Spanish_CI_AI = O1.orgCode COLLATE Modern_Spanish_CI_AI
	  INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] as E ON CO.CodigoEmpresa = E.entCode COLLATE Modern_Spanish_CI_AI
	  INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O2 ON SC.CodigoOrganismo  COLLATE Modern_Spanish_CI_AI = O2.orgCode COLLATE Modern_Spanish_CI_AI
	  INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] as E2 ON O2.orgEnterprise = E2.entCode
	  INNER JOIN [Estudios].[dbo].[sii_atrib_2021] as SII ON CAST(LEFT(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-',''))
        			,LEN(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-','')))-1) as VARCHAR(50)) = CAST(SII.RUT AS VARCHAR(50))  
	  INNER JOIN [Estudios].[dbo].[THTamanoProveedor] as TP ON TP.entcode = E.entCode AND TP.[AñoTributario]=2022 
 		---------------------------------------------------------------------------------------------------------
	  WHERE (SC.FechaPublicacion < @endDate) AND
          (SC.FechaPublicacion >= @startDate)
	  ) 
	  SELECT 
		TABLA.*,
		(CASE 
			WHEN [id_regionProv] = [Región de Despacho] AND Size = 'MiPyme' THEN 'Local' 
  				WHEN [id_regionProv] != [Región de Despacho] AND Size = 'MiPyme' THEN 'No Local'
			WHEN size = 'Sin categoría' THEN 'sin categoría'
			ELSE 'Grande' END) AS [Proveedor Local 2]
	FROM TABLA;
  ",x,y, window)
                                  ) 
                                },
	'órdenes de compra' = function(x = x, y = y, window = window) {
	  sqlQuery(con3,sprintf( 
	    "
    DECLARE @MONTH AS INT;
    DECLARE @YEAR AS INT;
                
    SET @MONTH = %s;
    SET @YEAR = %s;
                
    DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
    DECLARE @startDate datetime = dateadd(month,%s, @currentMonth)
    , @endDate datetime = dateadd(month, 1, @currentMonth);
      
      
    SELECT DISTINCT 
	  OL.[poiID]
    ,OL.[porID]
    ,OL.[CodigoOC]
	  ,LC.NumeroAdq
	  ,LC.Link
    ,T.Year
    ,T.Month
	  ,OL.[NombreItem] 
	  ,PR.NombreProducto 
	  ,RU.RubroN1
	  ,RU.RubroN2
	  ,RU.RubroN3
    ,OL.[DescripcionItem]
    ,DU.uomName [Unidad de Medida]
    ,OL.[CantidadItem]
    ,OL.[MonedaOC]
    ,OL.[Monto]
    ,OL.[MontoUSD]
    ,OL.[MontoCLP]
    ,OL.[MontoCLF]
    ,OL.[MontoUTM]
	  ,P.RazonSocialSucursal [Nombre Proveedor]
	  ,P.RUTSucursal [Rut Proveedor]
	  ,P.ActividadSucursal[Actividad Proveedor]
	  ,E.NombreEmpresa 
	  ,DT.Tamano
	  ,C.RazonSocialUnidaddeCompra [Unidad de Compra]
	  ,C.RUTUnidaddeCompra [Rut Unidad de Compra]
	  ,C.ActividadUnidaddeCompra [Actividad Unidad de Compra]
	  ,I.NombreInstitucion [Nombre Institución]
	  ,S.Sector [Sector Institución]
	  ,(CASE OC.porIsIntegrated
		WHEN 3 THEN 'Compra Ágil'
		ELSE (
			CASE  OC.IDProcedenciaOC
			WHEN 703 THEN 'Convenio Marco'
			WHEN 701 THEN 'Licitación Pública'
			WHEN 1401 THEN 'Licitación Pública'
			WHEN 702 THEN 'Licitación Privada'
			ELSE 'Trato Directo'
			END)
		END) 'PROCEDENCIA'
  FROM [DM_Transaccional].[dbo].[THOrdenesCompraLinea] as OL 
  INNER JOIN [DM_Transaccional].[dbo].[THOrdenesCompra] as OC ON OL.CodigoOC=OC.CodigoOC 
  INNER JOIN [DM_Transaccional].[dbo].[THOportunidadesNegocio] as LC ON OC.rbhCode = LC.rbhCode
  INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
  INNER JOIN [DM_Transaccional].[dbo].[DimProducto] as PR ON OL.IDProducto=PR.IDProducto
  INNER JOIN [DM_Transaccional].[dbo].[DimRubro] as RU ON PR.IdRubro = RU.IdRubro
  INNER JOIN [DM_Transaccional].[dbo].[DimProveedor] as P ON OC.IDSucursal=P.IDSucursal 
  INNER JOIN [DM_Transaccional].[dbo].[DimComprador] as C ON OC.IDUnidaddeCompra = C.IDUnidaddeCompra
  INNER JOIN [DM_Transaccional].[dbo].[DimEmpresa] as E ON P.entCode = E.entCode 
  INNER JOIN [DM_Transaccional].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
  INNER JOIN [DM_Transaccional].[dbo].[DimSector] as S ON I.IdSector = S.IdSector
  INNER JOIN [DM_Transaccional].[dbo].[THTamanoProveedor] as TP ON TP.entcode = E.entCode AND TP.[AñoTributario]=2022 
  INNER JOIN [DM_Transaccional].[dbo].[DimTamanoProveedor] as DT ON TP.idTamano = DT.IdTamano
  INNER JOIN [DM_Transaccional].[dbo].[DimUOM] as DU ON OL.UnidaddeMedida = DU.uomCode
  WHERE (T.Date < @endDate) 
  AND (T.Date >= @startDate) 
  AND OC.IDEstadoOC >= 5
      ",x,y, window))
	  
	},
	'usuarios proveedores' = function(x = x, y = y, window = window) {
	  query <- paste("DECLARE @MONTH AS INT;
      DECLARE @YEAR AS INT;
                  
      SET @MONTH = ",x,";
      SET @YEAR = ",y,";
                  
      DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
      DECLARE @startDate datetime = dateadd(month,",window,", @currentMonth)
      , @endDate datetime = dateadd(month, 1, @currentMonth);

                SELECT DISTINCT
                 CAST(
					CASE 
						WHEN LEN(LOWER(REPLACE(REPLACE(O.orgTaxID,'.',''),'-',''))) > 1 
						THEN LEFT(LOWER(REPLACE(REPLACE(O.orgTaxID,'.',''),'-',''))
						, LEN(LOWER(REPLACE(REPLACE(O.orgTaxID,'.',''),'-',''))) - 1)
						ELSE 'Sin rut: ' + O.orgTaxID 
					END as VARCHAR(50)) [Rut Proveedor]
                ,O.orgEnterprise [EntCode]
                ,UPPER(E.entname) [Razon social]
                , U.usrLastLogin
			    , MONTH(U.usrLastLogin) [Mes]
                , YEAR(U.usrLastLogin) [Anio]
				,U.usrEmail
				,U.usrFirstName+' '+U.usrLastName [Nombre Usuario]
				,U.usrTaxID
				,U.usrMobile
				,U.usrPhone
				,U.usrPosition
				, @MONTH [Mes Central]
				, @YEAR [Anio Central]
				, @startDate [Comienzo]
				, @endDate [Final]
                
                FROM  [DCCPPlatform].[dbo].[gblSecUserRole] as UR 
                INNER JOIN  [DCCPPlatform].[dbo].gblOrganization as O ON UR.uroOrganization      = O.orgCode
                INNER JOIN  [DCCPPlatform].[dbo].GblUser as U ON UR.uroUser              = U.usrCode
                LEFT JOIN   [DCCPPlatform].[dbo].gblEnterprise  as E  ON O.orgEnterprise         = E.entcode
                WHERE  U.usrIsActive       = 1
                AND O.orgIsActive   = 1
                AND E.entIsActive   = 1
                and o.orgistest = 0
                AND (U.usrEmail NOT LIKE '%ontraloria.cl' OR (U.usrEmail LIKE '%ontraloria.cl' AND E.entCode = '7231'))  -- 7231 Codigo de la Contraloria
                AND (U.usrEmail NOT LIKE '%ontroloria.cl' OR (U.usrEmail LIKE '%ontroloria.cl' AND E.entCode = '7231'))
                AND E.entName NOT IN ('MERCADOPUBLICOTEST','MPCOMPRADORTEST_SKY','MPCOMPRADORTEST_SKY2','DCCP-OPERACIONES-PRUEBA COMPRADOR') -- Usuarios de Prueba
                AND U.usrPosition != ''             -- No consideramos contactos sin usrPosition
                AND U.usrEmail  != ''        -- No consideramos contactos sin mail
                --AND YEAR(U.usrLastLogin) = @YEAR
                AND U.usrLastLogin <= @endDate AND U.usrLastLogin >= @startDate
                AND O.orgClass = 1
                AND o.orgtaxid not in ('0-0','0.000.000-0','1-9','A.t21-125','yyyyyyyyyy')")
	  
	  sqlQuery(con2, query)
	}
    )
    
    descargar_guardar <- function(x, y, window) {
      
      data <- ejecutarConsulta(x = x, y = y, window = window) 
      
      grupo1 = c('ofertan'
                 , 'adjudican'
                 ,'login'
                 ,'inscritos')
      
      if (depurar){
        if (tipoConsulta%in%grupo1){
          data <- data %>% 
            mutate(sello = ifelse(`Sello Mujer`=="Mujeres",1,0)) %>% 
            arrange(desc(sello)) %>% 
            filter(!duplicated(EntCode)) %>% 
            select(-sello)
        } else {
          data <- data %>% 
            mutate(sello = ifelse(`Sello Mujer`=="Mujeres",1,0)
                   ,codigo = paste0(Organismo,EntCode)) %>% 
            group_by(Organismo) %>% 
            arrange(desc(sello)) %>% 
            filter(!duplicated(codigo)) %>% 
            select(-sello)
        }
      }
      
      
      return(data)
    }
    
    total <- data.table::data.table()
    
    for (year in y) {
      if (year == year(today())) {
        start <- Sys.time()
        x <- month(today()) 
        y <- year
        window<- -(x-1)
        
        data <- descargar_guardar(x, y, window)
        total <- rbind(total, data)
        
        end <- Sys.time()
        
        tiempo_transcurrido <- difftime(end, start, units = "mins")
        
        cat("Descarga para el año", year, "completada en", round(tiempo_transcurrido,1), "minutos.", "\n")
        
      } else {
        start <- Sys.time()
        y <- year
        
        data <- descargar_guardar(x, y, window)
        total <- rbind(total, data)
        
        end <- Sys.time()
        tiempo_transcurrido <- difftime(end, start, units = "mins")
        
        cat("Descarga para el año", year, "completada en", round(tiempo_transcurrido,1), "minutos.", "\n")
      }
    }
    
    # Guarda el objeto data en un archivo con nombre diferente según el tipo de consulta
    saveRDS(total, file = paste0(gsub("-", "", today()),gsub(" ", "_", tipoConsulta), ".rds"))
  } else {
    total <- readRDS(file = file.path(wd_path, datasets[grep(gsub(" ","_", tipoConsulta), datasets$files)[1],c("files")]))  
  }
  
  return(total)
  
}



detalles = function(path = wd_path, pattern = "*.rds"){
  require(dplyr)
  
  details = file.info(path = paste0(wd_path), list.files(pattern=pattern))
  
  details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ] %>% 
    filter(isdir==FALSE)
  
  details$files = rownames(details)
  
  rownames(details) = NULL
  
  return(details)
}

datasets <- detalles(path = wd_path)

tipoConsulta <- 'órdenes de compra'

file.path(data_path, datasets[grep(gsub(" ","_", tipoConsulta), datasets$files)[1],c("files")])


years <- c(2023, 2024)

datos <- consultar_y_guardar(wd_path = data_path
                             , x = 12
                             , y = years
                             , tipoConsulta = "cotizaciones compra ágil"
                             , depurar = FALSE
                             , updated = TRUE)



data_ag <- datos %>% 
  #filter(year == 2023) %>% 
  group_by(year,level1, `Región de Despacho`,`Proveedor Local 2`) %>%
  #distinct(CodigoSolicitudCotizacion, .keep_all = TRUE) %>% 
  summarise(solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,ofertas = n_distinct(CodigoCotizacion)
            ,proveedores = n_distinct(entCode)
            ,MontoTotalProducto = sum(MontoTotalProducto*EsProveedorSeleccionado, na.rm = TRUE)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1+`Región de Despacho`~`Proveedor Local 2`
                    , value.var = c("solicitudes", "ofertas", "proveedores", "MontoTotalProducto")) %>% 
  mutate(interseccion = ifelse(`solicitudes_Local`>`solicitudes_No Local`
                               & ofertas_Local >`ofertas_No Local`
                               & proveedores_Local>`proveedores_No Local`,"Sí","No")
         ,union = ifelse(`solicitudes_Local`>`solicitudes_No Local`
                        | ofertas_Local >`ofertas_No Local`
                        | proveedores_Local>`proveedores_No Local`, "Sí", "No"))  


data_ag_productos <- datos %>% 
  #filter(year == 2023) %>% 
  group_by(year,level1, `Región de Despacho`,`Proveedor Local 2`, `productoname`) %>%
  #distinct(CodigoSolicitudCotizacion, .keep_all = TRUE) %>% 
  summarise(solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,ofertas = n_distinct(CodigoCotizacion)
            ,proveedores = n_distinct(entCode)
            ,MontoTotalProducto = sum(MontoTotalProducto*EsProveedorSeleccionado, na.rm = TRUE)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1+`Región de Despacho`+productoname~`Proveedor Local 2`
                    , value.var = c("solicitudes", "ofertas", "proveedores", "MontoTotalProducto")) %>% 
  mutate(interseccion = ifelse(`solicitudes_Local`>`solicitudes_No Local`
                               & ofertas_Local >`ofertas_No Local`
                               & proveedores_Local>`proveedores_No Local`,"Sí","No")
         ,union = ifelse(`solicitudes_Local`>`solicitudes_No Local`
                         | ofertas_Local >`ofertas_No Local`
                         | proveedores_Local>`proveedores_No Local`, "Sí", "No"))  

tabla_y <- data_ag %>% 
  group_by(year,level1, interseccion) %>% 
  filter(year == 2023) %>% 
  summarise(regiones = n()
            ,monto = sum(MontoTotalProducto_Grande+MontoTotalProducto_Local+`MontoTotalProducto_No Local`)) %>% 
  arrange(desc(interseccion)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1~interseccion, value.var = c("regiones", "monto"))

tabla_o <- data_ag %>% 
  group_by(year,level1, union) %>% 
  filter(year == 2023) %>% 
  summarise(regiones = n()
            ,monto = sum(MontoTotalProducto_Grande+MontoTotalProducto_Local+`MontoTotalProducto_No Local`)) %>% 
  #arrange(desc(interseccion)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1~union, value.var = c("regiones", "monto"))

segun_rg <- datos %>% 
  group_by(year,level1, `Región de Despacho`,id_regionProv) %>%
  #distinct(CodigoSolicitudCotizacion, .keep_all = TRUE) %>% 
  summarise(solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,ofertas = n_distinct(CodigoCotizacion)
            ,proveedores = n_distinct(entCode)
            ,MontoTotalProducto = sum(MontoTotalProducto, na.rm = TRUE)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1+`Región de Despacho`~`id_regionProv`
                    , value.var = c("solicitudes", "ofertas", "proveedores", "MontoTotalProducto")) 

wb <- createWorkbook()

addWorksheet(wb, sheetName = 'Proveedor Local - Rubro')

writeData(wb, sheet = 'Proveedor Local - Rubro', x = data_ag)

addWorksheet(wb, sheetName = 'Proveedor Local - Producto')

writeData(wb, sheet = 'Proveedor Local - Producto', x = data_ag_productos)

addWorksheet(wb, sheetName = 'Tabla (Y)')

writeData(wb, sheet = 'Tabla (Y)', x = tabla_y)

addWorksheet(wb, sheetName = 'Tabla (O)')

writeData(wb, sheet = "Tabla (O)", x = tabla_o)

addWorksheet(wb, sheetName = 'Proveedores inter - Rubro')

writeData(wb, sheet = "Proveedores inter - Rubro", x = segun_rg)

saveWorkbook(wb, "20240308 estadísticas proveedores.xlsx", overwrite = TRUE)


wb <- createWorkbook()

addWorksheet(wb, sheetName = 'Proveedor Local - producto')

writeData(wb, sheet = 'Proveedor Local - producto', x = data_ag)

saveWorkbook(wb, "20240307 estadísticas proveedores.xlsx", overwrite = TRUE)

# library(dplyr)
# 
# # Crear un data frame de ejemplo
# df <- data.frame(
#   var_1_column = c(1, 2, 3),
#   column_var_2 = c(4, 5, 6),
#   column_3_var = c(7, 8, 9)
# )
# 
# # Operación dependiendo del nombre de la columna
# df <- df %>%
#   mutate(across(
#     everything(),
#     ~ case_when(
#       grepl("var_1", names(df)) ~ . * 2,       # Duplicar los valores de las columnas que contienen "var_1" en su nombre
#       grepl("var_2", names(df)) ~ . + 10,      # Sumar 10 a los valores de las columnas que contienen "var_2" en su nombre
#       grepl("var_3", names(df)) ~ . - 5,       # Restar 5 a los valores de las columnas que contienen "var_3" en su nombre
#       TRUE ~ .                                  # Si ninguna condición coincide, mantener el valor original
#     )
#   ))
# 
# print(df)

