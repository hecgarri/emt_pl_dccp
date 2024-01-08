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
             , "RColorBrewer")

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

#
# Tablas involucradas en el análisis ============================================
#
#
gblEnterpriseAddress <- sqlQuery(con2, "SELECT TOP (1000) [eadCode]
      ,[eadIsActive]
      ,[eadEnterprise]
      ,[eadName]
      ,[eadAddress]
      ,[eadCountry]
      ,[eadCity]
      ,[eadCityOther]
      ,[eadDistrict]
      ,[eadDistrictOther]
      ,[eadPostalCode]
      ,[eadPhone]
      ,[eadFax]
FROM [DCCPPlatform].[dbo].[gblEnterpriseAddress]")

gblOrganizationAddress <- sqlQuery(con2, "SELECT TOP (1000) [oraID]
      ,[oraIsActive]
      ,[oraOrganization]
      ,[oraIsDefault]
      ,[oraType]
      ,[oraAddress]
  FROM [DCCPPlatform].[dbo].[gblOrganizationAddress]")

gblEnterprise <- sqlQuery(con2, "SELECT TOP (1000) [entID]
      ,[entIsActive]
      ,[entCode]
      ,[entIsTest]
      ,[entName]
      ,[entZoneId]
      ,[entSubZoneId]
      ,[entSinonimos]
      ,[NumLC]
      ,[NumOC]
      ,[entIsFosis]
      ,[IsPMG]
      ,[entInformar]
      ,[entOrganizationDefault]
      ,[entIsSIGFE]
      ,[entScore]
      ,[entCalifications]
  FROM [DCCPPlatform].[dbo].[gblEnterprise]")


gblOrganization <- sqlQuery(con2, "SELECT TOP (1000) [orgID]
      ,[orgIsActive]
      ,[orgCode]
      ,[orgIsTest]
      ,[orgEnterprise]
      ,[orgParentOrganization]
      ,[orgClass]
      ,[orgType]
      ,[orgSubType]
      ,[orgName]
      ,[orgLegalName]
      ,[orgTaxID]
      ,[orgMarketplaceID]
      ,[orgActivity]
      ,[orgUrl]
      ,[orgUserDefined1]
      ,[orgUserDefined2]
      ,[orgUserDefined3]
      ,[orgInformation]
      ,[orgCreationDate]
      ,[isAuditCGR]
  FROM [DCCPPlatform].[dbo].[gblOrganization]
")

gblCity <- sqlQuery(con2, "SELECT TOP (1000) [citID]
      ,[citIsActive]
      ,[citCountry]
      ,[citCode]
      ,[citName]
      ,[citOrder]
  FROM [DCCPPlatform].[dbo].[gblCity]")

gblDistrict <- sqlQuery(con2, "SELECT TOP (1000) [disID]
      ,[disIsActive]
      ,[disCountry]
      ,[disCity]
      ,[disCode]
      ,[disName]
      ,[disIdSII]
  FROM [DCCPPlatform].[dbo].[gblDistrict]")


prcPOHeader <- sqlQuery(con2, "SELECT TOP (10000) [porID]
      ,[porRequisition]
      ,[porSourceDocumentType]
      ,[porSourceDocumentNumber]
      ,[porIsEnabled]
      ,[porCreationDate]
      ,[porSendDate]
      ,[porIsIntegrated]
      ,[porMkpDocumentReference]
      ,[porMkpBuyerReference]
      ,[porMkpUserReference]
      ,[porMkpSellerReference]
      ,[porLanguage]
      ,[porSchema]
      ,[porCategory]
      ,[porCode]
      ,[porName]
      ,[porDescription]
      ,[porOrderType]
      ,[porOrderPurpose]
      ,[porJustifyType]
      ,[porCurrency]
      ,[porAccount]
      ,[porBuyerOrganization]
      ,[porBuyerUser]
      ,[porBuyerReference]
      ,[porBuyerStatus]
      ,[porBuyerAlternateStatus]
      ,[porBuyerStatusDate]
      ,[porBuyerNotes]
      ,[porSellerOrganization]
      ,[porSellerUser]
      ,[porSellerReference]
      ,[porSellerStatus]
      ,[porSellerAlternateStatus]
      ,[porSellerStatusDate]
      ,[porSellerNotes]
      ,[porPaymentType]
      ,[porPaymentMethod]
      ,[porShipmentType] -- Tipo de Despacho
      ,[porInvoiceAddress] -- Dirección de facturación 
      ,[porShipAddress] -- Dirección de despacho 
      ,[porShipInstructions]
      ,[porHandlingInstructions]
      ,[porSpecialInstructions]
      ,[porDeliveryInstructions]
      ,[porTotalCharges]
      ,[porTotalDiscounts]
      ,[porTotalTaxes]
      ,[porTotalAmount]
      ,[porDiscountsComments]
      ,[porChargesComments]
      ,[porPercentTaxes]
      ,[porTaxCode]
      ,[porUserDefined1]
      ,[porUserDefined2]
      ,[porUserDefined3]
      ,[porCancelDate]
      ,[porDateDelivery]
      ,[porBuyerCity]
      ,[porDateModification]
      ,[porBuyerEnterprise]
      ,[porSellerEnterprise]
      ,[presupuestoCode]
      ,[porJustificacionFormaPago]
      ,[porResponsibleName]
      ,[porResponsiblePhone]
      ,[porResponsibleEmail]
      ,[porEsObraPublica]
      ,[porEsGeoRefPorLic]
      ,[porExternalIdentity]
      ,[porEmisionAutomatica]
  FROM [DCCPProcurement].[dbo].[prcPOHeader]")


prcRFBHeader <- sqlQuery(con2, "SELECT TOP (1000) [rbhID]
      ,[rbhCode]
      ,[rbhIsTemplate]
      ,[rbhRequisition]
      ,[rbhCreationDate]
      ,[rbhDocumentStatus]
      ,[rbhTechnicalStatus]
      ,[rbhEconomicStatus]
      ,[rbhEnterprise]
      ,[rbhOrganization]
      ,[rbhUser]
      ,[rbhTemplate]
      ,[rbhExternalCode]
      ,[rbhName]
      ,[rbhDescription]
      ,[rbhDocumentType]
      ,[rbhDocumentSubType]
      ,[rbhProcessType]
      ,[rbhProcessSubType]
      ,[rbhAwardType]
      ,[rbhJustifyType]
      ,[rbhSteps]
      ,[rbhOwnerOrganizationCode]
      ,[rbhOwnerName]
      ,[rbhOwnerUnit]
      ,[rbhOwnerTaxID]
      ,[rbhOwnerAddress]
      ,[rbhOwnerCountry]
      ,[rbhOwnerCity]
      ,[rbhOwnerDistrict]
      ,[rbhContactCode]
      ,[rbhContactFirstName]
      ,[rbhContactLastName]
      ,[rbhContactPosition]
      ,[rbhContactEmail]
      ,[rbhContactPhone]
      ,[rbhContactFax]
      ,[rbhCurrency]
      ,[rbhIsUniqueCurrency]
      ,[rbhEstimatedAmount]
      ,[rbhEstimatedCurrency]
      ,[rbhContractObservations]
      ,[rbhContractDuration]
      ,[rbhContractTime]
      ,[rbhContractTimePeriod]
      ,[rbhContractPaymentMethod]
      ,[rbhOutsourcingAllowed]
      ,[rbhOutsourcingComments]
      ,[rbhLegalRecordComments]
      ,[rbhCallType]
      ,[rbhEstimationType]
      ,[rbhEstimatedAwardAmount]
      ,[rbhEstimatedAwardCurrency]
      ,[rbhObservationCheckText]
      ,[rbhObservationComment]
      ,[rbhStepsStatus]
      ,[rbhAprovTR]
      ,[rbhCommentTR]
      ,[rbhUserTR]
      ,[rbhTotalAward]
      ,[rbhSourceOfFunding]
      ,[rbhInformation]
      ,[rbhOptionPayment]
      ,[rbhContractManagerName]
      ,[rbhContractManagerEmail]
      ,[rbhContractManagerPhone]
      ,[rbhStatusCS]
      ,[rbhStatusDocTec]
      ,[rbhJustifyDocTecnico]
      ,[rbhContractManagerNamePayment]
      ,[rbhContractManagerEmailPayment]
      ,[rbhRevokeJustify]
      ,[Contrato]
      ,[rbhEstimatedAmountPublic]
      ,[rbhContractType]
      ,[rbhFundamentoMontoEstimado]
      ,[rbhRealizoAnalisisDePrecios]
      ,[rbhExtiendePlazo]
      ,[rbhExtensionProcesada]
      ,[rbhLicitacionBaseTipo]
      ,[rbhSubjectRecruitment]
      ,[rbhEstimatedIndicator]
      ,[rbhWarranty]
      ,[rbhEstimatedAwardAmountJustification]
      ,[rbhCondicionSeriedadOferta]
      ,[rbhCondicionFielCumplimiento]
      ,[rbhJustificacionExtiendePlazo]
      ,[rbhOpcionReadjudicacion]
      ,[rbhJustificacionReadjudicacion]
      ,[rbhOptionContratoRenovable]
      ,[rbhJustificacionContratoRenovable]
      ,[rbhReglamentoActivo]
      ,[rbhEsOfertaCiega]
      ,[rbhEsArquitecturaUrbanismo]
  FROM [DCCPProcurement].[dbo].[prcRFBHeader]")


SolicitudCotizacion <- sqlQuery(con2, "SELECT TOP (1000) SC.[Id]
      ,[CodigoSolicitudCotizacion]
      ,[CodigoOrganismo]
      ,[Nombre]
      ,[Descripcion]
      ,[FechaApertura]
      ,[FechaCierre]
      ,[FechaCreacion]
      ,[FechaSeleccion]
      ,[NombreContacto]
      ,[TelefonoContacto]
      ,[EmailContacto]
      ,[OtrosAntecedentes]
      ,[idEstado]
      ,[FechaPublicacion]
      ,[MontoDisponible]
      ,[CodigoEmpresa]
      ,[CodigoRegion] -- Prioridad 
      ,[CodigoComuna]
      ,[Direccion] -- Segunda prioridad 
      ,[PlazoEntrega]
      ,[FiltroTamanoEmpresa]
      ,[FiltroRegionNotificacion]
      ,[CodigoRegionFiltro]
      ,[Moneda]
      ,[EsMultaSancion]
      ,[Invitaciones]
      ,HEAD.porCode
      FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC
      inner join [DCCPCotizacion].[dbo].[RelacionOC] as ROC ON ROC.IdSolicitudCotizacion = SC.Id
      inner join DCCPProcurement.dbo.prcPOHeader as HEAD ON HEAD.porID=ROC.porId
      WHERE YEAR(FechaPublicacion) = 2023")



prcPOStaticRecipient <- sqlQuery(con2, "SELECT TOP (1000) [psrID]
                        ,[psrOrder]
                        ,[psrBuyerOrganizationName]
                        ,[psrBuyerOrganizationLegalName]
                        ,[psrBuyerTaxID]
                        ,[psrBuyerActivity]
                        ,[psrBuyerAddress]
                        ,[psrBuyerAddressComplement]
                        ,[psrBuyerCountry]
                        ,[psrBuyerCity]
                        ,[psrBuyerDistrict]
                        ,[psrBuyerPhone]
                        ,[psrBuyerFax]
                        ,[psrBuyerContactFirstName]
                        ,[psrBuyerContactLastName]
                        ,[psrBuyerContactPosition]
                        ,[psrBuyerContactEmail]
                        ,[psrBuyerContactPhone]
                        ,[psrBuyerContactMobile]
                        ,[psrBuyerContactFax]
                        ,[psrSellerOrganizationName]
                        ,[psrSellerOrganizationLegalName]
                        ,[psrSellerTaxID]
                        ,[psrSellerActivity]
                        ,[psrSellerAddress]
                        ,[psrSellerAddressComplement]
                        ,[psrSellerCountry]
                        ,[psrSellerCity]
                        ,[psrSellerDistrict]
                        ,[psrSellerPhone]
                        ,[psrSellerFax]
                        ,[psrSellerContactFirstName]
                        ,[psrSellerContactLastName]
                        ,[psrSellerContactPosition]
                        ,[psrSellerContactEmail]
                        ,[psrSellerContactPhone]
                        ,[psrSellerContactMobile]
                        ,[psrSellerContactFax]
                        FROM [DCCPProcurement].[dbo].[prcPOStaticRecipient]"
)

cotizacion <- sqlQuery(con2, "SELECT TOP (1000) [Id]
      ,[SolicitudCotizacionId]
      ,[CodigoEmpresa]
      ,[CodigoSucursalEmpresa]
      ,[FechaCreacion]
      ,[FechaModificacion]
      ,[EstadoId]
      ,[Activo]
      ,[Moneda]
      ,[EsProveedorSeleccionado]
      ,[Descripcion]
      ,[MontoTotal]
      ,[CodigoUsuario]
      ,[MontoNeto]
      ,[TotalImpuesto]
      ,[PorcentajeImpuesto]
      ,[CodigoImpuesto]
      ,[FechaVigencia]
      ,[DeclaracionJurada]
      ,[MontoDespacho]
      ,[NoSeleccionable]
      ,[EsOfertaParcial]
  FROM [DCCPCotizacion].[dbo].[Cotizacion]")

prcPOItem <- sqlQuery(con2, "
          SELECT TOP (1000) [poiID]
            ,[poiOrder]
            ,[poiCorrelative]
            ,[poiBuyerLineItem]
            ,[poiBuyerPartCode]
            ,[poiSellerPartCode]
            ,[poiMfrPartCode]
            ,[poiGoodAndService]
            ,[poiSchema]
            ,[poiCategory]
            ,[poiName]
            ,[poiDescription]
            ,[poiUOM]
            ,[poiQuantity]
            ,[poiCurrency]
            ,[poiNetPrice]
            ,[poiIsTaxable]
            ,[poiTotalCharges]
            ,[poiTotalDiscounts]
            ,[poiTotalTaxes]
            ,[poiTotalAmount]
            ,[poiRequestedShipDate]
            ,[poiRequestedDeliveryDate]
            ,[poiInvoicedQuantity]
            ,[poiShippedQuantity]
            ,[poiReceivedQuantity]
            ,[poiShipAddress]
            ,[poiShipInstructions]
            ,[poiHandlingInstructions]
            ,[poiSpecialInstructions]
            ,[poiDeliveryInstructions]
            ,[poiUserDefined1]
            ,[poiUserDefined2]
            ,[poiUserDefined3]
            ,[poiFrameworkContract]
            ,[poiIdConvenioMarco]
            ,[poiNroLicitacionPublica]
            FROM [DCCPProcurement].[dbo].[prcPOItem]"
)

prcGoodAndService <- sqlQuery(con2, "SELECT TOP (1000) [ProductoCode]
      ,[idcategory]
      ,[ProviderNumber]
      ,[level1]
      ,[level2]
      ,[level3]
      ,[levelcode]
      ,[productoname]
      ,[description]
      ,[sinonimos]
      ,[fulltextlevel3]
      ,[path]
      ,[NumOC]
      ,[NumLC]
      ,[typeLC]
  FROM [DCCPProcurement].[dbo].[prcGoodAndService]")

sii_atrib_2021 <- sqlQuery(con3, 
                           "SELECT TOP (1000) 
                                 [AÑO_COMERCIAL]
                                ,[RUT]
                                ,[DV]
                                ,[TRAMOS_5]
                                ,[TRAMOS_13]
                                ,[TRAMOS_18]
                                ,[TRAMOS_TRABAJADORES]
                                ,[CODIGO_RUBRO]
                                ,[DESCRIPCION_RUBRO]
                                ,[CODIGO_SUBRUBRO]
                                ,[DESCRIPCION_SUBRUBRO]
                                ,[CODIGO_ACTIVIDAD_ECONOMICA]
                                ,[DESCRIPCION_ACTIVIDAD_ECONOMICA]
                                ,[GENERO]
                                ,[DESCRIPCION_REGION]
                                ,[DESCRIPCION_PROVINCIA]
                                ,[DESCRIPCION_COMUNA]
                                FROM [Estudios].[dbo].[sii_atrib_2021]")

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


start <- Sys.time()


datos <- sqlQuery(con2, "
    SELECT
    TABLA2.*
    ,(CASE 
    WHEN TABLA2.[Proveedor Local 2]='1' AND TABLA2.Size='MiPyme' THEN 'Proveedor Local' 
    ELSE 'Otro Caso' END
    ) [Proveedor Local Ley]
    ,GS.level1 [Rubro ONU 1]
    ,GS.level2 [Rubro ONU 2]
    ,GS.level3 [Rubro ONU 3]
    ,GS.productoname
    ,PI.poiQuantity
    ,PI.poiNetPrice
    ,PI.poiTotalAmount
    ,PI.poiName
    ,PI.poiDescription
          FROM  
          (
          SELECT 
           TABLA.*
            ,(CASE 
            WHEN TABLA.[id_region_sr] = TABLA.[Región  Unidad de Compra] THEN 1
            ELSE 0 END) [Proveedor Local 1]
            ,(CASE 
            WHEN TABLA.[id_region_sr] = TABLA.[Región Despacho (SolicitudCotizacion)] THEN 1
            ELSE 0 END) [Proveedor Local 2]
            ,SII.DESCRIPCION_ACTIVIDAD_ECONOMICA
            ,(CASE  
            WHEN SII.TRAMOS_5 IN (1,2,3,4) THEN 'MiPyme'
            ELSE 'Grande' END
            ) [Size]
          
                    FROM (
                    SELECT DISTINCT --TOP 10000 
                    	'http://www.mercadopublico.cl/fichaLicitacion.html?idLicitacion='+PO.porCode as Link
                      ,PO.porID
                      ,SC.CodigoSolicitudCotizacion
                    	,PO.porDescription [Descripción orden de compra]
                    	,SC.Descripcion [Descripción Cotización]
                    	,PO.[porBuyerOrganization]
                    	,LOWER(REPLACE(REPLACE(SR.psrBuyerTaxID,'.',''),'-','')) [Rut Comprador] 
                    	,SR.psrBuyerOrganizationLegalName  [Nombre Organismo Comprador]
                    	,SR.psrBuyerActivity [Actividad del Comprador] 
                    	,SR.psrBuyerAddress [Dirección  Unidad de Compra]
                    	,SR.psrBuyerCity [Región  Unidad de Compra]
                    	,SC.CodigoRegion [Región Despacho (SolicitudCotizacion)]
                    	,SR.psrBuyerDistrict [Comuna  Unidad de Compra]
                      --,CO.MontoTotal
                    	,PO.[porSellerOrganization]
                    	,SR2.psrSellerOrganizationLegalName  [Nombre Proveedor (sucursal)]
                    	,SR2.psrSellerActivity [Actividad Proveedor]
                    	,CO.EsProveedorSeleccionado [Ganadora]
                    	,LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')) [Rut Proveedor] 
                    	,LEFT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-',''))
                    		,LEN(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')))-1) [Rut_numero]
                    	,RIGHT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')),1) [Rut_dv]
                    	,SR2.psrSellerOrganizationLegalName  [Razón Social Proveedor]
                    	,SR2.psrSellerActivity [Actividad del Proveedor  (prcPOStaticRecipient)] 
                    	,SR2.psrSellerAddress [Dirección  Proveedor  (prcPOStaticRecipient)]
                    	,SR2.psrSellerCity [Región  Proveedor  (prcPOStaticRecipient)]
                    	,SR2.psrSellerDistrict [Comuna  Proveedor  (prcPOStaticRecipient)]
                    	
                      ,(CASE SR2.psrSellerCity 
                    	WHEN 'DEL BIO BIO' THEN 8 
                    	WHEN 'ñUBLE' THEN 16
                    	WHEN 'Región Aysén del General Carlos Ibáñez del Campo' THEN 11
                    	WHEN 'Región de Arica y Parinacota' THEN 15
                    	WHEN 'Región de Coquimbo' THEN 4
                    	WHEN 'Región de los Lagos' THEN 10
                    	WHEN 'Región de Magallanes y de la Antártica' THEN 12
                    	WHEN 'Región de Valparaíso' THEN 5
                    	WHEN 'Región del Libertador General Bernardo O´Higgins' THEN 6
                    	WHEN 'Región del Ñuble' THEN 16
                    	WHEN 'Bio - Bio' THEN 8
                    	WHEN 'Ñuble' THEN 16
                    	WHEN 'Región de Antofagasta' THEN 2
                    	WHEN 'Región de Atacama' THEN 3
                    	WHEN 'Región de la Araucanía' THEN 9
                    	WHEN 'Región de Los Ríos' THEN 14
                    	WHEN 'Región de Tarapacá' THEN 1
                    	WHEN 'Región del Biobío' THEN 8
                    	WHEN 'Región del Maule' THEN 7
                    	WHEN 'Región Metropolitana de Santiago' THEN 13 
                    	ELSE 0 END) [id_region_sr]
                      ,COUNT(DISTINCT PO.porCode) [Cantidad OC]
                      ,SUM(
                			CASE 
                			WHEN CO.EsProveedorSeleccionado = 1 THEN PO.porTotalAmount
                			ELSE 0 END
                			) [Montos]
                                  
                  	FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC
                  	LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] as co ON co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
                  	INNER JOIN [DCCPCotizacion].[dbo].[RelacionOC] as ROC ON ROC.IdSolicitudCotizacion = SC.Id
                  	INNER JOIN [DCCPProcurement].[dbo].[prcPOHeader] as PO ON PO.porID=ROC.porId
                  	INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] as SR ON PO.porID =SR.psrOrder 
                  	INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] as SR2 ON PO.porID =SR2.psrOrder
                  	WHERE porIsIntegrated = 3
                  		AND (YEAR([porSendDate]) = 2023 
                  		--AND (SR.psrBuyerActivity != 'UNIVERSIDADES')
                  		AND (SC.idEstado>=2)
                  		--AND MONTH([porSendDate]) = 11
                  		) 
                  		AND ([porBuyerStatus] IN (4, 5, 6, 7, 12))
                              	AND SR.psrBuyerCity = 16
                  	-- ORDER BY NEWID() -- Asegura la selección aleatoria
                    GROUP BY 
              			PO.porCode
              			,PO.porID
              			,SC.CodigoSolicitudCotizacion
              			,PO.porDescription
              			,SC.Descripcion
              			,PO.porBuyerOrganization
              			,SR.psrBuyerTaxID
              			,SR.psrBuyerOrganizationLegalName
              			,SR.psrBuyerActivity
              			,SR.psrBuyerAddress
              			,SR.psrBuyerCity
              			,SC.CodigoRegion
              			,SR.psrBuyerDistrict
              			,PO.porSellerOrganization
              			,SR2.psrSellerOrganizationLegalName
              			,SR2.psrSellerActivity
              			,CO.EsProveedorSeleccionado
              			,SR2.psrSellerTaxID
              			,SR2.psrSellerAddress
              			,SR2.psrSellerCity
              			,SR2.psrSellerDistrict
                          
                        	)  TABLA 
            INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON TABLA.Rut_numero = SII.RUT
          ) TABLA2
    INNER JOIN [DCCPProcurement].[dbo].[prcPOItem] as PI ON TABLA2.porID=PI.poiOrder
    INNER JOIN [DCCPProcurement].[dbo].[prcGoodAndService] as GS ON PI.poiGoodAndService = GS.ProductoCode
                 ")


end <- Sys.time()

difftime(end, start, units = "mins")

saveRDS(datos, file = paste0(gsub("-", "", today()), " proveedores locales nuble", ".rds"))

#####################
# Análisis ==========
#####################
#####################


rubros3 <- datos %>% 
  filter(Ganadora == 1) %>% 
  group_by(`Rubro ONU 1`, `Proveedor Local Ley`) %>% count() %>% 
  data.table::setDT() %>% 
  data.table::dcast(`Rubro ONU 1`~`Proveedor Local Ley`, value.var = 'n') %>% 
  mutate(n_var = round((((`Proveedor Local`/(`Proveedor Local`+`Otro Caso`))-1))*100,2),
         n_oc = (`Proveedor Local`+`Otro Caso`)) %>%
  arrange(desc(n_var)) %>% 
  arrange(desc(n_oc)) 

(
productos <- datos %>% 
  filter(Ganadora == 1, `Rubro ONU 1` == rubros3$`Rubro ONU 1`[2]) %>% 
  group_by(`Rubro ONU 1`,productoname, `Proveedor Local Ley`) %>% count() %>% 
    data.table::setDT() %>% 
    data.table::dcast(`productoname`~`Proveedor Local Ley`, value.var = 'n') %>% 
    mutate(n_var = round((((`Proveedor Local`/(`Proveedor Local`+`Otro Caso`))-1))*100,2),
           n_oc = (`Proveedor Local`+`Otro Caso`)) %>%
    arrange(desc(n_var)) %>% 
    arrange(desc(n_oc)) 
)

(
  productos_especificos <- datos %>%
    filter(Ganadora == 1, `productoname` == productos$productoname[5]) %>% 
    group_by(productoname,poiName, poiDescription, `Descripción Cotización`, `Proveedor Local Ley`) %>%
    select(productoname
           ,poiName
           , poiDescription
           , `Descripción Cotización`
           , `Proveedor Local Ley`
           , poiQuantity
           , poiNetPrice
           ,`Nombre Organismo Comprador`)  
    
  )


datos %>% 
  filter(productoname == "Clavo-tornillo") %>% View()

data %>% 
  filter(`Rubro ONU` == "Equipamiento y suministros médicos") %>% 
  View()

data %>% 
  filter(DESCRIPCION_ACTIVIDAD_ECONOMICA == "VENTA AL POR MENOR DE OTROS PRODUCTOS EN COMERCIOS ESPECIALIZADOS N.C.P.") %>% 
  View()

data %>% 
  filter(DESCRIPCION_ACTIVIDAD_ECONOMICA == "INSTALACIONES DE GASFITERIA, CALEFACCION Y AIRE ACONDICIONADO") %>% 
  group_by(`Nombre Organismo Comprador`) %>% 
  count() %>% 
  View()

data %>% 
  group_by(`Actividad Proveedor`) %>% 
  summarise(n_oc = n(), 
            monto_transado = sum(porTotalAmount)/1000000
            ,oc_promedio = median(porTotalAmount)/1000000
  ) %>%  
  arrange(desc(n_oc)) %>% View()


data %>% 
  filter(Rut_numero == "2706385") %>%
  group_by(porCode) %>% 
  summarise(rubros = n()) %>% 
  View()


data %>% 
  filter(`Actividad Proveedor` == "ACTIVIDADES DE CONSULTORIA DE GESTION") %>% 
  View()
# 
# 
# start <- Sys.time()
# data <- sqlQuery(con2, "
#                  SELECT DISTINCT
#                  
#                  Subquery.porCode
#                   ,Subquery.porDescription
#                   ,Subquery.[porBuyerOrganization]
#                   ,Subquery.entCode
#                   ,Subquery.[Nombre Unidad de Compra]
#                   ,Subquery.[Nombre Organismo Comprador]
#                   ,Subquery.[Nombre dirección Unidad de Compra]
#                   ,Subquery.[Dirección  Unidad de Compra]
#                   ,Subquery.[Región  Unidad de Compra]
#                   ,Subquery.[Comuna  Unidad de Compra]
# 
#                   ,PI.poiName
#                   ,PI.poiDescription
#                   ,O2.orgLegalName [Nombre Proveedor (sucursal)]
#                   , E2.entName [Nombre Proveedor (matriz)]
#                   --, (CASE WHEN C.citCode = C2.citCode THEN 1 ELSE 0 END) [Proveedor Local]
#                   , EA2.eadName [Nombre dirección sucursal]
#                   , EA2.eadAddress [Dirección  Sucursal]
#                   , C2.citName [Región  Sucursal]
#                   , D2.disName [Comuna  Sucursal]
#                   /*, ST.pstDescription
#                   ,[porShipmentType] -- Tipo de Despacho
#                   ,[porInvoiceAddress] -- Dirección de facturación 
#                   ,[porShipAddress] -- Dirección de despacho
#                   ,[porShipInstructions]
#                   ,[porHandlingInstructions]
#                   ,[porSpecialInstructions]
#                   ,[porDeliveryInstructions]*/
#                  FROM(
#                  
#                   SELECT TOP 1000 
#                   porCode
#                   ,porDescription
#                   ,[porBuyerOrganization]
#                   ,E.entCode
#                   ,O.orgLegalName [Nombre Unidad de Compra]
#                   ,E.entName [Nombre Organismo Comprador]
#                   , EA.eadName [Nombre dirección Unidad de Compra]
#                   , EA.eadAddress [Dirección  Unidad de Compra]
#                   , C.citName [Región  Unidad de Compra]
#                   , D.disName [Comuna  Unidad de Compra]
#                   FROM [DCCPProcurement].[dbo].[prcPOHeader] PO
#                    INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] O ON PO.porBuyerOrganization = O.orgCode -- Identifica unidad de compra
#                    INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] E ON O.orgEnterprise = E.entCode      -- Identifica organismo comprador           
#                    INNER JOIN [DCCPPlatform].[dbo].[gblOrganizationAddress] OA ON O.orgCode = OA.oraOrganization
#                    INNER JOIN [DCCPPlatform].[dbo].[gblEnterpriseAddress] EA ON OA.oraAddress = EA.eadCode
#           				 INNER JOIN [DCCPPlatform].[dbo].[gblCity] C ON EA.eadCity = C.citCode
#           				 INNER JOIN [DCCPPlatform].[dbo].[gblDistrict] D ON EA.eadDistrict = D.disCode
#           				 INNER JOIN [DCCPPlatform].[dbo].[gblAddressType] AT ON OA.oraType = AT.atyCode
#                   WHERE porIsIntegrated = 3
#                       AND (YEAR([porSendDate]) = 2023 AND MONTH([porSendDate]) = 11) 
#                       AND ([porBuyerStatus] IN (4, 5, 6, 7, 12))
#           				    AND C.citCode = 16
#                   ORDER BY NEWID() -- Asegura la selección aleatoria
#                   
#           				 ) AS Subquery
#         				 INNER JOIN [DCCPProcurement].[dbo].[prcPOHeader] PO ON Subquery.[porCode] = PO.[porCode]
#         				 INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] O2 ON PO.porSellerOrganization = O2.orgCode -- Identifica sucursal prov.
#                  INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] E2 ON O2.orgEnterprise = E2.entCode -- Identifica Proveedor
#         				 INNER JOIN [DCCPPlatform].[dbo].[gblOrganizationAddress] OA2 ON O2.orgCode = OA2.oraOrganization 
#         				 INNER JOIN [DCCPPlatform].[dbo].[gblEnterpriseAddress] EA2 ON OA2.oraAddress = EA2.eadCode
#         				 INNER JOIN [DCCPPlatform].[dbo].[gblCity] C2 ON EA2.eadCity = C2.citCode
#         				 INNER JOIN [DCCPPlatform].[dbo].[gblDistrict] D2 ON EA2.eadDistrict = D2.disCode
#                  INNER JOIN [DCCPProcurement].[dbo].[prcPOShipmentType] ST ON PO.porShipmentType = ST.pstCode 
#         				 INNER JOIN [DCCPProcurement].[dbo].[prcPOItem] PI ON PO.[porID] = PI.poiOrder
# 
#                  ")
# end <- Sys.time()
# difftime(end, start, units = "mins")
# 
# 
# data %>% 
#   group_by(poiDescription) %>% 
#   summarise(n_ = n()) %>%  
#   order_by(desc(n_))
# 
# 
# 
# 
# 
# data <- sqlQuery(con2, "
# SELECT A.*
#   ,SII.CODIGO_ACTIVIDAD_ECONOMICA
# ,SII.DESCRIPCION_ACTIVIDAD_ECONOMICA
# ,(CASE 
#   WHEN  A.[Región  Proveedor  (prcPOStaticRecipient)] = A.[Región  Unidad de Compra] THEN 1 
#   ELSE 0 END) [Proveedor Local 1]
# FROM (
#   SELECT DISTINCT
#   
#   Subquery.*
#     
#     ,PO.porName
#   --,PO.porDescription
#   ,PO.porTotalAmount
#   --,PI.poiName
#   --,PI.poiDescription
#   ,SR2.psrSellerOrganizationLegalName  [Nombre Proveedor (sucursal)]
#   ,SR2.psrSellerActivity [Actividad Proveedor]
#   ,[porSellerOrganization]
#   ,LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')) [Rut Proveedor] 
#   ,LEFT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-',''))
#         ,LEN(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')))-1) [Rut_numero]
#   ,RIGHT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')),1) [Rut_dv]
#   ,SR2.psrSellerOrganizationLegalName  [Razón Social Proveedor]
#   ,SR2.psrSellerActivity [Actividad del Proveedor  (prcPOStaticRecipient)] 
#   ,SR2.psrSellerAddress [Dirección  Proveedor  (prcPOStaticRecipient)]
#   ,SR2.psrSellerCity [Región  Proveedor  (prcPOStaticRecipient)]
#   ,SR2.psrSellerDistrict [Comuna  Proveedor  (prcPOStaticRecipient)]
#   
#   
#   
#   
#   
#   /*
#     , EA2.eadName [Nombre dirección sucursal]
#   , EA2.eadAddress [Dirección  Sucursal]
#   , C2.citName [Región  Sucursal]
#   , D2.disName [Comuna  Sucursal]
#   , ST.pstDescription
#   ,[porShipmentType] -- Tipo de Despacho
#   ,[porInvoiceAddress] -- Dirección de facturación 
#   ,[porShipAddress] -- Dirección de despacho
#   ,[porShipInstructions]
#   ,[porHandlingInstructions]
#   ,[porSpecialInstructions]
#   ,[porDeliveryInstructions]
#   
#   */
#     
#     FROM (
#       SELECT  DISTINCT --TOP 100000 
#       PO.porCode
#       ,PO.porDescription [Descripción orden de compra]
#       ,SC.Descripcion [Descripción Cotización]
#       ,PO.[porBuyerOrganization]
#       ,LOWER(REPLACE(REPLACE(SR.psrBuyerTaxID,'.',''),'-','')) [Rut Comprador] 
#       ,SR.psrBuyerOrganizationLegalName  [Nombre Organismo Comprador]
#       ,SR.psrBuyerActivity [Actividad del Comprador] 
#       ,SR.psrBuyerAddress [Dirección  Unidad de Compra]
#       ,SR.psrBuyerCity [Región  Unidad de Compra]
#       ,SR.psrBuyerDistrict [Comuna  Unidad de Compra]
#       
#       FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC
#       LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] co on co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
#       INNER JOIN [DCCPCotizacion].[dbo].[RelacionOC] as ROC ON ROC.IdSolicitudCotizacion = SC.Id
#       INNER JOIN DCCPProcurement.dbo.prcPOHeader as PO ON PO.porID=ROC.porId
#       INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR ON PO.porID =SR.psrOrder 
#       INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR2 ON PO.porID =SR2.psrOrder
#       WHERE porIsIntegrated = 3
#       AND (YEAR([porSendDate]) = 2023 
#            AND (psrBuyerActivity != 'UNIVERSIDADES')
#            AND (SC.idEstado>=2)
#            --AND MONTH([porSendDate]) = 11
#       ) 
#       AND ([porBuyerStatus] IN (4, 5, 6, 7, 12))
#       AND SR.psrBuyerCity = 16
#       -- ORDER BY NEWID() -- Asegura la selección aleatoria
#     ) AS Subquery
#   INNER JOIN [DCCPProcurement].[dbo].[prcPOHeader] PO ON Subquery.[porCode] = PO.[porCode]
#   INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR2 ON PO.porID =SR2.psrOrder
#   
#   /* 
#     INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] O2 ON PO.porSellerOrganization = O2.orgCode -- Identifica sucursal prov.
#   INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] E2 ON O2.orgEnterprise = E2.entCode -- Identifica Proveedor
#   INNER JOIN [DCCPPlatform].[dbo].[gblOrganizationAddress] OA2 ON O2.orgCode = OA2.oraOrganization 
#   INNER JOIN [DCCPPlatform].[dbo].[gblEnterpriseAddress] EA2 ON OA2.oraAddress = EA2.eadCode
#   INNER JOIN [DCCPPlatform].[dbo].[gblCity] C2 ON EA2.eadCity = C2.citCode
#   INNER JOIN [DCCPPlatform].[dbo].[gblDistrict] D2 ON EA2.eadDistrict = D2.disCode
#   
#   INNER JOIN [DCCPProcurement].[dbo].[prcPOShipmentType] ST ON PO.porShipmentType = ST.pstCode 
#   INNER JOIN [DCCPProcurement].[dbo].[prcPOItem] PI ON PO.[porID] = PI.poiOrder -- Para analizar por tipo de producto 
#   */
# ) A
# INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON A.Rut_numero = SII.RUT -- De aquí voy a sacar el tamaño
# 
# 
# 
# ")
