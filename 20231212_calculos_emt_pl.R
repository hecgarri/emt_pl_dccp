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

data <- sqlQuery(con2, "
                 SELECT DISTINCT
                 
                  Subquery.porCode
                  ,Subquery.porDescription
                  ,Subquery.[porBuyerOrganization]
                  --,Subquery.[Nombre Unidad de Compra]
                  ,Subquery.[Nombre Organismo Comprador]
                  --,Subquery.[Nombre dirección Unidad de Compra]
                  ,Subquery.[Actividad del Comprador]
                  ,Subquery.[Dirección \n Unidad de Compra]
                  ,Subquery.[Región \n Unidad de Compra]
                  ,Subquery.[Comuna \n Unidad de Compra]

                  ,PO.porName
                  ,PO.porDescription
                  ,PO.porTotalAmount
                  ,PI.poiName
                  ,PI.poiDescription
                  ,SR2.psrSellerOrganizationLegalName  [Nombre Proveedor (sucursal)]
                  ,SR2.psrSellerActivity [Actividad Proveedor]
                  ,[porSellerOrganization]
                  ,LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')) [Rut Comprador] 
                  ,SR2.psrSellerOrganizationLegalName  [Nombre Organismo Comprador]
                  ,SR2.psrSellerActivity [Actividad del Proveedor] 
                  ,SR2.psrSellerAddress [Dirección \n Unidad de Compra]
                  ,SR2.psrSellerCity [Región \n Unidad de Compra]
                  ,SR2.psrSellerDistrict [Comuna \n Unidad de Compra]
                 
                  ,(CASE WHEN SR2.psrSellerCity = Subquery.[Región \n Unidad de Compra] THEN 1 ELSE 0 END) [Proveedor Local]
                  
                 /*
                 , EA2.eadName [Nombre dirección sucursal]
                  , EA2.eadAddress [Dirección \n Sucursal]
                  , C2.citName [Región \n Sucursal]
                  , D2.disName [Comuna \n Sucursal]
                  , ST.pstDescription
                  ,[porShipmentType] -- Tipo de Despacho
                  ,[porInvoiceAddress] -- Dirección de facturación 
                  ,[porShipAddress] -- Dirección de despacho
                  ,[porShipInstructions]
                  ,[porHandlingInstructions]
                  ,[porSpecialInstructions]
                  ,[porDeliveryInstructions]
                 */
                 FROM(
                  SELECT  DISTINCT --TOP 100000 
                  porCode
                  ,porDescription
                  ,[porBuyerOrganization]
                  ,LOWER(REPLACE(REPLACE(SR.psrBuyerTaxID,'.',''),'-','')) [Rut Comprador] 
                  ,SR.psrBuyerOrganizationLegalName  [Nombre Organismo Comprador]
                  ,SR.psrBuyerActivity [Actividad del Comprador] 
                  ,SR.psrBuyerAddress [Dirección \n Unidad de Compra]
                  ,SR.psrBuyerCity [Región \n Unidad de Compra]
                  ,SR.psrBuyerDistrict [Comuna \n Unidad de Compra]
                  FROM [DCCPProcurement].[dbo].[prcPOHeader] PO
                  INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR ON PO.porID =SR.psrOrder 
                   WHERE porIsIntegrated = 3
                        AND (YEAR([porSendDate]) = 2023 
                        AND (psrBuyerActivity != 'UNIVERSIDADES')
                 --AND MONTH([porSendDate]) = 11
                 ) 
                        AND ([porBuyerStatus] IN (4, 5, 6, 7, 12))
            				    -- AND SR.psrBuyerCity = 16
                    -- ORDER BY NEWID() -- Asegura la selección aleatoria
                    ) AS Subquery
        				 INNER JOIN [DCCPProcurement].[dbo].[prcPOHeader] PO ON Subquery.[porCode] = PO.[porCode]
      				   INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR2 ON PO.porID =SR2.psrOrder
        				 /* 
        				 INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] O2 ON PO.porSellerOrganization = O2.orgCode -- Identifica sucursal prov.
                 INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] E2 ON O2.orgEnterprise = E2.entCode -- Identifica Proveedor
        				 INNER JOIN [DCCPPlatform].[dbo].[gblOrganizationAddress] OA2 ON O2.orgCode = OA2.oraOrganization 
        				 INNER JOIN [DCCPPlatform].[dbo].[gblEnterpriseAddress] EA2 ON OA2.oraAddress = EA2.eadCode
        				 INNER JOIN [DCCPPlatform].[dbo].[gblCity] C2 ON EA2.eadCity = C2.citCode
        				 INNER JOIN [DCCPPlatform].[dbo].[gblDistrict] D2 ON EA2.eadDistrict = D2.disCode
        				 */
                 INNER JOIN [DCCPProcurement].[dbo].[prcPOShipmentType] ST ON PO.porShipmentType = ST.pstCode 
        				 INNER JOIN [DCCPProcurement].[dbo].[prcPOItem] PI ON PO.[porID] = PI.poiOrder

                 ")

end <- Sys.time()

difftime(end, start, units = "mins")


data %>% 
  group_by(`Actividad Proveedor`) %>% 
  summarise(n_oc = n(), 
            monto_transado = sum(porTotalAmount)/1000000
            ,oc_promedio = median(porTotalAmount)/1000000
            ) %>%  
  arrange(desc(n_oc)) %>% View()



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
#                   ,Subquery.[Dirección \n Unidad de Compra]
#                   ,Subquery.[Región \n Unidad de Compra]
#                   ,Subquery.[Comuna \n Unidad de Compra]
# 
#                   ,PI.poiName
#                   ,PI.poiDescription
#                   ,O2.orgLegalName [Nombre Proveedor (sucursal)]
#                   , E2.entName [Nombre Proveedor (matriz)]
#                   --, (CASE WHEN C.citCode = C2.citCode THEN 1 ELSE 0 END) [Proveedor Local]
#                   , EA2.eadName [Nombre dirección sucursal]
#                   , EA2.eadAddress [Dirección \n Sucursal]
#                   , C2.citName [Región \n Sucursal]
#                   , D2.disName [Comuna \n Sucursal]
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
#                   , EA.eadAddress [Dirección \n Unidad de Compra]
#                   , C.citName [Región \n Unidad de Compra]
#                   , D.disName [Comuna \n Unidad de Compra]
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
