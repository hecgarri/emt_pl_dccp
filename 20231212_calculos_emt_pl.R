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









