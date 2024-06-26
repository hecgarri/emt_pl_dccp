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