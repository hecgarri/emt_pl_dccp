SELECT 
 TABLA.*
  ,(CASE 
  WHEN TABLA.[id_region_sr] = TABLA.[Región  Unidad de Compra] THEN 1
  ELSE 0 END) [Proveedor Local 1]
  ,(CASE 
  WHEN TABLA.[id_region_sr] = TABLA.[Región Despacho (SolicitudCotizacion)] THEN 1
  ELSE 0 END) [Proveedor Local 2]

FROM (
SELECT  DISTINCT --TOP 100000 
	PO.porCode
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
	,[porSellerOrganization]
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
	, (CASE SR2.psrSellerCity 
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
          
	FROM [DCCPCotizacion].[dbo].[SolicitudCotizacion] as SC
	LEFT JOIN  [DCCPCotizacion].[dbo].[Cotizacion] co on co.SolicitudCotizacionId=SC.id and CO.ESTADOID = 2
	INNER JOIN [DCCPCotizacion].[dbo].[RelacionOC] as ROC ON ROC.IdSolicitudCotizacion = SC.Id
	INNER JOIN DCCPProcurement.dbo.prcPOHeader as PO ON PO.porID=ROC.porId
	INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR ON PO.porID =SR.psrOrder 
	INNER JOIN [DCCPProcurement].[dbo].[prcPOStaticRecipient] SR2 ON PO.porID =SR2.psrOrder
	WHERE porIsIntegrated = 3
		AND (YEAR([porSendDate]) = 2023 
		AND (SR.psrBuyerActivity != 'UNIVERSIDADES')
		AND (SC.idEstado>=2)
		--AND MONTH([porSendDate]) = 11
		) 
		AND ([porBuyerStatus] IN (4, 5, 6, 7, 12))
            	AND SR.psrBuyerCity = 16
	-- ORDER BY NEWID() -- Asegura la selección aleatoria
	)  TABLA 
	INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON A.Rut_numero = SII.RUT -- De aquí voy a sacar el tamaño