SELECT 
 TABLA.*
  ,(CASE 
  WHEN TABLA.[id_region_sr] = TABLA.[Regi�n  Unidad de Compra] THEN 1
  ELSE 0 END) [Proveedor Local 1]
  ,(CASE 
  WHEN TABLA.[id_region_sr] = TABLA.[Regi�n Despacho (SolicitudCotizacion)] THEN 1
  ELSE 0 END) [Proveedor Local 2]

FROM (
SELECT  DISTINCT --TOP 100000 
	PO.porCode
	,PO.porDescription [Descripci�n orden de compra]
	,SC.Descripcion [Descripci�n Cotizaci�n]
	,PO.[porBuyerOrganization]
	,LOWER(REPLACE(REPLACE(SR.psrBuyerTaxID,'.',''),'-','')) [Rut Comprador] 
	,SR.psrBuyerOrganizationLegalName  [Nombre Organismo Comprador]
	,SR.psrBuyerActivity [Actividad del Comprador] 
	,SR.psrBuyerAddress [Direcci�n  Unidad de Compra]
	,SR.psrBuyerCity [Regi�n  Unidad de Compra]
	,SC.CodigoRegion [Regi�n Despacho (SolicitudCotizacion)]
	,SR.psrBuyerDistrict [Comuna  Unidad de Compra]
	,[porSellerOrganization]
	,SR2.psrSellerOrganizationLegalName  [Nombre Proveedor (sucursal)]
	,SR2.psrSellerActivity [Actividad Proveedor]
	,CO.EsProveedorSeleccionado [Ganadora]
	,LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')) [Rut Proveedor] 
	,LEFT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-',''))
		,LEN(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')))-1) [Rut_numero]
	,RIGHT(LOWER(REPLACE(REPLACE(SR2.psrSellerTaxID,'.',''),'-','')),1) [Rut_dv]
	,SR2.psrSellerOrganizationLegalName  [Raz�n Social Proveedor]
	,SR2.psrSellerActivity [Actividad del Proveedor  (prcPOStaticRecipient)] 
	,SR2.psrSellerAddress [Direcci�n  Proveedor  (prcPOStaticRecipient)]
	,SR2.psrSellerCity [Regi�n  Proveedor  (prcPOStaticRecipient)]
	,SR2.psrSellerDistrict [Comuna  Proveedor  (prcPOStaticRecipient)]
	, (CASE SR2.psrSellerCity 
	WHEN 'DEL BIO BIO' THEN 8 
	WHEN '�UBLE' THEN 16
	WHEN 'Regi�n Ays�n del General Carlos Ib��ez del Campo' THEN 11
	WHEN 'Regi�n de Arica y Parinacota' THEN 15
	WHEN 'Regi�n de Coquimbo' THEN 4
	WHEN 'Regi�n de los Lagos' THEN 10
	WHEN 'Regi�n de Magallanes y de la Ant�rtica' THEN 12
	WHEN 'Regi�n de Valpara�so' THEN 5
	WHEN 'Regi�n del Libertador General Bernardo O�Higgins' THEN 6
	WHEN 'Regi�n del �uble' THEN 16
	WHEN 'Bio - Bio' THEN 8
	WHEN '�uble' THEN 16
	WHEN 'Regi�n de Antofagasta' THEN 2
	WHEN 'Regi�n de Atacama' THEN 3
	WHEN 'Regi�n de la Araucan�a' THEN 9
	WHEN 'Regi�n de Los R�os' THEN 14
	WHEN 'Regi�n de Tarapac�' THEN 1
	WHEN 'Regi�n del Biob�o' THEN 8
	WHEN 'Regi�n del Maule' THEN 7
	WHEN 'Regi�n Metropolitana de Santiago' THEN 13 
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
	-- ORDER BY NEWID() -- Asegura la selecci�n aleatoria
	)  TABLA 
	INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON A.Rut_numero = SII.RUT -- De aqu� voy a sacar el tama�o