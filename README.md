Cuaderno de Laboratorio: Análisis de los cambios a la ley 19.886 y su
eventual impacto sobre proveedores locales
================

## Motivación

En el contexto de la reforma a la ley N° 19.886 en la que se introducen
cambios relevantes en materia de promoción de la participación de
empresas de menor tamaño en las compras públicas es que se establece, en
el artículo 47, que además de ampliar el umbral para el uso de la Compra
Ágil de 10 UTM a 30 UTM será exclusivo para proveedores locales,
definidos estos como aquellas Pymes cuya dirección de la casa matriz
coincide con la dirección de despacho

La consulta para obtener los datos es la siguiente:

``` r
datos <- sqlQuery(con2, 
    "WITH TABLA AS (SELECT 
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
      ,SC.CodigoRegion [Región de Despacho]
      ,O1.orgLegalName [Razón Social Proveedor]
      ,CO.EsProveedorSeleccionado
      ,O1.orgActivity [Actividad Proveedor]
      ,O2.orgLegalName [Razón Social Comprador]
--    ,O2.orgActivity [Actividad Comprador]
      ,(CASE  
            WHEN SII.TRAMOS_13 = 1 THEN 'Sin Ventas'
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
  INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O1 ON SC.CodigoEmpresa COLLATE Modern_Spanish_CI_AI = O1.orgCode COLLATE Modern_Spanish_CI_AI
  INNER JOIN [DCCPPlatform].[dbo].[gblOrganization] as O2 ON SC.CodigoOrganismo  COLLATE Modern_Spanish_CI_AI = O2.orgCode COLLATE Modern_Spanish_CI_AI
  INNER JOIN [10.34.71.202].[Estudios].[dbo].[sii_atrib_2021] SII ON CAST(LEFT(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-',''))
                ,LEN(LOWER(REPLACE(REPLACE(O1.orgTaxID,'.',''),'-','')))-1) as VARCHAR(50))  = CAST(SII.RUT AS VARCHAR(50))
  WHERE YEAR(SC.FechaPublicacion) = 2023
  ) 
  SELECT 
    *,
    CASE 
        WHEN [id_regionProv] = [Región de Despacho] THEN 1
        ELSE 0 
    END AS [Proveedor Local 2]
FROM TABLA; 
                  ")
```
