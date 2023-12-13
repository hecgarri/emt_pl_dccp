WITH OC_2022 AS (
SELECT T.Year
  , CASE
    WHEN OC.porIsIntegrated = 3 THEN 'CA'
    WHEN [IDProcedenciaOC] IN (701,702) THEN 'LC'
    WHEN [IDProcedenciaOC] IS NULL AND OCExcepcional=1 THEN 'LC'
    WHEN [IDProcedenciaOC] IN (703) THEN 'CM'
ELSE 'TD' END AS Procedimiento
,OC.* 
--,dp.RUTSucursal
,CASE [DESCRIPCION_REGION] 
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
ELSE 6 END AS id_regionProv
,dl.IDRegion id_regionComp
,case [TRAMOS_13] when 1 then 5
 when 2 then 3
 when 3 then 3
 when 4 then 3
 when 5 then 4
 when 6 then 4
 when 7 then 4
 when 8 then 2
 when 9 then 2
 when 10 then 1
 when 11 then 1
 when 12 then 1
 when 13 then 1
 end as IdTamanoOficial
  --  , SUM(OC.MontoCLP+OC.ImpuestoCLP) AS MontoBrutoCLP
  -- , SUM(OC.MontoUSD+OC.ImpuestoUSD) AS MontoBrutoUSD
  -- , SUM(OC.MontoCLF+OC.ImpuestoCLF) AS MontoBrutoCLF
  -- , COUNT(DISTINCT OC.porID) AS CantOC
FROM DM_Transaccional.dbo.THordenescompra OC
LEFT JOIN DM_Transaccional.dbo.DimTiempo T  ON OC.IDFechaEnvioOC = T.DateKey
LEFT JOIN DM_Transaccional.dbo.DimProveedor DP on oc.IDSucursal = DP.IDSucursal
LEFT JOIN [Estudios].[dbo].[sii_atrib_2021] REG ON REPLACE(REPLACE(DP.rutsucursal,'.',''),'-','') = CAST(REG.Rut AS VARCHAR)+REG.DV
LEFT JOIN DM_TRANSACCIONAL.dbo.DimComprador DC ON OC.IDUnidaddeCompra = DC.IDUnidaddeCompra
LEFT JOIN DM_TRANSACCIONAL.dbo.DimLocalidad DL ON dc.IDLocalidadUnidaddeCompra = DL.IDLocalidad
WHERE  T.Year =2022
)


insert into Estudios.dbo.TMP_prov_locales_2023
SELECT OC.*,
CASE 
WHEN OC.id_regionProv = OC.id_regionComp
THEN 1
ELSE 0
END ES_PROV_LOCAL
FROM OC_2022 OC
--where 


SELECT ES_PROV_LOCAL,
CASE 
WHEN (pl.IdTamanoOficial IN (2,3,4) OR pl.IdTamanoOficial IS NULL) THEN
'Mipyme'
when pl.IdTamanoOficial =5 THEN
'Sin Dato'
WHEN pl.IdTamanoOficial =1 THEN
'Grande'
ELSE
'OTRO'
END tamano_descripcion
, sum(pl.montoUSD+pl.impuestoUSD)MONTO_TRANSADO,COUNT(pl.porid)NRO_OC,
COUNT( DISTINCT DP.entcode) NRO_PROVEEDORES
FROM Estudios.dbo.TMP_prov_locales_2023 pl
LEFT JOIN DM_TRANSACCIONAL.dbo.dimproveedor DP on pl.IDSucursal = DP.IDSucursal
WHERE Procedimiento = 'CA'
GROUP BY ES_PROV_LOCAL,
CASE 
WHEN (pl.IdTamanoOficial IN (2,3,4) OR pl.IdTamanoOficial IS NULL) THEN
'Mipyme'
when pl.IdTamanoOficial =5 THEN
'Sin Dato'
WHEN pl.IdTamanoOficial =1 THEN
'Grande'
ELSE
'OTRO'
END