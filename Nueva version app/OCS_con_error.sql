SELECT  
        T.Year
        ,L.Region
        ,T.Date [Fecha Env�o OC]
        ,REPLACE(REPLACE(REPLACE(REPLACE(OC.NombreOC, 'CHAR(13)', ''), CHAR(10), ''),';',','),' ;',',') AS [NombreOC]
        ,OC.CodigoOC
		    ,REPLACE(ISNULL(OC.MonedaOC, 'Sin Tipo'), '', 'Sin Tipo') [Tipo de moneda]
    ,OL.Monto [Monto (neto) producto] 
		,REPLACE(REPLACE(REPLACE(OL.NombreItem, CHAR(13), ''), CHAR(10), ''),';',',') AS [Nombre producto]
		,REPLACE(REPLACE(REPLACE(OL.DescripcionItem, CHAR(13), ''), CHAR(10), ''),';',',') AS DescripcionItem
		,RU.RubroN1 [Rubro Producto]
		    ,OC.MontoUSD [Monto neto OC (d�lares)]
		    ,OC.MontoCLP [Monto neto OC (pesos)]
		    ,OC.MontoCLF [Monto neto OC (UF)]
		    ,OC.ImpuestoUSD [Impuesto OC (d�lares)]
		    ,OC.ImpuestoCLP [Impuesto OC (pesos)]
		    ,OC.ImpuestoCLF [Impuesto OC (UF)]
        ,C.RUTUnidaddeCompra [RUT Unidad de Compra]
        ,UPPER(C.NombreUnidaddeCompra) [Nombre Unidad de Compra]
		    ,I.entCode [entCode (Comprador)]
        ,REPLACE(REPLACE(REPLACE(UPPER(I.NombreInstitucion), CHAR(13), ''), CHAR(10), ''),';',',') AS [Nombre Institucion]
        ,S.Sector
        ,REPLACE(REPLACE(REPLACE(UPPER(P.RazonSocialSucursal), CHAR(13), ''), CHAR(10), ''),';',',') AS [Raz�n social Proveedor]
        ,E.entCode [entCode (Proveedor)]
        ,P.RUTSucursal [Rut Proveedor]
        ,L2.Region [Regi�n Proveedor]
        ,DTP.Tamano [Tama�o Proveedor],(CASE  WHEN OC.porisintegrated=3 THEN 'Compra Agil'
                ELSE (CASE  OC.IDProcedenciaOC
                      WHEN 703 THEN 'Convenio Marco'
                      WHEN 701 THEN 'Licitaci�n P�blica'
                      WHEN 1401 THEN 'Licitaci�n P�blica'
                      WHEN 702 THEN 'Licitaci�n Privada'
                      ELSE 'Trato Directo' END)
        END) [Procedencia]
      FROM [DM_Transaccional].[dbo].[THOrdenesCompra] as OC
        INNER JOIN [DM_Transaccional].[dbo].[THOrdenesCompraLinea] as OL  ON OC.porID = OL.porID
        INNER JOIN [DM_Transaccional].[dbo].[DimProducto] as DPR ON  OL.IDProducto = DPR.IDProducto
        INNER JOIN [DM_Transaccional].[dbo].[DimRubro] as RU ON DPR.IdRubro = RU.IdRubro
                      
        INNER JOIN [DM_Transaccional].[dbo].[DimProveedor] as P ON OC.IDSucursal=P.IDSucursal
        INNER JOIN [DM_Transaccional].[dbo].[DimEmpresa] as E ON P.entCode = E.entCode 
        INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L2 ON L2.IDLocalidad = P.IDLocalidadSucursal
        LEFT JOIN [DM_Transaccional].[dbo].[THTamanoProveedor] as TP ON P.entCode=TP.entCode AND TP.A�oTributario = 2022
        LEFT JOIN [DM_Transaccional].[dbo].[DimTamanoProveedor] as DTP ON TP.idTamano = DTP.IdTamano
                      
        INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
        INNER JOIN [DM_Transaccional].[dbo].[DimComprador] as C ON OC.IDUnidaddeCompra = C.IDUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
        INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L ON L.IDLocalidad = C.IDLocalidadUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimSector] as S ON I.IdSector = S.IdSector
      WHERE  OC.IDFechaEnvioOC BETWEEN '20240201' AND '20240202'
        AND OC.IDEstadoOC IN  (4,5,6,7,12) 
		AND OC.CodigoOC IN ('4179-156-SE24','3661-115-SE24')
		