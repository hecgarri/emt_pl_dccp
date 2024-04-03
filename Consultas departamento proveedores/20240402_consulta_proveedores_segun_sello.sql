SELECT  DISTINCT
        T.Year
		,U.Nombres+' '+U.Apellidos [Nombre completo usuario]
		,U.RUT [Rut usuario]
		,U.Sexo [Sexo usuario]
		,U.eMail
		,L.Region
		,P.NombreSucursal
		,P.RUTSucursal
		,E.NombreEmpresa
		,CASE OC.porisintegrated 
            WHEN 3 THEN 'Compra Agil'
            ELSE (CASE  OC.IDProcedenciaOC
                    WHEN 703 THEN 'Convenio Marco'
                    WHEN 701 THEN 'Licitación Pública'
                    WHEN 1401 THEN 'Licitación Pública'
                    WHEN 702 THEN 'Licitación Privada'
                    ELSE 'Trato Directo' 
                END)
        END AS Procedencia
		,MAX(OC.IDFechaEnvioOC) [Fecha última OC]
		,COUNT(DISTINCT OC.CodigoOC) [Cantidad de OC]
		,SUM(OC.MontoCLP+OC.ImpuestoCLP) [Monto Transado (pesos)]
		,SUM(OC.MontoCLF+OC.ImpuestoCLF) [Monto Transado (UF)]
		,SUM(OC.MontoUSD+OC.ImpuestoUSD) [Monto Transado (USD)]
        FROM [DM_Transaccional].[dbo].[THOrdenesCompra] as OC 
        INNER JOIN [DM_Transaccional].[dbo].DimUsuario as U ON OC.usrID = U.usrID 
		INNER join Dm_Usuario.dbo.tablonusuarioproveedor as UP ON OC.usrID = UP.usrID
		INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
        INNER JOIN [DM_Transaccional].[dbo].[DimComprador] as C ON OC.IDUnidaddeCompra = C.IDUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L ON L.IDLocalidad = C.IDLocalidadUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
        INNER JOIN [DM_Transaccional].[dbo].[DimSector] as S ON I.IdSector = S.IdSector
		INNER JOIN [DM_Transaccional].dbo.DimProveedor as P ON P.IDSucursal = OC.IDSucursal
		INNER JOIN [DM_Transaccional].dbo.DimEmpresa as E ON P.entCode = E.entCode
	  WHERE T.Year = 2023 
	  AND E.entCode IN (SELECT EntCode FROM [DM_RegistroProveedores].[dbo].[Sello_Proveedor] s 
	  WHERE Id_Tipo_Sello = 3 and YEAR(s.Fecha_Caducidad)>2023)
	  AND  UP.FechaUltLogin = (
		SELECT MAX(cast(FechaUltLogin as date)) 
		FROM Dm_Usuario.dbo.tablonusuarioproveedor 
		WHERE usrID = U.usrID
		)
	  GROUP BY 
	  T.Year
		,U.Nombres+' '+U.Apellidos 
		,U.RUT 
		,U.Sexo 
		,U.eMail
		,L.Region
		,P.NombreSucursal
		,P.RUTSucursal
		,E.NombreEmpresa
		,CASE OC.porisintegrated 
            WHEN 3 THEN 'Compra Agil'
            ELSE (CASE  OC.IDProcedenciaOC
                    WHEN 703 THEN 'Convenio Marco'
                    WHEN 701 THEN 'Licitación Pública'
                    WHEN 1401 THEN 'Licitación Pública'
                    WHEN 702 THEN 'Licitación Privada'
                    ELSE 'Trato Directo' 
                END)
        END