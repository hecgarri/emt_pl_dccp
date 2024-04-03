SELECT  [idReclamo]
		,er.NombreEstado
		,mr.NombreMotivoReclamo
		,tr.NombreTipoReclamo
		,cast([FechaIngresoReclamo] as date) FechaIngresoReclamo
		,[NombreReclamante] 
		,[RutReclamante]
		,[DetalleReclamo]
		,[NombreOOPP]
		,C.[entCode] [entCode OOPP]
		,C.[RazonSocialUnidaddeCompra]
		,C.[RUTUnidaddeCompra]
		,S.Sector
		,I.NombreInstitucion
		,L.Region
		,cast([FechaAsignacionReclamoOOPP] as date) FechaAsignacionReclamoOOPP
		,[NumLicOC]
		,[idReclamoCRM]
		,[RespuestaOOPP]
		,r.[orgCode]
		,r.[orgName]
		,[entCodeReclamante]
		,o.orgtaxid 'RUT_empresa'
		,cast([FechaRecepcionConforme] as date) FechaRecepcionConforme                     
        FROM [DCCPReclamos].[dbo].[Reclamo] r 
        left join [DCCPReclamos].[dbo].[Par_EstadoReclamo] er on er.idEstado=r.idEstado 
        left join [DCCPReclamos].[dbo].[Par_MotivoReclamo] mr on mr.idMotivoReclamo=r.idMotivoReclamo 
        left join [DCCPPlatform].[dbo].[gblOrganization] o on o.orgenterprise=r.entcodereclamante 
		inner join [DCCPPlatform].dbo.gblOrganization as o2 on r.orgCode = o2.orgCode collate Modern_Spanish_CI_AI
		inner join [estudios_procesamientodatosabiertos].[dbo].[DimComprador] as C on C.orgCode = o2.orgCode
		inner join [estudios_procesamientodatosabiertos].[dbo].[DimInstitucion] as I on C.entCode = I.entCode
		inner join [estudios_procesamientodatosabiertos].[dbo].[DimLocalidad] as L on C.IDLocalidadUnidaddeCompra = L.IDLocalidad
		inner join [estudios_procesamientodatosabiertos].dbo.DimSector as S on I.IdSector = S.IdSector
        left join [DCCPReclamos].[dbo].[Par_TipoReclamo] tr on tr.idtiporeclamo=mr.idtiporeclamo
                     where  C.[RUTUnidaddeCompra] in ('76.082.106-3')                      
                     and  year(FechaIngresoReclamo) >=2014