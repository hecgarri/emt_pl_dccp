SELECT  [idReclamo]
      ,er.NombreEstado
		,mr.NombreMotivoReclamo
		,tr.NombreTipoReclamo
		,cast([FechaIngresoReclamo] as datetime) FechaIngresoReclamo
		,[NombreReclamante] 
		,[RutReclamante]
		,[DetalleReclamo]
		,[NombreOOPP]
		,o2.orgEnterprise [entCode OOPP]
		,o2.orgTaxID [Rut OOPP]
		,o2.orgActivity [Sector]
		,e2.eadCity [Region OOPP]		
		,cast([FechaAsignacionReclamoOOPP] as datetime) FechaAsignacionReclamoOOPP
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
		left join [DCCPPlatform].[dbo].[gblOrganization] o2 on o2.orgCode COLLATE Modern_Spanish_CI_AI = r.orgCode COLLATE Modern_Spanish_CI_AI
		left join [DCCPPlatform].[dbo].[gblOrganizationAddress] oa on o2.orgCode = oa.oraOrganization
		left join [DCCPPlatform].dbo.gblEnterpriseAddress e2 on oa.oraAddress  = e2.eadCode 
        left join [DCCPReclamos].[dbo].[Par_TipoReclamo] tr on tr.idtiporeclamo=mr.idtiporeclamo
                     where  o.orgTaxID in ('76.082.106-3') 
                     
                     and  year(FechaIngresoReclamo) >=2014