select distinct
u.usrFirstName+u.usrLastName [Nombre]
,u.usrTaxID
,u.usrPhone	
,u.usrPosition
,u.usremail
,u.usrLastLogin
,E.entName
,o.orgTaxID
,COUNT(OC.CodigoOC) [Total OC]
,SUM(OC.MontoCLP+OC.ImpuestoCLP) [MontoCLP]
,SUM(OC.MontoUSD+OC.ImpuestoUSD) [MontoUSD]
from DCCPPlatform.dbo.gblOrganization as O 
inner join estudios_procesamientodatosabiertos.dbo.THOrdenesCompra as OC ON OC.IDSucursal =O.orgCode 
inner join estudios_procesamientodatosabiertos.dbo.DimTiempo as T ON OC.IDFechaEnvioOC=T.DateKey
inner join DCCPProcurement.dbo.prcPOHeader as PO ON OC.CodigoOC = PO.porCode
inner join DCCPPlatform.dbo.gblUser as U ON U.usrID = PO.porSellerUser
inner join DCCPPlatform.dbo.gblEnterprise as E ON O.orgEnterprise =E.entCode 
where E.entCode collate Modern_Spanish_CI_AI  in (SELECT distinct s.EntCode
                            FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                            WHERE EntCode NOT IN ('N/A') AND
                            ((s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                            (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= 2024) and
                            (year(s.fechacreacion)<= 2024)))
and o.orgClass=1
and T.Year = 2023
group by 
u.usrFirstName+u.usrLastName
,u.usrTaxID
,u.usrPhone	
,u.usrPosition
,u.usremail
,u.usrLastLogin
,E.entName
,o.orgTaxID