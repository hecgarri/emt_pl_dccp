
rm(list=ls())
gc()

library(odbc)
library(RODBC)
library(AzureStor)
library(zip)

con2 <- odbcConnect("Aquiles", uid = "datawarehouse", pwd = "datawarehouse")

reclamos <- sqlQuery(con2,"

                SELECT  [idReclamo]
      ,er.NombreEstado
                     ,mr.NombreMotivoReclamo
                     ,tr.NombreTipoReclamo
                     ,cast([FechaIngresoReclamo] as datetime) FechaIngresoReclamo
                     ,[NombreReclamante] 
                     ,[RutReclamante]
                     ,[DetalleReclamo]
                     ,[NombreOOPP]
                   ,cast([FechaAsignacionReclamoOOPP] as datetime) FechaAsignacionReclamoOOPP
                    -- , 'http://www.mercadopublico.cl/fichaLicitacion.html?idLicitacion='+[NumLicOC] AS Link	
                     ,[NumLicOC]
                     ,[idReclamoCRM]
                     ,[RespuestaOOPP]
                     ,r.[orgCode]
                     ,r.[orgName]
                
                     ,[entCodeReclamante]
                     ,o.orgtaxid 'RUT_empresa'
                     ,cast([FechaRecepcionConforme] as date) FechaRecepcionConforme
                     
                     FROM [DCCPReclamos].[dbo].[Reclamo] r left join
                     [DCCPReclamos].[dbo].[Par_EstadoReclamo] er on er.idEstado=r.idEstado left join
                     [DCCPReclamos].[dbo].[Par_MotivoReclamo] mr on mr.idMotivoReclamo=r.idMotivoReclamo left join
                     [DCCPPlatform].[dbo].[gblOrganization] o on o.orgenterprise=r.entcodereclamante left join
                     [DCCPReclamos].[dbo].[Par_TipoReclamo] tr on tr.idtiporeclamo=mr.idtiporeclamo
                     where  orgTaxID in ('76.082.106-3') 
                     
                     and  year(FechaIngresoReclamo) >=2014",as.is=T)




setwd("C:/o/OneDrive - DCCP/Documents/transparencias")



nombre <- "Reclamos_76082106-3"
nombreCSV <- paste0(nombre,".csv")
nombreZIP <- paste0(nombre,".zip")


#escribir tabla en csv
write.csv2(reclamos, nombreCSV, row.names = FALSE)

zip(nombreZIP,nombreCSV)
unlink(nombreCSV)



##-----------------------------------------Cargar datos a BLOB "trnspchc" de AZURE----------------------
#Token
bl_endp_key <- storage_endpoint("https://transparenciachc.blob.core.windows.net", key="YAnO9hVnk3w5Pi4JlwiBm7eLTbzRDAwzGjiazzavDmosdinXT3sMVnixw2uAVBG22/1WwPXUc1F3QUd63nmBgw==")
##nombre del contenedor
cont <- storage_container(bl_endp_key, "trnspchc")

##Cargar datos a BLOB "oc-ag" de AZURE
arch <- nombreZIP
storage_multiupload(cont, src=arch)
unlink(nombreZIP)


link <- paste0("https://transparenciachc.blob.core.windows.net/trnspchc/",nombreZIP)
print(link)

