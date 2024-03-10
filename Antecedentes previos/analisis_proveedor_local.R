
library(dplyr)
library(lubridate)
library(ggplot2)
library(RODBC)

# Conexiones
myconn <- RODBC::odbcConnect("DW_new", uid="datawarehouse" , pwd="datawarehouse")

# datos de proveedores locales considerando la info del SII
data_oc <- sqlQuery(myconn,"

SELECT 
	YEAR,
	Procedimiento,
	ES_PROV_LOCAL AS es_prov_local,
	CASE idtamanooficial
		WHEN 1 THEN 'Grande' 
		WHEN 2 THEN 'Mipyme'  
		WHEN 3 THEN 'Mipyme'  
		WHEN 4 THEN 'Mipyme'  
		WHEN 5 THEN 'Sin Dato'
		ELSE 'Mipyme' END AS Tamanio,
	sum(Montousd+impuestousd) as monto_usd
FROM Estudios.dbo.TMP_prov_locales_2023
WHERE 
--YEAR = 2023 AND
--(MontoUTM+ImpuestoUTM)>30 AND (MontoUTM+ImpuestoUTM)<=100
(MontoUTM+ImpuestoUTM)<=30
GROUP BY YEAR,
	Procedimiento,
	ES_PROV_LOCAL,
	CASE idtamanooficial
		WHEN 1 THEN 'Grande' 
		WHEN 2 THEN 'Mipyme'  
		WHEN 3 THEN 'Mipyme'  
		WHEN 4 THEN 'Mipyme'  
		WHEN 5 THEN 'Sin Dato'
		ELSE 'Mipyme' END
ORDER BY 
	YEAR,
	Procedimiento,
	es_prov_local,
	Tamanio                 
 
                    ")


data_oc  %>%
  group_by(YEAR,Procedimiento) %>%
  summarise(monto = sum(monto_usd)) %>%
  ggplot(aes(x = YEAR, y = monto, color = Procedimiento)) +
  geom_line()


data_oc_region <- sqlQuery(myconn,"

SELECT dt.Year,PL.Procedimiento,ES_PROV_LOCAL,
  ( select distinct region 
    from dm_transaccional.dbo.dimlocalidad dl 
    where pl.id_regionProv=dl.IDRegion) as region_prov_desc,
  ( select distinct region 
    from dm_transaccional.dbo.dimlocalidad dl 
    where pl.id_regionComp=dl.IDRegion) as region_comp_desc,
  CASE 
    WHEN (pl.IdTamanoOficial IN (2,3,4) OR pl.IdTamanoOficial IS NULL) THEN 'Mipyme'
    WHEN pl.IdTamanoOficial =5 THEN 'Sin Dato'
    WHEN pl.IdTamanoOficial =1 THEN 'Grande'
    ELSE 'OTRO' END tamano_descripcion, 
  sum(pl.montoUSD+pl.impuestoUSD) as MONTO_TRANSADO,
  COUNT(pl.porid) as NRO_OC,
  COUNT( DISTINCT DP.entcode) NRO_PROVEEDORES
FROM Estudios.dbo.TMP_prov_locales_2023 pl
LEFT JOIN DM_TRANSACCIONAL.dbo.DimTiempo DT ON pl.IDFechaEnvioOC = dt.DateKey
LEFT JOIN DM_TRANSACCIONAL.dbo.dimproveedor DP on pl.IDSucursal = DP.IDSucursal
WHERE DT.YEAR IN (2023) AND pl.Procedimiento ='CA'
GROUP BY DT.YEAR,PL.Procedimiento,ES_PROV_LOCAL,pl.id_regionProv,pl.id_regionComp
,CASE 
	WHEN (pl.IdTamanoOficial IN (2,3,4) OR pl.IdTamanoOficial IS NULL) THEN 'Mipyme'
	WHEN pl.IdTamanoOficial = 5 THEN 'Sin Dato'
	WHEN pl.IdTamanoOficial = 1 THEN 'Grande'
	ELSE 'OTRO'
END
                    
")

# proveedores locales
data_oc_region %>%
  group_by(region_comp_desc,region_prov_desc,ES_PROV_LOCAL) %>%
  summarise(monto_total = sum(MONTO_TRANSADO)) %>%
  group_by(region_comp_desc) %>%
  mutate(monto_total_region = sum(monto_total),
         particip = monto_total/monto_total_region) %>%
  filter(ES_PROV_LOCAL == 1) %>%
  ggplot(aes(x =  particip, y = monto_total_region/1000000, label = region_comp_desc)) +
  geom_point() +
  geom_label() +
  labs(title = 'Compras a Proveedores Locales mediante Compra Ágil',
       subtitle = 'Sin considerar la RM, 2023',
       y = 'Monto USD, MM.') 

# proveedores locales sin la RM
data_oc_region %>%
  group_by(region_comp_desc,region_prov_desc,ES_PROV_LOCAL) %>%
  summarise(monto_total = sum(MONTO_TRANSADO)) %>%
  group_by(region_comp_desc) %>%
  mutate(monto_total_region = sum(monto_total),
         particip = monto_total/monto_total_region) %>%
  filter(ES_PROV_LOCAL == 1 & region_comp_desc != 'Metropolitana') %>%
  ggplot(aes(x =  particip, y = monto_total_region/1000000, label = region_comp_desc)) +
  geom_point() +
  geom_label() +
  labs(title = 'Compras a Proveedores Locales mediante Compra Ágil',
       subtitle = 'Sin considerar la RM, 2023',
       y = 'Monto USD, MM.') 

# tabla con la principal region transando, sin considerar proveedores locales
data_oc_region %>%
  group_by(region_comp_desc,region_prov_desc,ES_PROV_LOCAL) %>%
  summarise(monto_total = sum(MONTO_TRANSADO)) %>%
  group_by(region_comp_desc) %>%
  mutate(monto_total_region = sum(monto_total),
         particip = monto_total/monto_total_region) %>%
  filter(ES_PROV_LOCAL == 0) %>%
  arrange(region_comp_desc,desc(monto_total)) %>%
  group_by(region_comp_desc) %>%
  top_n(1)

  # ggplot(aes(x =  particip, y = monto_total_region/1000000, label = region_comp_desc)) +
  # geom_point() +
  # geom_label() +
  # labs(title = 'Compras a Proveedores Locales mediante Compra Ágil',
  #      subtitle = 'Sin considerar la RM, 2023',
  #      y = 'Monto USD, MM.') 

  
# tabla con la principal region transando, sin considerar proveedores locales
tabla <- data_oc_region %>%
  filter(ES_PROV_LOCAL == 0) %>%
  group_by(region_comp_desc,region_prov_desc,tamano_descripcion) %>%
  summarise(monto_total = sum(MONTO_TRANSADO)) %>%
  group_by(region_comp_desc,region_prov_desc) %>%
  mutate(monto_total_region = sum(monto_total),
         particip = monto_total/monto_total_region) %>%
  arrange(region_comp_desc,desc(monto_total_region))


