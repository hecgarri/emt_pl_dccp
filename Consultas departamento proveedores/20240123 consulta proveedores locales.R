#Quita archivos del WorkSpace ===========================================================

rm(list = ls())

#Fija el directorio de trabajo ==========================================================

wd_path = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/datos/"

data_path = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/datos"

setwd(wd_path)

#Carga de paquetes necesarios para el análisis ==========================================

load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

packages = c("tidyverse" #Conjunto integral de paquetes para manipular y analizar datos de manera coherente y eficiente.
             , "RODBC" #facilita la conexión y manipulación de bases de datos a través de ODBC (Open Database Connectivity).
             , "plotly" #proporciona herramientas interactivas para la creación de gráficos dinámicos y visualizaciones interactivas
             , "data.table" #Paquete optimizado para manipulación eficiente de grandes conjuntos de datos, destacando por su velocidad y funcionalidades avanzadas.
             , "formattable"
             , "hutils"
             , "readr"
             , "VennDiagram"
             , "RColorBrewer"
             , "openxlsx"
             , "shiny"
             , "ggplot2"
             , "DT"
             , "shinyjs"
             , "sqldf")

load_pkg(packages)

# Llama a la función consultar
# Más adelante quiero hacer un paquete con estas funcinoes

main_folder <- "C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/"

source(paste0(main_folder,"Consultas departamento proveedores/20240314_funcion_consultar.R"))


detalles = function(path = wd_path, pattern = "*.rds"){
  require(dplyr)
  
  details = file.info(path = paste0(wd_path), list.files(pattern=pattern))
  
  details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ] %>% 
    filter(isdir==FALSE)
  
  details$files = rownames(details)
  
  rownames(details) = NULL
  
  return(details)
}

datasets <- detalles(path = wd_path)

tipoConsulta <- 'órdenes de compra'

file.path(data_path, datasets[grep(gsub(" ","_", tipoConsulta), datasets$files)[1],c("files")])


years <- c(2023,2024)

datos <- consultar_y_guardar(wd_path = data_path
                             , x = 12
                             , y = years
                             , tipoConsulta = "cotizaciones compra ágil"
                             , depurar = FALSE
                             , updated = TRUE)

datos_usuarios <- consultar_y_guardar(wd_path = data_path
                             , x = 12
                             , y = years
                             , window = -11
                             , tipoConsulta = "usuarios proveedores"
                             , depurar = FALSE
                             , updated = TRUE)


data_ag <- datos %>% 
  group_by(year,level1, `Región de Despacho`,`Proveedor Local 2`) %>%
  summarise(solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,ofertas = n_distinct(CodigoCotizacion)
            ,proveedores = n_distinct(entCode)
            ,MontoTotalProducto = sum(MontoTotalProducto*EsProveedorSeleccionado, na.rm = TRUE)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1+`Región de Despacho`~`Proveedor Local 2`
                    , value.var = c("solicitudes", "ofertas", "proveedores", "MontoTotalProducto")) %>% 
  mutate(interseccion = ifelse(`solicitudes_Local`>`solicitudes_No Local`
                               & ofertas_Local >`ofertas_No Local`
                               & proveedores_Local>`proveedores_No Local`,"Sí","No")
         ,union = ifelse(`solicitudes_Local`>`solicitudes_No Local`
                        | ofertas_Local >`ofertas_No Local`
                        | proveedores_Local>`proveedores_No Local`, "Sí", "No"))  

print(data_ag, n=5)

ranking_proveedores <- datos %>%
  filter(EsProveedorSeleccionado==1
         ,year == 2023
         ,`Región de Despacho` == 12) %>% 
  group_by(entCode, `Región de Despacho`,`Razón Social Proveedor`, `Proveedor Local 2`, id_regionProv) %>%
  summarise(solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,ofertas = n_distinct(CodigoCotizacion)
            ,MontoTotalProducto = sum(MontoTotalProducto*EsProveedorSeleccionado, na.rm = TRUE)) %>% 
  inner_join(datos_usuarios %>% rename(entCode = EntCode), by = "entCode") %>% 
  select(`Rut Proveedor`, `Razon social`,`id_regionProv`, `Proveedor Local 2`,`solicitudes`,`ofertas`,`MontoTotalProducto`,`usrLastLogin`, `usrEmail`, `Nombre Usuario`,
         `usrTaxID`,`usrMobile`, `usrPhone`,`usrPosition`)
  
  



#Revisar este cálculo! 
#
data_ag_productos <- datos %>% 
  #filter(year == 2023) %>% 
  group_by(year,level1, `Región de Despacho`,`Proveedor Local 2`, `productoname`) %>%
  #distinct(CodigoSolicitudCotizacion, .keep_all = TRUE) %>% 
  summarise(solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,ofertas = n_distinct(CodigoCotizacion)
            ,proveedores = n_distinct(entCode)
            ,MontoTotalProducto = sum(MontoTotalProducto*EsProveedorSeleccionado, na.rm = TRUE)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1+`Región de Despacho`+productoname~`Proveedor Local 2`
                    , value.var = c("solicitudes", "ofertas", "proveedores", "MontoTotalProducto")) %>% 
  mutate(interseccion = ifelse(`solicitudes_Local`>`solicitudes_No Local`
                               & ofertas_Local >`ofertas_No Local`
                               & proveedores_Local>`proveedores_No Local`,"Sí","No")
         ,union = ifelse(`solicitudes_Local`>`solicitudes_No Local`
                         | ofertas_Local >`ofertas_No Local`
                         | proveedores_Local>`proveedores_No Local`, "Sí", "No"))  

tabla_y <- data_ag %>% 
  group_by(year,level1, interseccion) %>% 
  filter(year == 2023) %>% 
  summarise(regiones = n()
            ,monto = sum(MontoTotalProducto_Grande+MontoTotalProducto_Local+`MontoTotalProducto_No Local`)) %>% 
  arrange(desc(interseccion)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1~interseccion, value.var = c("regiones", "monto"))

tabla_o <- data_ag %>% 
  group_by(year,level1, union) %>% 
  filter(year == 2023) %>% 
  summarise(regiones = n()
            ,monto = sum(MontoTotalProducto_Grande+MontoTotalProducto_Local+`MontoTotalProducto_No Local`)) %>% 
  #arrange(desc(interseccion)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1~union, value.var = c("regiones", "monto"))

segun_rg <- datos %>% 
  group_by(year,level1, `Región de Despacho`,id_regionProv) %>%
  #distinct(CodigoSolicitudCotizacion, .keep_all = TRUE) %>% 
  summarise(solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,ofertas = n_distinct(CodigoCotizacion)
            ,proveedores = n_distinct(entCode)
            ,MontoTotalProducto = sum(MontoTotalProducto, na.rm = TRUE)) %>% 
  setDT() %>% 
  data.table::dcast(year+level1+`Región de Despacho`~`id_regionProv`
                    , value.var = c("solicitudes", "ofertas", "proveedores", "MontoTotalProducto")) 

wb <- createWorkbook()

addWorksheet(wb, sheetName = 'Proveedor Local - Rubro')

writeData(wb, sheet = 'Proveedor Local - Rubro', x = data_ag)

addWorksheet(wb, sheetName = 'Proveedor Local - Producto')

writeData(wb, sheet = 'Proveedor Local - Producto', x = data_ag_productos)

addWorksheet(wb, sheetName = 'Tabla (Y)')

writeData(wb, sheet = 'Tabla (Y)', x = tabla_y)

addWorksheet(wb, sheetName = 'Tabla (O)')

writeData(wb, sheet = "Tabla (O)", x = tabla_o)

addWorksheet(wb, sheetName = 'Proveedores inter - Rubro')

writeData(wb, sheet = "Proveedores inter - Rubro", x = segun_rg)

saveWorkbook(wb, "20240308 estadísticas proveedores.xlsx", overwrite = TRUE)


wb <- createWorkbook()

addWorksheet(wb, sheetName = 'Datos proveedores')

writeData(wb, sheet = 'Datos proveedores', x = ranking_proveedores)

saveWorkbook(wb, "20240314 contacto proveedores CAg Magallanes.xlsx", overwrite = TRUE)


# library(dplyr)
# 
# # Crear un data frame de ejemplo
# df <- data.frame(
#   var_1_column = c(1, 2, 3),
#   column_var_2 = c(4, 5, 6),
#   column_3_var = c(7, 8, 9)
# )
# 
# # Operación dependiendo del nombre de la columna
# df <- df %>%
#   mutate(across(
#     everything(),
#     ~ case_when(
#       grepl("var_1", names(df)) ~ . * 2,       # Duplicar los valores de las columnas que contienen "var_1" en su nombre
#       grepl("var_2", names(df)) ~ . + 10,      # Sumar 10 a los valores de las columnas que contienen "var_2" en su nombre
#       grepl("var_3", names(df)) ~ . - 5,       # Restar 5 a los valores de las columnas que contienen "var_3" en su nombre
#       TRUE ~ .                                  # Si ninguna condición coincide, mantener el valor original
#     )
#   ))
# 
# print(df)

