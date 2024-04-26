rm(list = ls())

# Carga de paquetes ================================

library(odbc)
library(DBI)

# Conexión a la base de datos ========================
con4 <- dbConnect(odbc(), Driver = "ODBC Driver 17 for SQL Server", Server = "10.34.71.202", 
                  Database = "DM_Transaccional", UID = "datawarehouse", PWD = "datawarehouse")

# Definir la clase QueryBuilder =========================================
# 
setClass("QueryBuilder", slots = list(
  fields = "character",
  from = "character",
  where = "character",
  joins = "list",
  parameters = "list"
))

# Constructor para la clase QueryBuilder
QueryBuilder <- function(fields = NULL, from = NULL, where = NULL, joins = NULL, parameters = NULL) {
  new("QueryBuilder", fields = fields, from = from, where = where, joins = joins, parameters = parameters)
}

# Método para agregar campos a seleccionar
setGeneric("add_select_fields", function(query, ...) standardGeneric("add_select_fields"))
setMethod("add_select_fields", signature("QueryBuilder"), function(query, ...) {
  query@fields <- c(query@fields, ...)
  return(query)
})

# Método para definir la tabla principal
setGeneric("set_from", function(query, table) standardGeneric("set_from"))
setMethod("set_from", signature("QueryBuilder"), function(query, table) {
  query@from <- table
  return(query)
})

# Método para agregar condiciones WHERE
setGeneric("add_where", function(query, condition) standardGeneric("add_where"))
setMethod("add_where", signature("QueryBuilder"), function(query, condition) {
  if (is.null(query@where)) {
    query@where <- condition
  } else {
    query@where <- paste(query@where, "AND", condition)
  }
  return(query)
})

# Método para agregar joins
setGeneric("add_join", function(query, table, condition, type) standardGeneric("add_join"))
setMethod("add_join", signature("QueryBuilder"), function(query, table, condition, type) {
  last_index <- length(query@joins)
  query@joins[[last_index + 1]] <- list(table = table, condition = condition, type = type)
  return(query)
})


# Método para agregar parámetros
setGeneric("add_parameter", function(query, name, value) standardGeneric("add_parameter"))
setMethod("add_parameter", signature("QueryBuilder"), function(query, name, value) {
  query@parameters[[name]] <- value
  return(query)
})

# Método para generar la consulta final
setGeneric("build", function(query) standardGeneric("build"))
setMethod("build", signature("QueryBuilder"), function(query) {
  query_string <- paste("SELECT", paste(query@fields, collapse = ", "), "FROM", query@from)
  if (length(query@joins) > 0) {
    for (join in query@joins) {
      query_string <- paste(query_string, join[["type"]], join[["table"]], "ON", join[["condition"]])
    }
  }
  
  if (!is.null(query@where)) {
    query_string <- paste(query_string, "WHERE", query@where)
  }
  return(list(query_string = query_string, parameters = query@parameters))
})

# Explora características de la clase ====================

getClass("QueryBuilder")
methods(class = "QueryBuilder")

# Fija los inputs =========================================

input <- list()

input$fecha <- c('2022-01-01','2022-02-01')
input$detalle = TRUE
input$prv_detalle = TRUE
input$procedencia <- c('Licitación Pública', 'Convenio Marco')
input$detalle_lic <- TRUE
input$detalle_cm <- TRUE



query <- QueryBuilder(
  fields = c("T.Year"
             ,"L.Region","L.Comuna"
             , "T.Date [Fecha Envío OC]"
             ,"REPLACE(REPLACE(REPLACE(REPLACE(OC.NombreOC, 'CHAR(13)', ''), CHAR(10), ''),';',','),'~',' ') AS [NombreOC]"
             ,"OC.CodigoOC"
             ,"REPLACE(OC.MonedaOC, '', 'Sin Tipo') [Tipo de moneda]"
             ,"OC.MontoUSD [Monto neto OC (dólares)]"
             ,"OC.MontoCLP [Monto neto OC (pesos)]"
             ,"OC.MontoCLF [Monto neto OC (UF)]"
             ,"OC.ImpuestoUSD [Impuesto OC (dólares)]"
             ,"OC.ImpuestoCLP [Impuesto OC (pesos)]"
             ,"OC.ImpuestoCLF [Impuesto OC (UF)]"
             ,"C.RUTUnidaddeCompra [RUT Unidad de Compra]"
             ,"UPPER(C.NombreUnidaddeCompra) [Nombre Unidad de Compra]"
             ,"I.entCode [entCode (Comprador)]"
             ,"REPLACE(REPLACE(REPLACE(UPPER(I.NombreInstitucion), CHAR(13), ''), CHAR(10), ''),';',',') AS [Nombre Institucion]"
             ,"S.Sector"
             ,"(CASE  WHEN OC.porisintegrated=3 THEN 'Compra Agil'
               ELSE (CASE  OC.IDProcedenciaOC
                     WHEN 703 THEN 'Convenio Marco'
                     WHEN 701 THEN 'Licitación Pública'
                     WHEN 1401 THEN 'Licitación Pública'
                     WHEN 702 THEN 'Licitación Privada'
                     ELSE 'Trato Directo' END)
               END) [Procedencia]"
    
  )
  ,from = "[DM_Transaccional].[dbo].[THOrdenesCompra] as OC"
  ,joins = list(
    list(table ="[DM_Transaccional].[dbo].[DimTiempo] as T", condition = "OC.IDFechaEnvioOC = T.DateKey", type = "INNER JOIN")
    ,list(table ="[DM_Transaccional].[dbo].[DimComprador] as C",condition ="OC.IDUnidaddeCompra = C.IDUnidaddeCompra" , type = "INNER JOIN")
    ,list(table = "[DM_Transaccional].[dbo].[DimInstitucion] as I", condition = "C.entCode = I.entCode",type = "INNER JOIN")
    ,list(table = "[DM_Transaccional].[dbo].[DimLocalidad] as L", condition = "L.IDLocalidad = C.IDLocalidadUnidaddeCompra", type = "INNER JOIN")
    ,list(table = "[DM_Transaccional].[dbo].[DimSector] as S", condition = "I.IdSector = S.IdSector", type = "INNER JOIN")
  )
  ,where = "OC.IDFechaEnvioOC BETWEEN ? AND ?"
  ,parameters = list(start_date = gsub("-", "", input$fecha[1]), end_date = gsub("-", "", input$fecha[2]))
  
)

if (input$detalle){
  query <- add_select_fields(query
    ,"OL.Monto [Monto (neto) producto]"
    ,"REPLACE(REPLACE(REPLACE(OL.NombreItem, CHAR(13), ''), CHAR(10), ''),';',',') AS [Nombre producto]"
    ,"REPLACE(REPLACE(REPLACE(REPLACE(OL.DescripcionItem, CHAR(13), ''), CHAR(10), ''),';',','),'~','') AS DescripcionItem"
    ,"RU.RubroN1 [Rubro Producto]")
  
  query <- add_join(query, table = '[DM_Transaccional].[dbo].[THOrdenesCompraLinea] as OL'
                            , condition = 'OC.porID = OL.porID'
                            , type = 'INNER JOIN')
  query <- add_join(query, table = '[DM_Transaccional].[dbo].[DimProducto] as DPR'
                            , condition = 'OL.IDProducto = DPR.IDProducto'
                            , type = 'INNER JOIN')
  query <- add_join(query, table = '[DM_Transaccional].[dbo].[DimRubro] as RU'
                            ,condition = 'DPR.IdRubro = RU.IdRubro'
                            ,type = 'INNER JOIN')
}


if (input$prv_detalle){
  query <- add_select_fields(query, 
    "REPLACE(REPLACE(REPLACE(UPPER(P.RazonSocialSucursal), CHAR(13), ''), CHAR(10), ''),';',',') AS [Razón social Proveedor]"
    ,"E.entCode [entCode (Proveedor)]"
    ,"P.RUTSucursal [Rut Proveedor]"
    ,"L2.Region [Región Proveedor]"
    ,"DTP.Tamano [Tamaño Proveedor]"
  )
  
  query <- add_join(query
                    , table = "[DM_Transaccional].[dbo].[DimProveedor] as P"
                    , condition = "OC.IDSucursal=P.IDSucursal"
                    ,type = "INNER JOIN")
  query <- add_join(query
                    ,table = "[DM_Transaccional].[dbo].[DimEmpresa] as E"
                    ,condition = "P.entCode = E.entCode"
                    , type = "INNER JOIN")
  query <- add_join(query
                    ,table = "[DM_Transaccional].[dbo].[DimLocalidad] as L2"
                    ,condition = "L2.IDLocalidad = P.IDLocalidadSucursal"
                    ,type = "INNER JOIN")
  query <- add_join(query
                    ,table = "[DM_Transaccional].[dbo].[THTamanoProveedor] as TP"
                    ,condition = "P.entCode=TP.entCode AND TP.AñoTributario = 2022"
                    ,type = "INNER JOIN")
  query <- add_join(query
                    ,table = "[DM_Transaccional].[dbo].[DimTamanoProveedor] as DTP"
                    ,condition = "TP.idTamano = DTP.IdTamano"
                    ,type = "LEFT JOIN")
}


if (any(input$procedencia =='Licitación Pública' | input$procedencia =='Licitación Privada')){
  if (input$detalle_lic){
    query <- add_select_fields(query
      ,"LIC.NumeroAdq"
      ,"LIC.NombreAdq"
      ,"LIC.Link"
      )
    
    query <- add_join(query
                    ,table = "DM_Transaccional.dbo.THOportunidadesNegocio as LIC"
                    ,condition = "OC.rbhCode=LIC.rbhCode"
                    ,type = "INNER JOIN")
  }
} else {
  NULL
}

if (any(input$procedencia %in% c('Convenio Marco'))){
  if (input$detalle_cm){
    query <- add_select_fields(query
      ,"CM.NombreCM"
      ,"CM.NroLicitacionPublica"
    )
    
    query <- add_join(query
                      ,table = "DM_Tienda.dbo.THOrdenesCompraLinea as OCM"
                      ,condition = "OC.porID=OCM.porID"
                      ,type = "INNER JOIN")
    
    query <- add_join(query
                      ,table = "DM_Tienda.dbo.DimConvenioMarco as CM"
                      ,condition = "OCM.IdConvenioMarco = CM.IdConvenioMarco"
                      ,type = "INNER JOIN")
  } else {
    NULL
  }
}


if (lubridate::year(input$fecha[1]) <= 2023) {
  query <- add_where(query, condition = "OC.EsDatoCerrado = 1")
} else if (lubridate::year(input$fecha[2]) >= 2024) {
  query <- add_where(query, condition = "OC.EsDatoActual = 1")
}

final_query <- build(query)


# Ejecutar la consulta ================
result <- dbGetQuery(con4, final_query$query_string, params = final_query$parameters)


# Agregar condición de región si no es "Todas las regiones"
if(!is.null(region_seleccionada)) {
  query <- paste0(query, " AND L.Region = '", region_seleccionada, "'")
}

# Agregar condición de región si no es "Todas las regiones"
if(!is.null(comuna_seleccionada)) {
  query <- paste0(query, " AND L.Comuna = '", comuna_seleccionada, "'")
}

# Agregar condición de RUT si el campo no es nulo
if(!is.null(rut_seleccionado)) {
  query <- paste0(query, " AND C.RUTUnidaddeCompra = '", rut_seleccionado, "'")
}

# Agregar condición de entCode si el campo no es nulo
if(!is.null(entcode_seleccionado)) {
  query <- paste0(query, " AND I.entCode = '", entcode_seleccionado, "'")
}

# Agregar condición de procedencia si no es "Todas las procedencias"
if(!is.null(procedencia_seleccionada)) {
  query <- paste0(query, " AND (CASE OC.porisintegrated WHEN 3 THEN 'Compra Agil'
                                              ELSE (CASE  OC.IDProcedenciaOC
                                              WHEN 703 THEN 'Convenio Marco'
                                              WHEN 701 THEN 'Licitación Pública'
                                              WHEN 1401 THEN 'Licitación Pública'
                                              WHEN 702 THEN 'Licitación Privada'
                                              ELSE 'Trato Directo' END) END) = '", procedencia_seleccionada, "'")
}

# Agregar condición de región si no es "Todas las regiones"
if(!is.null(institucion_seleccionada)) {
  query <- paste0(query, " AND I.NombreInstitucion = '", institucion_seleccionada, "'")
}
#rowser()


# Agregar condición de sector si no es "Todos los sectores"
if(!is.null(sector_seleccionado)) {
  query <- paste0(query, " AND S.Sector = '", sector_seleccionado, "'")
}



# Agregar condición de sector si no es "Todos los sectores"
if(!is.null(rubro_seleccionado)) {
  query <- paste0(query, " AND Ru.RubroN1 = '", rubro_seleccionado, "'")
}



# Agregar condición de sector si no es "Todos los sectores"
if(!is.null(convenio_seleccionado)) {
  query <- paste0(query, " AND CM.NombreCM = '", convenio_seleccionado, "'")
}

# Agrega filtro de RUT por proveedor 
# 

if (!is.null(rut_proveedor)){
  query <- paste0(query, " AND P.RUTSucursal = '",rut_proveedor ,"'")
}

if (!is.null(entcode_proveedor)){
  query <- paste0(query, "AND E.entCode = '",entcode_proveedor ,"'")
}

if (!is.null(tamano_proveedor)){
  query <- paste0(query, "AND DTP.Tamano = '",tamano_proveedor ,"'")
}

cat("La Query es la siguiente:"
    ,"\n ==============================================================\n\n"
    ,query
    ,"\n ==============================================================\n\n"
    ,"\n ==============================================================\n\n"
    ,"\n ==============================================================\n\n")

consulta_(query)