library(shiny)
library(RODBC)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(DT)
library(shinyjs)
library(sqldf)

# Establece conexiones a los diferentes servidores
con3 <- RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse")

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Consulta a base de datos"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("fecha", "Rango de fechas:",
                     start = Sys.Date() %m-% months(13),
                     end = Sys.Date() %m-% months(1)),
      uiOutput("region_select"),
      uiOutput("procedencia_select"), # Nuevo selectInput para procedencia
      uiOutput("institucion_select"), # Nuevo selectInput según institución
<<<<<<< HEAD
      uiOutput("sector_select"), #Nuevo selectInput según sector 
=======
>>>>>>> 3198ac5e88cc38320dd05a4c38959c0d31a7eca5
      actionButton("consultar", "Consultar"),
      downloadButton("downloadData", "Descargar Excel")
    ),
    mainPanel(
      DTOutput("resultado")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Definir una variable para almacenar las regiones disponibles
  regiones_disponibles <- NULL
  procedencias_disponibles <- NULL # Variable para procedencias
  institucion_disponibles <- NULL # Variable para procedencias
<<<<<<< HEAD
  sectores_disponibles <- NULL # variable para sectores
=======
>>>>>>> 3198ac5e88cc38320dd05a4c38959c0d31a7eca5
  
  # Cargar las regiones disponibles al iniciar la aplicación
  observe({
    # Ejecutar la consulta SQL para obtener las regiones
    regiones_disponibles <<- sqlQuery(con3, "SELECT DISTINCT L.Region, L.IDRegion FROM [DM_Transaccional].[dbo].[DimLocalidad] L")
    # Agregar la opción "Todas las regiones"
    regiones_disponibles <- rbind(data.frame(Region = "Todas las regiones", IDRegion = NA), regiones_disponibles)
  })
  
  # Cargar las regiones disponibles al iniciar la aplicación
  observe({
    # Obtener las procedencias disponibles
    procedencias_disponibles <<- unique(sqlQuery(con3, "SELECT DISTINCT 
                                                        CASE OC.porisintegrated 
                                                          WHEN 3 THEN 'Compra Agil'
                                                          ELSE (CASE  OC.IDProcedenciaOC
                                                                  WHEN 703 THEN 'Convenio Marco'
                                                                  WHEN 701 THEN 'Licitación Pública'
                                                                  WHEN 1401 THEN 'Licitación Pública'
                                                                  WHEN 702 THEN 'Licitación Privada'
                                                                  ELSE 'Trato Directo' 
                                                                END)
                                                        END AS Procedencia FROM [DM_Transaccional].[dbo].[THOrdenesCompra] OC"))
    # Agregar la opción "Todas las regiones"
    #procedencias_disponibles <- rbind(data.frame(Region = "Todas las procedencias", IDRegion = NA), procedencias_disponibles)
  })
  
  # Obtener las institucion disponibles al cambiar la región seleccionada
  # Aquí incluí un archivo rds que muestra la combinación de institucion y regiones a la fecha indicada
  observe({
    
    region_seleccionada <- input$region
    print(class(region_seleccionada))
    print(region_seleccionada)
    
    if (is.null(region_seleccionada) || region_seleccionada == "") {
      # Asigna un valor predeterminado si input$region es nulo o vacío
      region_seleccionada <- "Todas las regiones"
    }
    # Obtener la región seleccionada por el usuario
    if(region_seleccionada == "Todas las regiones") {
      # Si se selecciona "Todas las regiones", no se aplica filtro por región en la consulta SQL
      consulta_instituciones <- "SELECT DISTINCT 
                      UPPER([NombreInstitucion]) AS NombreInstitucion,
                      L.Region 
                      FROM [DM_Transaccional].[dbo].[DimInstitucion] AS I 
                      INNER JOIN [DM_Transaccional].[dbo].[DimComprador] AS C ON I.entCode = C.entCode
                      INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] AS L ON C.IDLocalidadUnidaddeCompra = L.IDLocalidad"
    } else {
      consulta_instituciones <- paste0("SELECT DISTINCT 
                            UPPER([NombreInstitucion]) AS NombreInstitucion
                            FROM [DM_Transaccional].[dbo].[DimInstitucion] AS I 
                            INNER JOIN [DM_Transaccional].[dbo].[DimComprador] AS C ON I.entCode = C.entCode
                            INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] AS L ON C.IDLocalidadUnidaddeCompra = L.IDLocalidad
                            WHERE L.Region = '", region_seleccionada, "'")
    }
    
    institucion_disponibles <<- sqlQuery(con3, consulta_instituciones)
  })
  
<<<<<<< HEAD
  # Cargar las regiones disponibles al iniciar la aplicación
  observe({
    # Ejecutar la consulta SQL para obtener las regiones
    sectores_disponibles <<- sqlQuery(con3, "SELECT DISTINCT [IdSector]
      ,[Sector]
  FROM [DM_Transaccional].[dbo].[DimSector]")
    # Agregar la opción "Todas las regiones"
    #regiones_disponibles <- rbind(data.frame(Region = "Todas las regiones", IDRegion = NA), regiones_disponibles)
  })
=======
  
>>>>>>> 3198ac5e88cc38320dd05a4c38959c0d31a7eca5
  
  # En el selectInput, puedes hacer la selección de "Todas las regiones"
  output$region_select <- renderUI({
    selectInput("region", "Selecciona una región:",
                choices = c("Todas las regiones", regiones_disponibles$Region),
                selected = "Todas las regiones")
  })
  
  # Nuevo selectInput para procedencia
  output$procedencia_select <- renderUI({
    selectInput("procedencia", "Selecciona una procedencia:",
                choices = c("Todas las procedencias", procedencias_disponibles$Procedencia),
                selected = "Todas las procedencias")
  })
  
  # Nuevo selectInput para institucion
  output$institucion_select <- renderUI({
    selectInput("institucion", "Selecciona una Institución:",
                choices = c("Todas las instituciones", institucion_disponibles$NombreInstitucion),
                selected = "Todas las instituciones")
  })
  
<<<<<<< HEAD
  # Nuevo selectInput para institucion
  output$sector_select <- renderUI({
    selectInput("sector", "Selecciona un sector:",
                choices = c("Todos los sectores", sectores_disponibles$Sector),
                selected = "Todos los sectores")
  })
  
=======
>>>>>>> 3198ac5e88cc38320dd05a4c38959c0d31a7eca5
  # Definir variable reactiva para almacenar los datos consultados
  datos_consultados <- reactiveVal(NULL)
  
  observeEvent(input$consultar, {
    # Realizar la consulta solo cuando se presiona el botón "Consultar"
    req(input$consultar)  # Espera a que se presione el botón "Consultar"
    
    # Obtener la región seleccionada por el usuario
    if(input$region == "Todas las regiones") {
      # Si se selecciona "Todas las regiones", no se aplica filtro por región en la consulta SQL
      region_seleccionada <- NULL
    } else {
      region_seleccionada <- input$region
    }
    
    # Obtener la procedencia seleccionada por el usuario
    if(input$procedencia == "Todas las procedencias") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      procedencia_seleccionada <- NULL
    } else {
      procedencia_seleccionada <- input$procedencia
    }
    
    # Obtener la Institución seleccionada por el usuario
    if(input$institucion == "Todas las instituciones") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      institucion_seleccionada <- NULL
    } else {
      institucion_seleccionada <- input$institucion
    }
    
<<<<<<< HEAD
    # Obtener la Institución seleccionada por el usuario
    if(input$sector == "Todos los sectores") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      sector_seleccionado <- NULL
    } else {
      sector_seleccionado <- input$sector
    }
    
=======
>>>>>>> 3198ac5e88cc38320dd05a4c38959c0d31a7eca5
    consulta <- paste0(
      "SELECT  
        T.Year
        ,L.Region
        ,T.Date [Fecha Envío OC]
        ,OC.NombreOC
<<<<<<< HEAD
        ,OC.CodigoOC
        ,OL.NombreItem [Nombre producto]
        ,OL.DescripcionItem
		,OC.MonedaOC [Tipo de moneda]
		,OL.Monto [Monto item] 
		,OC.MontoUSD [Monto total USD]
		,OC.MontoCLP [Monto total pesos]
		,OC.MontoCLF [Monto total UF]
=======
        ,OL.NombreItem [Nombre producto (ONU)]
        ,OL.DescripcionItem
>>>>>>> 3198ac5e88cc38320dd05a4c38959c0d31a7eca5
        ,C.RUTUnidaddeCompra [RUT Unidad de Compra]
        ,UPPER(C.NombreUnidaddeCompra) [Nombre Unidad de Compra]
        ,UPPER(I.NombreInstitucion) [Nombre Institucion]
        ,S.Sector
        ,(CASE  WHEN OC.porisintegrated=3 THEN 'Compra Agil'
                ELSE (CASE  OC.IDProcedenciaOC
                      WHEN 703 THEN 'Convenio Marco'
                      WHEN 701 THEN 'Licitación Pública'
                      WHEN 1401 THEN 'Licitación Pública'
                      WHEN 702 THEN 'Licitación Privada'
                      ELSE 'Trato Directo' END)
        END) [Procedencia]
        ,UPPER(P.RazonSocialSucursal) [RUT Proveedor]
        ,P.RUTSucursal
        ,L2.Region [Región Proveedor]
        ,DTP.Tamano [Tamaño Proveedor]
        ,RU.RubroN1 [Rubro Proveedor]
        ,U.RUT [Rut Usuario comprador]
        ,U.Nombres+' '+U.Apellidos as [Nombre Usuario comprador]
        ,U.eMail [Correo Usuario comprador]
        ,U.FechaUltLogin
      FROM [DM_Transaccional].[dbo].[THOrdenesCompra] as OC 
        INNER JOIN [DM_Transaccional].[dbo].[THOrdenesCompraLinea] as OL  ON OC.porID = OL.porID
        INNER JOIN [DM_Transaccional].[dbo].[DimProducto] as DPR ON  OL.IDProducto = DPR.IDProducto
        INNER JOIN [DM_Transaccional].[dbo].[DimRubro] as RU ON DPR.IdRubro = RU.IdRubro
        INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
        INNER JOIN [DM_Transaccional].[dbo].[DimProveedor] as P ON OC.IDSucursal=P.IDSucursal 
        INNER JOIN [DM_Transaccional].[dbo].[DimComprador] as C ON OC.IDUnidaddeCompra = C.IDUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L ON L.IDLocalidad = C.IDLocalidadUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L2 ON L2.IDLocalidad = P.IDLocalidadSucursal
        INNER JOIN [DM_Transaccional].[dbo].[DimEmpresa] as E ON P.entCode = E.entCode 
        INNER JOIN [DM_Transaccional].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
        INNER JOIN [DM_Transaccional].[dbo].[DimSector] as S ON I.IdSector = S.IdSector
        LEFT JOIN [DM_Transaccional].[dbo].[THTamanoProveedor] as TP ON P.entCode=TP.entCode AND TP.AñoTributario = 2022
        LEFT JOIN [DM_Transaccional].[dbo].[DimTamanoProveedor] as DTP ON TP.idTamano = DTP.IdTamano
        LEFT JOIN [Dm_Usuario].[dbo].[TablonUsuarioComprador] as U ON OC.usrID = U.usrID
      WHERE  OC.IDFechaEnvioOC BETWEEN '", gsub("-", "", input$fecha[1]), "' AND '", gsub("-", "", input$fecha[2]), "'
        AND OC.IDEstadoOC IN  (4,5,6,7,12)"
    )
    
    # Agregar condición de región si no es "Todas las regiones"
    if(!is.null(region_seleccionada)) {
      consulta <- paste0(consulta, " AND L.Region = '", region_seleccionada, "'")
    }
    
    # Agregar condición de procedencia si no es "Todas las procedencias"
    if(!is.null(procedencia_seleccionada)) {
      consulta <- paste0(consulta, " AND (CASE OC.porisintegrated WHEN 3 THEN 'Compra Agil'
                                              ELSE (CASE  OC.IDProcedenciaOC
                                              WHEN 703 THEN 'Convenio Marco'
                                              WHEN 701 THEN 'Licitación Pública'
                                              WHEN 1401 THEN 'Licitación Pública'
                                              WHEN 702 THEN 'Licitación Privada'
                                              ELSE 'Trato Directo' END) END) = '", procedencia_seleccionada, "'")
    }
    
    # Agregar condición de región si no es "Todas las regiones"
    if(!is.null(institucion_seleccionada)) {
      consulta <- paste0(consulta, " AND I.NombreInstitucion = '", institucion_seleccionada, "'")
    }
<<<<<<< HEAD
    
    # Agregar condición de sector si no es "Todos los sectores"
    if(!is.null(sector_seleccionado)) {
      consulta <- paste0(consulta, " AND S.Sector = '", sector_seleccionado, "'")
    }
    
    
=======
>>>>>>> 3198ac5e88cc38320dd05a4c38959c0d31a7eca5
    withProgress(message = "Realizando consulta a la base de datos", {
    resultado <- sqlQuery(con3, consulta)
    datos_consultados(resultado)
    updateActionButton(session, "consultar", label = "Consultar", icon = icon("search"))
    })
  })
  
  output$resultado <- renderDT({
    # Renderiza los datos en la tabla DT
    req(datos_consultados())  # Requiere que los datos estén disponibles
    datatable(datos_consultados())
  })
  
  # Lógica para la descarga del archivo Excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Crear un libro de Excel
      wb <- openxlsx::createWorkbook()
      # Agregar una hoja de trabajo
      sheet_name <- "Datos"
      openxlsx::addWorksheet(wb, sheet_name)
      # Escribir los datos en la hoja de trabajo
      openxlsx::writeData(wb, sheet = sheet_name, x = datos_consultados())
      # Guardar el libro de Excel
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
