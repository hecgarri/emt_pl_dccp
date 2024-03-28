library(shiny)
library(RODBC)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(DT)
library(shinyjs)
library(sqldf)
library(cowplot)
library(plotly)
library(markdown)

# Establece conexiones a los diferentes servidores
con3 <- RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse")

# Aquí va la Interfaz de usuario
# 
{
  ui <- fluidPage(
    
    titlePanel("Consulta a base de datos"),
    
    tabsetPanel(
      tabPanel("Transacciones",
               sidebarLayout(
                 sidebarPanel(
                   dateRangeInput("fecha", "Rango de fechas:",
                                  start = Sys.Date() %m-% months(14),
                                  end = Sys.Date() %m-% months(13)),
                   h5(HTML("<b>Detalle por producto:</b>")),
                   radioButtons("detalle", label = NULL,
                                choices = list("Sí" = TRUE, "No" = FALSE),
                                selected = TRUE),
                   uiOutput("region_select"),
                   uiOutput("procedencia_select"), # Nuevo selectInput para procedencia
                   uiOutput("institucion_select"), # Nuevo selectInput según institución
                   uiOutput("sector_select"), #Nuevo selectInput según sector 
                   actionButton("consultar_btn", "Consultar"),
                   downloadButton("downloadData_transacciones", "Descargar Excel")
                 ),
                 mainPanel(
                   tags$head(
                     tags$style(HTML("
          .title {
            font-size: 24px;
            font-weight: bold;
            margin-bottom: 10px;
          }
          .description {
            font-size: 16px;
            margin-bottom: 20px;
          }
        "))
                   ),
        tags$div(
          # tags$p(class = "title", "Vista resumida"),
          # tags$p(class = "description", "Aquí podrás ver un resumen de la data en términos de cantidad de órdenes de compra y montos transados. 
          #        Cualquier problema de funcionamiento, informarlo a hector.garrido@chilecompra.cl"),
          # # Agrega aquí el contenido principal de tu aplicación, como la tabla o el gráfico
          
          #DTOutput("resultado")
          plotlyOutput("combined_plot")
        )
        
        
                 )
        
               )
      )
      
      
      ,tabPanel("Usuarios",
                
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput("usr_fecha", "Rango de fechas:",
                                   start = Sys.Date() %m-% months(14),
                                   end = Sys.Date() %m-% months(13)),
                    uiOutput("usr_region_select"),
                    uiOutput("usr_procedencia_select"), # Nuevo selectInput para procedencia
                    uiOutput("usr_institucion_select"), # Nuevo selectInput según institución
                    uiOutput("usr_sector_select"), #Nuevo selectInput según sector 
                    actionButton("usr_consultar_btn", "Consultar"),
                    downloadButton("usr_downloadData", "Descargar Excel")
                  ),
                  mainPanel(
                    tags$head(
                      tags$style(HTML("
          .title {
            font-size: 24px;
            font-weight: bold;
            margin-bottom: 10px;
          }
          .description {
            font-size: 16px;
            margin-bottom: 20px;
          }
        "))
                    ),
        tags$div(
          # tags$p(class = "title", "Vista resumida"),
          # tags$p(class = "description", "Aquí podrás ver un resumen de la data en términos de cantidad de órdenes de compra y montos transados. 
          #        Cualquier problema de funcionamiento, informarlo a hector.garrido@chilecompra.cl"),
          # # Agrega aquí el contenido principal de tu aplicación, como la tabla o el gráfico
          
          DTOutput("usr_resultado")
          #plotlyOutput("usr_combined_plot")
        )
                  )
                )
      )
      
    )
  )
  
}

# Aquí va el Servidor
server <- function(input, output, session) {
  
  # Aquí defino variables vacías para llenarlas con los parámetros región, procedencia, institución y sector
  {
    regiones_disponibles <- NULL
    procedencias_disponibles <- NULL # Variable para procedencias
    institucion_disponibles <- NULL # Variable para procedencias
    sectores_disponibles <- NULL # variable para sectores  
  }
  
  
  # Cargar las regiones disponibles al iniciar la aplicación
  observe({
    # Ejecutar la consulta SQL para obtener las regiones
    regiones_disponibles <<- sqlQuery(con3, "SELECT DISTINCT L.Region, L.IDRegion FROM [DM_Transaccional].[dbo].[DimLocalidad] L")
    # Agregar la opción "Todas las regiones"
    regiones_disponibles <- rbind(data.frame(Region = "Todas las regiones", IDRegion = NA), regiones_disponibles)
    print(regiones_disponibles)
  })
  
  # Cargar las regiones disponibles al iniciar la aplicación
  observe({
    # Obtener las procedencias disponibles
    procedencias_disponibles <<- unique(sqlQuery(con3, 
                                                 "SELECT DISTINCT 
            CASE OC.porisintegrated 
              WHEN 3 THEN 'Compra Agil'
              ELSE (CASE  OC.IDProcedenciaOC
                      WHEN 703 THEN 'Convenio Marco'
                      WHEN 701 THEN 'Licitación Pública'
                      WHEN 1401 THEN 'Licitación Pública'
                      WHEN 702 THEN 'Licitación Privada'
                      ELSE 'Trato Directo' 
                    END)
            END AS Procedencia 
     FROM [DM_Transaccional].[dbo].[THOrdenesCompra] OC"
    ))
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
  
  # Cargar las regiones disponibles al iniciar la aplicación
  observe({
    # Ejecutar la consulta SQL para obtener las regiones
    sectores_disponibles <<- sqlQuery(con3, "SELECT DISTINCT [IdSector]
      ,[Sector]
  FROM [DM_Transaccional].[dbo].[DimSector]")
    # Agregar la opción "Todas las regiones"
    #regiones_disponibles <- rbind(data.frame(Region = "Todas las regiones", IDRegion = NA), regiones_disponibles)
  })
  
  #Selectores para el panel de transacciones
  {
    # Aquí va el selector de regiones para el panel de transacciones
    output$region_select <- renderUI({
      selectInput("region", "Selecciona una región:",
                  choices = c("Todas las regiones", regiones_disponibles$Region),
                  selected = regiones_disponibles$Region[15])
    })
    
    # Aquí va el selector de procedencia para el panel de transacciones
    output$procedencia_select <- renderUI({
      selectInput("procedencia", "Selecciona una procedencia:",
                  choices = c("Todas las procedencias", procedencias_disponibles$Procedencia),
                  selected = procedencias_disponibles$Procedencia[5])
    })
    
    # Aquí va el selector de instituciones para el panel de transacciones 
    output$institucion_select <- renderUI({
      selectInput("institucion", "Selecciona una Institución:",
                  choices = c("Todas las instituciones", institucion_disponibles$NombreInstitucion),
                  selected = "Todas las instituciones")
    })
    
    #Aquí va el selector de sectores para el panel de transacciones 
    # Nuevo selectInput para sector
    output$sector_select <- renderUI({
      selectInput("sector", "Selecciona un sector:",
                  choices = c("Todos los sectores", sectores_disponibles$Sector),
                  selected = "Todos los sectores")
    })  
  }
  
  #Selectores para el panel de usuarios 
  
  {
    # Aquí va el selector de regiones para el panel de usuarios
    output$usr_region_select <- renderUI({
      selectInput("usr_region", "Selecciona una región:",
                  choices = c("Todas las regiones", regiones_disponibles$Region),
                  selected = regiones_disponibles$Region[15])
    })
    
    # Aquí va el selector de procedencia para el panel de usuarios 
    output$usr_procedencia_select <- renderUI({
      selectInput("usr_procedencia", "Selecciona una procedencia:",
                  choices = c("Todas las procedencias", procedencias_disponibles$Procedencia),
                  selected = procedencias_disponibles$Procedencia[5])
    })
    
    # Aquí va el selector de instituciones para el panel de usuarios 
    output$usr_institucion_select <- renderUI({
      selectInput("usr_institucion", "Selecciona una Institución:",
                  choices = c("Todas las instituciones", institucion_disponibles$NombreInstitucion),
                  selected = "Todas las instituciones")
    })
    
    # Aquí va el selector de sectores para el panel de usuarios 
    output$usr_sector_select <- renderUI({
      selectInput("usr_sector", "Selecciona un sector:",
                  choices = c("Todos los sectores", sectores_disponibles$Sector),
                  selected = "Todos los sectores")
    })  
  }
  
  
  # Este umbral es para desplegar un mensaje en caso de que la consulta esté vacía 
  umbral_filas <- 1
  
  # Definir variable reactiva para almacenar la consulta
  consulta_ <- reactiveVal(NULL)
  
  usr_consulta_ <- reactiveVal(NULL)
  # Definir variable reactiva para almacenar los datos consultados
  
  datos_consultados <- reactiveVal(NULL)
  
  compradores_consultados <- reactiveVal(NULL)
  
  
  #Ejecutar consulta para el panel de transacciones. Todo se ve ok. 
  observeEvent(input$consultar_btn, {
    
    print("Botón 'consultar_btn' presionado en el panel de usuarios")
    # Obtener la región seleccionada  para el panel de transacciones 
    if(input$region == "Todas las regiones") {
      # Si se selecciona "Todas las regiones", no se aplica filtro por región en la consulta SQL
      region_seleccionada <- NULL
    } else {
      region_seleccionada <- input$region
    }
    
    cat("La región seleccionada para el panel de transacciones es:\n"
        ,"===========================================================\n"
        , region_seleccionada
        ,"\n ===========================================================\n")
    
    
    # Obtener la procedencia seleccionada para el panel de transacciones
    if(input$procedencia == "Todas las procedencias") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      procedencia_seleccionada <- NULL
    } else {
      procedencia_seleccionada <- input$procedencia
    }
    
    cat("La procedencia seleccionada para el panel de transacciones es:\n"
        ,procedencia_seleccionada
        ,"\n ===========================================================\n")
    
    
    # Obtener la Institución para el panel de transacciones
    if(input$institucion == "Todas las instituciones") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      institucion_seleccionada <- NULL
    } else {
      institucion_seleccionada <- input$institucion
    }
    
    cat("La institucion seleccionada para el panel de transacciones es:\n"
        ,institucion_seleccionada
        ,"\n ===========================================================\n")
    
    # Obtener el sector seleccionado por el usuario
    if(input$sector == "Todos los sectores") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      sector_seleccionado <- NULL
    } else {
      sector_seleccionado <- input$sector
    }
    
    cat("El sector seleccionado para el panel de transacciones es:\n"
        ,sector_seleccionado
        ,"\n ===========================================================\n")
    
    query <- paste0(
      "SELECT  
        T.Year
        ,L.Region
        ,T.Date [Fecha Envío OC]
        ,OC.NombreOC
        ,OC.CodigoOC
		    ,OC.MonedaOC [Tipo de moneda]")
    
    if (input$detalle){
      query <- paste0(query,",OL.Monto [Monto item] 
		,OL.NombreItem [Nombre producto]
		,OL.DescripcionItem
		,RU.RubroN1 [Rubro Proveedor]")
    }
    
    
    query <- paste0(query,"
		    ,OC.MontoUSD [Monto total USD]
		    ,OC.MontoCLP [Monto total pesos]
		    ,OC.MontoCLF [Monto total UF]
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
      FROM [DM_Transaccional].[dbo].[THOrdenesCompra] as OC")
    
    if (input$detalle){
      query <- paste0(query, "
        --------------------------------------------------------------------------------------------
        INNER JOIN [DM_Transaccional].[dbo].[THOrdenesCompraLinea] as OL  ON OC.porID = OL.porID
        INNER JOIN [DM_Transaccional].[dbo].[DimProducto] as DPR ON  OL.IDProducto = DPR.IDProducto
        INNER JOIN [DM_Transaccional].[dbo].[DimRubro] as RU ON DPR.IdRubro = RU.IdRubro
		    --------------------------------------------------------------------------------------------              
                      ")
    }
    
    query <- paste0(query,"
        INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
        INNER JOIN [DM_Transaccional].[dbo].[DimComprador] as C ON OC.IDUnidaddeCompra = C.IDUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
        INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L ON L.IDLocalidad = C.IDLocalidadUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimSector] as S ON I.IdSector = S.IdSector
        INNER JOIN [DM_Transaccional].[dbo].[DimProveedor] as P ON OC.IDSucursal=P.IDSucursal
        INNER JOIN [DM_Transaccional].[dbo].[DimEmpresa] as E ON P.entCode = E.entCode 
        INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L2 ON L2.IDLocalidad = P.IDLocalidadSucursal
        LEFT JOIN [DM_Transaccional].[dbo].[THTamanoProveedor] as TP ON P.entCode=TP.entCode AND TP.AñoTributario = 2022
        LEFT JOIN [DM_Transaccional].[dbo].[DimTamanoProveedor] as DTP ON TP.idTamano = DTP.IdTamano
        /* 
        
        Recordar modificar esta parte 
        LEFT JOIN [Dm_Usuario].[dbo].[TablonUsuarioComprador] as U ON OC.usrID = U.usrID
        */
      WHERE  OC.IDFechaEnvioOC BETWEEN '", gsub("-", "", input$fecha[1]), "' AND '", gsub("-", "", input$fecha[2]), "'
        AND OC.IDEstadoOC IN  (4,5,6,7,12)"
    )
    
    # Agregar condición de región si no es "Todas las regiones"
    if(!is.null(region_seleccionada)) {
      query <- paste0(query, " AND L.Region = '", region_seleccionada, "'")
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
    
    cat("La Query es la siguiente:"
        ,"\n ==============================================================\n\n"
        ,query
        ,"\n ==============================================================\n\n"
        ,"\n ==============================================================\n\n"
        ,"\n ==============================================================\n\n")
    
    consulta_(query)
    
    
    # Realizar la consulta solo cuando se presiona el botón "Consultar"
    req(input$consultar_btn)  # Espera a que se presione el botón "Consultar"
    
    
    
    # Realizar una preconsulta rápida para obtener el número de filas
    num_filas <- withProgress(message = "Realizando preconsulta rápida...", value = 0, {
      sqlQuery(con3, paste("SELECT COUNT(*) AS NumFilas FROM (", query, ") AS SubConsulta"))
    })
    
    # Obtener el número de filas desde el resultado de la preconsulta
    num_filas <- as.numeric(num_filas$NumFilas)
    
    cat("El número de filas obtenido de la pre consulta es:"
        ,"\n ==============================================================\n\n"
        ,num_filas
        ,"\n ==============================================================\n\n"
        ,"\n ==============================================================\n\n"
        ,"\n ==============================================================\n\n")
    
    if (num_filas > umbral_filas) {
      # Si el número de filas supera el umbral, mostrar una ventana emergente para confirmar la consulta completa
      showModal(
        modalDialog(
          title = "Confirmación de consulta",
          paste("El resultado de la consulta contiene", num_filas, "filas.", "¿Desea continuar con la consulta?"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("confirmar_btn", "Continuar")
          )
        )
      )
    } else if (num_filas <= 0) {
      # Si el número de filas es menor o igual a cero, imprimir un mensaje en la pantalla
      showNotification("No se encontraron resultados para esta consulta.", duration = 10)
    } else {
      # Si el número de filas es mayor que cero pero menor o igual al umbral, ejecutar la consulta completa y mostrar los datos
      resultado <- withProgress(message = "Realizando consulta a la base de datos", value = 0, {
        sqlQuery(con3, consulta_())
      })
      datos_consultados(resultado)
      updateActionButton(session, "consultar", label = "Consultar", icon = icon("search"))
    }
    
    
  })
  
  # Observador para el botón de confirmación en la ventana emergente para el panel de transacciones
  observeEvent(input$confirmar_btn, {
    # Ocultar la ventana emergente
    removeModal()
    
    # Ejecuta la consulta completa 
    resultado <- withProgress(message = "Realizando consulta a la base de datos", value = 0, {
      sqlQuery(con3, consulta_())
    })
    datos_consultados(resultado)
    updateActionButton(session, "consultar_btn", label = "Consultar", icon = icon("search"))
    
    cat("Realizando consulta a la base de datos. \n\n\n"
        ,str(resultado)
        ,"========================================================================= \n\n\n")
  })
  
  #Ejecutar consulta para el panel de usuarios
  observeEvent(input$usr_consultar_btn, {
    
    
    # Obtener la región seleccionada por el usuario
    if(input$usr_region == "Todas las regiones") {
      # Si se selecciona "Todas las regiones", no se aplica filtro por región en la consulta SQL
      region_seleccionada <- NULL
    } else {
      region_seleccionada <- input$usr_region
    }
    
    cat("\n la región seleccionada para la consulta de usuarios es: \n"
        ,region_seleccionada
        ," ==============================================================\n\n\n")
    
    # Obtener la procedencia seleccionada por el usuario
    if(input$usr_procedencia == "Todas las procedencias") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      procedencia_seleccionada <- NULL
    } else {
      procedencia_seleccionada <- input$usr_procedencia
    }
    
    cat("\n la procedencia_seleccionada para la consulta de usuarios es: \n"
        ,procedencia_seleccionada
        ," ==============================================================\n\n\n")
    
    
    # Obtener la Institución seleccionada por el usuario
    if(input$usr_institucion == "Todas las instituciones") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      institucion_seleccionada <- NULL
    } else {
      institucion_seleccionada <- input$usr_institucion
    }
    
    cat("\n la institución seleccioada para la consulta de usuarios es: \n"
        ,institucion_seleccionada
        ," ==============================================================\n\n\n")
    
    # Obtener el sector seleccionado para el panel de usuarios
    if(input$usr_sector == "Todos los sectores") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      sector_seleccionado <- NULL
    } else {
      sector_seleccionado <- input$usr_sector
    }
    
    cat("\n El sector seleccionado para la consulta de usuarios es: \n"
        ,sector_seleccionado
        ," ==============================================================\n\n\n")
    
    usr_query <- paste0(
      "SELECT  DISTINCT
        T.Year
		,U.Nombres+' '+U.Apellidos [Nombre completo]
		,U.RUT [Rut usuario]
		,U.Sexo [Sexo usuario]
		,U.eMail
		,C.RUTUnidaddeCompra [Rut unidad de compra]
		,C.RazonSocialUnidaddeCompra [Razon social unidad de compra]
		,I.NombreInstitucion [Institucion]
		,L.Region
		,S.Sector [Sector Institucion]
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
		,T.Date [Fecha envío OC]
		,OC.CodigoOC
		,OC.MontoCLP+OC.ImpuestoCLP [Monto Bruto CLP]
		,OC.MontoCLF+OC.ImpuestoCLF [Monto Bruto CLF]
		,OC.MontoUSD+OC.ImpuestoUSD [Monto Bruto USD]
		--------------------------------------------------------------------------------------------------------------------------------------------------
		--------------------------------------------------------------------------------------------------------------------------------------------------
		/*
		** Esta parte esta comentada porque la agrupación la vamos a realizar con R, de forma tal de poder descargar los datos
		** de manera desagregada
		
		,MAX(OC.IDFechaEnvioOC) [Fecha última OC]
		,COUNT(DISTINCT OC.CodigoOC) [Cantidad de OC]
		,SUM(OC.MontoCLP+OC.ImpuestoCLP) [Monto Transado (pesos)]
		,SUM(OC.MontoCLF+OC.ImpuestoCLF) [Monto Transado (UF)]
		,SUM(OC.MontoUSD+OC.ImpuestoUSD) [Monto Transado (USD)]
		*/
		--------------------------------------------------------------------------------------------------------------------------------------------------
		--------------------------------------------------------------------------------------------------------------------------------------------------
        FROM [DM_Transaccional].[dbo].[THOrdenesCompra] as OC 
        LEFT JOIN [DM_Transaccional].[dbo].DimUsuario as U ON OC.usrID = U.usrID 
    		INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
        LEFT JOIN [DM_Transaccional].[dbo].[DimComprador] as C ON OC. IDUnidaddeCompra = C.IDUnidaddeCompra
        LEFT JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L ON L.IDLocalidad = C.IDLocalidadUnidaddeCompra
        LEFT JOIN [DM_Transaccional].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
        LEFT JOIN [DM_Transaccional].[dbo].[DimSector] as S ON I.IdSector = S.IdSector
		--------------------------------------------------------------------------------------------------------------------------------------------------
		--------------------------------------------------------------------------------------------------------------------------------------------------
      WHERE  OC.IDFechaEnvioOC BETWEEN '", gsub("-", "", input$usr_fecha[1]), "' AND '", gsub("-", "", input$usr_fecha[2]), "'
       AND OC.IDEstadoOC IN  (4,5,6,7,12)"
    )
    
    # Agregar condición de región si no es "Todas las regiones"
    if(!is.null(region_seleccionada)) {
      usr_query <- paste0(usr_query, " AND L.Region = '", region_seleccionada, "'")
    }
    
    # Agregar condición de procedencia si no es "Todas las procedencias"
    if(!is.null(procedencia_seleccionada)) {
      usr_query <- paste0(usr_query, " AND (CASE OC.porisintegrated WHEN 3 THEN 'Compra Agil'
                                              ELSE (CASE  OC.IDProcedenciaOC
                                              WHEN 703 THEN 'Convenio Marco'
                                              WHEN 701 THEN 'Licitación Pública'
                                              WHEN 1401 THEN 'Licitación Pública'
                                              WHEN 702 THEN 'Licitación Privada'
                                              ELSE 'Trato Directo' END) END) = '", procedencia_seleccionada, "'")
    }
    
    # Agregar condición de institución si no es "Todas las instituciones"
    if(!is.null(institucion_seleccionada)) {
      usr_query <- paste0(usr_query, " AND I.NombreInstitucion = '", institucion_seleccionada, "'")
    }
    #rowser()
    
    
    
    # Agregar condición de sector si no es "Todos los sectores"
    if(!is.null(sector_seleccionado)) {
      usr_query <- paste0(usr_query, " AND S.Sector = '", sector_seleccionado, "'")
    }
    
    cat("La Query es la siguiente:"
        ,"\n ==============================================================\n\n"
        ,usr_query
        ,"\n ==============================================================\n\n"
        ,"\n ==============================================================\n\n"
        ,"\n ==============================================================\n\n")
    
    usr_consulta_(usr_query)
    
    
    # Realizar la consulta solo cuando se presiona el botón "Consultar"
    req(input$usr_consultar_btn)  # Espera a que se presione el botón "Consultar"
    
    
    
    # Realizar una preconsulta rápida para obtener el número de filas
    num_filas <- withProgress(message = "Realizando preconsulta rápida...", value = 0, {
      sqlQuery(con3, paste("SELECT COUNT(*) AS NumFilas FROM (", usr_query, ") AS SubConsulta"))
    })
    
    # Obtener el número de filas desde el resultado de la preconsulta
    num_filas <- as.numeric(num_filas$NumFilas)
    
    if (num_filas > umbral_filas) {
      # Si el número de filas supera el umbral, mostrar una ventana emergente para confirmar la consulta completa
      showModal(
        modalDialog(
          title = "Confirmación de consulta",
          paste("El resultado de la consulta contiene", num_filas, "filas.", "¿Desea continuar con la consulta?"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("usr_confirmar_btn", "Continuar")
          )
        )
      )
    } else if (num_filas <= 0) {
      # Si el número de filas es menor o igual a cero, imprimir un mensaje en la pantalla
      showNotification("No se encontraron resultados para esta consulta.", duration = 10)
    } else {
      # Si el número de filas es mayor que cero pero menor o igual al umbral, ejecutar la consulta completa y mostrar los datos
      resultado <- withProgress(message = "Realizando consulta a la base de datos", value = 0, {
        sqlQuery(con3, usr_consulta_())
      })
      compradores_consultados(resultado)
      updateActionButton(session, "consultar", label = "Consultar", icon = icon("search"))
    }
  })
  
  
  
  
  # Observador para el botón de confirmación en la ventana emergente para el panel de usuarios 
  observeEvent(input$usr_confirmar_btn, {
    # Ocultar la ventana emergente
    removeModal()
    
    # Ejecuta la consulta completa 
    resultado <- withProgress(message = "Realizando consulta a la base de datos", value = 0, {
      sqlQuery(con3, usr_consulta_())
    })
    compradores_consultados(resultado)
    updateActionButton(session, "usr_consultar_btn", label = "Consultar", icon = icon("search"))
    
    cat("Realizando consulta a la base de datos.")
  })
  
  # Muestra los resultados en una tabla html =============================
  
  output$usr_resultado <- renderDT({
    # Renderiza los datos en la tabla DT
    req(compradores_consultados())  # Requiere que los datos estén disponibles
    
    cat(names(compradores_consultados()))
    
    if (!is.null(compradores_consultados) && nrow(compradores_consultados()) > 0) {
      
      usr_aggregated <- compradores_consultados() %>%
        group_by(`Rut usuario`, Procedencia) %>% 
        summarise(`Nombre completo` = `Nombre completo`[1]
                  ,Institucion = Institucion[1]
                  ,eMail = eMail[1]
                  ,`Primera OC del período` = as.Date(`Fecha envío OC`, format = "%Y%m%d")[1]
                  ,ultima_oc = max(as.Date(`Fecha envío OC`, format = "%Y%m%d"))
                  ,`Total de órdenes de compra` = n_distinct(CodigoOC)
                  ,`Monto total en pesos (millones)` = round(sum(`Monto Bruto CLP`, na.rm = TRUE)/1000000,1),
                  `Monto total en UF (millones)` = round(sum(`Monto Bruto CLF`, na.rm = TRUE)/1000000,1),
                  `Monto total en dólares (millones)` = round(sum(`Monto Bruto USD`, na.rm = TRUE)/1000000,1)) %>% 
        arrange(desc(`Total de órdenes de compra`))
      
    }
    
    datatable(usr_aggregated)
  })
  
  
  
  
  # Crear una función reactiva para renderizar la cuadrícula de gráficos solo cuando se presione el botón
  combined_plot_data <- eventReactive(input$confirmar_btn, {
    req(datos_consultados())
    
    
    if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0 && input$fecha[2]>input$fecha[1]) {
      # Verificar si el rango de fechas es de un día
      if (as.numeric(difftime(input$fecha[2], input$fecha[1], units = "days"))<=7) {
        # Calcular el total de órdenes de compra para ese día
        total_oc <- datos_consultados() %>% 
          summarise(n_oc = n_distinct(CodigoOC))
        
        
        # Crear un gráfico simple con el total como una cifra grande
        line_region_plot <- plot_ly(x = ~1, y = ~total_oc$n_oc, type = "bar", 
                                    text = paste("Total de Órdenes de Compra:", total_oc$n_oc),
                                    marker = list(color = "blue")) %>%
          layout(title = "Total de Órdenes de Compra",
                 yaxis = list(title = "Total de Órdenes de Compra"),
                 xaxis = list(title = "")) 
        
        line_region_plot <- ggplotly(line_region_plot)
      } else {
        # Si el rango de fechas no es de un día, realiza el gráfico normal
        total_oc <- datos_consultados() %>% 
          group_by(Fecha = as.Date(`Fecha Envío OC`)) %>%  
          summarise(total_oc = n_distinct(CodigoOC))
        
        # Agregar la columna para la media móvil de 7 días
        total_oc <- total_oc %>%
          mutate(media_movil_7d = zoo::rollmean(total_oc, k = 7, fill = NA, align = "right"))
        
        # Crear el gráfico de líneas
        line_region_plot <- ggplot(total_oc, aes(x = Fecha)) +
          geom_line(aes(y = total_oc), color = "blue", linetype = "solid") +
          geom_line(aes(y = media_movil_7d), color = "red", linetype = "solid", linewidth = 1.5) +
          geom_point(aes(y = total_oc), color = "blue") +
          labs(title = paste0("Total de Órdenes de Compra por Día entre el ", input$fecha[1], " y el ", input$fecha[2]),
               y = "Total de Órdenes de Compra",
               x = "Fecha")+
          theme_minimal() +
          theme(
            legend.position = "top",
            plot.title = element_text(face = "bold", size = 20)
          )
        
        line_region_plot <- ggplotly(line_region_plot)
      }
    }
    
    if (input$detalle) {
      # Verificar si los datos están disponibles y no están vacíos
      if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0 && input$fecha[2]>input$fecha[1]) {
        resumen_rubro <- datos_consultados() %>% 
          group_by(rubro = `Rubro Proveedor`) %>%  
          summarise(n_oc = n_distinct(CodigoOC)) %>% 
          top_n(10, n_oc) %>% arrange(desc(n_oc))
        
        # Crear el gráfico de barras
        bar_rubro_plot <- ggplot(resumen_rubro, aes(x = reorder(rubro, -n_oc), y = n_oc, fill = reorder(rubro, n_oc))) +
          geom_bar(stat = "identity", color = "white", position = "dodge") +
          geom_text(
            aes(label = scales::comma(round(n_oc), big.mark = ".", decimal.mark = ",")),
            position = position_dodge(width = 1),
            vjust = -0.5,
            size = 4,
            color = "black"
          ) +
          labs(title = paste0("Cantidad de Órdenes de compra entre el ", input$fecha[1], " y el ", input$fecha[2]))+
          theme_minimal() +
          theme(
            legend.position = "top",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.key.width = unit(1, "cm"),
            legend.key.height = unit(1, "cm"),
            plot.title =  element_text(face = "bold", size = 20)
          ) +
          scale_fill_manual(
            name = "Rubro",
            values = scales::hue_pal()(length(unique(resumen_rubro$rubro))),
            labels = function(x) str_wrap(x, width = 10)
          )
        
        bar_rubro_plot <- ggplotly(bar_rubro_plot)
      }
    }
    
    # Verificar si los datos están disponibles y no están vacíos
    if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0 && input$fecha[2]>input$fecha[1]) {
      
      if (as.numeric(difftime(input$fecha[2], input$fecha[1], units = "days"))<=7) {
        # Calcular el total de órdenes de compra 
        resumen_line_monto <- datos_consultados() %>% 
          distinct(CodigoOC, .keep_all = TRUE) %>%
          #group_by(Fecha = as.Date(`Fecha Envío OC`)) %>%  
          summarise(monto = sum(`Monto total pesos`)/1000000)  
        # Crear un gráfico simple con el total como una cifra grande
        line_monto_plot <- plot_ly(x = ~1, y = ~resumen$monto, type = "bar", 
                                   text = paste("Monto total transado:", resumen$monto),
                                   marker = list(color = "blue")) %>%
          layout(title = "Monto total transado",
                 yaxis = list(title = "Monto total transado"),
                 xaxis = list(title = "")) 
        
        line_monto_plot <- ggplotly(line_monto_plot)
        
      } else {
        
        resumen_line_monto <- datos_consultados() %>% 
          distinct(CodigoOC, .keep_all = TRUE) %>%
          group_by(Fecha = as.Date(`Fecha Envío OC`)) %>%  
          summarise(monto = sum(`Monto total pesos`)/1000000)
        
        # Agregar la columna para la media móvil de 7 días
        resumen_line_monto <- resumen_line_monto %>%
          mutate(media_movil_7d = zoo::rollmean(monto, k = 7, fill = NA, align = "right"))
        
        # Crear el gráfico de líneas
        line_monto_plot <- ggplot(resumen_line_monto, aes(x = Fecha)) +
          geom_line(aes(y = monto), color = "blue", linetype = "solid") +
          geom_line(aes(y = media_movil_7d), color = "red", linetype = "solid", linewidth = 1.5) +
          geom_point(aes(y = monto), color = "blue") +
          labs(title = paste0("Total de Órdenes de Compra por Día entre el ", input$fecha[1], " y el ", input$fecha[2]),
               y = "Total de Órdenes de Compra",
               x = "Fecha")+
          theme_minimal() +
          theme(
            legend.position = "top",
            plot.title = element_text(face = "bold", size = 20)
          )
        
        line_monto_plot <- ggplotly(line_monto_plot)
      }
      
    } 
    
    
    
    if (input$detalle){
      # Verificar si los datos están disponibles y no están vacíos
      if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0 && input$fecha[2]>input$fecha[1]) {
        resumen_rubro_monto <- datos_consultados() %>%
          distinct(CodigoOC, .keep_all = TRUE) %>%
          group_by(rubro = `Rubro Proveedor`) %>%
          summarise(n_oc = n(),
                    monto = sum(`Monto total pesos`, na.rm = TRUE)/1000000) %>%
          top_n(10, monto) %>%
          arrange(desc(monto)) 
        
        resumen_rubro_monto <- resumen_rubro_monto[1:10,]
        
        # Identificar todas las categorías únicas
        categorias_unicas <- unique(c(resumen_rubro$rubro, resumen_rubro_monto$rubro))
        
        cat("Las categorías únicas son: \n\n"
            ,categorias_unicas
            ,"\n\n\n")
        
        # Asignar colores a las categorías únicas
        colores_unicos <- scales::hue_pal()(length(categorias_unicas))
        
        cat("Los colores asignados a las categorías únicas son:\n\n\n"
            ,colores_unicos
            ,"===================================\n\n\n")
        
        nombres_colores <- setNames(colores_unicos, categorias_unicas)
        
        cat("Los nombres de los colores únicos son:\n\n\n"
            ,nombres_colores
            ,"===================================\n\n\n")
        
        # Crear el gráfico de barras
        bar_rubro_monto <- ggplot(resumen_rubro_monto, aes(x = reorder(rubro, -monto), y = monto, fill = reorder(rubro, monto))) +
          geom_bar(stat = "identity", color = "white", position = "dodge") +
          geom_text(
            aes(label = scales::comma(round(monto), big.mark = ".", decimal.mark = ",")),
            position = position_dodge(width = 1),
            vjust = -0.5,
            size = 4,
            color = "black"
          ) +
          labs(title = paste0("Cantidad de Órdenes de compra entre el ", input$fecha[1], " y el ", input$fecha[2]))+
          theme_minimal() +
          theme(
            legend.position = "top",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.key.width = unit(1, "cm"),
            legend.key.height = unit(1, "cm"),
            plot.title =  element_text(face = "bold", size = 20)
          ) +
          scale_fill_manual(
            name = "Rubro",
            values = nombres_colores
            ,labels = function(x) str_wrap(x, width = 10)
          )
        
        bar_rubro_monto <- ggplotly(bar_rubro_monto)
        
      }
    }
    
    
    # Verificar si los datos están disponibles y no están vacíos
    if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0) {
      
      if (input$detalle) {
        plotito <- subplot(line_region_plot, bar_rubro_plot, line_monto_plot, bar_rubro_monto, nrows = 4)
      } else {
        plotito <- subplot(line_region_plot, line_monto_plot, nrows = 2)
      }
      
      # Personalizar el diseño y otras configuraciones
      combined_plots <- plotito %>% config(displayModeBar = TRUE) %>% layout(height = 700)
      
      # Devolver el objeto de gráfico combinado
      return(combined_plots)
    } else {
      # Mostrar un mensaje de error si los datos están vacíos o no están disponibles
      return("No hay datos disponibles para crear el gráfico combinado.")
    }
  })
  
  # Renderiza la cuadrícula de gráficos de transacciones
  output$combined_plot <- renderPlotly({
    req(input$consultar_btn)
    req(datos_consultados())
    combined_plot_data()
  })
  
  
  
  # Descarga los datos extraídos ===================
  
  # Lógica para la descarga del archivo Excel
  output$downloadData_transacciones <- downloadHandler(
    filename = function() {
      paste(gsub("-","",Sys.Date())," Ordenes de compra", ".csv", sep="")
    },
    content = function(file) {
      write.csv2(datos_consultados(), file = file, sep = "|", fileEncoding = "latin1")
    }
  )
  
  # Lógica para la descarga del archivo Excel
  output$usr_downloadData <- downloadHandler(
    filename = function() {
      paste(gsub("-","",Sys.Date())," resumen usuarios compradores", ".csv", sep="")
    },
    content = function(file) {
      write.csv2(compradores_consultados(), file = file, sep = "|", fileEncoding = "latin1")
    }
  )
  
  # output$documentacion <- renderUI({
  #   markdown <- readLines("README.md", warn = FALSE)
  #   HTML(markdown::markdownToHTML(markdown))
  # })
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
