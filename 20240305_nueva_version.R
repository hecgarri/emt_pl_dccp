# Carga de paquetes
library(shiny)
library(RODBC)
library(openxlsx)

# Establece conexiones a los diferentes servidores
con3 <- RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse")

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Consulta a base de datos"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("fecha", "Rango de fechas:",
                     start = Sys.Date() %m-% months(2),
                     end = Sys.Date() %m-% months(1)),
      uiOutput("region_select"),
      uiOutput("procedencia_select"), # Nuevo selectInput para procedencia
      actionButton("consultar", "Consultar"),
      downloadButton("downloadData", "Descargar Excel")
    ),
    mainPanel(
      tableOutput("resultado")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Definir una variable para almacenar las regiones disponibles
  regiones_disponibles <- NULL
  procedencias_disponibles <- NULL # Variable para procedencias
  
  # Cargar las regiones disponibles al iniciar la aplicación
  observe({
    # Ejecutar la consulta SQL para obtener las regiones
    regiones_disponibles <<- sqlQuery(con3, "SELECT DISTINCT L.Region, L.IDRegion FROM [DM_Transaccional].[dbo].[DimLocalidad] L")
    # Agregar la opción "Todas las regiones"
    regiones_disponibles <- rbind(data.frame(Region = "Todas las regiones", IDRegion = NA), regiones_disponibles)
    
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
  })
  
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
  
  observeEvent(input$consultar, {
    # Realizar la consulta solo cuando se presiona el botón "Consultar"
    output$resultado <- renderTable({
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
      
      consulta <- paste0(
        "SELECT  
        T.Year
        ,L.Region
        ,(CASE  WHEN OC.porisintegrated=3 THEN 'Compra Agil'
                          ELSE (CASE  OC.IDProcedenciaOC
                          WHEN 703 THEN 'Convenio Marco'
                          WHEN 701 THEN 'Licitación Pública'
                          WHEN 1401 THEN 'Licitación Pública'
                          WHEN 702 THEN 'Licitación Privada'
                          ELSE 'Trato Directo' END)
        END) [Procedencia]
        ,(COUNT(DISTINCT OC.CodigoOC)) [Cantidad OC]
        ,(SUM(OL.MontoUSD)) [Monto USD]
        ,(SUM(OL.MontoCLP)) [Monto CLP]
        ,(SUM(OL.MontoCLF)) [Monto CLF]
        
        FROM [DM_Transaccional].[dbo].[THOrdenesCompraLinea] as OL 
        INNER JOIN [DM_Transaccional].[dbo].[THOrdenesCompra] as OC ON OL.CodigoOC=OC.CodigoOC 
        INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] as T ON OC.IDFechaEnvioOC = T.DateKey
        INNER JOIN [DM_Transaccional].[dbo].[DimProducto] as PR ON OL.IDProducto=PR.IDProducto
        INNER JOIN [DM_Transaccional].[dbo].[DimProveedor] as P ON OC.IDSucursal=P.IDSucursal 
        INNER JOIN [DM_Transaccional].[dbo].[DimComprador] as C ON OC.IDUnidaddeCompra = C.IDUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimLocalidad] as L ON L.IDLocalidad = C.IDLocalidadUnidaddeCompra
        INNER JOIN [DM_Transaccional].[dbo].[DimEmpresa] as E ON P.entCode = E.entCode 
        INNER JOIN [DM_Transaccional].[dbo].[DimInstitucion] as I ON C.entCode = I.entCode
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
      
      consulta <- paste0(consulta, " GROUP BY T.Year, L.Region,
                         (CASE OC.porisintegrated WHEN 3 THEN 'Compra Agil'
                          ELSE (CASE  OC.IDProcedenciaOC
                                          WHEN 703 THEN 'Convenio Marco'
                                          WHEN 701 THEN 'Licitación Pública'
                                          WHEN 1401 THEN 'Licitación Pública'
                                          WHEN 702 THEN 'Licitación Privada'
                                          ELSE 'Trato Directo' END)END)")
      
      withProgress(message = "Realizando consulta a la base de datos", {
        resultado <- sqlQuery(con3, consulta)
        # Después de completar la consulta, restablecer el valor del botón a 0
        updateActionButton(session, "consultar", label = "Consultar", icon = icon("search"))
      })
      
      return(resultado)
    })
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
      openxlsx::writeData(wb, sheet = sheet_name, x = output$resultado())
      # Guardar el libro de Excel
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
