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

# Interfaz de usuario
ui <- fluidPage(
  
  titlePanel("Consulta a base de datos"),
  
  tabsetPanel(
    tabPanel("Transacciones",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("fecha", "Rango de fechas:",
                                start = Sys.Date() %m-% months(13),
                                end = Sys.Date() %m-% months(13)),
                 uiOutput("region_select"),
                 uiOutput("procedencia_select"), # Nuevo selectInput para procedencia
                 uiOutput("institucion_select"), # Nuevo selectInput según institución
                 uiOutput("sector_select"), #Nuevo selectInput según sector 
                 actionButton("consultar_btn", "Consultar"),
                 downloadButton("downloadData", "Descargar Excel")
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
    
    
    # ,tabPanel("Usuarios",
    #          
    #          sidebarLayout(
    #            sidebarPanel(
    #              
    #            )
    #          )
    #          )
    
    ,tabPanel("Documentación", 
              mainPanel(
                # Contenido de la pestaña de documentación
                tags$div(
                  # uiOutput("documentacion") # Incluye la documentación aquí
                )
              ))
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Definir una variable para almacenar las regiones disponibles
  regiones_disponibles <- NULL
  procedencias_disponibles <- NULL # Variable para procedencias
  institucion_disponibles <- NULL # Variable para procedencias
  sectores_disponibles <- NULL # variable para sectores
  
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
  
  # Cargar las regiones disponibles al iniciar la aplicación
  observe({
    # Ejecutar la consulta SQL para obtener las regiones
    sectores_disponibles <<- sqlQuery(con3, "SELECT DISTINCT [IdSector]
      ,[Sector]
  FROM [DM_Transaccional].[dbo].[DimSector]")
    # Agregar la opción "Todas las regiones"
    #regiones_disponibles <- rbind(data.frame(Region = "Todas las regiones", IDRegion = NA), regiones_disponibles)
  })
  
  # En el selectInput, puedes hacer la selección de "Todas las regiones"
  output$region_select <- renderUI({
    selectInput("region", "Selecciona una región:",
                choices = c("Todas las regiones", regiones_disponibles$Region),
                selected = regiones_disponibles$Region[15])
  })
  
  # Nuevo selectInput para procedencia
  output$procedencia_select <- renderUI({
    selectInput("procedencia", "Selecciona una procedencia:",
                choices = c("Todas las procedencias", procedencias_disponibles$Procedencia),
                selected = procedencias_disponibles$Procedencia[5])
  })
  
  # Nuevo selectInput para institucion
  output$institucion_select <- renderUI({
    selectInput("institucion", "Selecciona una Institución:",
                choices = c("Todas las instituciones", institucion_disponibles$NombreInstitucion),
                selected = "Todas las instituciones")
  })
  
  
  
  # Nuevo selectInput para institucion
  output$sector_select <- renderUI({
    selectInput("sector", "Selecciona un sector:",
                choices = c("Todos los sectores", sectores_disponibles$Sector),
                selected = "Todos los sectores")
  })
  
  # Definir umbral para el número de filas
  umbral_filas <- 1
  
  # Definir variable reactiva para almacenar la consulta
  consulta_ <- reactiveVal(NULL)
  
  # Definir variable reactiva para almacenar los datos consultados
  datos_consultados <- reactiveVal(NULL)
  
  
  
  observeEvent(input$consultar_btn, {
    
    
    # Obtener la región seleccionada por el usuario
    if(input$region == "Todas las regiones") {
      # Si se selecciona "Todas las regiones", no se aplica filtro por región en la consulta SQL
      region_seleccionada <- NULL
    } else {
      region_seleccionada <- input$region
    }
    
    print(region_seleccionada)
    
    # Obtener la procedencia seleccionada por el usuario
    if(input$procedencia == "Todas las procedencias") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      procedencia_seleccionada <- NULL
    } else {
      procedencia_seleccionada <- input$procedencia
    }
    
    print(procedencia_seleccionada)
    
    
    # Obtener la Institución seleccionada por el usuario
    if(input$institucion == "Todas las instituciones") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      institucion_seleccionada <- NULL
    } else {
      institucion_seleccionada <- input$institucion
    }
    
    print(institucion_seleccionada) 
    
    # Obtener la Institución seleccionada por el usuario
    if(input$sector == "Todos los sectores") {
      # Si se selecciona "Todas las procedencias", no se aplica filtro por procedencia en la consulta SQL
      sector_seleccionado <- NULL
    } else {
      sector_seleccionado <- input$sector
    }
    
    print(sector_seleccionado)
    
    query <- paste0(
      "SELECT  
        T.Year
        ,L.Region
        ,T.Date [Fecha Envío OC]
        ,OC.NombreOC
        ,OC.CodigoOC
        ,OL.NombreItem [Nombre producto]
        ,OL.DescripcionItem
		,OC.MonedaOC [Tipo de moneda]
		,OL.Monto [Monto item] 
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
    
    consulta_(query)
    
    
    # Realizar la consulta solo cuando se presiona el botón "Consultar"
    req(input$consultar_btn)  # Espera a que se presione el botón "Consultar"
    
    
    
    # Realizar una preconsulta rápida para obtener el número de filas
    num_filas <- withProgress(message = "Realizando preconsulta rápida...", value = 0, {
      sqlQuery(con3, paste("SELECT COUNT(*) AS NumFilas FROM (", query, ") AS SubConsulta"))
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
            actionButton("confirmar_btn", "Continuar")
          )
        )
      )
    } else {
      # Si el número de filas es menor o igual al umbral, ejecutar la consulta completa y mostrar los datos
      resultado <- withProgress(message = "Realizando consulta a la base de datos", value = 0, {
        sqlQuery(con3, consulta_())
      })
      datos_consultados(resultado)
      updateActionButton(session, "consultar", label = "Consultar", icon = icon("search"))
    }
  })
  
  # Observador para el botón de confirmación en la ventana emergente
  observeEvent(input$confirmar_btn, {
    # Ocultar la ventana emergente
    removeModal()
    
    # Ejecuta la consulta completa 
    resultado <- withProgress(message = "Realizando consulta a la base de datos", value = 0, {
      sqlQuery(con3, consulta_())
    })
    datos_consultados(resultado)
    updateActionButton(session, "consultar_btn", label = "Consultar", icon = icon("search"))
    
    cat("Realizando consulta a la base de datos.")
  })
  
  # Muestra los resultados en una tabla html =============================
  
  # output$resultado <- renderDT({
  #   # Renderiza los datos en la tabla DT
  #   req(datos_consultados())  # Requiere que los datos estén disponibles
  #   datatable(datos_consultados())
  # })
  # 
  
  
  
  # Definir una variable reactiva para almacenar el gráfico de líneas
  line_region_plot <- eventReactive(input$consultar_btn, {
    req(input$consultar_btn)
    req(datos_consultados())
    
    
    #browser()
    
    # Verificar si los datos están disponibles y no están vacíos
    if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0) {
      # Verificar si el rango de fechas es de un día
      if (input$fecha[1] == input$fecha[2]) {
        # Calcular el total de órdenes de compra para ese día
        total_oc <- datos_consultados() %>% 
          summarise(n_oc = n_distinct(CodigoOC))
        
        
        # Crear un gráfico simple con el total como una cifra grande
        plot <- plot_ly(x = ~1, y = ~total_oc$n_oc, type = "bar", 
                        text = paste("Total de Órdenes de Compra:", total_oc$n_oc),
                        marker = list(color = "blue")) %>%
          layout(title = "Total de Órdenes de Compra",
                 yaxis = list(title = "Total de Órdenes de Compra"),
                 xaxis = list(title = "")) 
      } else {
        # Si el rango de fechas no es de un día, realiza el gráfico normal
        resumen <- datos_consultados() %>% 
          group_by(Fecha = as.Date(`Fecha Envío OC`)) %>%  
          summarise(total_oc = n_distinct(CodigoOC))
        
        # Agregar la columna para la media móvil de 7 días
        resumen <- resumen %>%
          mutate(media_movil_7d = zoo::rollmean(total_oc, k = 7, fill = NA, align = "right"))
        
        # Crear el gráfico de líneas
        plot <- ggplot(resumen, aes(x = Fecha)) +
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
      }
      
      return(plot)
    } else {
      # Retornar un objeto nulo si no hay datos disponibles
      return(NULL)
    }
    
  })
  
  
  # Definir una variable reactiva para almacenar el gráfico de barras
  bar_rubro_plot <- eventReactive(input$consultar_btn, {
    req(input$consultar_btn)
    req(datos_consultados())
    
    # Verificar si los datos están disponibles y no están vacíos
    if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0) {
      resumen <- datos_consultados() %>% 
        group_by(rubro = `Rubro Proveedor`) %>%  
        summarise(n_oc = n_distinct(CodigoOC)) %>% 
        top_n(10, n_oc) %>% arrange(n_oc)
      
      # Crear el gráfico de barras
      plot <- ggplot(resumen, aes(x = reorder(rubro, -n_oc), y = n_oc, fill = reorder(rubro, n_oc))) +
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
          values = scales::hue_pal()(length(unique(resumen$rubro)))
          ,labels = function(x) str_wrap(x, width = 10)
        )
      
      plot <- ggplotly(plot)
      
      return(plot)
      print(class(plot))
    } else {
      # Retornar un objeto nulo si no hay datos disponibles
      return(NULL)
    }
  })
  
  
  # Definir una variable reactiva para almacenar el gráfico de líneas
  line_monto_plot <- eventReactive(input$consultar_btn, {
    req(input$consultar_btn)
    req(datos_consultados())
    
    # Verificar si los datos están disponibles y no están vacíos
    if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0) {
      
      if (input$fecha[1] == input$fecha[2]) {
        # Calcular el total de órdenes de compra 
        resumen <- datos_consultados() %>% 
          distinct(CodigoOC, .keep_all = TRUE) %>%
          #group_by(Fecha = as.Date(`Fecha Envío OC`)) %>%  
          summarise(monto = sum(`Monto total pesos`)/1000000)  
        # Crear un gráfico simple con el total como una cifra grande
        plot <- plot_ly(x = ~1, y = ~resumen$monto, type = "bar", 
                        text = paste("Monto total transado:", resumen$monto),
                        marker = list(color = "blue")) %>%
          layout(title = "Monto total transado",
                 yaxis = list(title = "Monto total transado"),
                 xaxis = list(title = "")) 
        
        
      } else {
      
      resumen <- datos_consultados() %>% 
        distinct(CodigoOC, .keep_all = TRUE) %>%
        group_by(Fecha = as.Date(`Fecha Envío OC`)) %>%  
        summarise(monto = sum(`Monto total pesos`)/1000000)
      
      # Agregar la columna para la media móvil de 7 días
      resumen <- resumen %>%
        mutate(media_movil_7d = zoo::rollmean(monto, k = 7, fill = NA, align = "right"))
      
      # Crear el gráfico de líneas
      plot <- ggplot(resumen, aes(x = Fecha)) +
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
      }
      
      plot <- ggplotly(plot)
      
      return(plot)
      
    } else {
      # Retornar un objeto nulo si no hay datos disponibles
      return(NULL)
    }
  })
  
  
  
  
  # Definir una variable reactiva para almacenar el gráfico de barras
  bar_rubro_monto <- eventReactive(input$consultar_btn, {
    req(input$consultar_btn)
    req(datos_consultados())
    
    # Verificar si los datos están disponibles y no están vacíos
    if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0) {
      resumen <- datos_consultados() %>%
        distinct(CodigoOC, .keep_all = TRUE) %>%
        group_by(rubro = `Rubro Proveedor`) %>%
        summarise(n_oc = n(),
                  monto = sum(`Monto total pesos`, na.rm = TRUE)/1000000) %>%
        top_n(10, monto) %>%
        arrange(desc(monto)) 
      
      resumen <- resumen[1:10,]
      
      
      
      
      #print(resumen)
      
      # Crear el gráfico de barras
      plot <- ggplot(resumen, aes(x = reorder(rubro, -monto), y = monto, fill = reorder(rubro, monto))) +
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
          values = scales::hue_pal()(length(unique(resumen$rubro)))
          ,labels = function(x) str_wrap(x, width = 10)
        )
      
      plot <- ggplotly(plot)
      
      return(plot)
      #print(class(plot))
    } else {
      # Retornar un objeto nulo si no hay datos disponibles
      return(NULL)
    }
  })
  
  
  # Renderiza la cuadrícula de gráficos
  output$combined_plot <- renderPlotly({
    req(input$consultar_btn)
    req(datos_consultados())
    
    # Verificar si los datos están disponibles y no están vacíos
    if (!is.null(datos_consultados()) && nrow(datos_consultados()) > 0) {
      
      # Combinar los dos gráficos usando la función plot_grid de cowplot
      #combined_plots <- cowplot::plot_grid(line_region_plot(), bar_region_plot(), bar_rubro_plot(), ncol = 2,nrow = 2, align = "v")
      #combined_plot <- line_region_plot()
      line <- line_region_plot()
      
      rubro <- bar_rubro_plot()
      
      line_monto <- line_monto_plot()
      
      rubro_monto <- bar_rubro_monto()
      
      combined_plots <- subplot(line,rubro, line_monto, rubro_monto, nrows = 2) 
      
      combined_plots <- combined_plots %>% config(displayModeBar = FALSE) %>% layout(height = 700)
      
      
      # Devolver el objeto de gráfico combinado
      combined_plots
      #
      print(combined_plots)
    } else {
      # Mostrar un mensaje de error si los datos están vacíos o no están disponibles
      print("No hay datos disponibles para crear el gráfico combinado.")
    }
  }
  #, height = 1200
  )
  
  
  # Descarga los datos extraídos ===================
  
  # Lógica para la descarga del archivo Excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub("-","",Sys.Date())," Ordenes de compra", ".csv", sep="")
    },
    content = function(file) {
      write.csv2(datos_consultados(), file = file, sep = "|", fileEncoding = "latin1")
    }
  )
  
  output$documentacion <- renderUI({
    markdown <- readLines("README.md", warn = FALSE)
    HTML(markdown::markdownToHTML(markdown))
  })
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
