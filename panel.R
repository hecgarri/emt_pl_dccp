
loaded <- TRUE #¿Están los datos cargados? 

if (!loaded){
  source("C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/20240114 descarga datos.R")
}


datos_rubros <- datos %>% 
  group_by(level1) %>% 
  summarise(ofertas = n_distinct(CodigoCotizacion)
            ,solicitudes = n_distinct(CodigoSolicitudCotizacion)
            ,proveedores = n_distinct(entCode))

# ui
ui <- fluidPage(
  titlePanel("Datos compra Ágil"),
  navbarPage(
    "",
    
    tabPanel(
      "Análisis",
      fluidRow(
        column(
          width = 3,
          
          #h4("Según rubro"),
          h4("Filtros"),
          varSelectInput(
            inputId = "inputId_rubro",
            label = "Seleccionar variable:",
            data = datos_rubros[,c("ofertas", "solicitudes", "proveedores")]
            ,selected = "CodigoCotizacion",
            multiple = FALSE
          ),
          # selectInput(
          #   "region_filter_product", 
          #   "Seleccionar región:",
          #   choices = unique(datos$NombreRegion),
          #   selected = unique(datos$NombreRegion)[1],
          #   multiple = TRUE
          # ),
          # dateRangeInput(
          #   "date_filter_rubro", 
          #   "Seleccionar rango de fechas:",
          #   start = min(datos$FechaPublicacion),
          #   end = max(datos$FechaPublicacion),
          #   min = min(datos$FechaPublicacion),
          #   max = max(datos$FechaPublicacion)
          # ),
          selectInput(
            "product_filter_rubro", 
            "Seleccionar Rubro:",
            choices = unique(datos$level1),
            selected = unique(datos$level1)[1:10],
            multiple = TRUE
          )
          ,downloadButton("downloadButton_product", "Descargar Datos Filtrados")
          ,helpText("Aquí puedes descargar los datos filtrados")
        )
        ,column(width = 9, h4("Gráfico de Torta"), plotOutput("bar_chart_rubro"))
        ,column(width = 4, h4("Data Frame"), DTOutput("table_rubro"))
        
      )
      
      
    #   ,fluidRow(
    #     column(
    #       width = 3,
    #       h4("Configuración"),
    #       varSelectInput(
    #         inputId = "inputId_product",
    #         label = "Seleccionar variable:",
    #         data = datos[,c("CodigoCotizacion", "CodigoSolicitudCotizacion", "entCode")],
    #         selected = "CodigoCotizacion",
    #         multiple = FALSE
    #       ),
    #       selectInput(
    #         "region_filter_product", 
    #         "Seleccionar región:",
    #         choices = unique(datos$NombreRegion),
    #         selected = unique(datos$NombreRegion)[1],
    #         multiple = TRUE
    #       ),
    #       dateRangeInput(
    #         "date_filter_product", 
    #         "Seleccionar rango de fechas:",
    #         start = min(datos$FechaPublicacion),
    #         end = max(datos$FechaPublicacion),
    #         min = min(datos$FechaPublicacion),
    #         max = max(datos$FechaPublicacion)
    #       ),
    #       selectInput(
    #         "product_filter_product", 
    #         "Seleccionar Rubro:",
    #         choices = unique(datos$level1),
    #         selected = unique(datos$level1)[1:10],
    #         multiple = TRUE
    #       ),
    #       downloadButton("downloadButton_product", "Descargar Datos Filtrados"),
    #       helpText("Aquí puedes descargar los datos filtrados")
    #     )
    #     ,column(width = 9, h4("Gráfico de Torta"), plotOutput("bar_chart_product"))
    #     #,column(width = 4, h4("Data Frame"), DTOutput("table_product"))
    #     
    #   )
    #   
    #   
    #   
    #   
    #   ,    fluidRow(
    #     column(
    #       width = 3,
    #       h4("Configuración"),
    #       varSelectInput(
    #         inputId = "inputId_series",
    #         label = "Seleccionar variable:",
    #         data = datos[,c("CodigoCotizacion", "CodigoSolicitudCotizacion", "entCode")],
    #         selected = "ofertas",
    #         multiple = FALSE
    #       ),
    #       selectInput(
    #         "region_filter_series", 
    #         "Seleccionar región:",
    #         choices = unique(datos$NombreRegion),
    #         selected = unique(datos$NombreRegion),
    #         multiple = FALSE
    #       ),
    #       dateRangeInput(
    #         "date_filter_series", 
    #         "Seleccione rango de fechas:",
    #         start = min(datos$FechaPublicacion),
    #         end = max(datos$FechaPublicacion),
    #         min = min(datos$FechaPublicacion),
    #         max = max(datos$FechaPublicacion)
    #       ),
    #       selectInput(
    #         "product_filter_series", 
    #         "Seleccionar tipo de producto:",
    #         choices = unique(datos$level1),
    #         selected = unique(datos$level1)[1],
    #         multiple = TRUE
    #       )
    #     ),
    #     column(width = 5, h4("Gráfico de Series de Tiempo"), plotOutput("plot_series")),
    #     column(width = 4, h4("Vista de datos"), DTOutput("table_series"))
    #   )
   
    
     )
    ),
    tabPanel(
      "Series de Tiempo",
  
  )
)


#****************************************************************************************************
# server ============================================================================================
#****************************************************************************************************


server <- function(input, output, session) {
  
  # Función para actualizar opciones del filtro del rubro en tiempo real
  # 
  observe({
  
      updateSelectInput(session, "product_filter_rubro", choices = unique(datos_rubros$level1),
                        selected = input$product_filter_rubro)
    
  })
  
  # Función para actualizar opciones del filtro del rubro en tiempo real
  # 
  # observe({
  #   updateSelectInput(session, "product_filter_product", choices = unique(datos$level1),
  #                     selected = input$product_filter_product)
  # })
  
  # Función para actualizar opciones del filtro de tipo de producto en tiempo real
  # observe({
  #   updateSelectInput(session, "product_filter_series", choices = unique(datos$level1),
  #                     selected = input$product_filter_series)
  # })
  
  # Filtrar datos según la fecha y el tipo de producto seleccionados
  filtered_data_rubro <- reactive({
    datos_rubros[
     # datos$FechaPublicacion >= input$date_filter_product[1] &
    #    datos$FechaPublicacion <= input$date_filter_product[2] &
        datos$level1 %in% input$product_filter_product,
    ] 
  })
  
  # Filtrar datos según la fecha y el tipo de producto seleccionados
  # filtered_data_product <- reactive({
  #   datos[
  #     datos$FechaPublicacion >= input$date_filter_product[1] &
  #       datos$FechaPublicacion <= input$date_filter_product[2] &
  #       datos$level1 %in% input$product_filter_product &
  #       datos$`NombreRegion` %in% input$region_filter_product,
  #   ] %>% group_by(NombreRegion, level1) %>% 
  #     summarise(CodigoCotizacion = n_distinct(CodigoCotizacion)
  #               ,CodigoSolicitudCotizacion = n_distinct(CodigoSolicitudCotizacion)
  #               ,entCode = n_distinct(entCode))
  # })
  
  
  
  # Filtrar datos según la fecha y el tipo de producto seleccionados
  # filtered_data_series <- reactive({
  #   datos[
  #     datos$FechaPublicacion >= input$date_filter_product[1] &
  #       datos$FechaPublicacion <= input$date_filter_product[2] &
  #       datos$level1 %in% input$product_filter_product &
  #       datos$`NombreRegion` %in% input$region_filter_product,
  #   ] %>% group_by(FechaPublicacion, NombreRegion, level1) %>% 
  #     summarise(ofertas = n_distinct(CodigoCotizacion)
  #               ,solicitudes = n_distinct(CodigoSolicitudCotizacion)
  #               ,proveedores = n_distinct(entCode))
  # })
  
  #***************************************************************************
  # Servidor: Gráfico de barras ==============================================
  #***************************************************************************
  
  output$table_rubro <- renderDT({
    datatable(
      aggregate(get(input$inputId_rubro) ~ level1,
                data = filtered_data_rubro(), sum),
      options = list(scrollY = "700px", paging = FALSE),
      colnames = c('Código ONU (Nivel 1)' = 'level1', 'Total' = paste0("get(input$inputId_product)"))
    )
  })
  
  output$bar_chart_rubro <- renderPlot({
    variable_seleccionada <- input$inputId_rubro
    
    total_por_tipo <- filtered_data_rubro() %>%
      group_by(level1) %>%
      summarise(value = sum(!!sym(input$inputId_rubro)))
    
    total_por_tipo$level1 <- str_wrap(total_por_tipo$level1, width = 15)
    
    bar_chart <- ggplot(total_por_tipo, aes(x = reorder(level1, -value), y = value, fill = reorder(level1, -value))) +
      geom_bar(stat = "identity", color = "white", position = "dodge") +
      geom_text(
        aes(
          label = scales::comma(round(value), big.mark = ".", decimal.mark = ",")
        ),
        position = position_dodge(width = 1),
        vjust = -0.5,
        size = 4,
        color = "black"
      ) +
      labs(y = "Cantidad de ")+
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.width = unit(1, "cm"),  # Ajusta el ancho de la leyenda
        legend.key.height = unit(1, "cm")
        ,plot.title =  element_text(face = "bold", size = 100)
      ) +
      scale_fill_manual(
        name = "Rubro",
        values = scales::hue_pal()(length(unique(datos$level1)))
      ) +
      facet_grid(~NombreRegion, scales = "free_y", space = "free_y")+
      coord_flip()
    
    print(bar_chart)
  }, height = 500)
  
  #*************************************************************************************
  # Servidor: Gráfico de series de tiempo ==============================================
  #*************************************************************************************
  
  # Filtrar datos según la fecha y el tipo de producto seleccionados
  filtered_data_series <- reactive({
    datos[
      datos$FechaPublicacion >= input$date_filter_series[1] &
        datos$FechaPublicacion <= input$date_filter_series[2] &
        datos$level1 %in% input$product_filter_series &
        datos$`NombreRegion` %in% input$region_filter_series,
    ]
  })
  
  output$table_series <- renderDT({
    datatable(filtered_data_series(), options = list(scrollY = "400px", paging = TRUE))
  })
  
  output$plot_series <- renderPlot({
    
    variable_seleccionada <- input$inputId_series
    
    total_por_tipo <- filtered_data_series() %>%
      group_by(FechaPublicacion, NombreRegion, level1) %>%
      summarise(value = sum(!!sym(variable_seleccionada)))
    
    ggplot(total_por_tipo, aes(x = `FechaPublicacion`, y = value, group = level1)) +
      geom_line(aes(color = level1),  linewidth = 1.5) +
      labs(title = "Series de Tiempo", x = "Fecha", y = "ofertas") +
      theme_minimal() +
      theme(legend.position = "top") +
      scale_color_manual(
        name = "Rubro",
        values = setNames(
          scales::hue_pal()(length(unique(datos$level1))),
          unique(datos$level1)
        )
      )
  },height = 700)
  
  
  #*************************************************************************************
  # Servidor: Habilitar shinyjs ==============================================
  #*************************************************************************************
  
  shinyjs::enable("downloadButton_product")
  
  # Manejador para el botón de descarga
  output$downloadButton_product <- downloadHandler(
    filename = function() {
      paste("filtered_data_product", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data_product(), file, row.names = FALSE)
    }
  )
  
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
