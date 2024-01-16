
# Contenido de script1.R
cat("Ejecutando 20240114 descarga datos_rubros_region.R ...\n")

# Llamar a script2.R
#source("20240114 descarga datos.R")

cat("Fin de 20240114 descarga datos_rubros_region.R\n")

# datos_rubros_region <- datos %>% 
#   mutate(dia = as.Date(FechaPublicacion)) %>% 
#   group_by(dia, level1) %>% 
#   summarise(ofertas = n())

ui <- fluidPage(
  titlePanel("Datos compra Ágil"),
  navbarPage(
    "Análisis",
    tabPanel("por Rubro (Nivel 1, código ONU)",
             fluidRow(
               column(width = 3,
                      h4("Configuración")
                      ,helpText("En este panel, puedes filtrar los datos por variable, región, fecha y rubro de la solicitud (Codigo ONU)")
                      #Filtro de variable
                      ,varSelectInput(inputId = "inputId", label = "Seleccionar variable:"
                                      , data = datos_rubros_region[,c("ofertas", "solicitudes", "proveedores")]
                                      ,selected = "ofertas"
                                      , multiple = FALSE)
                      
                      # Filtro de región
                      ,selectInput("region_filter_product", "Seleccionar región:",
                                  choices = unique(datos_rubros_region$`NombreRegion`),
                                  selected = unique(datos_rubros_region$`NombreRegion`)[1],
                                  multiple = TRUE)
                      # Filtro de fecha
                      ,dateRangeInput("date_filter_product", "Seleccionar rango de fechas:",
                                     start = min(datos_rubros_region$FechaPublicacion), end = max(datos_rubros_region$FechaPublicacion),
                                     min = min(datos_rubros_region$FechaPublicacion), max = max(datos_rubros_region$FechaPublicacion))
                      # Filtro de tipo de producto
                      ,selectInput("product_filter_product", "Seleccionar Rubro:",
                                  choices = unique(datos_rubros_region$level1),
                                  selected = unique(datos_rubros_region$level1)[1:3], # Selecciona solo los primeros tres
                                  multiple = TRUE)
               ),
               column(width = 3,
                      h4("Data Frame"),
                      DTOutput("table_product")
               ),
               column(width = 6,
                      h4("Gráfico de Torta"),
                      plotOutput("pie_chart_product")
                      ,downloadButton("downloadButton", "Descargar Datos Filtrados")
                      ,helpText("Aquí puedes descargar los datos filtrados")
               )
             )
    ),
    tabPanel("Series de Tiempo",
             fluidRow(
               column(width = 3,
                      h4("Configuración")
                      ,varSelectInput(inputId = "inputId", label = "Seleccionar variable:"
                                      , data = datos_rubros_region[,c("ofertas", "solicitudes", "proveedores")]
                                      ,selected = "ofertas"
                                      , multiple = FALSE)
                      # Filtro de región
                      ,selectInput("region_filter", "Seleccionar región:",
                                  choices = unique(datos_rubros_region$`NombreRegion`),
                                  selected = unique(datos_rubros_region$`NombreRegion`),
                                  multiple = FALSE),
                      # Filtro de fecha
                      dateRangeInput("date_filter", "Seleccione rango de fechas:",
                                     start = min(datos_rubros_region$FechaPublicacion), end = max(datos_rubros_region$FechaPublicacion),
                                     min = min(datos_rubros_region$FechaPublicacion), max = max(datos_rubros_region$FechaPublicacion)),
                      # Filtro de tipo de producto
                      selectInput("product_filter", "Seleccionar tipo de producto:",
                                  choices = unique(datos_rubros_region$level1),
                                  selected = unique(datos_rubros_region$level1)[1], # Selecciona solo los primeros tres
                                  multiple = TRUE)
               ),
               column(width = 4,
                      h4("Vista de datos_rubros_region"),
                      DTOutput("table")
               ),
               column(width = 5,
                      h4("Gráfico de Series de Tiempo"),
                      plotOutput("plot")
               )
             )
    )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  # Función para actualizar opciones del filtro del rubro en tiempo real
  observe({
    updateSelectInput(session, "product_filter", choices = unique(datos_rubros_region$level1),
                      selected = input$product_filter)
  })
  
  # Función para actualizar opciones del filtro de tipo de producto en tiempo real
  observe({
    updateSelectInput(session, "product_filter_product", choices = unique(datos_rubros_region$level1),
                      selected = input$product_filter_product)
  })
  
  # Filtrar datos_rubros_region según la fecha y el tipo de producto seleccionados
  filtered_data <- reactive({
    datos_rubros_region[datos_rubros_region$FechaPublicacion >= input$date_filter[1] & datos_rubros_region$FechaPublicacion <= input$date_filter[2] &
                          datos_rubros_region$level1 %in% input$product_filter & datos_rubros_region$`NombreRegion` %in% input$region_filter, ]
  })
  
  # Filtrar datos_rubros_region según la fecha y el tipo de producto seleccionados
  filtered_data_product <- reactive({
    datos_rubros_region[datos_rubros_region$FechaPublicacion >= input$date_filter_product[1] & datos_rubros_region$FechaPublicacion <= input$date_filter_product[2] &
                          datos_rubros_region$level1 %in% input$product_filter_product & datos_rubros_region$`NombreRegion` %in% input$region_filter_product, ]
  })
  

  
  output$table_product <- renderDT({
    #variable_seleccionada <- input$value
    datatable(aggregate(get(input$inputId) ~ level1
        , data = filtered_data_product(), sum), options = list(scrollY = "400px", paging = FALSE))
  })
  

  output$pie_chart_product <- renderPlot({
    variable_seleccionada <- input$inputId
    
    total_por_tipo <- filtered_data_product() %>%
      group_by(level1, NombreRegion) %>%
      summarise(value = sum(!!sym(variable_seleccionada)))
    
    pie_chart <- ggplot(total_por_tipo, aes(x = level1, y = value, fill = level1)) +
      geom_bar(stat = "identity", color = "white", position = "dodge") +
      geom_text(aes(label = scales::comma(round(value), big.mark = ".", decimal.mark = ",")),
                position = position_dodge(width = 1), vjust = -0.5, size = 4, color = "black")+
      theme_minimal() +
      theme(legend.position = "top", axis.text.x = element_blank()) +
      scale_fill_manual(name = "Tipo de Producto", values = scales::hue_pal()(length(unique(datos_rubros_region$level1)))) +
      facet_grid(~NombreRegion, scales = "free_y", space = "free_y")
    
    print(pie_chart)
  })
  
  
  output$table <- renderDT({
    datatable(filtered_data(), options = list(scrollY = "400px", paging = TRUE))
  })
  
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = `FechaPublicacion`, y = ofertas, group = level1)) +
      geom_line(aes(color = level1)) +
      labs(title = "Series de Tiempo", x = "Fecha", y = "ofertas") +
      theme_minimal() +
      theme(legend.position = "top")+
      scale_color_manual(name = "Rubro", 
                         values = setNames(scales::hue_pal()(length(unique(datos_rubros_region$level1))), 
                                           unique(datos_rubros_region$level1)))
  })
  
  # Habilitar shinyjs
  shinyjs::enable("downloadButton")
  
  # Manejador para el botón de descarga
  output$downloadButton <- downloadHandler(
    filename = function() {
      paste("filtered_data_product", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(total_por_tipo(), file, row.names = FALSE)
    }
  )

}


# Crear la aplicación Shiny
shinyApp(ui, server)
