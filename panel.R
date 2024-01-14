
# Contenido de script1.R
cat("Ejecutando 20240114 descarga datos_.R ...\n")

# Llamar a script2.R
#source("20240114 descarga datos_.R.R")

cat("Fin de 20240114 descarga datos_.R\n")

# datos_ <- datos %>% 
#   mutate(dia = as.Date(FechaPublicacion)) %>% 
#   group_by(dia, level1) %>% 
#   summarise(ofertas = n())

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("datos_ compra Ágil"),
  navbarPage(
    "Análisis",
    tabPanel("Series de Tiempo",
             fluidRow(
               column(width = 3,
                      h4("Configuración"),
                      # Filtro de fecha
                      dateRangeInput("date_filter", "Seleccione rango de fechas:",
                                     start = min(datos_$FechaPublicacion), end = max(datos_$FechaPublicacion),
                                     min = min(datos_$FechaPublicacion), max = max(datos_$FechaPublicacion)),
                      # Filtro de tipo de producto
                      selectInput("product_filter", "Seleccionar tipo de producto:",
                                  choices = unique(datos_$level1),
                                  selected = unique(datos_$level1),
                                  multiple = TRUE)
               ),
               column(width = 3,
                      h4("Vista de datos_"),
                      DTOutput("table")
               ),
               column(width = 6,
                      h4("Gráfico de Series de Tiempo"),
                      plotOutput("plot")
               )
             )
    ),
    tabPanel("Tipo de Producto",
             fluidRow(
               column(width = 3,
                      h4("Configuración"),
                      # Filtro de fecha
                      dateRangeInput("date_filter_product", "Seleccionar rango de fechas:",
                                     start = min(datos_$FechaPublicacion), end = max(datos_$FechaPublicacion),
                                     min = min(datos_$FechaPublicacion), max = max(datos_$FechaPublicacion)),
                      # Filtro de tipo de producto
                      selectInput("product_filter_product", "Seleccionar tipo de producto:",
                                  choices = unique(datos_$FechaPublicacion),
                                  selected = unique(datos_$FechaPublicacion),
                                  multiple = TRUE)
               ),
               column(width = 3,
                      h4("Data Frame"),
                      DTOutput("table_product")
               ),
               column(width = 6,
                      h4("Gráfico de Torta"),
                      plotOutput("pie_chart_product")
               )
             )
    )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  observe({
    # Actualizar opciones del filtro de tipo de producto en tiempo real
    updateSelectInput(session, "product_filter", choices = unique(datos_$level1),
                      selected = input$product_filter)
  })
  
  observe({
    # Actualizar opciones del filtro de tipo de producto en tiempo real
    updateSelectInput(session, "product_filter_product", choices = unique(datos_$level1),
                      selected = input$product_filter_product)
  })
  
  # Filtrar datos_ según la fecha y el tipo de producto seleccionados
  filtered_data <- reactive({
    datos_[datos_$FechaPublicacion >= input$date_filter[1] & datos_$FechaPublicacion <= input$date_filter[2] &
           datos_$level1 %in% input$product_filter, ]
  })
  
  # Filtrar datos_ según la fecha y el tipo de producto seleccionados
  filtered_data_product <- reactive({
    datos_[datos_$FechaPublicacion >= input$date_filter_product[1] & datos_$FechaPublicacion <= input$date_filter_product[2] &
           datos_$level1 %in% input$product_filter_product, ]
  })
  
  output$table <- renderDT({
    datatable(filtered_data(), options = list(scrollY = "400px", paging = FALSE))
  })
  
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = `FechaPublicacion`, y = ofertas, group = level1)) +
      geom_line(aes(color = level1)) +
      labs(title = "Series de Tiempo", x = "Fecha", y = "ofertas") +
      theme_minimal() +
      scale_color_manual(name = "Rubro", 
                         values = setNames(scales::hue_pal()(length(unique(datos_$level1))), 
                                           unique(datos_$level1)))
  })
  
  output$table_product <- renderDT({
    datatable(filtered_data_product(), options = list(scrollY = "400px", paging = FALSE))
  })
  
  output$pie_chart_product <- renderPlot({
    total_por_tipo <- aggregate(ofertas ~ level1, data = filtered_data_product(), sum)
    pie_chart <- ggplot(total_por_tipo, aes(x = "", y = ofertas, fill = level1)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      labs(title = "Total por Rubro") +
      theme_minimal() +
      scale_fill_manual(name = "Tipo de Producto",
                        values = scales::hue_pal()(length(unique(datos_$level1))))
    print(pie_chart)
  })
}

# Crear la aplicación Shiny
shinyApp(ui, server)
