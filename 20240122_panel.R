
loaded <- TRUE #¿Están los datos cargados? 

if (!loaded){
  source("C:/o/OneDrive - DCCP/Escritorio/Proyectos/Preferencias EMT y Compras Regionales/emt_pl_dccp/20240114 descarga datos.R")
}




# ui
ui <- fluidPage(
  titlePanel("Estadísticas de usuarios del sistema"),
  navbarPage(
    "",
    
    tabPanel(
      "Análisis de datos Compra Ágil",
      fluidRow(
        column(
          width = 3,
          
          #h4("Según rubro"),
          h4("Filtros"),
          varSelectInput(
            inputId = "inputId_rubro",
            label = "Seleccionar variable:",
            data = datos_rubros[,c("ofertas", "solicitudes", "proveedores")]
            ,selected = "ofertas",
            multiple = FALSE
          ),
          selectInput(
            "product_filter_rubros", 
            "Seleccionar Rubro:",
            choices = unique(datos_rubros$level1),
            selected = unique(datos_rubros$level1)[1:10],
            multiple = TRUE
          )
          ,downloadButton("downloadButton_rubros", "Descargar Datos Filtrados")
          ,helpText("Aquí puedes descargar los datos filtrados")
        )
        ,column(
                width = 9
                , h4("Los rubros más transados")
                , plotOutput("bar_chart_rubros")
                )
        #,column(width = 3, h4("Data Frame"), DTOutput("table_rubro"))
        
      )
      ,
      fluidRow(
        column(
          width = 3,
          
          #h4("Según rubro"),
          h4("Filtros"),
          varSelectInput(
            inputId = "inputId_rureg",
            label = "Seleccionar variable:",
            data = datos_rureg[,c("ofertas", "solicitudes", "proveedores")]
            ,selected = "ofertas",
            multiple = FALSE
          )
          ,selectInput(
            "product_filter_ruregreg", 
            "Seleccionar Región:",
            choices = unique(datos_rureg$NombreRegion.x),
            selected = unique(datos_rureg$NombreRegion.x)[13],
            multiple = TRUE
          )
          ,selectInput(
            "product_filter_ruregru", 
            "Seleccionar Rubro:",
            choices = unique(datos_rureg$level1),
            selected = unique(datos_rureg$level1)[1:10],
            multiple = TRUE
          )
          ,downloadButton("downloadButton_rureg", "Descargar Datos Filtrados")
          ,helpText("Aquí puedes descargar los datos filtrados")
        )
        ,column(
          width = 9
          , h4("Los rubros más transados según Región de Despacho")
          , plotOutput("bar_chartrureg")
          )
        #,column(width = 3, h4("Data Frame"), DTOutput("table_rubro"))
        
      )
      ,
      fluidRow(
        column(
          width = 3,
          
          #h4("Según rubro"),
          h4("Filtros"),
          varSelectInput(
            inputId = "inputId_ruregMonth",
            label = "Seleccionar variable:",
            data = datos_ruregMonth[,c("ofertas", "solicitudes", "proveedores")]
            ,selected = "ofertas",
            multiple = FALSE
          )
          ,selectInput(
            "product_filter_ruregregMonth", 
            "Seleccionar Región:",
            choices = unique(datos_ruregMonth$NombreRegion.x),
            selected = unique(datos_ruregMonth$NombreRegion.x)[13],
            multiple = TRUE
          )
          ,selectInput(
            "product_filter_ruregruMonth", 
            "Seleccionar Rubro:",
            choices = unique(datos_ruregMonth$level1),
            selected = unique(datos_ruregMonth$level1)[1:10],
            multiple = TRUE
          )
          ,downloadButton("downloadButton_ruregMonth", "Descargar Datos Filtrados")
          ,helpText("Aquí puedes descargar los datos filtrados")
        )
        ,column(
          width = 9
          , h4("Los rubros más transados según mes y Región de Despacho")
          , plotOutput("plot_ruregMonth")
          #,DTOutput("table_ruregMonth")
        )
        #,column(width = 3, h4("Data Frame"), DTOutput("table_rubro"))
        
      )
      

      
    )
  )
  # ,
  # tabPanel(
  #   "Series de Tiempo",
  #   
  # )
)


#****************************************************************************************************
# server ============================================================================================
#****************************************************************************************************


server <- function(input, output, session) {
  
  # Función para actualizar opciones del filtro del rubro en tiempo real
  # 
  observe({
    
    updateSelectInput(session, "product_filter_rubros", choices = unique(datos_rubros$level1),
                      selected = input$product_filter_rubros)
    
  })
  
  # Filtrar datos según la fecha y el tipo de producto seleccionados
  filtered_data_rubros <- reactive({
    datos_rubros[
      datos_rubros$level1 %in% input$product_filter_rubros,
    ] 
  })
  

  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
    
  observe({
    
    updateSelectInput(session, "product_filter_ruregreg", choices = unique(datos_rureg$NombreRegion.x),
                      selected = input$product_filter_ruregreg)
    
  })
  
  observe({
    
    updateSelectInput(session, "product_filter_ruregru", choices = unique(datos_rureg$level1),
                      selected = input$product_filter_ruregru)
    
  })
  
  
  # Filtrar datos según la fecha y el tipo de producto seleccionados
  filtered_data_rureg <- reactive({
    datos_rureg[
      (datos_rureg$level1 %in% input$product_filter_ruregru) & 
      (datos_rureg$NombreRegion.x %in% input$product_filter_ruregreg),
    ] 
  })
  
  
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  
  observe({
    
    updateSelectInput(session, "product_filter_ruregregMonth", choices = unique(datos_ruregMonth$NombreRegion.x),
                      selected = input$product_filter_ruregregMonth)
    
  })
  
  observe({
    
    updateSelectInput(session, "product_filter_ruregruMonth", choices = unique(datos_ruregMonth$level1),
                      selected = input$product_filter_ruregruMonth)
    
  })
  
  
  # Filtrar datos según la fecha y el tipo de producto seleccionados
  filtered_dataruregMonth <- reactive({
    datos_ruregMonth[
      (datos_ruregMonth$level1 %in% input$product_filter_ruregruMonth) & 
        (datos_ruregMonth$NombreRegion.x %in% input$product_filter_ruregregMonth),
    ] 
  })
  
  #***************************************************************************
  # Servidor: Gráfico de barras ==============================================
  #***************************************************************************
  
  output$table_rubro <- renderDT({
    datatable(
      aggregate(get(input$inputId_rubro) ~ level1,
                data = filtered_data_rubros(), sum),
      options = list(scrollY = "700px", paging = FALSE),
      colnames = c('Código ONU (Nivel 1)' = 'level1', 'Total' = paste0("get(input$inputId_rubro)"))
    )
  })
  
  output$bar_chart_rubros <- renderPlot({
    variable_seleccionada <- input$inputId_rubro
    
    total_por_tipo <- filtered_data_rubros() %>%
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
      labs(y = paste0("Cantidad de ",input$inputId_rubro))+
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
        values = scales::hue_pal()(length(unique(datos_rubros$level1)))
      ) 
    # +
    #   facet_grid(~NombreRegion.x, scales = "free_y", space = "free_y")+
    #   coord_flip()
    
    print(bar_chart)
  }, height = 500)
  
  #***************************************************************************
  # Servidor: Gráfico de barras según regiones==============================================
  #***************************************************************************
  
  output$table_rureg <- renderDT({
    datatable(
      aggregate(get(input$inputId_rubro) ~ level1,
                data = filtered_data_rubros(), sum),
      options = list(scrollY = "700px", paging = FALSE),
      colnames = c('Código ONU (Nivel 1)' = 'level1', 'Total' = paste0("get(input$inputId_rubro)"))
    )
  })
  
  output$bar_chartrureg <- renderPlot({
    variable_seleccionada <- input$inputId_rureg
    
    total_por_tipo <- filtered_data_rureg() %>%
      group_by(NombreRegion.x,level1) %>%
      summarise(value = sum(!!sym(input$inputId_rureg)))
    
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
      labs(y = paste0("Cantidad de ",input$inputId_rureg))+
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
        values = scales::hue_pal()(length(unique(datos_rureg$level1)))
      ) +facet_grid(~NombreRegion.x, scales = "free_y", space = "free_y")+
      coord_flip()+
      theme(strip.text.x = element_text(size = 25))
    
    print(bar_chart)
  }, height = 500)
  

  
    
  #***************************************************************************
  # Servidor: Gráfico de series de tiempo ====================================
  #***************************************************************************
  
  
  output$table_ruregMonth <- renderDT({
    datatable(
      total_por_tipo <- filtered_dataruregMonth() 
      #%>%
      #  mutate(value = !!sym(input$inputId_ruregMonth))
    )
  })
  
  output$plot_ruregMonth <- renderPlot({
    
      total_por_tipo <- filtered_dataruregMonth() %>%
        mutate(value = !!sym(input$inputId_ruregMonth))

      total_por_tipo$level1 <- str_wrap(total_por_tipo$level1, width = 15)

    
    
    # %>%
    #   group_by(month,NombreRegion.x,level1) %>%
    #   summarise(value = sum(!!sym(input$inputId_rubro)))
    # 
    # total_por_tipo$level1 <- str_wrap(total_por_tipo$level1, width = 15)
    
    
    (time_series <- ggplot(total_por_tipo, aes(x = month, y = value, color = level1)) +
      geom_line() +
      #  geom_point(aes(color = level1), na.rm = TRUE) +
      labs(title = "Series de Tiempo", x = "Fecha", y = "ofertas") +
      theme_minimal() +
      theme(legend.position = "top")+
      scale_color_manual(name = "Rubro", 
                         values = setNames(scales::hue_pal()(length(unique(datos_ruregMonth$level1))), 
                                           unique(datos_ruregMonth$level1)))+
      facet_grid(~NombreRegion.x, scales = "free_y", space = "free_y")+
      theme(strip.text.x = element_text(size = 25)))
    
    
  })
  
  
  #*************************************************************************************
  # Servidor: Habilitar shinyjs ==============================================
  #*************************************************************************************
  
  shinyjs::enable("downloadButton_rubros")
  
  # Manejador para el botón de descarga
  output$downloadButton_rubros <- downloadHandler(
    filename = function() {
      paste("filtered_data_rubros", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data_rubros(), file, row.names = FALSE)
    }
  )
  
  shinyjs::enable("downloadButton_ru_reg")
  
  # Manejador para el botón de descarga
  output$downloadButton_ru_reg <- downloadHandler(
    filename = function() {
      paste("filtered_data_ru_reg", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data_ru_reg(), file, row.names = FALSE)
    }
  )
  
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
