# Crea gráficos de TRANSACCIONES ====================================================================

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
      line_monto_plot <- plot_ly(x = ~1, y = ~resumen_line_monto$monto, type = "bar", 
                                 text = paste("Monto total transado:", resumen_line_monto$monto),
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



# Descarga los datos extraídos 
