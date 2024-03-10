datos_rubros_region <- datos %>% 
  mutate(FechaPublicacion = as.Date(FechaPublicacion)) %>% 
  group_by(NombreRegion, level1) %>% 
    summarise(ofertas = n_distinct(CodigoCotizacion)
              ,solicitudes = n_distinct(CodigoSolicitudCotizacion)
              ,proveedores = n_distinct(entCodes))