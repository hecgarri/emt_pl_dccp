
consultar_y_guardar <- function(x,y, window = -11
                                ,wd_path = data_path
                                ,tipoConsulta = "cotizaciones compra ágil"
                                ,depurar = TRUE
                                ,updated = TRUE) {
  
  detalles = function(path = wd_path, pattern = "*.rds"){
    require(dplyr)
    
    details = file.info(path = paste0(wd_path), list.files(pattern=pattern))
    
    details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ] %>% 
      filter(isdir==FALSE)
    
    details$files = rownames(details)
    
    rownames(details) = NULL
    
    return(details)
  }
  
  datasets <- detalles(path = wd_path)
  
  
  
  if (!updated){
    
    con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles
    
    con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse
    
    if (length(years) == 0 | !tipoConsulta %in% c('cotizaciones compra ágil'
                                                  , 'órdenes de compra'
                                                  ,'usuarios proveedores')) {
      mensaje <- "Parámetros inválidos. Asegúrate de proporcionar años y/o tipo de consulta válidos."
      return(mensaje)
    }
    
    
    require(lubridate)
    
    # Es importante notar que la función switch es sensible al uso del operador de asignación
    # porque al usar <- en lugar de = arroja un error. OJO 
    
  ejecutarConsulta <- switch (tipoConsulta,
  'cotizaciones compra ágil' = function(x = x, y = y, window = window) {
                                  sqlQuery(con2, sprintf(
                                    paste(
                                      readLines(paste0("C:/o/OneDrive - DCCP/Escritorio/"
                                                        ,"Proyectos/Preferencias EMT y Compras Regionales/"
                                                        ,"emt_pl_dccp/Consultas departamento proveedores/"
                                                        ,"cotizaciones compra ágil.txt"),encoding = "latin1")
                                     , collapse = "\n")
                                                         ,x,y, window)
                                  ) 
                                },
	'órdenes de compra' = function(x = x, y = y, window = window) {
                              	  sqlQuery(con3,sprintf(
                              	    paste(
                              	      readLines(paste0("C:/o/OneDrive - DCCP/Escritorio/"
                              	                           ,"Proyectos/Preferencias EMT y Compras Regionales/"
                              	                           ,"emt_pl_dccp/Consultas departamento proveedores/"
                              	                           ,"órdenes de compra.txt"),encoding = "latin1")
                              	          , collapse = "\n"),x,y, window))
	  
	},
	'usuarios proveedores' = function(x = x, y = y, window = window) {
                              	  sqlQuery(con2,sprintf(
                              	    paste(
                              	      readLines(paste0("C:/o/OneDrive - DCCP/Escritorio/"
                              	                       ,"Proyectos/Preferencias EMT y Compras Regionales/"
                              	                       ,"emt_pl_dccp/Consultas departamento proveedores/"
                              	                       ,"usuarios proveedores.txt"),encoding = "latin1")
                              	      , collapse = "\n"),x,y, window))
	}
    )
    
    descargar_guardar <- function(x, y, window) {
      
      data <- ejecutarConsulta(x = x, y = y, window = window) 
      
      grupo1 = c('ofertan'
                 , 'adjudican'
                 ,'login'
                 ,'inscritos')
      
      if (depurar){
        if (tipoConsulta%in%grupo1){
          data <- data %>% 
            mutate(sello = ifelse(`Sello Mujer`=="Mujeres",1,0)) %>% 
            arrange(desc(sello)) %>% 
            filter(!duplicated(EntCode)) %>% 
            select(-sello)
        } else {
          data <- data %>% 
            mutate(sello = ifelse(`Sello Mujer`=="Mujeres",1,0)
                   ,codigo = paste0(Organismo,EntCode)) %>% 
            group_by(Organismo) %>% 
            arrange(desc(sello)) %>% 
            filter(!duplicated(codigo)) %>% 
            select(-sello)
        }
      }
      
      
      return(data)
    }
    
    total <- data.table::data.table()
    
    for (year in y) {
      if (year == year(today())) {
        start <- Sys.time()
        x <- month(today()) 
        y <- year
        window<- -(x-1)
        
        data <- descargar_guardar(x, y, window)
        total <- rbind(total, data)
        
        end <- Sys.time()
        
        tiempo_transcurrido <- difftime(end, start, units = "mins")
        
        cat("Descarga para el año", year, "completada en", round(tiempo_transcurrido,1), "minutos.", "\n")
        
      } else {
        start <- Sys.time()
        y <- year
        
        data <- descargar_guardar(x, y, window)
        total <- rbind(total, data)
        
        end <- Sys.time()
        tiempo_transcurrido <- difftime(end, start, units = "mins")
        
        cat("Descarga para el año", year, "completada en", round(tiempo_transcurrido,1), "minutos.", "\n")
      }
    }
    
    # Guarda el objeto data en un archivo con nombre diferente según el tipo de consulta
    saveRDS(total, file = paste0(gsub("-", "", today()),gsub(" ", "_", tipoConsulta), ".rds"))
  } else {
    total <- readRDS(file = file.path(wd_path, datasets[grep(gsub(" ","_", tipoConsulta), datasets$files)[1],c("files")]))  
  }
  
  return(total)
  
}
