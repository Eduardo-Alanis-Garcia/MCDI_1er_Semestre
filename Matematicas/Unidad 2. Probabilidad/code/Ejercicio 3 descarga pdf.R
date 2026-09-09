url_base = "https://smn.conagua.gob.mx/tools/DATA/Climatolog%C3%ADa/Pron%C3%B3stico%20clim%C3%A1tico/Temperatura%20y%20Lluvia/TMED/"
anios = 1993:2023



for (anio in anios) {
  
  url = paste0(url_base, anio, ".pdf")
  archivo = file.path("Unidad 2. Probabilidad/inputs/pdf/original/", paste0(anio, ".pdf"))
  
  tryCatch({
    
    download.file(
      url = url,
      destfile = archivo,
      mode = "wb",
      quiet = TRUE
    )
    
    cat("Descargado:", anio, "\n")
    
  }, error = function(e) {
    
    cat("Error en:", anio, "\n")
    
  })
  
}
