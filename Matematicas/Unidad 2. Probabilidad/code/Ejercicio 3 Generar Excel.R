archivos = list.files(path = "Unidad 2. Probabilidad/inputs/pdf/original/", full.names = T, pattern = "\\.pdf$")

anios = basename(archivos) |>  
  gsub(pattern = ".pdf", replacement = "") |> 
  stringr::str_squish() |> 
  as.numeric()


datos = tabulapdf::extract_tables("Unidad 2. Probabilidad/inputs/pdf/original/1993.pdf")[[1]]
datos = datos |> 
  dplyr::select(Estado) |> 
  dplyr::mutate(
    Estado = Estado |> stringr::str_squish()
      )


datos_anual = datos 

for (i in 1:length(archivos)) {
  cat("Vamos en", archivos[i] |>  basename(), "\n")
  
  tabla = tabulapdf::extract_tables(archivos[i])[[1]]
  df_anual = tabla |>  dplyr::select(Estado, Anual)
  names(df_anual)[2] = paste("Anual", anios[i])
  
  tabla = tabla |> 
    dplyr::select(-Anual)
  
  columnas = tabla |> names()
  columnas = columnas[columnas != "Estado"]
  
  tabla = tabla |> 
    dplyr::rename_with(
      .cols = dplyr::all_of(columnas),
      .fn = ~ paste0(.x, "_", anios[i]) |> stringr::str_squish()
    )
  
  tabla = tabla |> 
    dplyr::mutate(
      Estado = Estado |>  stringr::str_squish()
    )
  
  
  tabla |> openxlsx::write.xlsx(paste0("Unidad 2. Probabilidad/inputs/pdf/excel/", anios[i], ".xlsx"))
  
  
  datos = datos |> 
    dplyr::left_join(
      y = tabla,
      by = "Estado"
    )
  
  datos_anual = datos_anual |> 
    dplyr::left_join(
      y = df_anual,
      by = "Estado"
    )
  
}


datos |>  openxlsx::write.xlsx("Unidad 2. Probabilidad/outputs/Historicos Temperatura Media Clima.xlsx")
datos_anual |> openxlsx::write.xlsx("Unidad 2. Probabilidad/outputs/Historicos Temperatura Anual Media Clima.xlsx")
