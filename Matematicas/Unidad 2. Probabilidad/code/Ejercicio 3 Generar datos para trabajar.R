datos = "Unidad 2. Probabilidad/outputs/Historicos Temperatura Media Clima.xlsx" |> 
  readxl::read_excel()


datos = datos |> 
  tidyr::pivot_longer(
    cols = -Estado,
    names_to = "Fecha",
    values_to = "Temperatura"
  )


datos = datos |> 
  tidyr::pivot_wider(
    names_from = Estado,
    values_from = Temperatura
  )

datos |>  openxlsx::write.xlsx("Unidad 2. Probabilidad/outputs/Historicos Temperatura Media Clima para trabajar.xlsx")



# Verificar 
datos$Fecha |>  sub(pattern = ".*_", replacement = "") |>  table() |>  sort()






#############
### Anual ###
#############
datos = "Unidad 2. Probabilidad/outputs/Historicos Temperatura Anual Media Clima.xlsx" |> 
  readxl::read_excel()


datos = datos |> 
  tidyr::pivot_longer(
    cols = -Estado,
    names_to = "Fecha",
    values_to = "Temperatura"
  )


datos = datos |> 
  tidyr::pivot_wider(
    names_from = Estado,
    values_from = Temperatura
  )


datos = datos |> 
  dplyr::mutate(Fecha = Fecha |>  gsub(pattern = "Anual", replacement = "") |>  stringr::str_squish() |>  as.numeric())


datos |>  openxlsx::write.xlsx("Unidad 2. Probabilidad/outputs/Historicos Temperatura Media Clima Anual para trabajar.xlsx")
