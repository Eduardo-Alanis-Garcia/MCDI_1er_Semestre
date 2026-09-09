datos = "Unidad 2. Probabilidad/outputs/Historicos Temperatura Media Clima para trabajar.xlsx" |> 
  readxl::read_excel()

datos = "Unidad 2. Probabilidad/outputs/Historicos Temperatura Media Clima Anual para trabajar.xlsx" |> 
  readxl::read_excel()


datos = datos |>
  dplyr::select(Fecha, Aguascalientes) |>
  dplyr::rename(Media = Aguascalientes) |>
  dplyr::mutate(
    `Anio computacional` = dplyr::row_number()
  ) |>
  dplyr::relocate(`Anio computacional`, .after = Fecha)

datos = datos |>
  dplyr::mutate(
    yt   = `Anio computacional`    * Media,
    yt_2 = `Anio computacional`^2  * Media,
    yt_3 = `Anio computacional`^3  * Media
  )

n = nrow(datos)                                          

t_1 = datos$`Anio computacional` |> sum(na.rm = T)
t_2 = (datos$`Anio computacional`^2) |> sum(na.rm = T)
t_3 = (datos$`Anio computacional`^3) |> sum(na.rm = T)
t_4 = (datos$`Anio computacional`^4) |> sum(na.rm = T)
t_5 = (datos$`Anio computacional`^5) |> sum(na.rm = T)
t_6 = (datos$`Anio computacional`^6) |> sum(na.rm = T)

y    = datos$Media |> sum(na.rm = T)
yt   = datos$yt    |> sum(na.rm = T)
yt_2 = datos$yt_2  |> sum(na.rm = T)
yt_3 = datos$yt_3  |> sum(na.rm = T)


# lineal

A_lineal = matrix(
  c(n,   t_1,
    t_1, t_2),
  nrow = 2, ncol = 2, byrow = T
)

b_lineal = c(y, yt)

x_lineal = solve(A_lineal) %*% b_lineal
x_lineal


# Caso cuadrático

A_cuadratico = matrix(
  c(n,   t_1, t_2,
    t_1, t_2, t_3,
    t_2, t_3, t_4),
  nrow = 3, ncol = 3, byrow = T
)

b_cuadratico = c(y, yt, yt_2)

x_cuadratico = solve(A_cuadratico) %*% b_cuadratico
x_cuadratico


# Caso cúbico

A_cubico = matrix(
  c(n,   t_1, t_2, t_3,
    t_1, t_2, t_3, t_4,
    t_2, t_3, t_4, t_5,
    t_3, t_4, t_5, t_6),
  nrow = 4, ncol = 4, byrow = T
)

b_cubico = c(y, yt, yt_2, yt_3)

x_cubico = solve(A_cubico) %*% b_cubico
x_cubico







##################
### Ecuaciones ###
##################

datos = datos |>
  dplyr::mutate(
    ajuste_lineal = x_lineal[1, 1] +
      x_lineal[2, 1] * `Anio computacional`,
    
    ajuste_cuadratico = x_cuadratico[1, 1] +
      x_cuadratico[2, 1] * `Anio computacional` +
      x_cuadratico[3, 1] * `Anio computacional`^2,
    
    ajuste_cubico = x_cubico[1, 1] +
      x_cubico[2, 1] * `Anio computacional` +
      x_cubico[3, 1] * `Anio computacional`^2 +
      x_cubico[4, 1] * `Anio computacional`^3
  )



cat("Ecuación lineal:    Y =", round(x_lineal[1,1], 4), "+", round(x_lineal[2,1], 4), "* t\n")

cat("Ecuación cuadrática: Y =", round(x_cuadratico[1,1], 4), "+", round(x_cuadratico[2,1], 4),
    "* t +", round(x_cuadratico[3,1], 4), "* t^2\n")

cat("Ecuación cúbica:    Y =", round(x_cubico[1,1], 4), "+", round(x_cubico[2,1], 4),
    "* t +", round(x_cubico[3,1], 4), "* t^2 +", round(x_cubico[4,1], 4), "* t^3\n")










# Grafica

library(ggplot2)

datos_grafica = datos |>
  tidyr::pivot_longer(
    cols = c(ajuste_lineal, ajuste_cuadratico, ajuste_cubico),
    names_to  = "Modelo",
    values_to = "Ajuste"
  ) |>
  dplyr::mutate(
    Modelo = dplyr::case_when(
      Modelo == "ajuste_lineal"     ~ "Lineal",
      Modelo == "ajuste_cuadratico" ~ "Cuadrático",
      Modelo == "ajuste_cubico"     ~ "Cúbico"
    )
  )







# Grafico


colores_modelo <- c(
  "Lineal"    = "#1f77b4",
  "Cuadrático" = "#d62728",
  "Cúbico"    = "#2ca02c"
)
# Si tus niveles de "Modelo" tienen otros nombres, ajusta los names() de arriba

ggplot(datos_largos, aes(x = `Anio computacional`)) +
  # Puntos observados
  geom_point(
    aes(y = Media),
    color = "grey20",
    fill  = "white",
    shape = 21,
    size  = 2.5,
    stroke = 0.8,
    alpha = 0.9
  ) +
  # Curvas ajustadas
  geom_line(
    aes(y = Ajuste, color = Modelo),
    linewidth = 1.1
  ) +
  scale_color_manual(values = colores_modelo) +
  labs(
    title    = "Ajuste por Mínimos Cuadrados - Temperatura Media Anual Aguascalientes",
    subtitle = "Comparación de modelos lineal, cuadrático y cúbico",
    x        = "Año computacional (t)",
    y        = "Temperatura media (°C)",
    color    = "Modelo",
    caption  = "Los puntos representan los valores originales; las líneas, los modelos."
  ) +
  theme_minimal(base_size = 13, base_family = "sans") +
  theme(
    plot.title       = element_text(face = "bold", size = 16, margin = margin(b = 4)),
    plot.subtitle    = element_text(color = "grey35", size = 11, margin = margin(b = 12)),
    plot.caption     = element_text(color = "grey50", size = 8.5, hjust = 0, margin = margin(t = 10)),
    axis.title       = element_text(face = "bold", size = 11),
    axis.text        = element_text(color = "grey25"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    plot.margin      = margin(15, 15, 10, 15)
  )

