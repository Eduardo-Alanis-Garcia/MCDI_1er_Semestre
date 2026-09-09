# a)

data("USArrests")

datos =  USArrests

datos_centrados = datos |>  scale(center = T, scale = F) |>  as.data.frame()
lapply(datos_centrados, mean)


datos_centrados |>  head(n = 10)

#b)
matriz_covarianza = datos_centrados |>  cov() |>  as.data.frame()
lapply(datos_centrados, var)

datos_estandarizados = datos |>  scale(center = T, scale = T) |>  as.data.frame()
lapply(datos_estandarizados, var)
matriz_covarianza_estandarizada = datos_estandarizados |>  cov() |>  as.data.frame()

#c)
valores = matriz_covarianza_estandarizada |>  eigen()
valores_propios  = valores$values
vectores_propios = valores$vectors |>  as.data.frame()


#d) 
pca = datos |> prcomp(center = TRUE, scale. = TRUE)
componentes = pca[["x"]] |>  as.data.frame()

plot(x = componentes$PC1, y = componentes$PC2,
     xlab = "PC1", ylab = "PC2", pch = 19, col = "steelblue")
text(componentes$PC1, componentes$PC2, labels = rownames(datos), pos = 3, cex = 0.6)
  