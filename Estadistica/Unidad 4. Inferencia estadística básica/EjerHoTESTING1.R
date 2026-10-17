
#Upper Tail Test of Population Mean with Known Variance

#Problem
#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie.
#In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. 
#Assume that the population standard deviation is 0.25 grams. At .05 significance level, can we reject 
#the claim on food label?
 # 
 # Solution
#The null hypothesis is that mu0 = 2. We begin with computing the test statistic.

 xbar = 2.1             # sample mean 
 mu0 = 2                # hypothesized value 
 sigma = 0.25           # population standard deviation 
 n = 35                 # sample size 
 z = (xbar-mu0)/(sigma/sqrt(n)) 
 z                      # test statistic 

#We then compute the critical value at .05 significance level.

 alpha = .05 
#The function qnorm(): given an area, find the boundary value that determines this area.
 z.alpha = qnorm(1-alpha) 
 z.alpha                # critical value 
# 1.6449
#Answer
#The test statistic 2.3664 is greater than the critical value of 1.6449. Hence, at .05 significance level, 
 #we reject the claim that there is at most 2 grams of saturated fat in a cookie.

#Alternative Solution
#Instead of using the critical value, we apply the pnorm function to compute the upper tail p-value of 
#the test statistic. As it turns out to be less than the .05 significance level, we reject the null hypothesis
#that mu0 2.

 pval = pnorm(z, lower.tail=FALSE) 
 pval                   # upper tail p-value 
 #0.0089802
 
 
 #Two-Tailed Test of Population Mean with Unknown Variance
 
 
 #Problem
# Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 
 #35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. 
 #Assume the sample standard deviation is 2.5 kg. At .05 significance level, can we reject the null 
 #hypothesis that the mean penguin weight does not disffer from last year?
   
  # Solution
 #The null hypothesis is that mu0 = 15.4. We begin with computing the test statistic.
 
  xbar = 14.6            # sample mean 
  mu0 = 15.4             # hypothesized value 
  s = 2.5                # sample standard deviation 
  n = 35                 # sample size 
  t = (xbar-mu0)/(s/sqrt(n)) 
  t                      # test statistic 
# ???1.8931
 #We then compute the critical values at .05 significance level.
 
  alpha = .05 
  #La distribución t de Student es similar a la distribución normal, pero tiene colas más gruesas, lo que la hace adecuada para muestras pequeñas 
  #o cuando se desconoce la desviación estándar de la población
  # the qt function for the t distribution
  t.half.alpha = qt(1-alpha/2, df=n-1) 
  c(-t.half.alpha, t.half.alpha) 
 # -2.0322  2.0322
 #Answer
 #The test statistic -1.8931 lies between the critical values -2.0322, and 2.0322. Hence, at .05 significance
  #level, we do not reject the null hypothesis that the mean penguin weight does not differ from last year.
 
#Alternative Solution
 #Instead of using the critical value, we apply the pt function to compute the two-tailed p-value of the test 
#statistic. It doubles the lower tail p-value as the sample mean is less than the hypothesized value. 
#Since it turns out to be greater than the .05 significance level, we do not reject the null hypothesis that ?? = 15.4.
 
  pval = 2 * pt(t, df=n-1)  # lower tail 
  pval                      # two???tailed p???value 
 # 0.066876
  
  
#t-test: Comparing Group Means
  
#The assumption for the test is that both groups are sampled from normal distributions with equal variances
  
#The null hypothesis is that the two means are equal, and the alternative is that they are not
  
#Population Mean Between Two Independent Samples
  

  mtcars$mpg  
  mtcars$am 
  
#Problem
#Assuming that the data in mtcars follows the normal distribution, find the 95% confidence interval 
  #estimate of the difference between the mean gas mileage of manual and automatic transmissions.
  
  #Solution
  #the gas mileage for automatic transmission can be listed as follows:
    
   L = mtcars$am == 0 
   mpg.auto = mtcars[L,]$mpg 
   mpg.auto                    # automatic transmission mileage 
  # 21.4 18.7 18.1 14.3 24.4 ...
  #By applying the negation of L, we can find the gas mileage for manual transmission.
  
   mpg.manual = mtcars[!L,]$mpg 
   mpg.manual                  # manual transmission mileage 
  #[1] 21.0 21.0 22.8 32.4 30.4 ...
  #We can now apply the t.test function to compute the difference in means of the two sample data.
  
   t.test(mpg.auto, mpg.manual)   
#Answer
  # In mtcars, the mean mileage of automatic transmission is 17.147 mpg and the manual transmission is 24.392 
   #mpg. The 95% confidence interval of the difference in mean gas mileage is between 3.2097 and 11.2802 mpg. 
   
   
#One-sample t-tests: Compare the sample mean with a known value, when the variance of the population is unknown.
   
   
   library(ggplot2)        # plotting & data
   library(dplyr)          # data manipulation
   library(tidyr)          # data re-shaping
   library(magrittr)       # pipe operator
   library(gridExtra)      # provides side-by-side plotting
   library(knitr)         #provides kable function
   head(midwest)
#The one-sample t-test compares a sample's mean with a known value, when the variance of the population is unknown.
   #Consider we want to assess the percent of college educated adults in the midwest and compare it to a certain value.
   #For example, let's assume the nation-wide average of college educated adults is 32% (Bachelor's degree or higher) and we
   #want to see if the midwest mean is significantly different than the national average; in particular we want to test if 
   #the midwest average is less than the national average.  
   
#
   head(midwest$percollege, 10)
   summary(midwest$percollege)   
   p1 <- ggplot(midwest, aes(percollege)) + 
     geom_histogram(fill = "white", color = "grey30")
   
   p2 <- ggplot(midwest, aes(percollege)) + 
     geom_histogram(fill = "white", color = "grey30") +
     scale_x_log10()
   
   grid.arrange(p1, p2, ncol = 2)   
   
   
#To test if the midwest average is less than the national average I'll perform three tests. First I test with a normal t.test without any distribution transformations. The results below show a p-value < .001 supporting the alternative hypothesis that "the true mean is less than 32%.   
   
   t.test(midwest$percollege, mu = 32, alternative = "less")   

#Alternatively, due to the non-normality concerns we can perform this test in two additional ways to ensure our results are not being biased due to assumption violations. We can perform the test with t.test and transform our data and we can also perform the nonparametric test with the wilcox.test function. Both results support our initial conclusion that the percent of college educated adults in the midwest is statistically less than the nationwide average.
   t.test(log(midwest$percollege), mu = log(32), alternative = "less")
   wilcox.test(midwest$percollege, mu = 32, alternative = "less")   

   
#Two-sample t-test
   
#Now let's say we want to compare the differences between the average percent of college educated adults in Ohio versus Michigan. Here, we want to perform a two-sample t-test.
   
  
   
   df <- midwest[midwest$state == "OH" | midwest$state == "MI", c("state","percollege")]
   # Ohio summary stats
   summary(df[df$state == "OH", "percollege"])
   summary(df[df$state == "MI", "percollege"])
#We can see Ohio appears to have slightly less college educated adults than Michigan but the graphic doesn't tell us if it is statistically significant or not.   
   ggplot(df, aes(state, percollege)) +
     geom_boxplot()   

#We also see similar skewness within the sample distributions.
   
   p1 <- ggplot(df, aes(percollege)) +
     geom_histogram(fill = "white", color = "grey30") +
     facet_wrap(~ state)
   
   p2 <- ggplot(df, aes(percollege)) +
     geom_histogram(fill = "white", color = "grey30") +
     facet_wrap(~ state) +
     scale_x_log10()
   
   grid.arrange(p1, p2, nrow = 2)
#Similar to the previous section, test if the Ohio and Michigan averages differ I'll perform three tests. Also, note that I am searching for any differences between the means rather than if one is specifically less than or greater than the other. First I test with a normal t.test without any distribution transformations. The results below show a p-value < .01 supporting the alternative hypothesis that "true difference in means is not equal to 0"; essentially it states there is a statistical difference between the two means. 
   
t.test(percollege ~ state, data = df) 

#Alternatively, due to the non-normality concerns we can perform this test in two additional ways to ensure our results are not being biased due to assumption violations. We can perform the test with t.test and transform our data and we can also perform the nonparametric test with the wilcox.test function. Both results support our initial conclusion that the percent of college educated adults in Ohio is statistically different than the percent in Michigan.
t.test(log(percollege) ~ state, data = df)

#Paired t-test
#To illustrate the paired t-test I'll use the built-in sleep data set.
sleep
ggplot(sleep, aes(group, extra)) +
  geom_boxplot()
#In this case we are assessing if there is a statistically significant effect of a particular drug on sleep (increase in hours of sleep compared to control) for 10 patients. See sleep for more details on the variables. We want to see if the mean values for the extra variable differs between group 1 and group 2. Here, we perform the t.test as in the previous sections but just add the paired = TRUE argument:
t.test(extra ~ group, data = sleep, paired = TRUE)

#In this example it appears that the drug does have an effect as the p-value = 0.0028 suggesting that the drug increases sleep on average by 1.58 hours.

#Diferencia entre dos medias
#En muchas investigaciones, se desea inferir si las medias de dos poblaciones son iguales; esto se puede realizar analizando las diferencias entre muestras tomadas aleatoriamente de esas poblaciones.

#Cuando las dos muestras a compararse, cumplen con las siguientes condiciones:
 # provienen de dos poblaciones con distribuci?n normal,
#las poblaciones tienen varianzas iguales,
#podemos usar el estadistico t:
#Prueba t de dos colas
#Ejemplo 1: Tenemos datos de un experimento con dos drogas (A y B) que afectan la coagulación de la sangre (medida en minutos) en conejos adultos. Las hipótesis son:
library(kableExtra)
#datos
drogaA <- c(8.8,8.4,7.9,8.7,9.1,9.6)
drogaB <- c(9.9,9.0,11.1,9.6,8.7,10.4,9.5)
#tabla de datos
tabla <- list(Droga_A = drogaA, Droga_B = drogaB)
as.data.frame(lapply(tabla, `length<-`, max(sapply(tabla, length))))
#tama?o de las muestras
nA <- length(drogaA)
nB <- length(drogaB)
#grados de libertad
vA = nA - 1
vB = nB - 1
#medias
mediaA <- mean(drogaA)
mediaB <- mean(drogaB)
#sumas de cuadrados de diferencias
ssA <- sum((drogaA - mean(drogaA))^2)
ssB <- sum((drogaB - mean(drogaB))^2)
#tablas parámetros de las muestras
dA <- data.frame(nA, vA, mediaA, ssA)
dB <- data.frame(nB, vB, mediaB, ssB)
dA
#varianza agrupada
s2p <- (ssA+ssB)/(vA+vB)
sprintf("s2_p: %g", s2p)

#error estándar de la diferencia entre medias
sxAxB <- sqrt((s2p/nA) + (s2p/nB))
sprintf("s_xA-xB: %g", sxAxB)
#cálculo de t
tcalc <- abs((mediaA-mediaB)/sxAxB)
sprintf("t-calculada: %g", tcalc)

#t para un alfa dado
alfa = 0.05
P1 = 1 - alfa
P2 = 1 - alfa/2
gl = vA + vB
t1 <- qt(P1, gl)
t2 <- qt(P2, gl)
sprintf("t_1cola: %g", t1)
sprintf("t_2colas: %g", t2)
#p-errorI para la t-calculada
Ptcalc1 <- pt(tcalc, gl, lower.tail = FALSE)
sprintf("P_errorI_1cola: %g", Ptcalc1)
sprintf("P_errorI_2cola: %g", Ptcalc1*2)
#Prueba t dos colas con R
#hip?tesis alterna: droga A - drogaB diferente de 0 (menor o mayor)
pruebat <- t.test(drogaA,drogaB, var.equal = TRUE, alternative = "two.sided")
pruebat

#Prueba t de una cola
#Cuando el investigador esta interesado en probar la diferencia entre muestras en una dirección, las hipótesis pasan a ser una de las siguientes alternativas

#Ejemplo 2: Un jardinero experimenta con un nuevo fertilizante, el cual es anunciado como mejor a otro que ha estado usando el jardinero. El jardinero aplica una misma cantidad del fertilizante tradicional (T) a 10 plantas de una especie, y del nuevo fertilizante (N) a 8 plantas de la misma especie; se sabe que esta especie responde a los fertilizantes con un mayor crecimiento en altura (cm). El jardinero quiere probar si el nuevo fertilizante produce una media de altura (??2) mayor que la media de altura (??1) de las plantas con fertilizante tradicional:

library(kableExtra)
#datos
fertT <- c(48.2,54.6,58.3,47.8,51.4,52.0,55.2,49.1,49.9,52.6)
fertN <- c(52.3,57.4,55.6,53.2,61.3,58.0,59.8,54.8)
#tabla de datos
tabla <- list(Fert_Trad. = fertT, Fert_Nuevo = fertN)
as.data.frame(lapply(tabla, `length<-`, max(sapply(tabla, length))))
#tama?o de las muestras
nT <- length(fertT)
nN <- length(fertN)
#grados de libertad
vT = nT - 1
vN = nN - 1
#medias
mediaT <- mean(fertT)
mediaN <- mean(fertN)
#sumas de cuadrados de diferencias
ssT <- sum((fertT - mean(fertT))^2)
ssN <- sum((fertN - mean(fertN))^2)
#tablas parámetros de las muestras
dT <- data.frame(nT, vT, mediaT, ssT)
dN <- data.frame(nN, vN, mediaN, ssN)
#varianza agrupada
s2p <- (ssT+ssN)/(vT+vN)
sprintf("s2_p: %g", s2p)
#error estandar de la diferencia entre medias
sxTxN <- sqrt((s2p/nT) + (s2p/nN))
sprintf("s_xT-xN: %g", sxTxN)
#cálculo de t
t.calc <- abs((mediaT-mediaN)/sxTxN)
sprintf("t-calculada: %g", t.calc)
#t para un alfa dado
alfa = 0.05
Pr = 1 - alfa
gl = vT + vN
t.alfa <- qt(Pr, gl)
sprintf("t_alfa1cola: %g", t.alfa)
#p-errorI para la t-calculada
Prcalc1 <- pt(t.calc, gl, lower.tail = FALSE)
sprintf("P_errorI_1cola: %g", Prcalc1)
#Prueba t una cola con R

#hip?tesis alterna: fertT - fertN < 0
pruebat <- t.test(fertT, fertN, var.equal = TRUE, alternative = "less")
pruebat


#Supuestos para la prueba t
#Antes de continuar, debemos recordar que para aplicar las pruebas de hipótesis, usando la t de Student, asumimos: (i) una distribución normal de los datos de cada muestra, y (ii) varianzas iguales de las muestras. A continuación se muestran las pruebas para estos supuestos.
#Prueba de distribución normal para muestras aleatorias
#Gráficamente podemos observar si los datos de una muestra se ajustan a una distribución normal, usando un 'q-q normal plot', en el cual se grafican los cuantiles estandarizados de los valores de las muestras y los cuantiles teóricos. Si la distribución de los datos es normal, los puntos deben caer sobre la recta de igualdad.
#datos ejemplo 1
drogaA <- c(8.8,8.4,7.9,8.7,9.1,9.6)
drogaB <- c(9.9,9.0,11.1,9.6,8.7,10.4,9.5)
#gráficas
qqnorm(drogaA, main = "Droga A"); qqline(drogaA)
qqnorm(drogaB, main = "Droga B"); qqline(drogaB)
#También podemos utilizar pruebas estadásticas para probar la normalidad; en estas pruebas la normalidad es la hipótesis nula, por lo tanto si obtenemos un valor de probabilidad menor que un nivel \alfa establecido, entonces tenemos que rechazar la hipótesis nula, y no tenemos normalidad en los datos.

library(nortest)
#prueba de normalidad Shapiro-Wilks
shapiro.test(drogaA)
shapiro.test(drogaB)
#prueba de normalidad Kolmogorov-Smirnov
ks.test(drogaA, "pnorm", mean(drogaA), sqrt(var(drogaA)))
ks.test(drogaB, "pnorm", mean(drogaB), sqrt(var(drogaB)))

#Prueba de varianzas iguales para muestras
#El otro aspecto a probar, para poder usar la prueba t regular, es la igualdad de las varianzas de las muestras. Esto se puede realizar usando la prueba de razón de varianzas, cuando los datos tienen distribución normal. En caso de no-normalidad, se pueden realizar diversas pruebas, como la de Levene.

#Las hipótesis a probar son:
library(kableExtra)
#datos ejemplo 1
drogaA <- c(8.8,8.4,7.9,8.7,9.1,9.6)
drogaB <- c(9.9,9.0,11.1,9.6,8.7,10.4,9.5)
#tamaño de las muestras
nA <- length(drogaA)
nB <- length(drogaB)
#grados de libertad
vA = nA - 1
vB = nB - 1
#sumas de cuadrados de diferencias
ssA <- sum((drogaA - mean(drogaA))^2)
ssB <- sum((drogaB - mean(drogaB))^2)
#varianzas
s2A <- ssA/vA
s2B <- ssB/vB
#tablas parámetros de las muestras
dA <- data.frame(nA, vA, ssA, s2A)
dB <- data.frame(nB, vB, ssB, s2B)
#estadistico F calculado
Fcalc <- s2A/s2B
sprintf("F calculado: %g", Fcalc)
#F teórico para alfa = 0.05 (dos colas)
Ftabla <- qf(c(0.025,0.975), vA, vB)
sprintf("F alfa,v1,v2: %g", Ftabla)
#Probabilidad error tipo I
Pr <- pf(Fcalc, vA, vB)
sprintf("Pr Fcalc: %g", Pr)
#Si el valor de F no es significativo, no rechazamos la hipótesis nula de igualdad de las varianzas.

#Cuando los valores de las muestras no se ajustan a la distribución normal, podemos usar la prueba de Levene para la igualdad de varianza; las hipótesis son las mismas de la prueba anterior.
library(car)
#acomodando los datos en data.frame con droga como grupo
df.droga <- data.frame(droga=c(rep("A", times=length(drogaA)),
                               rep("B", times=length(drogaB))),
                       min=c(drogaA, drogaB))
df.droga
#prueba de Levene para dos varianzas, alfa = 0.05
leveneTest(df.droga$min, df.droga$droga, location=c("mean"))
#Prueba t con varianzas diferentes
#Cuando ocurre que las varianzas resultan ser diferentes, según las pruebas anteriores, pero los datos tienen distribución normal, o el tamaño de muestra es grande (> 30), podemos usar una variante de la prueba t para varianzas diferentes (prueba Behrens-Fisher).

#El estadístico t a usar se transforma en el estadístico t', debido a que no se puede calcular una varianza agrupada (sp2) al ser las varianzas diferentes.
#Ejemplo 4: Los datos son los tiempos (días) que toman en eclosionar huevos de cucaracha sometidos a dos temperaturas (T1:30?C, T2:10?C). Queremos comprobar las hip?tesis:
#pero antes haremos la prueba de igualdad de varianza.
library(car)
#datos tiempo para eclosion en dias
T1 <- c(40,38,32,37,39,41,35)
T2 <- c(36,45,32,52,59,41,48,55)
#ahora probaremos si las varianzas son iguales
#primero acomodamos los datos en data.frame con Temp como grupo
df.T <- data.frame(Temp=c(rep("T130C", times=length(T1)),
                          rep("T210C", times=length(T2))),
                   dias=c(T1, T2))
#prueba de Levene para dos varianzas, alfa = 0.05
leveneTest(df.T$dias, df.T$Temp, location=c("mean"))
#Rechazamos la hipotesis de que las varianzas son iguales, y procedemos a realizar la prueba de diferencias de las medias usando t'.
library(kableExtra)
#estad?sticas b?sicas
nT1 <- length(T1)
nT2 <- length(T2)
vT1 <- nT1 - 1
vT2 <- nT2 - 1
mediaT1 <- mean(T1)
mediaT2 <- mean(T2)
varT1 <- var(T1)
varT2 <- var(T2)
var.mediaT1 <- varT1/nT1
var.mediaT2 <- varT2/nT2
#tablas
dT1 <- data.frame(nT1, vT1, mediaT1, varT1, var.mediaT1)
dT2 <- data.frame(nT2, vT2, mediaT2, varT2, var.mediaT2)
#error est?ndar de la diferencia de las medias
s.prima <- sqrt(var.mediaT1 + var.mediaT2)
sprintf("error est?ndar dif medias: %g", s.prima)
#t prima para varianzas diferentes
t.prima <- (mediaT1 - mediaT2)/s.prima
sprintf("t': %g", t.prima)
#grados de libertad ajustados
v.prima <- ((var.mediaT1 + var.mediaT2)^2)/(((var.mediaT1^2)/vT1) + ((var.mediaT2^2)/vT2))
sprintf("v': %g", v.prima)
#Para obtener el valor teorico podemos interpolar entre los valores disponibles en la tabla t regular (o usar el valor entero inferior de v').

#Tambi?n podemos usar funciones de R:
tprimoteorico <- qt(0.025,8.7)
sprintf("t'(0.05(2),8.7): %g", tprimoteorico)
Pr <- pt(-2.44373, 8.7)*2
sprintf("Pr t': %g", Pr)
#En conclusión, rechazamos la H0 de igualdad de las medias de los dias hasta eclosion, y aceptamos la hipotesis alterna sobre el efecto de la temperatura en el tiempo de eclosión.
#Intervalo de confianza para las medias


#Para una muestra utilizamos la formula del intervalo de confianza (IC), para una probabilidad dada (P), usualmente 95%
#Usaremos los datos del Ejemplo 1, con una s2p = 0.5193 min2:
library(kableExtra)
#datos
drogaA <- c(8.8,8.4,7.9,8.7,9.1,9.6)
drogaB <- c(9.9,9.0,11.1,9.6,8.7,10.4,9.5)
#tabla de datos
tabla <- list(Droga_A = drogaA, Droga_B = drogaB)
as.data.frame(lapply(tabla, `length<-`, max(sapply(tabla, length))))
#tama?o de las muestras
nA <- length(drogaA)
nB <- length(drogaB)
#grados de libertad
vA = nA - 1
vB = nB - 1
#medias
mediaA <- mean(drogaA)
mediaB <- mean(drogaB)
#sumas de cuadrados de diferencias
ssA <- sum((drogaA - mean(drogaA))^2)
ssB <- sum((drogaB - mean(drogaB))^2)
#varianza agrupada
s2p <- (ssA+ssB)/(vA+vB)
#tabla de calculos
df.calc <- data.frame(mediaA, nA, mediaB, nB, s2p)
kable(df.calc, format = "markdown", col.names = c("m droga A", "n droga A", "m droga B", "n droga B", "var. agrupada"))
#ic media droga A
icA <- qt(0.975, vA+vB)*sqrt(s2p/nA)
l.icA <- mediaA - icA
h.icA <- mediaA + icA
df.icA <- data.frame(l.icA, mediaA, h.icA)
#ic media droga B
icB <- qt(0.975, vA+vB)*sqrt(s2p/nB)
l.icB <- mediaB - icB
h.icB <- mediaB + icB
df.icB <- data.frame(l.icB, mediaB, h.icB)



#Selección de pruebas de hipótesis para dos muestras


#Vamos a repasar las alternativas para pruebas de hipótesis con dos muestras:
  
#Si las dos muestras representan problaciones distribuidas normalmente, y sus varianzas son iguales (o si se desvian moderadamente de una distribuci?n normal, y sus varianzas son semejantes): se usa la prueba t para dos muestras, para un alfa establecido (si hay algo de desviaci?n de la normalidad, el valor de error tipo I debe estar muy por debajo de alfa para llegar a una conclusi?n sobre las hip?tesis).

#Si las dos poblaciones muestreadas tienen distribución normal (o con poca desviación), pero sus varianzas son muy diferentes: podemos usar la prueba t modificada de Behrens-Fisher.

#Si las dos poblaciones muestreadas tienen distribuciones muy diferentes de la normalidad, pero su forma o varianza son similares: se pueden utilizar pruebas no-paramtericas, que no requieren el cumplimiento extricto de los supuestos para las pruebas parametricas.
#Si no hay normalidad, ni similitud en las varianzas: las alternativas pueden ser:

#usar transformaciones de los datos, para tratar de llegar a una distribución más o menos normal, y con varianza similar;
#buscar pruebas no-paramétricas especiales;
#reportar solamente los resultados y gráficas, sin llegar a conclusiones definitivas. También se puede considerar repetir las observaciones o experimentos con un mayor tamaño de las muestras.



#Pruebas no-paramétricas



#En las pruebas de hipótesis anteriores, se hacen inferencias acerca de los parametros de las poblaciones de donde se sacan las muestras, examinando los estad?sticos de estas muestras.

#En aquellos casos en que no se cumplen los supuestos de estos metodos parametricos, podemos acudir a metodos de comparacion que no recurren a esos parametros, y por eso se denominan no-parametricos.

#Las pruebas no-parametricas más comunes son:
 # Prueba de Mann-Whitney
#Prueba de rango-suma de Wilcoxon ('Wilcoxon rank-sum')

#Prueba de Mann-Whitney para dos muestras
#Para este tipo de pruebas no se utilizan los valores reales de los datos, sino el rango de las mediciones ordenadas.

#Las mediciones de cada muestra se ordenan de menor a mayor (también puede ser al reves), y luego se le procede a dar un numero de rango, tomando en cuenta los valores de ambas muestras.

#Si hay valores iguales, se les asigna el valor promedio de los rangos que le correspondería si no fueran iguales, pero contiguos.

#Una vez determinados los rangos, se calcula la suma de rangos de cada muestra (R1 y R2).

#El estadístico para la prueba es el siguiente (n1 y n2 son los tamaños de cada muestra):
#Para una prueba de dos colas, se utiliza el valor mayor entre U y U'; para la prueba de una cola (con los datos en orden de menor a mayor) se decide según los siguientes criterios:

#si la hipotesis alterna es que el grupo 1 es menor que el grupo 2, se utiliza U

#si la hipotesis alterna es que el grupo 1 es mayor que el grupo 2, se utiliza U'

#Ejemplo 5: Se desea saber si el adiestramiento en escritura con teclado en la escuela superior, se refleja en una mayor velocidad de escritura en la universidad; el grupo 1 recibió adiestramiento en escritura con teclado en la escuela superior, y el grupo 2 no la recibió. Las hipótesis son:
  
#  H0: La velocidad de escritura con teclado en la universidad no es mayor en los estudiantes que recibieron adiestramiento en la escuela superior.

# HA: La velocidad de escritura con teclado en la universidad es mayor en los estudiantes que recibieron adiestramiento en la escuela superior.

library(kableExtra)
#datos: grupo 1 si tuvo adiestramiento previo (siT),
# grupo 2 no tuvo adiestramiento previo (noT) 
siT <- c(44,48,36,32,51,45,54,56)
noT <- c(32,40,44,44,34,30,26)
#cálculo de tamaños de muestras
n.siT <- length(siT)
n.noT <- length(noT)
#creación de data frame de factor 'Tr' y medición 'wpm'
df.T <- data.frame(Tr=c(rep("ST", times=length(siT)),
                        rep("NT", times=length(noT))),
                   wpm=c(siT, noT))
#asignación de rangos de menor a mayor, con promedio de iguales
R <- rank(df.T$wpm, ties.method = "average")
#tabla con resultados
ranked <- cbind(df.T,R)
ranked
#suma de rangos de cada grupo
Ri <- aggregate(ranked$R, list(ranked$Tr), sum)
#tabla de valores de R1 (RST) y  R2 (RNT)
RNT <- Ri[1,2]
RST <- Ri[2,2]
Up <- n.noT*n.siT + (n.noT*(n.noT + 1))/2 - RNT
U <- n.siT*n.noT + (n.siT*(n.siT + 1))/2 - RST
Ues <- data.frame(n.siT, n.noT, RST, RNT, U, Up)
kable(Ues, format = "markdown", col.names = c("n1", "n2", "R1", "R2", "U", "U'"))
#Usamos el valor de U', porque la hip?tesis alterna establece que el grupo 1 (siT) es m?s r?pido que el grupo 2 (noT).

#Debemos buscar el valor de U0.05(1),8,7 de una tabla 'U-Mann-Whitney'. En este caso el valor es 13, entonces U'(47.5)>U0.05(1),8,7(13), por lo tanto podemos rechazar la hipótesis nula, y aceptar la alterna (más velocidad de escritura en teclado en la universidad, si los estudiantes reciben adiestramiento en la escuela superior).

#Otra prueba no-parametrica es la de 'Wilcoxon rank-sum', y a continuación la usamos con un procedimiento directo en R
wilcox.test(siT, noT, alternative = c("greater"))
#valor cr?tico para una cola, alfa = 0.05
Wc <- qwilcox(0.05, 8, 7, lower.tail = FALSE)
sprintf("W 0.05(1),8,7: %g", Wc)

#prueba chi-cuadrado (datos categoticos)

# chi-cuadrada de homogeneidad
#use a chi-squared goodness-of-fit test to check whether the race demographics of members at your church or school match that of the entire U.S. population or whether the computer browser preferences of your friends match those of Internet uses as a whole.

#When working with categorical data the values the observations themselves aren't of much use for statistical testing because categories like "male", "female," and "other" have no mathematical meaning. Tests dealing with categorical variables are based on variable counts instead of the actual value of the variables themselves.

#Let's generate some fake demographic data for U.S. and Minnesota and walk through the chi-square goodness of fit test to check whether they are different.

national_demographics <- c(rep("white",100000),      # Fake demographic data
                           rep("hispanic",60000),
                           rep("black",50000),
                           rep("asian",15000),
                           rep("other",35000))

minnesota_demographics <- c(rep("white", 600),      # Fake sample data
                            rep("hispanic", 300),
                            rep("black", 250),
                            rep("asian", 75),
                            rep("other", 150))

table(national_demographics)            # Check counts

table(minnesota_demographics)

#Let's calculate the chi-squared statistic for our data to illustrate

observed <- table(minnesota_demographics)

# Get population ratios
national_ratios <- prop.table(table(national_demographics))

# Get expected counts
expected <- national_ratios * length(minnesota_demographics)

expected        # Check expected counts

# Calculate the statistic
chi_squared_statistic <- sum(((observed-expected)^2)/expected)

chi_squared_statistic


#Similar to the t-test where we compared the t-test statistic to a critical value based on the t-distribution to determine whether the result is significant, in the chi-square test we compare the chi-square test statistic to a critical value based on the chi-square distribution. In R, the nickname for the chi-square distribution is "chisq"
#Let's use this knowledge to find the critical value for 95% confidence level and check the p-value of our result.

qchisq(p=0.95,      # Find the critical value for 95% significance*
       df=4)        # Degrees of freedom = number of variable categories - 1

1-pchisq(q=18.1948, # Find the p-value for the chi-square statistic
         df=4)
#Since our chi-squared statistic exceeds the critical value, we'd reject the null hypothesis that the two distributions are the same
# You can carry out a chi-squared goodness-of-fit test automatically using the built-in R function chisq.test().

chisq.test(x= observed,          # Table of observed counts
           p= national_ratios)   # Expected proportions

#Chi-Squared Test of Independence
#The chi-squared test of independence tests whether two categorical variables are independent. The test of independence is commonly used to determine whether variables like education, political views and other preferences vary based on demographic factors like gender, race and religion.

#Let's generate some fake voter polling data and perform a test of independence.

set.seed(12)

# Generate race data
voter_race <- sample(c("white", "hispanic", 
                       "black", "asian", "other"),                
                     prob = c(0.5, 0.25 ,0.15, 0.05, 0.15), 
                     size=1000,
                     replace=TRUE)

table(voter_race)         # Check counts

# Generate party data
voter_party <- sample(c("democrat","republican","independent"),  
                      prob = c(0.4, 0.4, 0.2), 
                      size=1000,
                      replace=TRUE)

voter_table <- table(voter_race, voter_party) 
voter_table


# Take the outer product of row and col totals
expected <- outer(rowSums(voter_table),   
                  colSums(voter_table))/sum(voter_table)    # Divide by number of obs

expected        # Inspect expected values


#Now we can follow the same steps we took before to calculate the chi-square statistic, the critical value and the p-value.
chi_squared_statistic <-  sum(((voter_table-expected)^2)/expected)  

chi_squared_statistic

qchisq(p=0.95,      # Find the critical value for 95% significance
       df=8)        # Degrees of freedom*

# Find the p-value for the chi-square statistic
1-pchisq(q=chi_squared_statistic,   
         df=8)

chisq.test(x=voter_race,   # First variable to test
           y=voter_party)  # Second variable to test

#As expected, the test does not detect a significant relationship between the variables, since we constructed them to be independent.