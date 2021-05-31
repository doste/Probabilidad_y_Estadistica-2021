# 1)

# X = #defectuosos en una caja de 4
# Y = sede ( Quilmes = 0 / Pilar = 1)
alfajores <- read.table("alfajores.txt", header = TRUE)
table(alfajores)
View(alfajores)


# a) la probabilidad de que una caja provenga de la sede Quilmes.

# esta proba es P( Y = 0)

# miro cuantos son de Quilmes (Y=0) y cuantos de Pilar (Y=1) :
alfajores$fabrica # esto me da 0 y 1 segun de donde sean. 
# se que si es 1 entonces son de Pilar Entonces miro cuantos de Pilar hay:
cantidadDePilar <- sum(alfajores$fabrica)
# y como en total tengo 500 entonces para saber cuantos son de Quilmes:
cantidadDeQuilmes <- 500 - cantidadDePilar

# bueno pero yo quiero la proba estimada, entonces hallo la proporcion:
proba <- cantidadDeQuilmes / 500

# RESPUESTA : 0.256


# b)

# miro cuantas cajas tienen 3 defectuosos Y son de Quilmes
# osea seria algo asi: 
#   alfajores$defectuosos[alfajores$defectuosos == 3] && alfajores$fabrica[alfajores$fabrica==0]
# mejor dicho:
cantidadQueCumplenEso <- sum(alfajores$defectuosos == 3 & alfajores$fabrica == 0)

proba <- cantidadQueCumplenEso / 500

# c)


# ????????


# d)
X <- alfajores$defectuosos
estimacionEsperanzaX <- mean(X)
estimacionVarianzaX <- var(X)

hist(X, probability = TRUE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2)

#lamparas <- read.table("lamparas.txt", header = TRUE)
datos <- scan("./lamparas.txt")

# a) Estimar la probabilidad de que una lampara producida por esta fabrica dure mas de 30 horas.
# b) Implementar y graficar la funcio ́n de distribucio ́n emp ́ırica de este conjunto de datos.

# Comienzo por visualizar los datos
par( mfrow = c(2,2) )

# Plot eje cartesiano
plot(datos,
     main="Duración de las lámparas",
     xlab="Nro. de dato",
     ylab="Duración (en hs)",
     col="blue", pch=19, cex=0.5)
grid()

# Frecuencia de los datos
hist(datos,
     main="Frecuencia de los datos",
     xlab="Rango de duración (en horas)",
     ylab="Cantidad de lámparas en rango",
     col="lightblue", freq=FALSE)

# Boxplot de datos
boxplot(datos,
        main="Duración de las lámparas",
        col="lightblue")

# QQ-plot de datos (contra D. Normal)
qqnorm(datos,
       col="blue", pch=19, cex=0.5)
grid()
qqline(datos,col="red")


# Para obtener la probabilidad de que una lámpara dure más que 30 hs,
# basta con contar la cantidad de datos mayores a 30hs, y dividirlos por la cantidad total de datos.

# Otra forma podría ser usando la función de densidad empírica graficada arriba (como un histograma),
# integrando de 30 en adelante, o equivalentemente, sumando áreas.

datos[datos > 30]

# Cantidad de datos > 30
length(datos[datos > 30])
# Cantidad total de datos
length(datos)

# Probabilidad estimada (empírica) de que dure más de 30hs
length(datos[datos > 30])/length(datos)

# Entonces la respuesta de la a) es : 0.3703704



# c)

#Completar: Estos datos permiten estimar que 
#el 90 % de las l ́amparas producidas por esta fa ́brica dura ma ́s de ........ ashor
# y el 10 % dura menos de ........ horas.


# Nos pide observar una cota/threshold a partir del cual el 90% de los datos están a su derecha:
 # el Percentil 10%
quantile(datos, seq(0,1,0.1))

# Respuesta = el 90 % de las lamparas producidas por esta fabrica dura mas de 2.866 horas

# Equivalentemente, usando el 90-percentil:

# Respuesta = el 10 % de las lamparas producidas por esta fabrica dura menos de 60.37 horas



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

par(mfrow=c(1,1))


# 3)

graduados <- scan("graduados.txt")

# a) Calcular la media muestral y la mediana muestral.
mean(graduados)
median(graduados)

# b) Calcular el desv ́ıo esta ́ndar muestral y la distancia intercuartil
sd(graduados)
IQR(graduados)


# c) Realice un histograma
#   con los datos y superponga la curva de una densidad normal con los para ́metros que considere pertinentes.
hist(graduados, prob=TRUE)
curve(dnorm(x, mean = mean(graduados), sd = sd(graduados)), add=TRUE)

# d) Realice un boxplot con este conjunto de datos. ¿Cua ́les son sus caracter ́ısticas ma ́s sobresaliente
#s? ¿Co ́mo relaciona lo observado en los gra ́ficos con los valores estimados de media y mediana obtenidos en a
#)? ¿Hay outliers?

boxplot(graduados)

# e)
# Rta: Los datos parecen tener distribución Normal con media 3.720667 y varianza 0.02122023


# f)
den <- density(graduados)
hist(graduados, prob=TRUE)
lines(den)


# g)
# el qqplot
qqnorm(graduados)
qqline(graduados,col="red")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# 4)

ciudades <- read.table("ciudades.txt", header=TRUE)
View(ciudades)

# a)
#Construir en paralelo, para facilitar la comparacio ́n,
# un boxplot para los datos de cada pa ́ıs e identificar los puntos extremos en cada uno de ellos.


#boxplot(ciudades$Argentina)
#boxplot(ciudades$EEUU)
#boxplot(ciudades$Holanda)
#boxplot(ciudades$Japon)

# mas piola:
boxplot(ciudades$Argentina, ciudades$EEUU, ciudades$Holanda, ciudades$Japon, names = c("Argentina", "EEUU", "Holanda", "Japon"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 5)

ingresos <- scan("./ingresos.txt")
View(ingresos)

# veo que onda los datos
hist(ingresos,prob=TRUE)

# a) ¿Cu ́al es el ingreso m ́ınimo percibido por los trabajadores encuestado
#s? Estime la proporcio ́n de los trabajadores de la ciudad que percibe el ingreso m ́ınimo.

ingresoMinimo <- min(ingresos)
cantiIngresosMinimos <- length(ingresos[ ingresos == ingresoMinimo])
cantidadTotal <- 1000 # == length(ingresos)
proporcion <- cantiIngresosMinimos / cantidadTotal
proporcion

#OTRA FORMA:

ingresoMinimo <- min(ingresos)
mean(ingresos == ingresoMinimo)



# b) Estimar el ingreso mensual que 
# se necesita para pertenecer al 10 % de trabajadores de la ciudad con ingresos ma ́s altos.

# busco primero cual es 10 % de trabajadores de la ciudad con ingresos mas altos

ingresosOrdenados <- sort(ingresos)
#me quedo con el ultimo 10% de ese vector ordenado
ingresoMinimoQueSeNecesita <- ingresosOrdenados[900]
ingresoMinimoQueSeNecesita

# Entonces, Respuesta: Para pertenecer al 10% de los trabajadores con ingresos altos,
#       el ingreso deberia ser al menos 240


# c)

# media muestral:
mean(ingresos)

# mediana muestral:
median(ingresos)

# media alfa-podada (alfa = 0.1)
mean(ingresos,trim=0.1)
# otra forma:
ingresosOrdenados <- sort(ingresos)
mean(ingresosOrdenados[101:900])


# d)

# desvio estandar:
sd(ingresos)

# distancia intercuartil
IQR(ingresos)


# e)

hist(ingresos,prob=TRUE)
# con el histograma se puede ver que es una distribucion bastante asimetrica, con cola larga a derecha 
# lo de la asimetria ya la podia ver porque la media y la mediana me dieron muy diferentes

boxplot(ingresos)
# con el boxplot se puede apreciar mejor que hay muchisimos outliers




# f)  No
hist(ingresos,prob=TRUE)
curve(dnorm(x, mean=mean(ingresos), sd=sd(ingresos)), add=TRUE)





# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




# 6)

# a)

# N = 25
muestra25 <- rnorm(25,0,1) # es lo mismo que poner rnorm(25) . por defaul mean=0 y sd=1
qqnorm(muestra25)
qqline(muestra25, col="steelblue", lwd=2)


# N = 50
muestra50 <- rnorm(50,0,1) # es lo mismo que poner rnorm(25) . por defaul mean=0 y sd=1
qqnorm(muestra50)
qqline(muestra50, col="red", lwd=2)


# N = 100
muestra100 <- rnorm(100,0,1) # es lo mismo que poner rnorm(25) . por defaul mean=0 y sd=1
qqnorm(muestra100)
qqline(muestra100, col="green", lwd=2)


# N = 500
muestra500 <- rnorm(500,0,1) # es lo mismo que poner rnorm(25) . por defaul mean=0 y sd=1
qqnorm(muestra500)
qqline(muestra500, col="gold", lwd=2)


# b)

# N = 25
muestra25 <- rgamma(25,5,0.5) 
qqnorm(muestra25)
qqline(muestra25, col="steelblue", lwd=2)

# N = 50
muestra50 <- rgamma(50,5,0.5)
qqnorm(muestra50)
qqline(muestra50, col="red", lwd=2)


# N = 100
muestra100 <- rgamma(100,5,0.5)
qqnorm(muestra100)
qqline(muestra100, col="green", lwd=2)


# N = 500
muestra500 <- rgamma(500,5,0.5)
qqnorm(muestra500)
qqline(muestra500, col="gold", lwd=2)


# c)

# N = 25
muestraNormal25 <- rnorm(25)
muestraUnif25 <- runif(25)
muestraY <- c()
for (i in 1:25) {
  y <- muestraNormal25[i] / muestraUnif25[i]
  muestraY <- c(muestraY, y)
}

qqnorm(muestraY)
qqline(muestraY, col="steelblue", lwd=2)


# N = 50
muestraNormal50 <- rnorm(50)
muestraUnif50 <- runif(50)
muestraY <- c()
for (i in 1:50) {
  y <- muestraNormal50[i] / muestraUnif50[i]
  muestraY <- c(muestraY, y)
}

qqnorm(muestraY)
qqline(muestraY, col="steelblue", lwd=2)


# N = 100
muestraNormal100 <- rnorm(100)
muestraUnif100 <- runif(100)
muestraY <- c()
for (i in 1:100) {
  y <- muestraNormal100[i] / muestraUnif100[i]
  muestraY <- c(muestraY, y)
}

qqnorm(muestraY)
qqline(muestraY, col="steelblue", lwd=2)








