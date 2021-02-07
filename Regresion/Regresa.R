library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)


# Obtenemos el directorio de trabajo actual para conocer donde tenemos instalado o posicionado Rstudio.
getwd()

# Establecemos el directorio en donde se encuentran nuestros archivos. Lo de abajo es un ejemplo, necesitas
# escribir tu propio directorio de trabajo porque sino no te correrá el archivo

setwd('M:/ant_m/Downloads/SANTANDER_BEDU/Estadistica_R/Sesion1') # Aquí mete la dirección. Neceitas borrar la que está aquí
# e ingresar la tuya.
"Nota: Recuerda que cuando copies tu dirección de archivos sistituye \ por / en tu direcció; al menos para windows"


"Durante años los problemas de salud en Méxco han tenido un aumento lineal bastante alto,
se cuenta con una base de datos mundial de niveles de obesidad a nivel mundial de distintos paises
desde los años 1975 a 2016 obtenido de https://ourworldindata.org/obesity  Dicho estudio al filtrar
los datos solamente de méxico y de acuerdo a dichos datos generamos un modelo líneal y un ajuste de
los datos a una línea recta que ajusto de forma muy precisa."

# Lemos los datos con los que se van a trabajar
dat <- read.csv('Peso.csv')

# Como el estudio es a nivel mundial, solo seleccionamos a méxico de la db.
dat <- dat[dat$Code == "MEX",]

# Cambiamos el nombre de la variable.
dat <- rename(dat, Peso = Prevalence.of.overweight.adults..both.sexes....WHO..2019.) 

# Convertimos a dataframe.
dat <- data.frame(dat)

# Generamos el modelo lineal de los datos.
m1 <- lm(dat$Peso ~ dat$Year )

summary(m1)



"Analizando la gráfica del aumento de peso en la población mexicana desde
1975-2016 se observa un incremento casi lineal por año. 
El modelo lineal generado durante esos años obtiene una pendiente de 0.75
aproximadamente, dicha pendiente representa una pendiente bastante 
inclinada indicando un alto aumento del porcentaje de obesidad  en 41 años;
por lo que podemos confirmar cuantitativamente el incremento que tendrá la 
demanda de ropa de este índole."

"Por otro lado los p-value al ser bastante pequeños, es una señal de que el modelo
lineal se ajusta bastante bien a los datos obtenidos del estudio hasta dichos años"


ggplot(dat, aes(x = Year, y = Peso)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", formula = y ~ poly(x, 2)) + 
  #abline(v=2000, col='red', lty=2, lwd=5) +
  xlab("Años") + 
  ylab("Peso (%)") + 
  ggtitle("Obesidad") + 
  scale_color_discrete(guide=FALSE) + theme_dark()

"Con la gráfica anterior y el modelo ajustado podemos constatar cuantitativamente
que el problema de obseidad en méxico tiene un incremento linal a lo largo de los años, 
por lo que por obvias razones la demanda de este tipo de ropa crecerá a lo largo de los
años en el pais y puede ser una buena fuente  de interés económico"


" Utilizando los datos de los años podemos predecir de acuerdo a ese modelo la
situación de méxico a futuro con la función predict para los años 2020 y 2021
de los niveles de obesidad en méxico."

dp <- data.frame(Year=c(2020,2021))
mod <- lm(Peso~Year,data=dat)
pred <- predict(mod,dp)
pred