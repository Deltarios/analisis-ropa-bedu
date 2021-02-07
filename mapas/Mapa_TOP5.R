library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)


# Obtenemos el directorio de trabajo actual para conocer donde tenemos instalado o posicionado Rstudio.
getwd()

# Establecemos el directorio en donde se encuentran nuestros archivos. Lo de abajo es un ejemplo, necesitas
# escribir tu propio directorio de trabajo porque sino no te correrá el archivo

setwd('M:/ant_m/Downloads/SANTANDER_BEDU/Estadistica_R/Sesion1') # Aquí mete la dirección. Neceitas borrar la que está aquí
# e ingresar la tuya.
"Nota: Recuerda que cuando copies tu dirección de archivos sistituye \ por / en tu direcció; al menos para windows"


"Para poder comprender de una mejor manera la districución de tiendas en el estado se procede 
a graficas las tiendas registradas ante la base de datos DENUE 
https://www.inegi.org.mx/servicios/api_denue.html obtenida por medio de un código en r"
"El mapa muestra con colores anaranjados los puntos de todas las tiendas de ropa registradas 
en el estado correspondiente al top 5.
Una vez que se tienen las tiendas se procede a crear una agrupación y conteo de ropas por el 
paramatro de colonia de la base de datos crudo, dicho parámetro fue filtrado para obtener 
intervalos de nuemros de tiendas, en este caso de default se selecciona un tamaño de 20-50 tiendas
por colonia, es decir que cada punto negro corresponde a puntos donde hay una concentración de 
20 a 50 tiendas de ropa en el estado del top. Pero ¿para qué nos sirve saber esto? La razón es
bastante sencilla, analizando dichos puntos podemos localizar zonas comerciales o puntos estratégicos
donde se dedican a la venta de ropa o textiles, si bien uno podría decir que en esos puntos hay
mayor competencia hay que tomar en cuenta tambien que lo que se necesita es darse a conocer, por lo
tanto aquellos puntos donde hay concentraciones de tiendas serían los primeros a considerar para
dar a conocer la tienda y su especialidad. Si bien podría optarse por puntos geográficos en donde
no se concetren tantas tiendas de ropa implica estar ubicacado en lugares no tn conocidos o 
poco concurrido para los consumidores."



"LIMITES A CONSIDERAR PARA LAS TIENDAS DE ROPA"

#LÍMITE MÍNIMO:
LMIN <- 20

# LÍMITE MÁXIMO:
LMAX <- 50


"Nota: El código debe correrse linea por linea de arriba hacia abajo, esto es porque las variables
se reescriben y si uno corre las variables o todo el codigo con la opcion de play puede provocar
malos resultados o errores, lo más viable es poner el curso en la primera línea e ir corriendo
con los comandos ctrl + Enter"

##################################     PRIMER MAPA      ################################## 
# MAPA DE BAJA SONORA #

shp1<-readOGR(dsn="SONORA2.shp", layer="SONORA2")
data <- read.csv("sonora.csv")

# Funci?n para transformas coordenadas geogr?ficas a UTM
LongLatToUTM <- function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

shp1.df <- fortify(shp1)

which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA

# Por si queremos ploter al el mapa solo sin puntos ni relleno.
map <- ggplot() + geom_path(data = shp1.df,aes(x = long, y = lat, group = group),
                            color = 'red', fill = 'gray', size = .2)
print(map)

data2 <- data
# Aplicamos la funci?n para transformas las coordenadas geogr?ficas a UTM.
UTM <- LongLatToUTM(data2$Longitud,data2$Latitud,12)

# Esos nuevos datos tranformados los agregamos como dos nuevas columnas al df original.
data3 <- data.frame(X = UTM$X, Y = UTM$Y, data2)

# Eliminamos outlier de los puntos.
data3 <- data3[-c(2671,2677),]

# Calculamos la fecuencia de las tiendas por colonias para obtener puntos
data4 <- table(data3$Colonia)

# Lo transformamos en df para un mejor manejo
data4 <- data.frame(data4)

# Establecemos el intervalo de entre cu?ntas tiendas por colonias vamos a filtrar.
data5 <- data3[data4$Freq>LMIN & data4$Freq<LMAX,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapS <- ggplot() + geom_polygon(data = shp1.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Sonora",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapS)


##################################     SEGUNDO MAPA       ################################## 
 # MAPA DE BAJA CALIFORNIA #

shp2<-readOGR(dsn="BAJA2.shp", layer="BAJA2")
data <- read.csv("bajacalifornia.csv")

shp2.df <- fortify(shp2)

which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA

# Por si queremos ploter al el mapa solo sin puntos ni relleno.
map <- ggplot() + geom_path(data = shp2.df,aes(x = long, y = lat, group = group),
                            color = 'red', fill = 'gray', size = .2)
print(map)

data2 <- data
# Aplicamos la funci?n para transformas las coordenadas geogr?ficas a UTM.
UTM <- LongLatToUTM(data2$Longitud,data2$Latitud,11)

# Esos nuevos datos tranformados los agregamos como dos nuevas columnas al df original.
data3 <- data.frame(X = UTM$X, Y = UTM$Y, data2)

# Eliminamos outlier de los puntos.
#data3 <- data3[-c(2671,2677),]

# Calculamos la fecuencia de las tiendas por colonias para obtener puntos
data4 <- table(data3$Colonia)

# Lo transformamos en df para un mejor manejo
data4 <- data.frame(data4)

# Establecemos el intervalo de entre cu?ntas tiendas por colonias vamos a filtrar.
data5 <- data3[data4$Freq>20 & data4$Freq<50,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapS <- ggplot() + geom_polygon(data = shp2.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Baja California",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapS)


##################################     TERCER MAPA       ################################## 
# MAPA DE NUEVO LEÓN #

shp3<-readOGR(dsn="NUEVO2.shp", layer="NUEVO2")
data <- read.csv("nuevoleon.csv")

shp3.df <- fortify(shp3)

which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA

# Por si queremos ploter al el mapa solo sin puntos ni relleno.
map <- ggplot() + geom_path(data = shp3.df,aes(x = long, y = lat, group = group),
                            color = 'red', fill = 'gray', size = .2)
print(map)

data2 <- data
# Aplicamos la funci?n para transformas las coordenadas geogr?ficas a UTM.
UTM <- LongLatToUTM(data2$Longitud,data2$Latitud,14)

# Esos nuevos datos tranformados los agregamos como dos nuevas columnas al df original.
data3 <- data.frame(X = UTM$X, Y = UTM$Y, data2)

# Eliminamos outlier de los puntos.
data3 <- data3[-c(46,126,705,905,1777,3490),]

# Calculamos la fecuencia de las tiendas por colonias para obtener puntos
data4 <- table(data3$Colonia)

# Lo transformamos en df para un mejor manejo
data4 <- data.frame(data4)

# Establecemos el intervalo de entre cu?ntas tiendas por colonias vamos a filtrar.
data5 <- data3[data4$Freq>LMIN & data4$Freq<LMAX,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapS <- ggplot() + geom_polygon(data = shp3.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Nuevo Le?n",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapS)


##################################     CUARTO MAPA       ################################## 
# MAPA DE COAHUILA #

shp4<-readOGR(dsn="COAHUILA2.shp", layer="COAHUILA2")
data <- read.csv("coahuila.csv")

shp4.df <- fortify(shp4)

which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA

# Por si queremos ploter al el mapa solo sin puntos ni relleno.
map <- ggplot() + geom_path(data = shp4.df,aes(x = long, y = lat, group = group),
                            color = 'red', fill = 'gray', size = .2)
print(map)

data2 <- data
# Aplicamos la funci?n para transformas las coordenadas geogr?ficas a UTM.
UTM <- LongLatToUTM(data2$Longitud,data2$Latitud,13)

# Esos nuevos datos tranformados los agregamos como dos nuevas columnas al df original.
data3 <- data.frame(X = UTM$X, Y = UTM$Y, data2)

# Eliminamos outlier de los puntos.
data3 <- data3[-c(1299,1837),]

# Calculamos la fecuencia de las tiendas por colonias para obtener puntos
data4 <- table(data3$Colonia)

# Lo transformamos en df para un mejor manejo
data4 <- data.frame(data4)

# Establecemos el intervalo de entre cu?ntas tiendas por colonias vamos a filtrar.
data5 <- data3[data4$Freq>LMIN & data4$Freq<LMAX,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapS <- ggplot() + geom_polygon(data = shp4.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Coahuila",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapS)


##################################     QUINTO MAPA       ################################## 
# MAPA DE TAMAULIPAS #

shp5<-readOGR(dsn="TAMAULIPAS2.shp", layer="TAMAULIPAS2")
data <- read.csv("tamaulipas.csv")

shp5.df <- fortify(shp5)

which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA

# Por si queremos ploter al el mapa solo sin puntos ni relleno.
map <- ggplot() + geom_path(data = shp5.df,aes(x = long, y = lat, group = group),
                            color = 'red', fill = 'gray', size = .2)
print(map)

data2 <- data
# Aplicamos la funci?n para transformas las coordenadas geogr?ficas a UTM.
UTM <- LongLatToUTM(data2$Longitud,data2$Latitud,14)

# Esos nuevos datos tranformados los agregamos como dos nuevas columnas al df original.
data3 <- data.frame(X = UTM$X, Y = UTM$Y, data2)

# Eliminamos outlier de los puntos.
data3 <- data3[-c(3065,3094),]

# Calculamos la fecuencia de las tiendas por colonias para obtener puntos
data4 <- table(data3$Colonia)

# Lo transformamos en df para un mejor manejo
data4 <- data.frame(data4)

# Establecemos el intervalo de entre cu?ntas tiendas por colonias vamos a filtrar.
data5 <- data3[data4$Freq>LMIN & data4$Freq<LMAX,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapT <- ggplot() + geom_polygon(data = shp5.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Tamaulipas",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapT)




