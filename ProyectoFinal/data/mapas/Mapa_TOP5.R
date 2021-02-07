library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)



dirmapas <- "E:/Users/afloresgu/Dropbox (GRUPO SALINAS)/2. CURSOS/BEDU DATA SCIENCE/MODULO 2 R Y PYTHON/1. PROGRA Y EST CON R/PROYECTO/CODE/mapas"
getwd()
setwd(dirmapas)

##################################     PRIMER MAPA      ################################## 


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
data5 <- data3[data4$Freq>20 & data4$Freq<50,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapS <- ggplot() + geom_polygon(data = shp1.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Sonora",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapS)


##################################     SEGUNDO MAPA       ################################## 


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
data5 <- data3[data4$Freq>20 & data4$Freq<50,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapS <- ggplot() + geom_polygon(data = shp3.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Nuevo Le?n",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapS)



##################################     CUARTO MAPA       ################################## 


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
data5 <- data3[data4$Freq>20 & data4$Freq<50,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapS <- ggplot() + geom_polygon(data = shp4.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Coahuila",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapS)



##################################     QUINTO MAPA       ################################## 


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
data5 <- data3[data4$Freq>20 & data4$Freq<50,]

# Ploteamos el nuevo mapa con los puntos sobrepuestos
mapT <- ggplot() + geom_polygon(data = shp5.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange") + geom_point(aes(x=data5$X,y=data5$Y), color="black") + theme_bw() + labs(title = "Tamaulipas",x="",y="",fill="black") + theme(plot.title = element_text(size=14, face= "bold.italic", color="black"), legend.position = "up" )
print(mapT)
