library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)
library(tidyverse)
library(data.table)
library(Hmisc)
library(sp)
library(RColorBrewer)

# Obtenemos el directorio de trabajo actual para conocer donde tenemos instalado o posicionado Rstudio.
getwd()

# Establecemos el directorio en donde se encuentran nuestros archivos. Lo de abajo es un ejemplo, necesitas
# escribir tu propio directorio de trabajo porque sino no te correrá el archivo

setwd('M:/ant_m/Downloads/SANTANDER_BEDU/Estadistica_R/Sesion1') # Aquí mete la dirección. Neceitas borrar la que está aquí
# e ingresar la tuya.
"Nota: Recuerda que cuando copies tu dirección de archivos sistituye \ por / en tu direcció; al menos para windows"


"Para comprender un poco la situación del país a nivel de obesida por entidad federativa
se procedió a trabajar con una base de datos de niveles de obesidad obtenidad del INEGI para
generar un mapa porcentual de dicho parámetro aplicado a la república mexicana."

# Lectura del mapa shapefile.
shp1<-readOGR(dsn="areas_geoestadisticas_estatales.shp", layer="areas_geoestadisticas_estatales")

# Lectura de datos csv de los porcentajes de obesidad
dat <- read.csv('OBESIDAD.csv')

# Se une la tabla de datos de obesidad al archivo shapefile
joined <- merge(shp1, dat, by.x="NOM_ENT")

# Creamos la paleta de colores que usaremos para colorear el mapa
pal <- brewer.pal(4, "YlOrRd") 
pal <- colorRampPalette(pal)(16)

# Imprimimos el mapa ya coloreado de acuerdo a la paleta de colores.
spplot(joined, "X2018",colorkey = list(space = "bottom"), scales = list(draw = TRUE),main = "Porcentaje de Obesidad en México por Entidad Federativa ENSANUT 2018",col.regions=pal)

"En dicho mapa se observa que para el año 2018 la zona norte y sur resulta ser donde los niveles
de obesidad son más altos"

