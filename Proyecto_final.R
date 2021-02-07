########################  LIBRARY REQUERIDAS  ########################
install.packages("dplyr")
library(openxlsx)
library(ggplot2)
library(sf)
library(stringr) 
suppressMessages(suppressWarnings(library(dplyr)))
library(viridis) 

########################  CARGAMOS DATOS REQUERIDOS  ########################
getwd()
# Establecemos directorio de trabajo
setwd("/cloud/project") # Establecemos directorio de trabajo

"Nota: El código debe correrse linea por linea de arriba hacia abajo, esto es porque las variables
se reescriben y si uno corre las variables o todo el codigo con la opcion de play puede provocar
malos resultados o errores, lo más viable es poner el curso en la primera línea e ir corriendo
con los comandos ctrl + Enter"

"Tenemos un total de 4 variables que fueron fundamentales en la generación de este análisis las cuales fueron:
Índice de Obesidad de México, PIB (Producto Interno Bruto) , ECI (Índice de Complejidad Económica.) y 
el número de ventas de ropa por Estado ."


" PORCENTAJE DE OBESIDAD https://www.inegi.org.mx/programas/ensanut/2018/#Tabulados
El problema de la obesidad en México  ha tenido un incremento contante en al menos los último 45
años, sin embargo, para analizar específicamente el Porcentaje de Obesidad en cada uno de las Entidades
Federativas y Municipios, se recurrió a las fuentes de Open Data del INEGI, insitución que 
lleva a caba de manera sexenal la Encuesta Nacional de Salud y Nutrición (ENSANUT) que tiene como objetivo 
conocer el estado de salud y las condiciones nutricionales de diversos grupos de población en México. 
Nosotros nos centramos en la variable del Porcentaje de Obesidad en la Población (por Entidad Federativa y
Municipio). Por definición, para que una persona sea diagnosticada con obesidad su Indice de Masa Corporal
debe ser mayor o igual a 30 kg/m2, además en este estudio solo se consideraron personas mayores de 20 años.
Además, se seleccionó esta variable debdio a que el sobrepeso y la obesidad son problemas de salud 
pública en México que van en aumento en todas las edades de la población  y no se ve una reducción al problema"

# DATOS: Porcentaje de Obesidad por Municipio y Entidad Federativa ENSANUT 2018
obesidad.Excel <- "obesidad ENSANUT2018.xlsx" 
obesidad <- read.xlsx(obesidad.Excel, sheet = 1, skipEmptyRows = FALSE)


" PRODUCTO INTERNO BRUTO https://www.inegi.org.mx/app/tabulados/default.aspx?pr=17&vr=7&in=27&tp=20&wr=1&cno=2
Otra variable a evaluar fue el PIB. Para ser más precisos en los resultados se tomó en consideración el
PIB por Entidad Federativa en México / Actividades terciarias/ Comercio al por menor - INEGI 2019.
El PIB a precios corrientes o nominal, expresa el valor monetario de la producción de bienes y servicios
finales de un país o de una región durante un periodo determinado de tiempo valuado con los precios 
existentes en ese periodo. El PIB es uno de los indicadores más utilizados para conocer la situación
económica, en este caso se analisó a nivel Entidad Federativa, y nos permitió evaluar aquellos estados 
en donde la economía tenía un tendencia creciente, es decir, entre mayor sea la variable, mayor será el 
poder adquisitivo del estado.
Sin embargo, debido a que los valores de la variable no son específicos  al comercio textil, 
esto provoca que al momento de realizar el top 5, dicha variable no tenga tanto peso a comparación de las 
otras."

# DATOS: PIB de las actividades económicas por entidad federativa/ Actividades terciarias/ Comercio al por menor INEGI 2019
PIB.Excel <- "PIBE_27 DATA.xlsx" 
PIB <- read.xlsx(PIB.Excel, sheet = 1, skipEmptyRows = FALSE)


" Índice de Complejidad Económica (ECI) (2020-S1) https://datamexico.org/es/eci/explore
El ECI es la diferencia en productividad e ingresos entre entidades puede ser explicada por diferencias
en la complejidad económica, entendida como la diversidad de las capacidades presentes en cada entidad 
(Hausmann, Cheston Santos, 2015), es decir es la cualidad que predice el desarrollo económico.
Esta variable resulta ampliamente relevante para nuestro análisis, ya que los hallazgos derivados de
este estudio pueden promover sectores con un potencial emergente con oportunidades de emprendimiento 
mostrando capacidades existentes y potenciales con las que cuenta específicamente cada estado."

# DATOS: Índice de Complejidad Económica (ECI) por Entidad Federativa datamexico.org 2020
ECI.Excel <- "ECI.xlsx" 
ECI <- read.xlsx(ECI.Excel, sheet = 1, skipEmptyRows = FALSE)


" Número de ventas de ropa por Estado https://datamexico.org/es/profile/industry/comercio-al-por-menor-de-ropa-bisuteria-y-accesorios-de-vestir
Esta variable resulta de vital importancia ya que nos indica Comercio al por Menor de Ropa y accesorios
de Vestir en el año 2020 por Entidad Federativa, lo que nos permite identificar los estados potenciales 
en este sector."

# DATOS: Ventas por Entidad Federativa USD $M datamexico.org 2020
ventas.estado <- read.csv('Ventas.csv', encoding="UTF-8", stringsAsFactors=FALSE)

########################  LIMPIAMOS LOS DATOS REQUERIDOS  ######################## 

# Renombramos algunos campos con nombres mejor identificables
obesidad <- rename(obesidad, UUID.Municipio = Identificador.único.del.municipio, Clave.Estado = Clave.de.entidad.federativa)
obesidad <- rename(obesidad, Clave.Municipio = Clave.de.municipio.o.delegación, Nombre.Municipio = Municipio.o.delegación)
obesidad <- rename(obesidad, Porcentaje.obesidad = Porcentaje.de.población.de.20.años.y.más.con.obesidad.)

# Transformamos a entero y redondeamos a 4 posiciones el Porcentaje de Obesidad
obesidad <- mutate(obesidad, Porcentaje.obesidad = round(as.numeric(Porcentaje.obesidad), 4)) 
# Seleccionamos las columnas requeridas
obesidad <- select(obesidad, UUID.Municipio : Porcentaje.obesidad) 
# Filtramos solo las filas con valor, que son los datos con los que vamos a trabajar
obesidad <- filter(obesidad, Estimador == "Valor") 

# Creamos un DataFrame con datos particulares por estado
#Filtramos solo las filas con valores correspondientes a los Total por estado
obesidad.estado <- filter(obesidad, Nombre.Municipio == "Total") 
#Quitamos el Total correspondiente al País
obesidad.estado <- filter(obesidad.estado, UUID.Municipio != "00000") 

# Filtramos el DataFrame de obesidad que utilizaremos más adelnte para datos específicos por Municipio
#Quitamos las filas de Total, que corresponde a los totales por municipio
obesidad <- filter(obesidad, Nombre.Municipio != "Total") 


# Datos PIB
#Seleccionamos solo las filas del PIB de Participación Porcentual por estado
PIB <- PIB %>% slice(44:75) 
# Renombramos algunos campos con nombres mejor identificables
PIB <- rename(PIB, Entidad.federativa = "Instituto.Nacional.de.Estadística.y.Geografía.(INEGI).", "PIB.2019" = X18)
# seleccionamos solo las columnas de Entidad Federativa y PIB.2019
PIB <- select(PIB, Entidad.federativa, PIB.2019)  
# Transformamos a entero y redondeamos a 4 posiciones
PIB <- mutate(PIB, PIB.2019 = round(as.numeric(PIB.2019), 4)) 
#Eliminamos caracteres blancos al principio para hacer el merge
PIB$Entidad.federativa <- str_trim(PIB$Entidad.federativa) 
# Unimos los DataFrames PIB y obesidad por Entidad Federativa
obesidad.estado <- merge(obesidad.estado, PIB, by = "Entidad.federativa")


# Datos ECI
# Renombramos algunos campos con nombres mejor identificables
ECI <- rename(ECI, Entidad.federativa = "State")
ECI <- rename(ECI, ECI.valor = "Number.of.Employees.Midpoint.ECI")
# Transformamos a entero y redondeamos a 4 posiciones el ECI
ECI <- mutate(ECI, ECI.valor = round(as.numeric(ECI.valor), 4)) 
# seleccionamos solo las columnas de Entidad Federativa y PIB.2019
ECI <- select(ECI, Entidad.federativa, ECI.valor)  
# Unimos el ECI al DataFrame de obesidad.estado
obesidad.estado <- merge(obesidad.estado, ECI, by = "Entidad.federativa")

# DATOS: VENTAS
ventas.estado <- ventas.estado[,c(1,6,7,8)]
ventas.estado <- group_by(ventas.estado, Year, State)
ventas.estado.2019 <- ventas.estado[ventas.estado$Year == 2019,]
ventas.estado <- ventas.estado.2019 %>% group_by(State) %>% summarise(Ventas = mean(Trade.Value), .groups = 'drop')
ventas.estado <- rename(ventas.estado, Entidad.federativa = State)
#Renombramos México como Estado de México para hacer correctamente el Merge (LEFT OUTER JOIN)
obesidad.estado$Entidad.federativa <- gsub("^México", "Estado de México", obesidad.estado$Entidad.federativa) 
# Realizamos el Left Outer JOIN, debido que los datos de ventas solo se tienen de 25 Entidades Federativas
obesidad.estado <- merge(x = obesidad.estado, y = ventas.estado, by = "Entidad.federativa", all.x = TRUE) 

########################  GRAFICAMOS RESULTADOS  ######################## 

" ANALIZANDO EL PORCENTAJE DE OBESIDAD POR ENTIDAD FEDERATIVA
Podemos observar en el siguiente gráfico de barras que muestra el porcentaje de obesidad por estado es
encabezado por Quintana Roo con un porcentaje de 49.93%, al ir reduciendo el rango para visualizar un 
Top 5 se denota que dicho top está constituido además por los estados de Baja California, Tabasco, 
Yucatán y Campeche con un porcentaje de 44.9%."

# GRAFICA: TOP Entidades Federativas con mayor obesidad en México 2018 TOP 15
meanPO <-mean(obesidad.estado$Porcentaje.obesidad)
medPO <- median(obesidad.estado$Porcentaje.obesidad)
obesidad.estado <- obesidad.estado[with(obesidad.estado, order(-obesidad.estado$Porcentaje.obesidad)), ] # Orden inverso
ggplot(head(obesidad.estado,15), aes(x=Porcentaje.obesidad , y=reorder(Entidad.federativa, Porcentaje.obesidad))) +
  geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
  ggtitle('Porcentaje de Obesidad por Entidad Federativa en México - ENSANUT 2018', paste("Media=",round(meanPO, 4), "Mediana=", round(medPO, 4))) +
  geom_vline(xintercept =  meanPO, col = "red", lwd = 1.0, lty =2)+
  geom_vline(xintercept =  medPO, col = "black", lwd = 1.0, lty =2)+
  xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
  ylab('Entidad Federativa') +
  geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
  scale_fill_continuous(type = "viridis", option = "C")


" ANALIZANDO EL ECI
Es claro que las entidades que pertenecen a las regiones norte y centro tienen los primeros lugares, posicionándose
en el primer puesto Nuevo León particularmente Nuevo Leon, Queretaro, Coahuila, Baja California y Chihuahua tienen
un mayor potencial de crecimiento"

# GRÁFICA: TOP Entidad Federativa con mayor Indice de Complejidad Económica (ECI) en México 2020 TOP 15
meanECI <-mean(obesidad.estado$ECI.valor)
medECI <- median(obesidad.estado$ECI.valor)
obesidad.estado <- obesidad.estado[with(obesidad.estado, order(-obesidad.estado$ECI.valor)), ] # Orden inverso
ggplot(head(obesidad.estado,15), aes(x=ECI.valor , y=reorder(Entidad.federativa, ECI.valor))) +
  geom_bar(stat='identity', aes(fill=ECI.valor), position='dodge') +
  ggtitle('ECI por Entidad Federativa en México - datamexico.org 2020', paste("Media=",round(meanECI, 4), "Mediana=", round(medECI, 4))) +
  geom_vline(xintercept =  meanECI, col = "red", lwd = 1.0, lty =2)+
  geom_vline(xintercept =  medECI, col = "black", lwd = 1.0, lty =2)+
  xlab('Indice de Complejidad Económica (ECI)') +
  ylab('Entidad Federativa') +
  geom_text(aes(label = ECI.valor), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
  scale_fill_continuous(type = "viridis", option = "D")


" ANALIZANDO LAS VENTAS DE ROPA POR ENTIDAD FEDERATIVA
El histograma  demuestra que los estados que cuentan con un mayor cantidad de ventas superiores de 100,000,000 
conforman  el top 5, siendo  Coahuila de Zaragoza el estado que encabeza nuestra clasificación con un valor 
superior a 190,000,000. Ademá claro que las entidades que pertenecen a las regiones norte y centro, 
particularmente Nuevo Leon,Queretaro,  Coahuila, Baja California y Chihuahua tienen un mayor potencial 
de crecimiento"

# GRAFICA: TOP Entidades Federativas con mayor número de Ventas en Millones de Dólares en México 2020 TOP 15
meanVe <- mean(na.omit(obesidad.estado$Ventas))
medPVe <- median(na.omit(obesidad.estado$Ventas))
obesidad.estado <- obesidad.estado[with(obesidad.estado, order(-obesidad.estado$Ventas)), ] # Orden inverso
ggplot(head(obesidad.estado,15), aes(x=Ventas , y=reorder(Entidad.federativa, Ventas))) +
  geom_bar(stat='identity', aes(fill=Ventas), position='dodge') +
  ggtitle('Número de ventas por Entidad Federativa dentro de México - datamexico.org 2019', paste("Media=",round(meanVe, 4), "Mediana=", round(medPVe, 4))) +
  geom_vline(xintercept =  meanVe, col = "red", lwd = 1.0, lty =2)+
  geom_vline(xintercept =  medPVe, col = "black", lwd = 1.0, lty =2)+
  xlab('Ventas internacionales USD $M') +
  ylab('Entidad Federativa') +
  geom_text(aes(label = Ventas), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
  scale_fill_continuous(type = "viridis", option = "D")

" ANALIZANDO EL PIB"
" Bien se sabe que dicha variable se relaciona con los valores monetarios de los bienes y servicios, por 
lo que entre mayor sea la variable , mayor será el poder adquisitivo del estado.
Sin embargo se debe tener cuidado en el análisis de esta debido a que los valores no abarcan solamente al 
comercio textil. Aun así podemos observar que los estados del centro del País encabezan los primeros 3 lugares: 
Ciudad de México, el Edo México y Jalisco, siendo los 2 últimos Nuevo León y Veracruz "

# GRAFICA: TOP Entidades Federativas con mayor Producto Interno Bruto (PIB) en México 2019 TOP 15
meanPIB <-mean(obesidad.estado$PIB.2019)
medPIB <- median(obesidad.estado$PIB.2019)
obesidad.estado <- obesidad.estado[with(obesidad.estado, order(-obesidad.estado$PIB.2019)), ] # Orden inverso
ggplot(head(obesidad.estado,15), aes(x=PIB.2019 , y=reorder(Entidad.federativa, PIB.2019))) +
  geom_bar(stat='identity', aes(fill=PIB.2019), position='dodge') +
  ggtitle('PIB por Entidad Federativa en México / Actividades terciarias/ Comercio al por menor - INEGI 2019', paste("Media=",round(meanPIB, 4), "Mediana=", round(medPIB, 4))) +
  geom_vline(xintercept =  meanPIB, col = "red", lwd = 1.0, lty =2)+
  geom_vline(xintercept =  medPIB, col = "black", lwd = 1.0, lty =2)+
  xlab('Producto Interno Bruto (PIB)') +
  ylab('Entidad Federativa') +
  geom_text(aes(label = PIB.2019), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
  scale_fill_continuous(type = "viridis", option = "C")


" ANALIZANDO EL PORCENTAJE DE OBESIDAD POR MUNICIÍO
Reduciendo el grafico a un Top 5 ,se puede apreciar que 3 de los 5 municipios, forman parte del Estado de
Sonora siendo solamente el municipio en segundo lugar tomado por Castaños que se encuentra en el estado de 
Coahuila con un porcentaje de 66.97% en el Índice de Obesidad a sólo 12.5% por debajo del primer puesto ocupado
por el Municipio de San Javier en Sonora."

# GRAFICA: Municipios con mayor obesidad en México 2018 TOP 15
meanMUN <-mean(obesidad$Porcentaje.obesidad)
medMUN <- median(obesidad$Porcentaje.obesidad)
obesidad <- obesidad[with(obesidad, order(-obesidad$Porcentaje.obesidad)), ] # Orden inverso
ggplot(head(obesidad,15), aes(x=Porcentaje.obesidad , y=reorder(paste(Nombre.Municipio,Entidad.federativa,sep=','), Porcentaje.obesidad))) +
  geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
  ggtitle('Municipios con Mayor Obesidad en México ENSANUT 2018', paste("Media=",round(meanMUN, 4), "Mediana=", round(medMUN, 4))) +
  xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
  geom_vline(xintercept =  meanMUN, col = "red", lwd = 1.0, lty =2)+
  geom_vline(xintercept =  medMUN, col = "black", lwd = 1.0, lty =2)+
  ylab('Municipio y Entidad Federativa') +
  geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
  scale_fill_continuous(type = "viridis", option = "D")

# Escribimos el archivo de salida con el DataFrame de obesidad.estado que nos servirá como insumo paar el Dashboard 
tablaf <- arrange(obesidad.estado, -Porcentaje.obesidad)
write.csv(tablaf,'final.csv')

tablaf.ventas <- select(tablaf, Ventas)
tablaf.PIB.2019 <- select(tablaf, PIB.2019)

#Obtenemos máximos y mínimos
apply(na.omit(tablaf.ventas), 2, max)
apply(na.omit(tablaf.ventas), 2, min)
apply(na.omit(tablaf.PIB.2019), 2, median)
apply(na.omit(tablaf.ventas), 2, median)
