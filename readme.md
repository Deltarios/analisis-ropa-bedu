## Tema: Análisis de mercado de Ropa para Tallas Extra

<img src="https://cdn.pixabay.com/photo/2015/10/12/15/18/store-984393_960_720.jpg" align="right" height="250" width="350" hspace="10">
<div style="text-align: justify;">

### Introducción: 

La industria textil ha proliferado en los territorios más extensos, abarcando países extranjeros y sobre todo México. Su comercio ha crecido exponencialmente durante estas décadas, exponiendo una división de grupos con requerimientos especiales. Vamos a abordar dicho tema desde un punto de vista estadístico.

La obesidad y el sobrepeso han sido temas recurrentes en México. Durante décadas no se ha parado de exponer las complicaciones de salud que conlleva padecer dichas enfermedades, sin embargo, adentrándonos a la vida cotidiana de este nicho, es evidente que más problemas se han creado alrededor de ellos.

> Estamos frente a ustedes para presentarles algo innovador, necesario y a su vez emprendedor.   

---
### Integrantes: 

- Axel Flores Guarneros.
- Marco Antonio Armas Santillán.
- Ariel Arturo Ríos Sierra.  
- Evelyn Jocelyn Gonzalez Acevedo

---
### Contenido del Repositorio:

-  [**`Proyecto_final.R`**](Proyecto_final.R)
   Incluye el análisis de las variables Porcentaje de Obesidad, ECI, PIB Y ventas en millones de dólares por Entidad Federativa. Se realizó la carga de datasets, limpieza, graficación y visualización de resultados.  
   
-  [**`Mapas`**](mapas/Mapa_TOP5.R)
   El programa Mapa_TOP5.r contiene la ubicación geográfica de tiendas de ropa por estado del top 5 así como las respectivas zonas de acumulación de mayor o menor número de tiendas.
   
-  [**`Colores:`**](Colores/Colores.R)
   Contiene el mapa de concentración visual de los niveles de obesidad por entidad federativa en México.  
   
-  [**`Regresion:`**](Regresion/Regresa.R)
   Muestra el modelo de regresión lineal de los años ajustados sobre obesidad en México durante  1975-2016.  
   
-  [**`Dashboard (Web APP):`**](ProyectoFinal/app.R)
   Dashboard del Proyecto de Tiendas de Ropa Tallas Extra, incluye el análisis, datos, gráficas, mapas de todo el proyecto para la selección de los estados     potenciales en donde ubicar tiendas de ropa tallas extra. __*Considere establecer correctamente el directorio del trabajo__

---
### Desarrollo: 

#### ¿Pero de qué estamos hablando? 

Partimos de la importancia de dignificar el vestir de una persona con sobrepeso, en la industria de la confección. Este sector de la Moda se denomina de varias maneras, tanto como tallas especiales, tallas extras y ya de manera socialmente más sutil tallas Extra (CURVI). 

__Medidas que sobrepasan los estándares normales y son desde la conocida talla 40 hasta la extraordinaria talla 50 en delante en México.__

#### ¿Por qué  razón se escogió este tema a desarrollar en nuestro proyecto?

Recae principalmente en 2 puntos:

- El ámbito social que lo engloba, es incluyente.
- El ámbito económico es rentable, lo que nos llevó a analizar lo escaso y poco común que es su comercialización.

*Si bien esta tendencia en el vestir no es novedad, son insuficientes las sucursales que proveen este tipo de tallas extras; cada vez más solicitadas a temprana edad y de manera constante en la edad adulta.*

#### ¿Cuál es nuestro objetivo?

Se busca encontrar una ubicación geográfica idónea y estratégica a nivel estatal para la apertura de nuevos establecimientos de esta índole.

#### Nuestra hipótesis fue planteada desde las complicaciones anteriormente mencionadas ¿Que abarca?

Debido al panorama social que se está viviendo en México, se estima que la ubicación idónea para establecimientos comerciales en el rubro de venta de Tallas Extra, se encontrará en el CENTRO  del país debido al crecimiento económico presentado en los últimos años .

#### ¿Cuáles son las variables principales a tomar en cuenta en nuestro análisis?

 Tenemos un total de 4 variables que fueron fundamentales en la generación de este análisis

-  **Índice de obesidad:**
El sobrepeso y la obesidad constituyen un serio problema de salud pública a nivel nacional en México debido a la dimensión que están adquiriendo en la sociedad, ya no sólo en edades adultas sino también desde la edad infantil y juvenil. El problema se extendió en un tiempo relativamente breve, y se manifiesta con un número muy elevado de casos 

-  **PIB (Producto interno bruto):**
Bien se sabe que dicha variable se relaciona con los valores monetarios de los bienes y servicios, por lo que entre mayor sea la variable , mayor será el poder adquisitivo del estado.

-  **ECI (Indice de complejidad económica):**
Esta variable resulta ampliamente relevante para nuestro análisis, ya que los hallazgos derivados de este estudio pueden promover sectores con un potencial emergente con oportunidades de emprendimiento mostrando capacidades existentes y potenciales con las que cuenta específicamente cada estado.

-  **Número de ventas de ropa por Estado:**
El Número de ventas al por menor realizadas en cada estado de México


#### _Análisis de los parámetros para calcular el top 5 en los estado_

Utilizando los resultados obtenidos por las variables ya enunciadas se procede a generar un listado de los principales 5 estados que reúnen las características ideales con el fin de encontrar la ubicación ideal para la apertura de un comercio en el ámbito de Venta de Tallas Extra.

-   Tabla de Agrupación de variables Porcentaje de Obesidad & PIB
ECI \* INDICE SOBREPESO \* Ventas \*PIB

Se contemplaron 4 variables estadísticas en orden de importancia.

##### ¿Cuáles serían las ubicaciones ideales para la apertura de un comercio enfocado a este sector?


Los mapas muestran la distribución municipal de las tiendas de ropa registradas mediante puntos naranjas, y haciendo un recuento por colonia  Los puntos negros representan una concentracion de 20 a 50 tiendas en una misma colonia apreciando asi lsa zonas con mayor y menor concentración de tiendas de ropa.

---
### PROBLEMAS PRESENTADOS

-   Bases de Datos Incompletas o con atributos relacionales distintos : es común que la información en cada Base de datos presente variaciones ya sea por su acentuación o abreviación en los nombres, por lo cual representa una problemática al momento de correlacionar Bases de datos. 
<br>

-   Desactualización en las bases de Datos (presentaban periodos de tiempos diferentes ): La inclusión de las fuentes obedece a razones de cobertura geográfica y temporal, sin embargo mientras que de una  base de datos se obtiene información a nivel nacional para un cierto periodo de tiempo, las demás cubrían una serie de tiempo diferente.
<br>


-   Difícil acceso y variedad a bases de Datos(algunas de ellas exigen un pago por su adquisición):  Debido a que la mayoría de las bases de datos gratuitas solo presentan análisis globales y aquellas que contienen un análisis específico de  un tema o variable, comúnmente deben ser adquiridas de páginas o plataformas que exigen un pago por la adquisición de ellas. 

---
### CONCLUSIÓN
##### Los Hallazgos presentados en este análisis estadístico fueron los siguientes:

El principal Hallazgo en nuestro análisis estadístico representó una sorpresa para el equipo ya que al inicio se tenía estipulado que el  área idónea para la apertura dle establecimeinto  se encontraría en la Zona Central de Nuestro Pais, sin embargo gracias al análisis elaborado se pudo discernir que el área ideal se encuentra en el Norte del Pais debido a su alto numero de ECI y sus altos  casos de Obesidad.

Igualmente se pudo certificar en cada uno de los estados la ubicacion estrategica para el asentamiento de nuestro Comercio ,dicho analisis arrojo las siguientes ubicaciones

-   Baja California (NORTE)
-   Sonora(NORTE)
-   Nuevo Leon(OESTE)
-   Coahuila(SUR)
-   Tamaulipas(SUR)

---
### Fuente de datos:
-	Porcentaje de Obesidad:
    -  [**`ENSANUT 2018-2019`**](https://www.inegi.org.mx/programas/ensanut/2018/#Tabulados)
    -   [**`2016`**](https://www.alcaldesdemexico.com/notas-principales/estados-con-mayor-tasa-de-obesidad/)
    - [**`2014`**](https://www.elfinanciero.com.mx/rankings/los-10-estados-con-las-mayores-tasas-de-obesisdad)
    - [**`2012 (Página 15)`**](http://oment.salud.gob.mx/indicadores_descargas/reporte_resultados_oment.pdf)
    -   [**`2006 (Página 17)`**](https://iieg.gob.mx/contenido/PoblacionVivienda/libros/LibroDiezproblemas/Capitulo1.pdf)
-	[**`Histórico de Obesidad a nivel Nacional 1975-2016– Regresión Lineal`**](https://ourworldindata.org/obesity)
-	[**`PIB por Entidad Federativa en México / Actividades terciarias/ Comercio al por menor - INEGI 2019`**](https://www.inegi.org.mx/app/tabulados/default.aspx?pr=17&vr=7&in=27&tp=20&wr=1&cno=2)
- [**`ECI 2020`**](https://datamexico.org/es/eci/explore)
- [**`VENTAS AL POR MENOR ROPA`**](https://datamexico.org/es/profile/industry/comercio-al-por-menor-de-ropa-bisuteria-y-accesorios-de-vestir)
