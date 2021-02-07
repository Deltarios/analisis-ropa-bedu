#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
############################################################################
"                        Equipo #11 SANTANDER BEDU"
############################################################################
#  - Ariel Arturo Ríos Sierra
#  - Axel Flores Guarneros
#  - Evelyn Jocelyn Gonzalez Acevedo
#  - Marco Antonio Armas 

############################################################################
"                  app.R - MERCADO DE ROPA TALLAS EXTRA"
############################################################################

############################# Library requeridas ###########################
library(shiny)
library(shinydashboard)
library(shinythemes)
library(openxlsx)
library(ggplot2)
library(sf)
library(stringr) 
suppressMessages(suppressWarnings(library(dplyr)))
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(rgdal)
library(raster)
library(rgeos)
library(tidyverse)
library(data.table)
library(Hmisc)
library(sp)
library(RColorBrewer)
library(viridis) 

################################# INTRODUCCIÓN #############################
"La obesidad y el sobrepeso han sido temas recurrentes en México. Durante décadas no se ha parado de exponer las 
complicaciones de salud que conlleva padecer dichas enfermedades, sin embargo, adentrándonos a la vida cotidiana de
este nicho , es evidente que más problemas se han creado alrededor de ellos."

"Partimos de la importancia de dignificar el vestir de una persona con sobrepeso, en la industria de la confección.
Este sector de la Moda se denomina de varias maneras, tanto como tallas especiales, tallas extras o tallas CURVI."

"¿Por qué  razón se escogió este tema a desarrollar? Recae principalmente en 2 puntos 
     - El ámbito social que lo engloba, es incluyente 
     - El ámbito económico es rentable
Tomando en cuenta  la escasa oferta de sucursales enfocadas en este rubro que es cada vez más solicitado por personas 
que padecen dicha condición  a temprana edad y de manera constante en la edad adulta. Además, es difícil encontrar, 
según testimonios de consumidores, prendas que reúnan características de moda, color de tendencias y deiseño apropiado.

¿Cuál es nuestro objetivo?
Se busca encontrar una ubicación geográfica idónea y estratégica a nivel estatal para la apertura de nuevos 
establecimientos de esta índole, con base en datos estadísticos recolectados"

"Nuestra hipótesis fue planteada desde las complicaciones anteriormente mencionadas ¿Que abarca?
Debido al panorama social que se está viviendo en México, se estima que la ubicación idónea para establecimientos
comerciales en el rubro de venta de Tallas Extra, se encontrará en el CENTRO  del país debido al crecimiento económico 
presentado en los últimos años."

"¿Cuáles son las variables principales a tomar en cuenta en nuestro análisis?
Tenemos un total de 4 variables que fueron fundamentales en la generación de este análisis las cuales fueron:  
     - Índice de Obesidad de México, 
     - PIB (Producto Interno Bruto)
     - ECI (Índice de Complejidad Económica.)
     - Número de ventas de ropa por Estado"

############################ LECUTAR DE DATOS #############################

# Obtenemos el directorio de trabajo actual
currentWD <- getwd()

# Obtenemos el directorio del proyecto
dirProyect <- dirname(rstudioapi::getSourceEditorContext()$path)

# Verificamos si estamos en el mismo directorio del proyecto
if (dirProyect != currentWD)
{
    setwd(dirProyect) # Ponemos el directorio del proyecto
}

# DATOS: Porcentaje de Obesidad, PIB, ECI y número de Ventas por Entidad Federativa. 
# Archivos obtenidos del proceso de Extract and transform Data del programa "Proyecto_final.R"
obesidad.Municipio <- read.csv("data/obesidadMunicipio.csv")
obesidad.Estado <- read.csv("data/final.csv")

# DATOS: mapa shapefile del Porcentaje de Obesidad por Entidad Federativa.
shp0<-readOGR(dsn="data/Colores/areas_geoestadisticas_estatales.shp", layer="areas_geoestadisticas_estatales")

# DATOS: Porcentaje de Obesidad por Entidad Federativa histórico
Datos.Obesidad.2018 <- read.csv('data/Colores/OBESIDAD.csv')
Datos.Obesidad.2012 <- read.csv('data/Colores/OBESIDAD2.csv')
Datos.Obesidad.2006 <- read.csv('data/Colores/OBESIDAD3.csv')

################################ FUNCIONES ################################
# FUNCION: Transforma coordenadas geográficas a UTM
LongLatToUTM <- function(x,y,zone){
    xy <- data.frame(ID = 1:length(x), X = x, Y = y)
    coordinates(xy) <- c("X", "Y")
    proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  
    res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
    return(as.data.frame(res))
}

############################## USER INTERFACE #############################
ui <- 
    
    fluidPage(
        
        # STYLE: Diseño de Slide Bars
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: red}")),
        tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: red}")),
        
        dashboardPage( 
            
            # STYLE: Diseño dashboard, Títulos y Menú
            skin = "red",
            dashboardHeader(title = "Mercado de Ropa Tallas Extra", titleWidth = 350),
            
            dashboardSidebar(
                width = 200,
                sidebarMenu(
                    menuItem("HISTOGRAMAS", tabName = "Dashboard", icon = icon("glyphicon glyphicon-equalizer", lib = "glyphicon")),
                    menuItem("REGRESION LINEAL", tabName = "Regresion", icon = icon("glyphicon glyphicon-calendar", lib = "glyphicon")),
                    menuItem("TABULADOS ESTADO", tabName = "data_table_estado", icon = icon("table")),
                    menuItem("TABULADOS MUNICIPIO", tabName = "data_table_municipio", icon = icon("table")),
                    menuItem("MAPAS ESTADO", tabName = "maps", icon = icon("glyphicon glyphicon-map-marker", lib = "glyphicon")),
                    menuItem("FUENTES", tabName = "fuentes", icon = icon("glyphicon glyphicon-save-file", lib = "glyphicon")),
                    menuItem("EQUIPO", tabName = "team", icon = icon("glyphicon glyphicon-sunglasses", lib = "glyphicon"))
                )
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    # HISTOGRAMAS: Porcentaje de Obesidad, Venta de Ropa, ECI, PIB 
                    tabItem(tabName = "Dashboard",
                            fluidRow(
                                titlePanel("ELECCION DE ENTIDAD FEDERATIVA"),
                                
                                
                                # SLIDER: "numEFPO" - Número de Entidades Federativas para visualizar su Porcentaje de Obesidad
                                box(
                                    title = "TOP Entidades Federativas con mayor obesidad en México",
                                    sliderInput("numEFPO", "Número de Entidades Federativas:", 1, 32, 10) 
                                ),
                                # SLIDER: "numEFV" - Número de Entidades Federativas para visualizar su ECI
                                box(
                                    title = "TOP Entidad Federativa con mayor Indice de Complejidad Económica (ECI) en México",
                                    sliderInput("numEFECI", "Número de Entidades Federativas:", 1, 32, 10)
                                ),
                                # GRÁFICA: TOP Entidades Federativas con mayor obesidad en México 2018
                                box(plotOutput("plotH1", height = 350)),
                                # GRÁFICA: TOP Entidad Federativa con mayor Indice de Complejidad Económica (ECI) en México 2020 
                                box(plotOutput("plotH2", height = 350)),
                                
                                
                                # SLIDER: "numEFV" - Número de Entidades Federativas para visualizar su Venta de Ropa en Millones de Dólares
                                box(
                                    title = "TOP Entidades Federativas con mayor número de Ventas de Ropa en Millones de Dólares en México",
                                    sliderInput("numEFV", "Número de Entidades Federativas:", 1, 32, 10) 
                                ),
                                # SLIDER: "numEFPIB" - Número de Entidades Federativas para vualizar su PIB
                                box(
                                    title = "TOP Entidades Federativas con mayor Producto Interno Bruto (PIB) en México",
                                    sliderInput("numEFPIB", "Número de Entidades Federativas:", 1, 32, 10) #
                                ),
                                # GRAFICA: TOP Entidades Federativas con mayor número de Ventas en Millones de Dólares en México 2019
                                box(plotOutput("plotH3", height = 350)),
                                # GRAFICA: TOP Entidades Federativas con mayor Producto Interno Bruto (PIB) en México 2019
                                box(plotOutput("plotH4", height = 350)),
                                
                                
                                # MAPA: Porcentaje Obesidad en México 2018, mapa de calor por Entidad Federativa
                                box(plotOutput("plotH5", height = 600)),
                                
                                
                                # SLIDER: "numMPO" - Número de Municipios para visualizar su Porcentaje de Obesidad
                                box(
                                    title = "Top Municipios con mayor obesidad en México",
                                    sliderInput("numMPO", "Número de Municipios:", 1, 50, 10)
                                ),
                                # GRAFICA: Municipios con mayor obesidad en México 2018
                                box(plotOutput("plotH6", height = 350))
                            )
                    ),
                    
                    # REGRESION LINEAL: Porcentaje de Obesidad en México, histórico 1975-2016
                    tabItem(tabName = "Regresion",
                            fluidRow(
                                titlePanel("REGRESION LINEAL - Porcentaje de Obesidad en México ourworldindata.org 1975-2016"), 
                                h4(textOutput("output_textPred1")),
                                h4(textOutput("output_textPred2")),
                                box(plotOutput("plotRL1", height = 700)),
                                
                                selectInput("datasetSelect", "Selecciona el Año de Mapeo:", 
                                            c("Datos.Obesidad.2018", "Datos.Obesidad.2012", "Datos.Obesidad.2006")), 
                                
                                box(plotOutput("plotRL2", height = 700))
                                
                            )
                    ),
                    
                    # DATA TABLE: Tabulado de datos Porcentaje Obesidad, ECI, Ventas y PIB a nivel Entidad Federativa
                    tabItem(tabName = "data_table_estado",
                            fluidRow(        
                                titlePanel(h3("Data Table a nivel Entidad Federativa")),
                                dataTableOutput ("data_table_estado"),
                                titlePanel(h3("Summary")),
                                verbatimTextOutput("summary_estado")
                            )
                    ), 
                    
                    # DATA TABLE: Tabulado de datos Porcentaje Obesidad a Nivel Municipio
                    tabItem(tabName = "data_table_municipio",
                            fluidRow(        
                                titlePanel(h3("Data Table a Nivel Municipio")),
                                dataTableOutput ("data_table_municipio"),
                                titlePanel(h3("Summary")),
                                verbatimTextOutput("summary_municipio")
                            )
                    ), 
                    
                    # MAPAS: Entidades Federativas objetivo con base en las variables Porcentaje Obesidad, ECI, Ventas y PIB
                    tabItem(tabName = "maps",
                            fluidRow(
                                titlePanel(h3("TIENDAS DE ROPA - ENTIDADES FEDERATIVAS SELECCIONADAS")),
                                tabsetPanel(
                                    
                                    # MAPA-Entidad Federativa: SONORA
                                    tabPanel("SONORA", 
                                             h3(textOutput("SONORA")), 
                                             
                                             # SLIDER: Tiendas.Sonora - Parámetro de Concentración del Número de Tiendas por Municipio en Sonora
                                             box(
                                                 title = "Concentración del Número de Tiendas por Municipio",
                                                 sliderInput("Tiendas.Sonora","Tiendas.Sonora:",min = 1, max = 50, value = c(20, 50)) 
                                             ),
                                             # SLIDER: "numMunSON" - Número de Municipios de Sonora para visualizar su Porcentaje de Obesidad
                                             box(
                                                 title = "Porcentaje de Obesidad por Municipios en Sonora",
                                                 sliderInput("numMunSON", "Municipios:", 1, 69, 15) 
                                             ),
                                             # GRAFICA: Concentración de Tiendas por Municipio en Sonora
                                             box(plotOutput("plotM1", height = 800)),
                                             # GRAFICA: Porcentaje de Obesidad por Municipio en Sonora
                                             box(plotOutput("plotM2", height = 800))
                                             
                                    ),
                                    
                                    
                                    # MAPA-Entidad Federativa: BAJA CALIFORNIA
                                    tabPanel("BAJA CALIFORNIA", 
                                             h3(textOutput("BAJA CALIFORNIA")), 
                                             
                                             # SLIDER: Tiendas.Baja.California - Parámetro de Concentración del Número de Tiendas por Municipio en Baja California
                                             box(
                                                 title = "Concentración del Número de Tiendas por Municipio",
                                                 sliderInput("Tiendas.Baja.California","Tiendas.Baja.California:",min = 1, max = 50, value = c(20, 50)) 
                                             ),
                                             # SLIDER: numMunBAJ - Número de Municipios de Baja California para visualizar su Porcentaje de Obesidad
                                             box(
                                                 title = "Porcentaje de Obesidad por Municipios en Baja California",
                                                 sliderInput("numMunBAJ", "Municipios:", 1, 5, 5) 
                                             ),
                                             # GRAFICA: Concentración de Tiendas por Municipio en Baja California
                                             box(plotOutput("plotM3", height = 800)),
                                             # GRAFICA: Porcentaje de Obesidad por Municipio en Baja California
                                             box(plotOutput("plotM4", height = 250))
                                    ),
                                    
                                    
                                    # MAPA-Entidad Federativa: NUEVO LEON
                                    tabPanel("NUEVO LEON", 
                                             h3(textOutput("NUEVO LEON")), 
                                             
                                             # SLIDER: Tiendas.Nuevo.Leon - Parámetro de Concentración del Número de Tiendas por Municipio en Nuevo León
                                             box(
                                                 title = "Concentración del Número de Tiendas por Municipio",
                                                 sliderInput("Tiendas.Nuevo.Leon","Tiendas.Nuevo.Leon:",min = 1, max = 50, value = c(20, 50)) 
                                             ),
                                             # SLIDER: numMunNUE - Número de Municipios de Nuevo León para visualizar su Porcentaje de Obesidad
                                             box(
                                                 title = "Porcentaje de Obesidad por Municipios en Nuevo León",
                                                 sliderInput("numMunNUE", "Municipios:", 1, 51, 15) 
                                             ),
                                             # GRAFICA: Concentración de Tiendas por Municipio en en Nuevo León
                                             box(plotOutput("plotM5", height = 800)),
                                             # GRAFICA: Porcentaje de Obesidad por Municipio en en Nuevo León
                                             box(plotOutput("plotM6", height = 800))
                                    ),
                                    
                                    
                                    # MAPA-Entidad Federativa: COAHUILA
                                    tabPanel("COAHUILA", 
                                             h3(textOutput("COAHUILA")), 
                                             
                                             # SLIDER: Tiendas.Coahuila - Parámetro de Concentración del Número de Tiendas por Municipio en Coahuila
                                             box(
                                                 title = "Concentración del Número de Tiendas por Municipio",
                                                 sliderInput("Tiendas.Coahuila","Tiendas.Coahuila:",min = 1, max = 50, value = c(20, 50))
                                             ),
                                             # SLIDER: numMunCOA - Número de Municipios de Coahuila para visualizar su Porcentaje de Obesidad
                                             box(
                                                 title = "Porcentaje de Obesidad por Municipios en Coahuila de Zaragoza",
                                                 sliderInput("numMunCOA", "Municipios:", 1, 38, 15) 
                                             ),
                                             # GRAFICA: Concentración de Tiendas por Municipio en en Coahuila
                                             box(plotOutput("plotM7", height = 800)),
                                             # GRAFICA: Porcentaje de Obesidad por Municipio en en Coahuila
                                             box(plotOutput("plotM8", height = 800))
                                    ),
                                    
                                    
                                    # MAPA-Entidad Federativa: TAMAULIPAS
                                    tabPanel("TAMAULIPAS", 
                                             h3(textOutput("TAMAULIPAS")),
                                             
                                             # SLIDER: Tiendas.Tamaulipas - Parámetro de Concentración del Número de Tiendas por Municipio en Tamaulipas
                                             box(
                                                 title = "Concentración del Número de Tiendas por Municipio",
                                                 sliderInput("Tiendas.Tamaulipas","Tiendas.Tamaulipas:",min = 1, max = 50, value = c(20, 50)) 
                                             ),
                                             # SLIDER: numMunTAM - Número de Municipios de Tamaulipas para visualizar su Porcentaje de Obesidad
                                             box(
                                                 title = "Porcentaje de Obesidad por Municipios en Tamaulipas",
                                                 sliderInput("numMunTAM", "Municipios:", 1, 43, 15) 
                                             ),
                                             # GRAFICA: Concentración de Tiendas por Municipio en en Tamaulipas
                                             box(plotOutput("plotM9", height = 800)),
                                             # GRAFICA: Porcentaje de Obesidad por Municipio en en Tamaulipas
                                             box(plotOutput("plotM10", height = 800))
                                    )
                                )
                            )
                    ),
                    
                    #FUENTES: Fuentes de consulta de los datos
                    tabItem(tabName = "fuentes",
                            fluidRow(        
                                titlePanel(h2("FUENTES DE CONSULTA - OPEN DATA", style='padding-left: 20px')),
                                
                                titlePanel(h4("• Encuesta Nacional de Salud y Nutrición (ENSANUT) 2018",style='padding-left: 30px')),
                                titlePanel(h5("- Fuente: https://www.inegi.org.mx/programas/ensanut/2018/#Tabulados",style='padding-left: 60px')),
                                titlePanel(h5("- Datos: Porcentaje de Obesidad por Entidad Federativa y Municipio (2018).",style='padding-left: 60px')),
                                
                                titlePanel(h4("• Sistema de Indicadores para Monitorear los Avances de la Estrategia Nacional para la Prevención y el Control del Sobrepeso, la Obesidad y la Diabetes (ENPCSOD) 2012 (Página 15):",style='padding-left: 30px')),
                                titlePanel(h5("- Fuente: http://oment.salud.gob.mx/indicadores_descargas/reporte_resultados_oment.pdf",style='padding-left: 60px')),
                                titlePanel(h5("- Datos: Porcentaje de Obesidad por Entidad Federativa (2012).",style='padding-left: 60px')),
                                
                                titlePanel(h4("• Diez Problemas de la Población de Jalisco: Una Perspectiva Sociodemográfi 2006 (Página 17):",style='padding-left: 30px')),
                                titlePanel(h5("- Fuente: https://iieg.gob.mx/contenido/PoblacionVivienda/libros/LibroDiezproblemas/Capitulo1.pdf",style='padding-left: 60px')),
                                titlePanel(h5("- Datos: Porcentaje de Obesidad por Entidad Federativa (2006).",style='padding-left: 60px')),
                                
                                titlePanel(h4("• The Global distribution of health impacts from obesity, México 1975-2016:",style='padding-left: 30px')),
                                titlePanel(h5("- Fuente: https://ourworldindata.org/obesity",style='padding-left: 60px')),
                                titlePanel(h5("- Datos: Regresión lineal del Porcentaje de Obesidad en el País, Histórico y Predicción.",style='padding-left: 60px')),
                                
                                titlePanel(h4("• PIB por Entidad Federativa en México / Actividades terciarias/ Comercio al por menor - INEGI 2019",style='padding-left: 30px')),
                                titlePanel(h5("- Fuente: https://www.inegi.org.mx/app/tabulados/default.aspx?pr=17&vr=7&in=27&tp=20&wr=1&cno=2",style='padding-left: 60px')),
                                
                                titlePanel(h4("• Índice de Complejidad Económica (ECI) (2020-S1):",style='padding-left: 30px')),
                                titlePanel(h5("- Fuente: https://datamexico.org/es/eci/explore",style='padding-left: 60px')),
                                
                                titlePanel(h4("• Comercio al por Menor de Ropa, Bisutería y Accesorios de Vestir (2020): ",style='padding-left: 30px')),
                                titlePanel(h5("- Fuente: https://datamexico.org/es/profile/industry/comercio-al-por-menor-de-ropa-bisuteria-y-accesorios-de-vestir",style='padding-left: 60px'))
                                
                            )
                    ),
                    
                    #TEAM: Integrantes del equipo
                    tabItem(tabName = "team",
                            fluidRow(        
                                #tags$pre(h4("Hello world!   \t \t   Hier I'm))
                                titlePanel(h1("INTEGRANTES EQUIPO 11 - SANTANDER BEDU", style='padding-left: 20px')),
                                titlePanel(h4("- Ariel Arturo Ríos Sierra",style='padding-left: 50px')),
                                titlePanel(h4("- Axel Flores Guarneros",style='padding-left: 50px')),
                                titlePanel(h4("- Evelyn Jocelyn Gonzalez Acevedo",style='padding-left: 50px')),
                                titlePanel(h4("- Marco Antonio Armas Santillán",style='padding-left: 50px'))
                            )
                    )
                )
            )
        )
    )

############################ SERVER #############################

server <- function(input, output) {
    
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
    
    " ANALIZANDO EL PORCENTAJE DE OBESIDAD POR ENTIDAD FEDERATIVA
    Podemos observar en el siguiente gráfico de barras que muestra el porcentaje de obesidad por estado es
    encabezado por Quintana Roo con un porcentaje de 49.93%, al ir reduciendo el rango para visualizar un 
    Top 5 se denota que dicho top está constituido además por los estados de Baja California, Tabasco, 
    Yucatán y Campeche con un porcentaje de 44.9%."
    
    # GRAFICA: TOP Entidades Federativas con mayor obesidad en México 2018
    output$plotH1 <- renderPlot({
        meanPO <-mean(obesidad.Estado$Porcentaje.obesidad)
        medPO <- median(obesidad.Estado$Porcentaje.obesidad)
        obesidad.Estado <- obesidad.Estado[with(obesidad.Estado, order(-obesidad.Estado$Porcentaje.obesidad)), ] # Orden inverso
        ggplot(head(obesidad.Estado, as.numeric(input$numEFPO)), aes(x=Porcentaje.obesidad , y=reorder(Entidad.federativa, Porcentaje.obesidad))) +
            geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
            ggtitle('Porcentaje de Obesidad por Entidad Federativa en México - ENSANUT 2018', paste("Media=",round(meanPO, 4), "Mediana=", round(medPO, 4))) +
            geom_vline(xintercept =  meanPO, col = "red", lwd = 1.0, lty =2)+
            geom_vline(xintercept =  medPO, col = "black", lwd = 1.0, lty =2)+
            xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
            ylab('Entidad Federativa') +
            geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") + 
            scale_fill_continuous(type = "viridis", option = "C")
    })
    
    
    " Índice de Complejidad Económica (ECI) (2020-S1) https://datamexico.org/es/eci/explore
    El ECI es la diferencia en productividad e ingresos entre entidades puede ser explicada por diferencias
    en la complejidad económica, entendida como la diversidad de las capacidades presentes en cada entidad 
    (Hausmann, Cheston Santos, 2015), es decir es la cualidad que predice el desarrollo económico.
    Esta variable resulta ampliamente relevante para nuestro análisis, ya que los hallazgos derivados de
    este estudio pueden promover sectores con un potencial emergente con oportunidades de emprendimiento 
    mostrando capacidades existentes y potenciales con las que cuenta específicamente cada estado."
    
    " ANALIZANDO EL ECI
    Es claro que las entidades que pertenecen a las regiones norte y centro tienen los primeros lugares, posicionándose
    en el primer puesto Nuevo León particularmente Nuevo Leon, Queretaro, Coahuila, Baja California y Chihuahua tienen
    un mayor potencial de crecimiento"
    
    # GRÁFICA: TOP Entidad Federativa con mayor Indice de Complejidad Económica (ECI) en México 2020
    output$plotH2 <- renderPlot({
        meanECI <-mean(obesidad.Estado$ECI.valor)
        medECI <- median(obesidad.Estado$ECI.valor)
        obesidad.Estado <- obesidad.Estado[with(obesidad.Estado, order(-obesidad.Estado$ECI.valor)), ] # Orden inverso
        ggplot(head(obesidad.Estado,as.numeric(input$numEFECI)), aes(x=ECI.valor , y=reorder(Entidad.federativa, ECI.valor))) +
            geom_bar(stat='identity', aes(fill=ECI.valor), position='dodge') +
            ggtitle('ECI por Entidad Federativa en México - datamexico.org 2020', paste("Media=",round(meanECI, 4), "Mediana=", round(medECI, 4))) +
            geom_vline(xintercept =  meanECI, col = "red", lwd = 1.0, lty =2)+
            geom_vline(xintercept =  medECI, col = "black", lwd = 1.0, lty =2)+
            xlab('Indice de Complejidad Económica (ECI)') +
            ylab('Entidad Federativa') +
            geom_text(aes(label = ECI.valor), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "D")
        
    })
    
    
    " Número de ventas de ropa por Estado 
    https://datamexico.org/es/profile/industry/comercio-al-por-menor-de-ropa-bisuteria-y-accesorios-de-vestir
    Esta variable resulta de vital importancia ya que nos indica Comercio al por Menor de Ropa y accesorios
    de Vestir en el año 2020 por Entidad Federativa, lo que nos permite identificar los estados potenciales 
    en este sector."
    
    " ANALIZANDO LAS VENTAS DE ROPA POR ENTIDAD FEDERATIVA
    El histograma  demuestra que los estados que cuentan con un mayor cantidad de ventas superiores de 100,000,000 
    conforman  el top 5, siendo  Coahuila de Zaragoza el estado que encabeza nuestra clasificación con un valor 
    superior a 190,000,000. Ademá claro que las entidades que pertenecen a las regiones norte y centro, 
    particularmente Nuevo Leon,Queretaro,  Coahuila, Baja California y Chihuahua tienen un mayor potencial 
    de crecimiento"
    
    # GRAFICA: TOP Entidades Federativas con mayor número de Ventas en Millones de Dólares en México 2020
    output$plotH3 <- renderPlot({
        meanVe <- mean(na.omit(obesidad.Estado$Ventas))
        medPVe <- median(na.omit(obesidad.Estado$Ventas))
        obesidad.Estado <- obesidad.Estado[with(obesidad.Estado, order(-obesidad.Estado$Ventas)), ] # Orden inverso
        ggplot(head(obesidad.Estado,as.numeric(input$numEFV)), aes(x=Ventas , y=reorder(Entidad.federativa, Ventas))) +
            geom_bar(stat='identity', aes(fill=Ventas), position='dodge') +
            ggtitle('Número de ventas por Entidad Federativa dentro de México - datamexico.org 2020', paste("Media=",round(meanVe, 4), "Mediana=", round(medPVe, 4))) +
            geom_vline(xintercept =  meanVe, col = "red", lwd = 1.0, lty =2)+
            geom_vline(xintercept =  medPVe, col = "black", lwd = 1.0, lty =2)+
            xlab('Ventas internacionales USD $M') +
            ylab('Entidad Federativa') +
            geom_text(aes(label = Ventas), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "D")
        
    })
    
    
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
    
    " ANALIZANDO EL PIB"
    " Bien se sabe que dicha variable se relaciona con los valores monetarios de los bienes y servicios, por 
    lo que entre mayor sea la variable , mayor será el poder adquisitivo del estado.
    Sin embargo se debe tener cuidado en el análisis de esta debido a que los valores no abarcan solamente al 
    comercio textil. Aun así podemos observar que los estados del centro del País encabezan los primeros 3 lugares: 
    Ciudad de México, el Edo México y Jalisco, siendo los 2 últimos Nuevo León y Veracruz "
    
    # GRAFICA: TOP Entidades Federativas con mayor Producto Interno Bruto (PIB) en México 2019
    output$plotH4 <- renderPlot({
        meanPIB <-mean(obesidad.Estado$PIB.2019)
        medPIB <- median(obesidad.Estado$PIB.2019)
        obesidad.Estado <- obesidad.Estado[with(obesidad.Estado, order(-obesidad.Estado$PIB.2019)), ] # Orden inverso
        ggplot(head(obesidad.Estado,as.numeric(input$numEFPIB)), aes(x=PIB.2019 , y=reorder(Entidad.federativa, PIB.2019))) +
            geom_bar(stat='identity', aes(fill=PIB.2019), position='dodge') +
            ggtitle('PIB por Entidad Federativa en México / Actividades terciarias/ Comercio al por menor - INEGI 2019', paste("Media=",round(meanPIB, 4), "Mediana=", round(medPIB, 4))) +
            geom_vline(xintercept =  meanPIB, col = "red", lwd = 1.0, lty =2)+
            geom_vline(xintercept =  medPIB, col = "black", lwd = 1.0, lty =2)+
            xlab('Producto Interno Bruto (PIB)') +
            ylab('Entidad Federativa') +
            geom_text(aes(label = PIB.2019), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "C")
        
    })
    
    ################## MAPA: Porcentaje Obesidad en México 2018, mapa de calor por Entidad Federativa ##################
    
    "Para comprender un poco la situación del país a nivel de obesidad por entidad federativa
    se procedió a trabajar con una base de datos de niveles de obesidad obtenidad del INEGI para
    generar un mapa porcentual de dicho parámetro aplicado a la república mexicana."
    
    output$plotH5 <- renderPlot({
        
        # Se une la tabla de datos de obesidad al archivo shapefile
        joined <- merge(shp0, Datos.Obesidad.2018, by.x="NOM_ENT")
        
        # Creamos la paleta de colores que usaremos para colorear el mapa
        pal <- brewer.pal(4, "YlOrRd") 
        pal <- colorRampPalette(pal)(16)
        
        "En dicho mapa se observa que para el año 2018 la zona norte y sur resulta ser donde los niveles
        de obesidad son más altos"
        
        # Imprimimos el mapa ya coloreado de acuerdo a la paleta de colores.
        spplot(joined, "X2018",colorkey = list(space = "bottom"), scales = list(draw = TRUE),main = "Porcentaje de Obesidad en México por Entidad Federativa ENSANUT 2018",col.regions=pal)
        
    })
    
    
    " ANALIZANDO EL PORCENTAJE DE OBESIDAD POR MUNICIPIO
    Reduciendo el grafico a un Top 5 ,se puede apreciar que 3 de los 5 municipios, forman parte del Estado de
    Sonora siendo solamente el municipio en segundo lugar tomado por Castaños que se encuentra en el estado de 
    Coahuila con un porcentaje de 66.97% en el Índice de Obesidad a sólo 12.5% por debajo del primer puesto ocupado
    por el Municipio de San Javier en Sonora."
    
    # GRAFICA: Municipios con mayor obesidad en México 2018
    output$plotH6 <- renderPlot({
        meanMUN <-mean(obesidad.Municipio$Porcentaje.obesidad)
        medMUN <- median(obesidad.Municipio$Porcentaje.obesidad)
        obesidad.Municipio <- obesidad.Municipio[with(obesidad.Municipio, order(-obesidad.Municipio$Porcentaje.obesidad)), ] # Orden inverso
        ggplot(head(obesidad.Municipio,as.numeric(input$numMPO)), aes(x=Porcentaje.obesidad , y=reorder(paste(Nombre.Municipio,Entidad.federativa,sep=','), Porcentaje.obesidad))) +
            geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
            ggtitle('Municipios con Mayor Obesidad en México ENSANUT 2018', paste("Media=",round(meanMUN, 4), "Mediana=", round(medMUN, 4))) +
            xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
            geom_vline(xintercept =  meanMUN, col = "red", lwd = 1.0, lty =2)+
            geom_vline(xintercept =  medMUN, col = "black", lwd = 1.0, lty =2)+
            ylab('Municipio y Entidad Federativa') +
            geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "D")
        
    })
    
    ################################## GRAFICA: REGRESION LINEAL #########################################
    
    "Durante años los problemas de salud en Méxco han tenido un aumento lineal bastante alto,
    se cuenta con una base de datos mundial de niveles de obesidad a nivel mundial de distintos paises
    desde los años 1975 a 2016 obtenido de https://ourworldindata.org/obesity  Dicho estudio al filtrar
    los datos solamente de méxico y de acuerdo a dichos datos generamos un modelo líneal y un ajuste de
    los datos a una línea recta que ajusto de forma muy precisa."
    
    output$plotRL1 <- renderPlot({
        dat <- read.csv('data/Regresion/Peso.csv')
        
        dat <- dat[dat$Code == "MEX",]
        
        dat <- rename(dat, Peso = Prevalence.of.overweight.adults..both.sexes....WHO..2019.) 
        
        dat <- data.frame(dat)
        m1 <- lm(dat$Peso ~ dat$Year )
        
        
        "Con la gráfica siguiente y el modelo ajustado podemos constatar cuantitativamente
        que el problema de obseidad en méxico tiene un incremento linal a lo largo de los años, 
        por lo que por obvias razones la demanda de este tipo de ropa crecerá a lo largo de los
        años en el pais y puede ser una buena fuente  de interés económico"
        
        "Utilizando los datos de los años podemos predecir de acuerdo a ese modelo la
        situación de méxico a futuro con la función predict para los años 2020 y 2021
        de los niveles de obesidad en méxico."
        
        dp <- data.frame(Year=c(2020,2021))
        mod <- lm(Peso~Year,data=dat)
        pred <- predict(mod,dp)
        
        # Se imprime el modelo de presicción 2020 en la User Interface
        output$output_textPred1 <- renderText( {paste("Modelo de Predicción Porcentual 2020:", round(pred[1], 4))})
        # Se imprime el modelo de presicción 2021 en la User Interface
        output$output_textPred2 <- renderText( {paste("Modelo de Predicción Porcentual 2021:", round(pred[2], 4))}) 
        
        
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
            xlab("Años") + 
            ylab("Peso (%)") + 
            ggtitle("Obesidad") + 
            scale_color_discrete(guide=FALSE) + theme_grey()
        
    })
    
    # MAPA Porcentaje Obesidad por Entidad Federativa
    output$plotRL2 <- renderPlot({
        
        if(input$datasetSelect == 'Datos.Obesidad.2018'){
            # Se une la tabla de datos de obesidad al archivo shapefile
            joined <- merge(shp0, Datos.Obesidad.2018, by.x="NOM_ENT")
            
            # Creamos la paleta de colores que usaremos para colorear el mapa
            pal <- brewer.pal(4, "YlOrRd") 
            pal <- colorRampPalette(pal)(16)
            
            # Imprimimos el mapa ya coloreado de acuerdo a la paleta de colores.
            spplot(joined, "X2018",colorkey = list(space = "bottom"), scales = list(draw = TRUE),main = "Porcentaje de Obesidad en México por Entidad Federativa ENSANUT 2018",col.regions=pal)  
            
        }
        else if(input$datasetSelect == 'Datos.Obesidad.2012'){
            # Se une la tabla de datos de obesidad al archivo shapefile
            joined <- merge(shp0, Datos.Obesidad.2012, by.x="NOM_ENT")
            
            # Creamos la paleta de colores que usaremos para colorear el mapa
            pal <- brewer.pal(4, "YlOrRd") 
            pal <- colorRampPalette(pal)(16)
            
            # Imprimimos el mapa ya coloreado de acuerdo a la paleta de colores.
            spplot(joined, "X2012",colorkey = list(space = "bottom"), scales = list(draw = TRUE),main = "Porcentaje de Obesidad en México por Entidad Federativa ENSANUT 2012",col.regions=pal)   
        }
        else if(input$datasetSelect == 'Datos.Obesidad.2006'){
            # Se une la tabla de datos de obesidad al archivo shapefile
            joined <- merge(shp0, Datos.Obesidad.2006, by.x="NOM_ENT")
            
            # Creamos la paleta de colores que usaremos para colorear el mapa
            pal <- brewer.pal(4, "YlOrRd") 
            pal <- colorRampPalette(pal)(16)
            
            # Imprimimos el mapa ya coloreado de acuerdo a la paleta de colores.
            spplot(joined, "X2006",colorkey = list(space = "bottom"), scales = list(draw = TRUE),main = "Porcentaje de Obesidad en México por Entidad Federativa ENSANUT 2006",col.regions=pal)       
        }
        
    })
    
    
    
    #Data Table por Estado
    output$data_table_estado <- renderDataTable( {obesidad.Estado[,c(1,2,3,4,5,8,9,10,11)]}, 
                                                 options = list(aLengthMenu = c(5,25,50),
                                                                iDisplayLength = 10)
    )
    
    output$summary_estado <- renderPrint( {summary(obesidad.Estado[,c(1,2,3,4,5,8,9,10,11)])} )   # Summary
    
    #Data Table por Municipio
    output$data_table_municipio <- renderDataTable( {obesidad.Municipio[,c(1,2,3,4,5,6,8)]}, 
                                                    options = list(aLengthMenu = c(5,25,50),
                                                                   iDisplayLength = 10)
    )
    
    output$summary_municipio <- renderPrint( {summary(obesidad.Municipio[,c(1,2,3,4,5,6,8)])} )   # Summary
    
    
    ################################# ELECCIÓN DE ENTIDADES FEDERATIVAS ""##################################
    "Se contemplaron 4 variables estadísticas en orden de importancia para poder escoger un top 5 en los Estados.
    Siendo el ECI el más importante y el PIB el menos importante La primera variable es el ECI; de dicha variable
    sólo contempla valores positivos registrados del estado, es decir, probabilidad de crecimiento. El segundo
    indicador fue el índice de sobrepeso de la población, el tercero fueron las ventas internacionales registradas
    del estado, esto nos indica el nivel de comercio presente en el estado y finalmente el PIB del propio estado.

    El top esta conformado por las siguientes Entidades Federativas: 
         - SONORA
         - BAJA CALIFORNIA
         - NUEVO LEON
         - COAHUILA DE ZARAGOZA
         - TAMAULIPAS"
    
    ################################ 5 ENTIDADES FEDERATIVAS SELCCIONADAS ##################################
    
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
    
    
    # MAPA ENTIDAD FEDERATIVA - SONORA
    output$plotM1 <- renderPlot({
        
        shp1<-readOGR(dsn="data/mapas/SONORA2.shp", layer="SONORA2")
        data <- read.csv("data/mapas/sonora.csv")
        
        shp1.df <- fortify(shp1)
        
        which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA
        
        # Por si queremos ploter al el mapa solo sin puntos ni relleno.
        map <- ggplot() + geom_path(data = shp1.df,aes(x = long, y = lat, group = group),
                                    color = 'red', fill = 'gray', size = .2)
        print(map)
        
        data2 <- data
        # Aplicamos la funcón para transformas las coordenadas geográficas a UTM.
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
        #data5 <- data3[data4$Freq>(as.numeric(input$numTSONmin)) & data4$Freq<(as.numeric(input$numTSONmax)),]
        data5 <- data3[data4$Freq>as.numeric(input$Tiendas.Sonora[1]) & data4$Freq<as.numeric(input$Tiendas.Sonora[2]),]
        
        # Ploteamos el nuevo mapa con los puntos sobrepuestos
        mapS <- ggplot() + geom_polygon(data = shp1.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange", size=5.0) + geom_point(aes(x=data5$X,y=data5$Y), color="black", size=3.0) + theme_bw() + labs(title = "SONORA",x="",y="",fill="black") + theme(plot.title = element_text(size=20, face= "bold.italic", color="black"), legend.position = "up" )
        print(mapS)
        
    })
    
    # GRAFICA: TOP Municipios de sonora con mayor obesidad 2018
    output$plotM2 <- renderPlot({
        
        obesidad.Sonora <- filter(obesidad.Municipio, Entidad.federativa == 'Sonora')
        obesidad.Sonora <- obesidad.Sonora[with(obesidad.Sonora, order(-obesidad.Sonora$Porcentaje.obesidad)), ] # Orden inverso
        
        ggplot(head(obesidad.Sonora, as.numeric(input$numMunSON)), aes(x=Porcentaje.obesidad , y=reorder(paste(Nombre.Municipio,Entidad.federativa,sep=','), Porcentaje.obesidad))) +
            geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
            ggtitle('Municipios con Mayor Obesidad en Sonora, México ENSANUT 2018') +
            xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
            ylab('Municipio y Entidad Federativa') +
            geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "D")
        
    })
    
    # MAPA ENTIDAD FEDERATIVA - BAJA CALIFORNIA
    output$plotM3 <- renderPlot({
        
        shp2<-readOGR(dsn="data/mapas/BAJA2.shp", layer="BAJA2")
        data <- read.csv("data/mapas/bajacalifornia.csv")
        
        shp2.df <- fortify(shp2)
        
        which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA
        
        # Por si queremos ploter al el mapa solo sin puntos ni relleno.
        map <- ggplot() + geom_path(data = shp2.df,aes(x = long, y = lat, group = group),
                                    color = 'red', fill = 'gray', size = .2)
        print(map)
        
        data2 <- data
        # Aplicamos la funcón para transformas las coordenadas geogr?ficas a UTM.
        UTM <- LongLatToUTM(data2$Longitud,data2$Latitud,11)
        
        # Esos nuevos datos tranformados los agregamos como dos nuevas columnas al df original.
        data3 <- data.frame(X = UTM$X, Y = UTM$Y, data2)
        
        # Eliminamos outlier de los puntos.
        #data3 <- data3[-c(2671,2677),]
        
        # Calculamos la fecuencia de las tiendas por colonias para obtener puntos
        data4 <- table(data3$Colonia)
        
        # Lo transformamos en df para un mejor manejo
        data4 <- data.frame(data4)
        
        # Establecemos el intervalo de entre cuántas tiendas por colonias vamos a filtrar.
        data5 <- data3[data4$Freq>as.numeric(input$Tiendas.Baja.California[1]) & data4$Freq<as.numeric(input$Tiendas.Baja.California[2]),]
        
        # Ploteamos el nuevo mapa con los puntos sobrepuestos
        mapS <- ggplot() + geom_polygon(data = shp2.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange", size=5.0) + geom_point(aes(x=data5$X,y=data5$Y), color="black", size=3.0) + theme_bw() + labs(title = "BAJA CALIFORNIA",x="",y="",fill="black") + theme(plot.title = element_text(size=20, face= "bold.italic", color="black"), legend.position = "up" )
        print(mapS)
        
        
    })
    
    # GRAFICA: TOP Municipios de Baja California con mayor obesidad 2018
    output$plotM4 <- renderPlot({
        
        obesidad.BajaC <- filter(obesidad.Municipio, Entidad.federativa == 'Baja California')
        obesidad.BajaC <- obesidad.BajaC[with(obesidad.BajaC, order(-obesidad.BajaC$Porcentaje.obesidad)), ] # Orden inverso
        
        ggplot(head(obesidad.BajaC, as.numeric(input$numMunBAJ)), aes(x=Porcentaje.obesidad , y=reorder(paste(Nombre.Municipio,Entidad.federativa,sep=','), Porcentaje.obesidad))) +
            geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
            ggtitle('Municipios con Mayor Obesidad en Baja California, México ENSANUT 2018') +
            xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
            ylab('Municipio y Entidad Federativa') +
            geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "C")
        
    })
    
    # MAPA ENTIDAD FEDERATIVA - NUEVO LEON
    output$plotM5 <- renderPlot({
        
        shp3<-readOGR(dsn="data/mapas/NUEVO2.shp", layer="NUEVO2")
        data <- read.csv("data/mapas/nuevoleon.csv")
        
        shp3.df <- fortify(shp3)
        
        which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA
        
        # Por si queremos ploter al el mapa solo sin puntos ni relleno.
        map <- ggplot() + geom_path(data = shp3.df,aes(x = long, y = lat, group = group),
                                    color = 'red', fill = 'gray', size = .2)
        print(map)
        
        data2 <- data
        # Aplicamos la función para transformas las coordenadas geogrÃ¡ficas a UTM.
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
        data5 <- data3[data4$Freq>as.numeric(input$Tiendas.Nuevo.Leon[1]) & data4$Freq<as.numeric(input$Tiendas.Nuevo.Leon[2]),]
        
        # Ploteamos el nuevo mapa con los puntos sobrepuestos
        mapS <- ggplot() + geom_polygon(data = shp3.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange", size=5.0) + geom_point(aes(x=data5$X,y=data5$Y), color="black", size=3.0) + theme_bw() + labs(title = "NUEVO LEON",x="",y="",fill="black") + theme(plot.title = element_text(size=20, face= "bold.italic", color="black"), legend.position = "up" )
        print(mapS)
        
    })
    
    # GRAFICA: TOP Municipios de Nuevo León con mayor obesidad 2018
    output$plotM6 <- renderPlot({
        
        obesidad.NuevoL <- filter(obesidad.Municipio, Entidad.federativa == 'Nuevo León')
        obesidad.NuevoL <- obesidad.NuevoL[with(obesidad.NuevoL, order(-obesidad.NuevoL$Porcentaje.obesidad)), ] # Orden inverso
        
        ggplot(head(obesidad.NuevoL, as.numeric(input$numMunNUE)), aes(x=Porcentaje.obesidad , y=reorder(paste(Nombre.Municipio,Entidad.federativa,sep=','), Porcentaje.obesidad))) +
            geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
            ggtitle('Municipios con Mayor Obesidad en Nuevo León, México ENSANUT 2018') +
            xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
            ylab('Municipio y Entidad Federativa') +
            geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "D")
        
    })
    
    # MAPA ENTIDAD FEDERATIVA - COAHUILA
    output$plotM7 <- renderPlot({
        
        shp4<-readOGR(dsn="data/mapas/COAHUILA2.shp", layer="COAHUILA2")
        data <- read.csv("data/mapas/coahuila.csv")
        
        shp4.df <- fortify(shp4)
        
        which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA
        
        # Por si queremos ploter al el mapa solo sin puntos ni relleno.
        map <- ggplot() + geom_path(data = shp4.df,aes(x = long, y = lat, group = group),
                                    color = 'red', fill = 'gray', size = 0.2)
        print(map)
        
        data2 <- data
        # Aplicamos la función para transformas las coordenadas geogr?ficas a UTM.
        UTM <- LongLatToUTM(data2$Longitud,data2$Latitud,13)
        
        # Esos nuevos datos tranformados los agregamos como dos nuevas columnas al df original.
        data3 <- data.frame(X = UTM$X, Y = UTM$Y, data2)
        
        # Eliminamos outlier de los puntos.
        data3 <- data3[-c(1299,1837),]
        
        # Calculamos la fecuencia de las tiendas por colonias para obtener puntos
        data4 <- table(data3$Colonia)
        
        # Lo transformamos en df para un mejor manejo
        data4 <- data.frame(data4)
        
        # Establecemos el intervalo de entre cuántas tiendas por colonias vamos a filtrar.
        data5 <- data3[data4$Freq>as.numeric(input$Tiendas.Coahuila[1]) & data4$Freq<as.numeric(input$Tiendas.Coahuila[2]),]
        
        # Ploteamos el nuevo mapa con los puntos sobrepuestos
        mapS <- ggplot() + geom_polygon(data = shp4.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange", size=5.0) + geom_point(aes(x=data5$X,y=data5$Y), color="black", size=3.0) + theme_bw() + labs(title = "COAHUILA",x="",y="",fill="black") + theme(plot.title = element_text(size=20, face= "bold.italic", color="black"), legend.position = "up" )
        print(mapS)
        
    })
    
    # GRAFICA: TOP Municipios de Coahuila de Zaragoza con mayor obesidad 2018
    output$plotM8 <- renderPlot({
        
        obesidad.CoahuilaZ <- filter(obesidad.Municipio, Entidad.federativa == 'Coahuila de Zaragoza')
        obesidad.CoahuilaZ <- obesidad.CoahuilaZ[with(obesidad.CoahuilaZ, order(-obesidad.CoahuilaZ$Porcentaje.obesidad)), ] # Orden inverso
        
        ggplot(head(obesidad.CoahuilaZ, as.numeric(input$numMunCOA)), aes(x=Porcentaje.obesidad , y=reorder(paste(Nombre.Municipio,Entidad.federativa,sep=','), Porcentaje.obesidad))) +
            geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
            ggtitle('Municipios con Mayor Obesidad en Coahuila de Zaragoza, ENSANUT México 2018') +
            xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
            ylab('Municipio y Entidad Federativa') +
            geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "C")
        
    })
    
    # MAPA ENTIDAD FEDERATIVA - TAMAULIPAS
    output$plotM9 <- renderPlot({
        
        shp5<-readOGR(dsn="data/mapas/TAMAULIPAS2.shp", layer="TAMAULIPAS2")
        data <- read.csv("data/mapas/tamaulipas.csv")
        
        shp5.df <- fortify(shp5)
        
        which(is.na(data$latitud), arr.ind=TRUE) # SIRVE PARA IDENTIFICAR INDICES DONDE HAY VALORES NA
        
        # Por si queremos ploter al el mapa solo sin puntos ni relleno.
        map <- ggplot() + geom_path(data = shp5.df,aes(x = long, y = lat, group = group),
                                    color = 'red', fill = 'gray', size = .2)
        print(map)
        
        data2 <- data
        # Aplicamos la función para transformas las coordenadas geogr?ficas a UTM.
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
        data5 <- data3[data4$Freq>as.numeric(input$Tiendas.Tamaulipas[1]) & data4$Freq<as.numeric(input$Tiendas.Tamaulipas[2]),]
        
        # Ploteamos el nuevo mapa con los puntos sobrepuestos
        mapT <- ggplot() + geom_polygon(data = shp5.df, aes( x = long, y = lat, group = group), fill = "#84b369", color = "white") + geom_point(aes(x=data3$X,y=data3$Y), color="orange", size=5.0) + geom_point(aes(x=data5$X,y=data5$Y), color="black", size=3.0) + theme_bw() + labs(title = "TAMAULIPAS",x="",y="",fill="black") + theme(plot.title = element_text(size=20, face= "bold.italic", color="black"), legend.position = "up" )
        print(mapT)
        
    })
    
    # GRAFICA: TOP Municipios de Tamaulipas con mayor obesidad 2018
    output$plotM10 <- renderPlot({
        
        obesidad.Tamaulipas <- filter(obesidad.Municipio, Entidad.federativa == 'Tamaulipas')
        obesidad.Tamaulipas <- obesidad.Tamaulipas[with(obesidad.Tamaulipas, order(-obesidad.Tamaulipas$Porcentaje.obesidad)), ] # Orden inverso
        
        ggplot(head(obesidad.Tamaulipas, as.numeric(input$numMunTAM)), aes(x=Porcentaje.obesidad , y=reorder(paste(Nombre.Municipio,Entidad.federativa,sep=','), Porcentaje.obesidad))) +
            geom_bar(stat='identity', aes(fill=Porcentaje.obesidad), position='dodge') +
            ggtitle('Municipios con Mayor Obesidad en Sonora, México ENSANUT 2018') +
            xlab('Proporción estimada de personas con obesidad respecto a la población de 20 o más años') +
            ylab('Municipio y Entidad Federativa') +
            geom_text(aes(label = Porcentaje.obesidad), size = 3, hjust = -0.2, vjust = 0.5, position = "stack") +
            scale_fill_continuous(type = "viridis", option = "D")
    })
    
    ######################################## PROPBLEMAS PRESENTADOS ###########################################
    
    " - Bases de Datos Incompletas o con atributos relacionales distintos : es común que la información en 
        cada Base de datos presente variaciones ya sea por su acentuación o abreviación en los nombres, por
        lo cual representa una problemática al momento de correlacionar Bases de datos. 

      - Desactualización en las bases de Datos (presentaban periodos de tiempos diferentes ): La inclusión
        de las fuentes obedece a razones de cobertura geográfica y temporal, sin embargo mientras que de una
        base de datos se obtiene información a nivel nacional para un cierto periodo de tiempo, las demás
        cubrían una serie de tiempo diferente.

      - Difícil acceso y variedad a bases de Datos(algunas de ellas exigen un pago por su adquisición): 
        Debido a que la mayoría de las bases de datos gratuitas solo presentan análisis globales y aquellas 
        que contienen un análisis específico de  un tema o variable, comúnmente deben ser adquiridas de páginas 
        o plataformas que exigen un pago por la adquisición de ellas. "
    
    ############################################### CONCLUSIÓN ################################################
    
    " El principal Hallazgo en nuestro análisis estadístico representó una sorpresa para el equipo ya que al
      inicio se tenía estipulado que el  área idónea para la apertura dle establecimeinto  se encontraría
      en la Zona Central de Nuestro Pais, sin embargo gracias al análisis elaborado se pudo discernir que 
      área ideal se encuentra en el Norte del Pais debido a su alto numero de ECI y sus altos  casos de Obesidad 

      Igualmente se pudo certificar en cada uno de los estados la ubicacion estrategica para el asentamiento de 
      nuestro Comercio ,dicho analisis arrojo las siguientes ubicaciones
           - Baja California (NORTE)
           - Sonora(NORTE)
           - Nuevo Leon(OESTE)
           - Coahuila(SUR)
           - Tamaulipas(SUR)
    
      Para poder obtener análisis más detallado de la ubicación es necesario la obtención de  bases de datos más
      específicas que muestre a detalle una segmentación geográfica en cuanto a la cantidad de tiendas físicas 
      en cada zona del estado igualmente pormenorizando el  giro de cada tienda. (venta de ropa de mujer, ropa de 
      Hombre, tallas extra etc..) 

      ¿Cuál sería el impacto de este análisis como ayudaría? Facilitar el vestir para tallas amplias y 
      especialmente con calidad y variedad   dignifica a ese sector, a la vez que por su poca difusión resulta 
      recomendable la inversión con la garantía de la mejor promoción que es de consumidor a consumidor,
      incluyendo lo lucrativo que puede resultar. 
        
      Debemos adaptarnos a los diversos escenarios que convergen en México . Es necesario que en tiendas 
      departamentales o sectores de comercialización textil  exista una área destinada al sector de tallas grandes, 
      claro, tomando en cuenta el nivel socioeconómico es un acuerdo que beneficiaría a los consumidores y al
      mismo tiempo a quien lo comercializa."
    
    ############################################# FUENTES DE CONSULTA - OPENA DATA ##############################
    
    " - Encuesta Nacional de Salud y Nutrición (ENSANUT) 2018:
         - Fuente: https://www.inegi.org.mx/programas/ensanut/2018/#Tabulados
         - Datos: Porcentaje de Obesidad por Entidad Federativa y Municipio (2018).
      - Sistema de Indicadores para Monitorear los Avances de la Estrategia Nacional para la Prevención y el
        Contro del Sobrepeso, la Obesidad y la Diabetes (ENPCSOD) 2012 (Página 15):
         - Fuente: http://oment.salud.gob.mx/indicadores_descargas/reporte_resultados_oment.pdf
         - Datos: Porcentaje de Obesidad por Entidad Federativa (2012).
      - Diez Problemas de la Población de Jalisco: Una Perspectiva Sociodemográfi 2006 (Página 17): 
         - Fuente: https://iieg.gob.mx/contenido/PoblacionVivienda/libros/LibroDiezproblemas/Capitulo1.pdf
         - Datos: Porcentaje de Obesidad por Entidad Federativa (2006).
      - The Global distribution of health impacts from obesity, México 1975-2016:
         - Fuente: https://ourworldindata.org/obesity
         - Datos: Regresión lineal del Porcentaje de Obesidad en el País, Histórico y Predicción
      - PIB por Entidad Federativa en México / Actividades terciarias/ Comercio al por menor - INEGI 2019:  https://www.inegi.org.mx/app/tabulados/default.aspx?pr=17&vr=7&in=27&tp=20&wr=1&cno=2
         - Índice de Complejidad Económica (ECI) (2020-S1):https://datamexico.org/es/eci/explore
         - Comercio al por Menor de Ropa, Bisutería y Accesorios de Vestir (2020): https://datamexico.org/es/profile/industry/comercio-al-por-menor-de-ropa-bisuteria-y-accesorios-de-vestir"
}


shinyApp(ui, server)