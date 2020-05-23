# Title     : TODO
# Objective : TODO
# Created by: iker_
# Created on: 09/05/2020
rm(list=ls());
is.installed <- function(mypkg) {
  is.element(mypkg, installed.packages()[,1])
}

#Instalamos paquetes para agrupar csv
if (!is.installed("data.table")){
  install.packages("data.table",repos = "http://cran.us.r-project.org")
};
#Intslamos paquete para manipular ts
if (!is.installed("xts")){
  install.packages("xts",repos = "http://cran.us.r-project.org")
};

#Instalamos paquete para agrupar datos
if (!is.installed("dplyr")){
  install.packages("dplyr",repos = "http://cran.us.r-project.org")
};
# Instalamos paquete para leer exccels
if (!is.installed("readxl")){
  install.packages("readxl",repos = "http://cran.us.r-project.org")
};
if (!is.installed("RMySQL")){
  install.packages("RMySQL")
}


if (!is.installed("DBI")){
  install.packages("DBI")
}

if (!is.installed("httr")){
  install.packages("httr")
}
require("httr")
set_config( config( ssl_verifypeer = 0L ) )

if (!is.installed("jsonlite")){
  install.packages("jsonlite")
}
require("jsonlite")

if (!is.installed("readxl")){
  install.packages("readxl",repos = "http://cran.us.r-project.org")
}
require("readxl")
is.installed <- function(mypkg) {
  is.element(mypkg, installed.packages()[,1])
}

#esto es una prueba
if (!is.installed("rpart")){
  install.packages("rpart")
}
if (!is.installed("dplyr")){
  install.packages("dplyr")
}
if (!is.installed("rpart.plot")){
  install.packages("rpart.plot")
}
if (!is.installed("tidyr")){
  install.packages("tidyr")
}
if (!is.installed("readxl")){
  install.packages("readxl")
}
if (!is.installed("readr")){
  install.packages("readr")
}
if (!is.installed("RQuantLib")){
  install.packages("RQuantLib")
}
if (!is.installed("data.table")){
  install.packages("data.table",repos = "http://cran.us.r-project.org")
}
if (!is.installed("party")){
  install.packages("party",repos = "http://cran.us.r-project.org")
};
if (!is.installed("rpart")){
  install.packages("rpart",repos = "http://cran.us.r-project.org")
};
if (!is.installed("caret")){
  install.packages("caret",repos = "http://cran.us.r-project.org")
};
if (!is.installed("randomForest")){
  install.packages("randomForest",repos = "http://cran.us.r-project.org")
};
if (!is.installed("rpart.plot")){
  install.packages("rpart.plot",repos = "http://cran.us.r-project.org")
};
if (!is.installed("rattle")){
  install.packages("rattle",repos = "http://cran.us.r-project.org")
};
if (!is.installed("RColorBrewer")){
  install.packages("RColorBrewer",repos = "http://cran.us.r-project.org")
}
library(dplyr)    # ManipulaciÃƒÂ³n de data.frames
library(tidyr)    # Datos ordenados
library(readxl)   # Leer ficheros excel
library(readr)
library(RQuantLib)
library(rpart.plot)
library(data.table)
library(party)
library(rpart)
library("caret")
library(randomForest)
library(rpart.plot)
library(RColorBrewer)
library(xts)
library(RMySQL)
library(DBI)
#conectamos a la base de datos
con <- DBI::dbConnect(RMySQL::MySQL(),user="uaneo",password="uaneo",dbname="uaneo")
#select
select_parte <- "SELECT     p.*,
                            t.anno,
                            t.mes,
                            t.dia,
                            t.dia_semana,
                            d.nombre,
                            d.idema,
                            v.edad_desde,
                            v.edad_hasta,
                            v.lesividad,
                            v.sexo,
                            v.tipo_persona,
                            m.prec,
                            m.vvu
                            FROM parte p
                            LEFT JOIN dim_tiempo t ON t.id = p.id_tiempo
                            LEFT JOIN distrito d ON d.id = p.id_distrito
                            LEFT JOIN victima v ON v.id = p.id_victima
                            LEFT JOIN meteorologia m ON m.id = p.id_meteo
                            GROUP BY p.id"
#añadimos en date la query realizada
date <- dbGetQuery(con,select_parte)

# Anadimos semilla para la particion de los conjuntos de entreamiento y test
set.seed(458)

valores = c("edad_desde", "lesividad","tipo_vehiculo", "tipo_persona","prec","vvu")
date <- date[valores]

# Para quitar los valores nulos
naPerColumn <- colSums(is.na(date))
percentageNA <- round(naPerColumn/nrow(date)*100, digits = 2)
percentageNA <- percentageNA[percentageNA > 0]
percentageNA <- percentageNA[order(percentageNA)]

percentageNA
barplot(percentageNA,
        main ="Porcentaje de NA",
        xlab ="Accidentes",
        ylab ="%",
        ylim = c(0,100),
        las = 1,
        cex.names = 1,
        col = gray.colors(15))

#cambiar vvu
date <- date %>%
  mutate(vvu = case_when (
    vvu >=0 & vvu <= 10.7    ~1,
    vvu >10.7 & vvu <= 17.1    ~2,
    vvu >17.1 & vvu <= 24.4    ~3,
    vvu >24.4 & vvu <= 32.6    ~4,
    vvu > 32.6 ~ 5
  ))
date$vvu <-as.character(date$vvu)

date <- date %>%
  mutate(vvu = case_when (
    vvu =="1"    ~ "SUAVE",
    vvu =="2"    ~ "MODERADO",
    vvu =="3"    ~ "FUERTE",
    vvu =="4"    ~ "TORMENTA",
    vvu =="5"    ~ "HURACAN"
  ))
#cambiar precipitacion

date <- date %>%
  mutate(prec = case_when (
    prec >=0 & prec <= 2    ~1,
    prec >2 & prec <= 15    ~2,
    prec >15 & prec <= 30    ~3,
    prec >30 & prec <= 60    ~4,
    prec >60    ~5
  ))
date$prec <-as.character(date$prec)

date <- date %>%
  mutate(prec = case_when (
    prec =="1"    ~ "DEBILES",
    prec =="2"    ~ "MODERADAS",
    prec =="3"    ~ "FUERTES",
    prec =="4"    ~ "MUY_FUERTES",
    prec =="5"    ~ "TORRENCIALES"
  ))
#cambiar edades
# Ahora tranformamos las edades en 6 categorias
date$edad_desde <-as.integer(date$edad_desde)

date <- date %>%
  mutate(edad_desde = case_when (
    edad_desde >=0 & edad_desde <= 17    ~1,
    edad_desde >17 & edad_desde <= 30    ~2,
    edad_desde >30 & edad_desde <= 40    ~3,
    edad_desde >40 & edad_desde <= 60    ~4,
    edad_desde >60 & edad_desde <=70     ~5,
    edad_desde >70                       ~6
  ))
date$edad_desde <-as.character(date$edad_desde)

date <- date %>%
  mutate(edad_desde = case_when (
    edad_desde =="1"    ~ "MENORES DE EDAD",
    edad_desde =="2"    ~ "JOVENES",
    edad_desde =="3"    ~ "ADULTOS_TEMPRANO",
    edad_desde =="4"    ~ "ADULTOS_MEDIO",
    edad_desde =="5"    ~ "ADULTOS_TARDIO",
    edad_desde =="6"    ~ "TERCERA_EDAD"
  ))


# Creamos los conjuntos de entrenamiento y test
prueba <-sample(2, nrow(date), replace=TRUE, prob = c (0.7,0.30))
entreno <- date[prueba==1,]
test <- date[prueba==2,]

# Creo la formula con la que quiero comparar
myFormula <- lesividad ~ tipo_vehiculo + tipo_persona + edad_desde  + vvu

#Aqui creamos el modelo del arbol de decision ajustando los parametros
control <- rpart.control(minbucket = 500, cp = 0)

modelo <- rpart(myFormula, data = entreno, method = 'class', control = control)

#rpart.plot(modelo, extra = 1)


# Añadimos la variable calculada al test
test$lesividad_esperada <-predict(modelo, newdata = test, type ="class")

#Creamos una tabla con los datos del test
predictTable <-table(test$lesividad_esperada, test$lesividad)
predictTable

#calcula el % de confianza
accuracy <-sum(diag(predictTable))/sum(predictTable)
accuracy

#desconectamos de la base de datos
lapply(dbListConnections(MySQL()), dbDisconnect)
