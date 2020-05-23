# Title     : TODO
# Objective : TODO
# Created by: iker_
# Created on: 16/05/2020

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
if (!is.installed("randomForest")){
  install.packages("randomForest",repos = "http://cran.us.r-project.org")
}
if (!is.installed("ROCR")){
  install.packages("ROCR",repos = "http://cran.us.r-project.org")
}
library(randomForest)
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

#conexion a la base de datos
con <- DBI::dbConnect(RMySQL::MySQL(),user="uaneo",password="uaneo",dbname="uaneo")
#select de la base de datos
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
#insertamos en date la query realizada
date <- dbGetQuery(con,select_parte)


#transformamos la edad
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
    edad_desde =="3"    ~ "ADULTOS_TEMPRANOS",
    edad_desde =="4"    ~ "ADULTOS_MEDIOS",
    edad_desde =="5"    ~ "ADULTOS_TARDIOS",
    edad_desde =="6"    ~ "TERCERA_EDAD"
  ))

valores = c("edad_desde","tipo_vehiculo", "tipo_persona", "lesividad")
date <- date[valores]

set.seed(458)
prueba <-sample(2, nrow(date), replace=TRUE, prob = c (0.70,0.30))
entreno <- date[prueba==1,]
test <- date[prueba==2,]


# valores que usaremos


myFormula <- lesividad ~ tipo_vehiculo + tipo_persona + edad_desde
#ponemos en factor los valores
entreno$lesividad = factor(entreno$lesividad)
entreno$edad_desde = factor(entreno$edad_desde)
entreno$tipo_vehiculo = factor(entreno$tipo_vehiculo)
entreno$tipo_persona = factor(entreno$tipo_persona)
test$lesividad = factor(test$lesividad)
test$edad_desde = factor(test$edad_desde)
test$tipo_vehiculo = factor(test$tipo_vehiculo)
test$tipo_persona = factor(test$tipo_persona)
#para nuestro modelo eliminamos los NA
entreno <- entreno[!is.na(entreno$tipo_vehiculo),]
entreno <- entreno[!is.na(entreno$tipo_persona),]
entreno <-entreno[!is.na(entreno$edad_desde),]
entreno <- entreno[!is.na(entreno$lesividad),]
test <- test[!is.na(test$tipo_vehiculo),]
test <- test[!is.na(test$tipo_persona),]
test <-test[!is.na(test$edad_desde),]
test <- test[!is.na(test$lesividad),]


#modelo
modelo <-randomForest(formula = myFormula, data = entreno, ntree = 500 , keep.forest = TRUE)
#dibujamos el modelo
#print(modelo)
#plot(modelo)
#importancias de las variables
#modelo$importance
#varImpPlot(modelo)

pred <- predict(modelo,test, type = "class")
table(test[,"lesividad"],pred, dnn = c("Actuales","Prediccion"))
probs <- predict(modelo,test, type = "prob")

head(probs)

prediccion <- predict(modelo,test);
mc <- with(test,table(prediccion,lesividad))
accuracy<- sum(diag(mc) / sum(mc))
accuracy



#desconectamos de la base de datos
lapply(dbListConnections(MySQL()), dbDisconnect)


