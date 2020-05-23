# Creacion modelo - ARBOL DE DECISION
# SEGUNDA ITERACION

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
if (!is.installed("PerformanceAnalytics")){
  install.packages("PerformanceAnalytics",repos = "http://cran.us.r-project.org")
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
library(PerformanceAnalytics)
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


date$lesividad = factor(date$lesividad)
date$edad_desde = factor(date$edad_desde)
date$tipo_vehiculo = factor(date$tipo_vehiculo)
date$tipo_persona = factor(date$tipo_persona)
date$prec = factor(date$prec)
date$vvu = factor(date$vvu)

date$lesividad = as.integer(date$lesividad)
date$edad_desde = as.integer(date$edad_desde)
date$tipo_vehiculo = as.integer(date$tipo_vehiculo)
date$tipo_persona = as.integer(date$tipo_persona)
date$vvu = as.integer(date$vvu)
date$prec = as.integer(date$prec)



#para nuestro modelo eliminamos los NA
date <- date[!is.na(date$tipo_vehiculo),]
date <- date[!is.na(date$edad_desde),]
date <- date[!is.na(date$lesividad),]
date <- date[!is.na(date$tipo_persona),]
date <- date[!is.na(date$prec),]
date <- date[!is.na(date$vvu),]


modelo_multiple <- lm(formula = myFormula, data = date)
summary(modelo_multiple)

chart.Correlation(date)


corrplot.mixed(corr = cor(date[,c("tipo_persona", "tipo_vehiculo", "edad_desde", "lesividad","vvu","prec")],
                          method = "pearson"))
step(modelo_multiple, direction = "both", trace = 0)
par(mfrow = c(1,5))
plot(modelo_multiple)
modelo_multiple <- as.matrix(modelo_multiple)
cor(date)
# Ahora tranformamos las edades en 6 categorias


#mostramos un estudio de los datos
str(date)
summary(date)


# ----------------------------  Comenzamos con el MODELO. ----------------------------------------

# Anadimos semilla para la particion de los conjuntos de entreamiento y test
set.seed(144)

valores = c("edad_desde", "lesividad","tipo_vehiculo", "tipo_persona","prec","vvu")
date <- date[valores]

# Para quitar los valores nulos
naPerColumn <- colSums(is.na(date))
percentageNA <- round(naPerColumn/nrow(date)*100, digits = 2)
percentageNA <- percentageNA[percentageNA > 0]
percentageNA <- percentageNA[order(percentageNA)]

percentageNA


#date <-date[!is.na(date$TIPO_VEHICULO),]

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
# Creamos los conjuntos de entrenamiento y test
prueba <-sample(2, nrow(date), replace=TRUE, prob = c (0.7,0.3))
entreno <- date[prueba==1,]
test <- date[prueba==2,]


myFormula <- lesividad ~ tipo_vehiculo + tipo_persona + edad_desde + vvu+ prec
# Creo la formula con la que quiero comparar
myFormula <- lesividad ~ tipo_vehiculo + tipo_persona + edad_desde

#Aqui creamos el modelo del arbol de decision ajustando los parametros
control <- rpart.control(#minsplit = 1000,
                         minbucket = 500,
                        # maxdepth = 7,
                         cp = 0)

modelo <- rpart(myFormula, data = entreno, method = 'class', control = control)

# Dibujamos el modelo
rpart.plot(modelo, extra = 1)

# Vemos los atributos del modelo
attributes(modelo)

# Vemos un desglose del modelo
summary(modelo)

# Añadimos la variable calculada al test
test$LESIVIDAD_ESPERADA <-predict(modelo, newdata = test, type ="class")

#Creamos una tabla con los datos del test
predictTable <-table(test$LESIVIDAD_ESPERADA, test$lesividad)
predictTable

#calcula el % de confianza
accuracy <-sum(diag(predictTable))/sum(predictTable)
accuracy
#desconectamos de la base de datos
lapply(dbListConnections(MySQL()), dbDisconnect)

