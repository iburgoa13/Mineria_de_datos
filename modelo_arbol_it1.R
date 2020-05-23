# Creacion modelo - ARBOL DE DECISION
# PRIMERA ITERACION

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





#mostramos un estudio de los datos
str(date)
summary(date)

# ----------------------------  Comenzamos con el MODELO. ----------------------------------------

# Anadimos semilla para la particion de los conjuntos de entreamiento y test
set.seed(100)

valores = c("tipo_vehiculo", "edad_desde", "lesividad")
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

#En la primera iteracion decidimos borrar los na de EDAD_DESDE por que podrian afectar al modelo.
#En posteriores iteraciones veremos si ha sido buena idea o no.
#date <-date[!is.na(date$edad_desde),]

# Creamos los conjuntos de entrenamiento y test
prueba <-sample(2, nrow(date), replace=TRUE, prob = c (0.5,0.5))
entreno <- date[prueba==1,]
test <- date[prueba==2,]

# Creamos la formula con la que quiero comparar
myFormula <- lesividad ~ tipo_vehiculo + edad_desde

# Aqui creamos el modelo del arbol de decision ajustando los parametros
control <- rpart.control(minsplit = 100,
                         minbucket = 2,
                         maxdepth = 4,
                         cp = 0)
modelo <- rpart(myFormula, data = entreno, method = 'class', control = control)

# Dibujamos el modelo
rpart.plot(modelo, extra = 1)

# Vemos los atributos del modelo
#attributes(modelo)

# Vemos un desglose del modelo
#summary(modelo)

# Añadimos la variable calculada al test
test$lesividad_esperada <-predict(modelo, newdata = test, type ="class")

#Creamos una tabla con los datos del test
predictTable <-table(test$lesividad_esperada, test$lesividad)
predictTable

#calcula el % de confianza
accuracy <-sum(diag(predictTable))/sum(predictTable)
accuracy

lapply(dbListConnections(MySQL()), dbDisconnect)
