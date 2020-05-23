# Title     : TODO
# Objective : TODO
# Created by: iker_
# Created on: 23/04/2020

rm(list=ls())

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
library(rpart)
library(rpart.plot)
library(data.table)
library(party)
library(rpart)
library("caret")
library(randomForest)
library(rpart.plot)

library(RColorBrewer)
#nombre de las columnas
names_col = c("ANIO","C_ACEITE","C_BARRO","C_GRAVA_SUELTA","C_HIELO","C_MOJADA","C_SECA_LIMPIA","DIA","DIA_SEMANA","DISTRITO","EDAD_DESDE","EDAD_HASTA","FECHA","GRANIZO","HIELO","HORA","LESIVIDAD","LLUVIA","LUGAR","MES","NIEBLA","NIEVE","NUM_VITIMAS","NUMERO_EXPEDIENTE","RANGO_EDAD",
              "RANGO_HORAS","SECO","SEXO","TIPO_ACCIDENTE","TIPO_PERSONA","TIPO_VEHICULO")
#leemos los datos

accidentes2010_2018 <- read.csv("2010_2018_Accidentalidad_limpio.csv",
                           header =TRUE, sep = ",", quote = "\"’", col.names = names_col, na.strings = "NA",  encoding = "UTF-8")

dias <-accidentes2010_2018
#date <- date %>%
 # mutate(VEHICULOS = case_when (

 #  VEHICULOS =="AMBULANCIA" |   VEHICULOS =="AUTO-TAXI" |  VEHICULOS =="AUTOBUS-AUTOCAR" |  VEHICULOS =="CAMION"|  VEHICULOS =="FURGONETA" |  VEHICULOS =="TURISMO" ~"4 RUEDAS",
  #  VEHICULOS =="BICICLETA" | VEHICULOS =="CICLOMOTOR"|  VEHICULOS =="MOTOCICLETA" |  VEHICULOS =="VEH.3 RUEDAS"             ~"2-3 RUEDAS",
   # TRUE                    ~"NA",
  #))
date <- dias[c(1,8,9,10,11,13,17,29,30,31)]
#tema de NA
naPerColumn <- colSums(is.na(date))
percentageNA <- round(naPerColumn/nrow(date)*100, digits = 2)
percentageNA <- percentageNA[percentageNA > 0]
percentageNA <- percentageNA[order(percentageNA)]
#percentageNA <- percentageNA%>%group_by(distrito)
percentageNA

#barplot(percentageNA,main ="Porcentaje de NA", xlab ="Accidentes", ylab ="%", ylim = c(0,100), las = 1, cex.names = 1, col = gray.colors(15))
#omitimos los NA
#selecciono unas columnas concretas

#elimino los NA que haya en TIPO_VEHICULO

date <-date[!is.na(date$TIPO_VEHICULO),]





#date<-table(dias$SEXO , dias$ANIO,dias$LESIVIDAD, dias$EDAD_DESDE)
#date <-as.data.frame(date)
#names(date)<-c("SEXO","ANIO","LESION","DESDE","FREQ")
str(date)

#aqui hago todo para transformar edad_desde a palabra
#date$EDAD_DESDE <-as.factor(date$EDAD_DESDE)
date$EDAD_DESDE <-as.integer(date$EDAD_DESDE)


date <- date %>%
  mutate(EDAD_DESDE = case_when (

    EDAD_DESDE >=0 & EDAD_DESDE <= 15   ~ 1,
    EDAD_DESDE >15 & EDAD_DESDE <= 33    ~2,
    EDAD_DESDE >33 & EDAD_DESDE <=65     ~3,
    EDAD_DESDE >65                       ~4,
  ))
date$EDAD_DESDE <-as.character(date$EDAD_DESDE)
date <- date %>%
  mutate(EDAD_DESDE = case_when (

    EDAD_DESDE =="1"  ~"JOVEN",
    EDAD_DESDE =="2"  ~"ADULTO TEMPRANO",
    EDAD_DESDE =="3"  ~"ADULTO MEDIO",
    EDAD_DESDE =="4"  ~"ANCIANO"
  ))

#mostrar datos
str(date)
summary(date)
head(date)


#empezamos
#añadimos semilla
set.seed(144)
#me creo dos porcentajes
prueba <-sample(2,nrow(date),replace=TRUE,prob = c (0.7,0.3))
entreno <-date[prueba==1,]
test <-date[prueba==2,]
#me creo la formula con la que quiero comparar, puse estas
myFormula <- LESIVIDAD ~TIPO_PERSONA + TIPO_VEHICULO + EDAD_DESDE #EDAD_DESDE ~ LESIVIDAD + TIPO_ACCIDENTE + TIPO_VEHICULO + DIA_SEMANA + TIPO_PERSONA
#hago modelo,
control <- rpart.control(minsplit = 10,
                         minbucket = 1,
                         maxdepth = 6,
                         cp = 0)
modelo <- rpart(myFormula,data = entreno,control = control)# rpart.control(minsplit = 10))
#ver atributos
attributes(modelo)
#aqui ves todo desglosado
summary(modelo)
#aqui muestras el arbol
rpart.plot(modelo, box.palette=list("Reds", "Oranges", "Grays", "Blues", "Greens"))
#añado la variable calculada al test
test$LESIVIDAD_ESPERADA <-predict(modelo,newdata = test, type ="class")
#me creo una tabla con los datos del test
predictTable <-table(test$LESIVIDAD_ESPERADA,test$LESIVIDAD)
predictTable
#calcula el % de confianza
accuracy <-sum(diag(predictTable))/sum(predictTable)
accuracy




#########################################hasta aqui################################3
plot(modelo)
text(modelo)

modelo_entrenamiento <-rpart(EDAD_DESDE~. , data = entreno)
modelo_entrenamiento
plot(modelo_entrenamiento)
text(modelo_entrenamiento)


prediccion_arbol <- predict(modelo_entrenamiento,prueba[,-4],type="class")


MC<-table(prueba$DESDE,prediccion_arbol)
MC
acierto <-sum(diag(MC))/sum(MC)
acierto
error <-1-acierto;
error





########################## de aqui para abajo olvidarse, lo que he sacado nuevo es lo de arriba ejecutarlo todo junto
names(modelo_prueba) <- c("SEXO","DIAS","MES","ANIO")
prediccion_ <-predict(modelo_prueba,prueba[-14],type ="class")




modelo_prueba
text(modelo_prueba)
date<-cbind(date,dias)
names(date) = c("SEXO","DIA","MES","ANIO","HOMBRES","SEXO")
date$Var1
dateProp <-prop.table(table(dias$SEXO))
dias$FECHA <- as.Date(dias$FECHA,format ="%d/%m/%Y")
length(date)
dias$FECHA  <-as.character(accidentes2010_2018$FECHA)
ppp <- data.frame("ANIO" = table(dias$ANIO),
"DIA"=table(dias$DIA))
#accidentes2010_2018 <-as.data.frame(accidentes2010_2018)
#Eliminamos filas
accidentes2010_2018$C_ACEITE = NULL
accidentes2010_2018$C_BARRO = NULL
accidentes2010_2018$C_GRAVA_SUELTA = NULL
accidentes2010_2018$C_HIELO = NULL
accidentes2010_2018$C_MOJADA = NULL
accidentes2010_2018$C_SECA_LIMPIA = NULL
accidentes2010_2018$GRANIZO = NULL
accidentes2010_2018$HIELO = NULL
accidentes2010_2018$LLUVIA = NULL
accidentes2010_2018$SECO = NULL
accidentes2010_2018$NIEBLA = NULL
accidentes2010_2018$NIEVE = NULL


accidentes2010_2018$LUGAR = NULL
accidentes2010_2018$NUMERO_EXPEDIENTE = NULL
accidentes2010_2018$RANGO_EDAD = NULL
accidentes2010_2018$RANGO_HORAS = NULL
accidentes2010_2018$DIA_SEMANA = NULL
pairs(accidentes2010_2018)
x <- cbind(accidentes2010_2018)
x <- accidentes2010_2018[c(1,2,4,5)]
ind <-sample(2,nrow(x),replace = TRUE, prob = c(0.7,0.3))
trainData <- x[ind == 1 ,]
testData <- x[ind == 2,]
x<-as.data.frame(x)
prueba <- rpart(formula = ANIO ~ . , data = trainData)
p <- predict(prueba,newdata = trainData,type = "classs")
prueba
print(prueba)
rpart.plot(prueba, extra = 100, box.palette = "green")
naPerColumn <- colSums(is.na(accidentes2010_2018))
summary(accidentes2010_2018)
set.seed(123)
prueba <- sample(2,nrow(accidentes2010_2018),replace = TRUE,prob = c(0.7,0.3))
entreno <- accidentes2010_2018[prueba==1,]
prueba <-accidentes2010_2018[prueba==2,]
modelo_arbol <-rpart(LESIVIDAD ~.,data = prueba)
percentageNA <- round(naPerColumn/nrow(accidentes2010_2018)*100, digits = 2)
percentageNA <- percentageNA[percentageNA > 0]
percentageNA <- percentageNA[order(percentageNA)]
#percentageNA <- percentageNA%>%group_by(distrito)
percentageNA
require(RQuantLib)
barplot(percentageNA,main ="Porcentaje de NA", xlab ="Accidentes", ylab ="%", ylim = c(0,100), las = 1, cex.names = 1, col = gray.colors(15))
#omitimos los NA
accidentes2010_2018$SEXO <-as.character(accidentes2010_2018$SEXO)
accidentes2010_2018 <-accidentes2010_2018[order(accidentes2010_2018$FECHA),]
mean(accidentes2010_2018$EDAD_DESDE)
prueba <- accidentes2010_2018$FECHA
prueba1 <-accidentes2010_2018$DISTRITO
EDAD_DESDE <-accidentes2010_2018$EDAD_DESDE
EDAD_HASTA <-accidentes2010_2018$EDAD_HASTA
juntat <- (c(prueba,prueba1,EDAD_HASTA,EDAD_DESDE))
accidentes2010_2018$FECHA <- as.Date(accidentes2010_2018$FECHA,format ="%d/%m/%Y")
#accidentes2010_2018$FECHA <-as.Date(accidentes2010_2018$FECHA,format ="%Y-%m-%d")
select(accidentes2010_2018,starts_with("FE"))
p <-accidentes2010_2018 %>% group_by(fecha, col_distrito) %>%
  summarise(EDAD_DESDE = mean(EDAD_DESDE))
sexo <- accidentes2010_2018$SEXO
sexo <- select(accidentes2010_2018,SEXO)
accidentes2010_2018$MES <-as.integer(accidentes2010_2018$MES)

accidentes2010_2018$RANGO_EDAD <-as.integer(accidentes2010_2018$RANGO_EDAD)
filt
filter(accidentes2010_2018,accidentes2010_2018$EDAD_DESDE>=25)
  length(accidentes2010_2018$SEXO[accidentes2010_2018$SEXO =="HOMBRE"])
length(accidentes2010_2018$SEXO[accidentes2010_2018$SEXO =="MUJER"])
length(sexo$MUJER)
mujeres <- filter(accidentes2010_2018, sexo =="MUJER")
accidentes2010_2018 <- na.omit(accidentes2010_2018)
accidentes2010_2018 <- as.data.frame(accidentes2010_2018)
select(accidentes2010_2018,contains("DESDE"))
#Modificamos valores
accidentes2010_2018 <- accidentes2010_2018 %>%
  mutate(SEXO = case_when (

    SEXO == "HOMBRE"       ~"H",
    SEXO == "MUJER"               ~"M",
    TRUE                    ~"NA",
  ))
accidentes2010_2018 <-accidentes2010_2018 %>% filter(TIPO_PERSONA != "TESTIGO")
#accidentes2010_2018 <- accidentes2010_2018 %>%
#  mutate(TIPO_PERSONA = case_when (

#    TIPO_PERSONA == "CONDUCTOR"                                       ~"C",
#    TIPO_PERSONA == "VIAJERO"                                         ~"V",
#    TIPO_PERSONA == "TESTIGO                                 "         ~"P",
#    TIPO_PERSONA == "PEATON                                  "        ~"P",
#    TRUE                                                              ~"NA",
#  ))
#HG HL IL MT
accidentes2010_2018$LESIVIDAD <-as.character(accidentes2010_2018$LESIVIDAD)

accidentes2010_2018$LESIVIDAD <- replace(accidentes2010_2018$LESIVIDAD,accidentes2010_2018$LESIVIDAD=="HL","HERIDO LEVE")
accidentes2010_2018$LESIVIDAD <- replace(accidentes2010_2018$LESIVIDAD,accidentes2010_2018$LESIVIDAD=="MT","MUERTO")
accidentes2010_2018$LESIVIDAD <- replace(accidentes2010_2018$LESIVIDAD,accidentes2010_2018$LESIVIDAD=="HG","HERIDO GRAVE")
accidentes2010_2018$LESIVIDAD <- replace(accidentes2010_2018$LESIVIDAD,accidentes2010_2018$LESIVIDAD=="IL","ILESO")


#limpiaDatosNULL <-function(x){
 # for (i in 1: nrow(accidentes2010_2018)){
  #  if(is.na(accidentes2010_2018[i,"sexo"]))
  #   filte
  #}
#}
