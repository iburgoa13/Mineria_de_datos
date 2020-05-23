# Title     : TODO
# Objective : TODO
# Created by: alvar
# Created on: 24/04/2020
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

library(xts)
library(data.table)
library(dplyr)
library(readxl)
library(RMySQL)
library(DBI)


d <- read.csv(file = "./proyecto_uaneo/limpieza_Datawarehouse/datos/datos_distritoEstacion/DistritosEstaciones_limpio.csv", header = TRUE)
dEs <-data.table::rbindlist(list(d),fill = TRUE)
distritos <-as.data.frame(dEs)

num_distritos <- length(distritos[,1])
col_distrito <- distritos$Distrito
idema <- distritos$idema
id_distrito <- distritos$id_distrito

con <- DBI::dbConnect(RMySQL::MySQL(),user="uaneo",password="uaneo",dbname="uaneo")
#INSERTAR EN LA BBDD TABLA DISTRITO
for(i in 1:num_distritos){

  insert_distrito <- "INSERT INTO distrito (id,idema,nombre) VALUES (?id,?idema,?nombre)"

  query <- sqlInterpolate(con, insert_distrito, id=id_distrito[i],idema=idema[i],nombre=col_distrito[i])
  resultat <- dbGetQuery(con,query)
}

##FUNCION PARA LA API
llamada_api_meteo <- function(api_key_meteo,idema,fecha_ini,fecha_hasta) {

  api_end_point <- paste0("https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/",fecha_ini,"/fechafin/",fecha_hasta,"/estacion/",idema)

  respuesta <- GET(api_end_point,add_headers(Accept = "application/json","api_key" = api_key_meteo))

  contenido <- content(respuesta, "text")

  if(is.na(contenido))
    return(-1)


  respuesta_json <- fromJSON(contenido , flatten = TRUE)

  if (respuesta_json$estado == 200){

    respuesta_datos <- GET(respuesta_json$datos, flatten = TRUE)

    contenido_datos <- content(respuesta_datos, "text")

    if(is.na(contenido_datos))
      return(-1)

    return(as.data.frame(fromJSON(contenido_datos, flatten = TRUE)))

  }else{
    if(respuesta_json$descripcion!="No hay datos que satisfagan esos criterios")
      print(respuesta_json$descripcion)
    return(-1)
  }

}
##FUNCION LECTURA DE CLAVES
getClaves <- function(ruta) {
  con <- file(ruta, "r")

  lineas <- readLines(con)

  close(con)

  return(strsplit(lineas, "\n"))
}

api_keys <- getClaves("api_keys.txt")
api_key_actual <- 1
api_key_pos <- 0 ## VA DE 1 A 50 CUANDO LLEGUE A 50 SE AUMENTA api_key_actual


directorio_datasets <- "./proyecto_uaneo/limpieza_Datawarehouse/datos/datos_accidentes/"

anios <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

names_col <- c("FECHA", "RANGO_HORAS", "DIA_SEMANA", "DISTRITO", "LUGAR", "NUMERO", "NUMERO_EXPEDIENTE", "GRANIZO",
               "HIELO", "LLUVIA", "NIEBLA", "SECO", "NIEVE", "C_MOJADA", "C_ACEITE", "C_BARRO", "C_GRAVA_SUELTA", "C_HIELO",
               "C_SECA_LIMPIA", "NUM_VITIMAS", "TIPO_ACCIDENTE", "TIPO_VEHICULO", "TIPO_PERSONA", "SEXO", "LESIVIDAD",
               "RANGO_EDAD")


con <- DBI::dbConnect(RMySQL::MySQL(),user="uaneo",password="uaneo",dbname="uaneo")


for(n in anios){
  dir <- paste(directorio_datasets, "_Accidentalidad.csv", sep = n)

  #Leemos datos de accidentes.
  accidentes <- read.csv(dir, header=TRUE, sep = ";", quote = "\"’", col.names = names_col,
                         na.strings = "",  encoding = "UTF-8")

  num_parte_anterior <- ""

  for (i in 1:nrow(accidentes)) {
    # Aprovecho el recorrido para asignar fecha.
    fecha_split <- strsplit(as.character(accidentes[i,"FECHA"]), "/")
    fecha_dia <- as.numeric(fecha_split[[1]][1])
    fecha_mes <- as.numeric(fecha_split[[1]][2])
    fecha_anio <- as.numeric(fecha_split[[1]][3])

    # Aprovecho recorrido para poner horas estandarizadas (como tipo numerico)

    hora_split_rango <- strsplit(as.character(accidentes[i,"RANGO_HORAS"]), " ")
    hora_split_desde <- strsplit(hora_split_rango[[1]][2], ":")
    hora_split_hasta <- strsplit(hora_split_rango[[1]][4], ":")
    fecha_desde <- as.numeric(hora_split_desde[[1]][1])
    fecha_hasta <- as.numeric(hora_split_hasta[[1]][1]) + 1

    if(fecha_hasta>23){
      fecha_hasta <- 0;
    }

    # Unificamos el formato de nombres de distrito (mayuscula y sin tildes) y
    a <- as.character(accidentes[i,"DISTRITO"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    distrito <- newdata

    a <- as.character(accidentes[i,"LUGAR"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    lugar_accidente <- newdata


    a <- as.character(accidentes[i,"DIA_SEMANA"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    dia_semana_ <- newdata


    # Unificamos el formato de nombres de tipo_accidente (mayuscula y sin tildes)
    # Ademas ponemos a NA los individuos que hayan tenido un accidente por otras
    # causas ya que como pone en la documentacion no esta asignado.
    a <- as.character(accidentes[i,"TIPO_ACCIDENTE"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    if(newdata == "OTRAS CAUSAS"){
      tipo_accidente <- NA
    }
    else { tipo_accidente <- newdata }

    # Hacemos lo mismo con el tipo de vehiculo
    a <- as.character(accidentes[i,"TIPO_VEHICULO"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    if(newdata == "NO ASIGNADO"){
      tipo_vehiculo <- NA
    }
    else { tipo_vehiculo <- newdata }

    a <- as.character(accidentes[i,"TIPO_PERSONA"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    tipo_persona <- newdata

    #Hacemos lo mismo con el sexo
    a <- as.character(accidentes[i,"SEXO"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    if(newdata == "NO ASIGNADO") {
      tipo_sexo <- NA
    }
    else{
      tipo_sexo <- newdata
    }

    #Hacemos lo mismo con la lesividad
    a <- as.character(accidentes[i,"LESIVIDAD"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    if(newdata == "NO ASIGNADA") {
      lesividad <- NA
    }
    else{
      lesividad <- newdata
    }

    #Modifico el rango de edad
    str <- strsplit(as.character(accidentes[i,"RANGO_EDAD"]), " ")
    if(str[[1]][1] == "DESCONOCIDA"){
      accidentes[i,"RANGO_EDAD"] <- NA
      edad_desde <- NA
      edad_hasta <- NA
    }
    else if(str[[1]][1] == "DE" && str[[1]][2] != "MAS"){
      edad_desde <- as.numeric(str[[1]][2])
      edad_hasta <- as.numeric(str[[1]][4])
    }
    else {#sino es que es mayor de X años y simplemente cojo el valor
      edad_desde <- as.numeric(str[[1]][4])
      edad_hasta <- 100
    }

    insert_victima <- "INSERT INTO victima (sexo,edad_desde,edad_hasta,lesividad,tipo_persona) VALUES (?sexo,?edad_desde,?edad_hasta,?lesividad,?tipo_persona)"

    query <- sqlInterpolate(con, insert_victima, sexo=tipo_sexo, edad_desde=edad_desde, edad_hasta=edad_hasta, lesividad=lesividad, tipo_persona=tipo_persona)

    #print(req)
    res <- dbGetQuery(con,query)
    res <- dbSendQuery(con, "SELECT LAST_INSERT_ID() AS id;")
    id_victima <- fetch(res)$id
    dbClearResult(res)

    insert_dim_tiempo <- "INSERT INTO dim_tiempo (anno,dia,dia_semana,hora_desde,hora_hasta,mes) VALUE (?anno,?dia,?dia_semana,?hora_desde,?hora_hasta,?mes)"
    query <- sqlInterpolate(con, insert_dim_tiempo, anno =fecha_anio, dia=fecha_dia, dia_semana= dia_semana_, hora_desde = fecha_desde, hora_hasta= fecha_hasta, mes = fecha_mes)
    res <- dbGetQuery(con,query)
    res <- dbSendQuery(con, "SELECT LAST_INSERT_ID() AS id;")
    id_dim_tiempo <- fetch(res)$id
    dbClearResult(res)

    idema <- distritos[distritos$Distrito == distrito,"idema"]
    id_distrito <- distritos[distritos$Distrito == distrito,"id_distrito"]


    a <- as.character(accidentes[i,"NUMERO_EXPEDIENTE"])
    newdata <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(a))
    newdata <- sub(pattern = " +$", replacement = "", x = newdata)
    num_parte <- newdata

    if(num_parte != num_parte_anterior){
      api_key <- api_keys[[api_key_actual]]
      api_key_pos <- api_key_pos+1

      id_meteo <- NA

      if(api_key_pos>=19){
        api_key_pos <- 0

        api_key_actual <- api_key_actual+1

        if(api_key_actual>length(api_keys)){
          api_key_actual <- 1
        }
        print("Cambio de API")
        print(api_key_actual)
        Sys.sleep(5)
      }

      fecha_ini_api <- sprintf("%4d-%02d-%02dT%02d:00:00UTC", fecha_anio,fecha_mes,fecha_dia,fecha_desde)
      fecha_hasta_api <- sprintf("%4d-%02d-%02dT%02d:00:00UTC", fecha_anio,fecha_mes,fecha_dia,fecha_hasta)

        salida <- tryCatch({
             datos_meteo <- llamada_api_meteo(api_key_meteo =api_key, idema =idema, fecha_ini =fecha_ini_api, fecha_hasta =fecha_hasta_api)

              if(datos_meteo==-1){
                fecha_ini_api <- sprintf("%4d-%02d-%02dT00:00:00UTC", fecha_anio,fecha_mes,fecha_dia)
                fecha_hasta_api <- sprintf("%4d-%02d-%02dT23:59:00UTC", fecha_anio,fecha_mes,fecha_dia)
                datos_meteo <- llamada_api_meteo(api_key_meteo =api_key, idema =idema, fecha_ini =fecha_ini_api, fecha_hasta =fecha_hasta_api)
              }

              id_meteo <- NA

              if(datos_meteo!=-1){

                prec <- as.numeric(sub(",", ".", datos_meteo$prec, fixed = TRUE))
                if(length(prec)==0)
                  prec <- 0

                vvu <- as.numeric(sub(",", ".", datos_meteo$velmedia, fixed = TRUE))
                if(length(vvu)==0)
                  vvu <- 0

                hr <- as.numeric(sub(",", ".", datos_meteo$horaracha, fixed = TRUE))
                if(length(hr)==0)
                  hr <- 0

                ta <- as.numeric(sub(",", ".", datos_meteo$tmed, fixed = TRUE))
                if(length(ta)==0)
                  ta <- 0

                insert_dim_meteo <- "INSERT INTO meteorologia (idema,prec,vvu,hr,ta) VALUE (?idema,?prec,?vvu,?hr,?ta)"
                query <- sqlInterpolate(con, insert_dim_meteo, idema =idema, prec=prec, vvu = vvu, hr= hr, ta = ta)
                res <- dbGetQuery(con,query)
                res <- dbSendQuery(con, "SELECT LAST_INSERT_ID() AS id;")
                id_meteo <- fetch(res)$id
              }

           },
           error = function(error_condition) {
              print(error_condition)
              return(-1)
          })

    }



    #INSERT PARTE

    #print(num_parte)
    #print(id_dim_tiempo)
    #print(id_distrito)
    #print(id_meteo)
    #print(id_victima)
    #print(tipo_accidente)
    #print(tipo_vehiculo)
    #print(lugar_accidente)

    insert_parte <- "INSERT INTO parte (num_expediente,id_tiempo,id_distrito,id_meteo,id_victima,tipo_accidente,tipo_vehiculo,lugar) VALUE (?num_expediente,?id_tiempo,?id_distrito,?id_meteo,?id_victima,?tipo_accidente,?tipo_vehiculo,?lugar)"
    query <- sqlInterpolate(con, insert_parte, num_expediente =num_parte, id_tiempo =id_dim_tiempo, id_distrito =id_distrito, id_meteo =id_meteo, id_victima =id_victima, tipo_accidente =tipo_accidente, tipo_vehiculo =tipo_vehiculo, lugar =lugar_accidente)
    res <- dbGetQuery(con,query)

    num_parte_anterior <- num_parte


  }


}

lapply(dbListConnections(MySQL()), dbDisconnect)
