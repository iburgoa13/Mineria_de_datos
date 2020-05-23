# Title     : TODO
# Objective : TODO
# Created by: Jorge Trabajo
# Created on: 25/04/2020

is.installed <- function(mypkg) {
  is.element(mypkg, installed.packages()[,1])
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


llamada_api_meteo <- function(api_key_meteo,idema,fecha_ini,fecha_hasta) {

  api_end_point <- paste0("https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/",fecha_ini,"/fechafin/",fecha_hasta,"/estacion/",idema)

  respuesta <- GET(api_end_point,add_headers(Accept = "application/json","api_key" = api_key_meteo))


  respuesta_json <- fromJSON( content(respuesta, "text"), flatten = TRUE)

  if (respuesta_json$estado == 200){


    respuesta_datos <- GET(respuesta_json$datos, flatten = TRUE)

    return(as.data.frame(fromJSON(content(respuesta_datos, "text"), flatten = TRUE)))

  }else{
    print(respuesta_json)
    stop("error peticion")
  }

}

getClaves <- function(ruta) {
  con <- file(ruta, "r")

  lineas <- readLines(con)

  close(con)

  return(strsplit(lineas, "\n"))
}

api_keys <- getClaves("api_keys.txt")
api_key_actual <- 0
idema <- "3195"
fecha_ini <- "2015-02-07T00:00:00UTC"
fecha_hasta <- "2015-02-07T23:59:00UTC"

api_key_usada <- 0




print(length(api_keys))

datos_meteo <- llamada_api_meteo(api_key_meteo = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJpYnVyZ29hQHVjbS5lcyIsImp0aSI6IjRmNGRhMTUwLTlkYzItNDZiOC05NmIxLTgwMTRkZmNhZTE2NiIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNTg4ODczNTcwLCJ1c2VySWQiOiI0ZjRkYTE1MC05ZGMyLTQ2YjgtOTZiMS04MDE0ZGZjYWUxNjYiLCJyb2xlIjoiIn0.J5TCxuxaWD2KomS28-_NyguKrHd8IiMHQLkOk2xblgI",idema = idema,fecha_ini = fecha_ini,fecha_hasta = fecha_hasta)

print(datos_meteo)