#UrbANA 0.1.1-Tester

#Cargar librerias:
library(httr)
source("./Libraries/Rtrava.R")
source("./Libraries/stravaUrbANA.R")
source("./Libraries/tidyDataEfforts.R")

app_name <- "" #Colocar entre las comillas el nombre de la aplicacion que registraron en Strava
app_client_id <- "" #Colocar entre las comillas el ID de cliente de la aplicacion
app_secret <- "" #Colocar entre las comillas el Secreto de la aplicacion
app_scope <- "view_private"

strava_token <- strava_oauth(app_name, app_client_id, app_secret, app_scope)

stoken <- config(token = strava_token)
rm(strava_token)

usage_left <- as.integer(c(600, 30000))