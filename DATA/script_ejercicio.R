rm(list=ls())


#librerias 
library(ggplot2)
library(here)
library(RMAWGEN)
library(stringr)


dir.create(file.path(getwd(), "Graficas"), showWarnings = FALSE)

#Script Ejercicio practico 


#Ejercicio 1
#Lectura de los siguientes datos:

datos_TM <- read.table(paste0(here("DATA"),"/ChiapasAgroipsa_TM_CD.txt"), header = T)
datos_TX <- read.table(paste0(here("DATA"),"/ChiapasAgroipsa_TX_CD.txt"), header = T)
datos_SR <- read.table(paste0(here("DATA"),"/ChiapasAgroipsa_SR_WAM2.txt"), header = T)
datos_P <- read.table(paste0(here("DATA"),"/ChiapasAgroipsa_P_MM.txt"), header = T)
datos_RH <- read.table(paste0(here("DATA"),"/ChiapasAgroipsa_RH_NE.txt"), header = T)

#1. Formato a los archivos 

datos_TM$Date <- as.Date(as.character(datos_TM$Date), format= "%Y-%m-%d")
datos_TX$Date <- as.Date(as.character(datos_TX$Date), format= "%Y-%m-%d")
datos_SR$Date <- as.Date(as.character(datos_SR$Date), format= "%Y-%m-%d")
datos_P$Date <- as.Date(as.character(datos_P$Date), format= "%Y-%m-%d")
datos_RH$Date <- as.Date(as.character(datos_RH$Date), format= "%Y-%m-%d")

#2. Valores superiores e inferiores para cada variable.

#Temperatura Maxima
#Supongamos que la temperatura máxima está entre los 25 y 40 grados

index_TX <- which(datos_TX$Value < 25 | datos_TX$Value > 40)
datos_TX$Datos <- "Estandar"
datos_TX$Datos[index_TX] <- "Fuera_Limites"
ggplot(datos_TX, aes(x=Date, y=Value, color = Datos)) + geom_point() + labs(title="Datos de temperatura máxima durante los años 2013 a 2018", subtitle = "Chiapas estación Agroipsa")
ggsave("./Graficas/Temperatura_Maxima.pdf")
datos_TX$Value[index_TX] <- NA


#Temperatura Minima 
#Supongamos que la temperaturas mínima está entre los 25 y 40 grados

index_TM <- which(datos_TM$Value < 17 | datos_TM$Value > 24)
datos_TM$Datos <- "Estandar"
datos_TM$Datos[index_TM] <- "Fuera_Limites"
ggplot(datos_TM, aes(x=Date, y=Value, color = Datos)) + geom_point() + labs(title="Datos de temperatura mínima durante los años 2013 a 2018", subtitle = "Chiapas estación Agroipsa")
ggsave("./Graficas/Temperatura_Minima.pdf")
datos_TM$Value[index_TM] <- NA



#Radiacion solar 
#Radiacion solar mayoes a 1080 WAM2

index_SR <- which(datos_SR$Value > 650 | datos_SR$Value < 200)
datos_SR$Datos <- "Estandar"
datos_SR$Datos[index_SR] <- "Fuera_Limites"
ggplot(datos_SR, aes(x=Date, y=Value, color = Datos)) + geom_point() + labs(title="Datos de radiación solar durante los años 2013 a 2018", subtitle = "Chiapas estación Agroipsa")
ggsave("./Graficas/Radiacion_solar.pdf")
datos_SR$Value[index_SR] <- NA



#Precipitacion
#Supongamos que esa región que sea mayor a 150
index_P <- which(datos_P$Value > 150 | datos_P$Value < 0)
datos_P$Datos <- "Estandar"
datos_P$Datos[index_P] <- "Fuera_Limites"
ggplot(datos_P, aes(x=Date, y=Value, color = Datos)) + geom_point() + labs(title="Datos de radiación solar durante los años 2013 a 2018", subtitle = "Chiapas estación Agroipsa")
ggsave("./Graficas/Precipitacion.pdf")
datos_P$Value[index_P] <- NA


#RMWAGEN


#temperatura m\'axima

fechas_TX <- as.data.frame(str_split_fixed(datos_TX$Date, "-", 3))
colnames(fechas_TX) <- c("year", "month", "day")
datos_TX$year <- as.numeric(as.character(fechas_TX$year))
datos_TX$month <- as.numeric(fechas_TX$month)
datos_TX$day <- as.numeric(fechas_TX$day)
names(datos_TX)[which((names(datos_TX))=="Value")] <- "Agroipsa"
datos_TX$Agroipsa_aux <- datos_TX$Agroipsa
datos_TX$Date <- NULL
datos_TX$Datos <- NULL
datos_TX$Agroipsa_aux <- as.numeric(datos_TX$Agroipsa_aux)
datos_TX$Agroipsa <- as.numeric(datos_TX$Agroipsa)



#temperatura minima
fechas_TM <- as.data.frame(str_split_fixed(datos_TM$Date, "-", 3))
colnames(fechas_TM) <- c("year", "month", "day")
datos_TM$year <- as.numeric(as.character(fechas_TM$year))
datos_TM$month <- as.numeric(fechas_TM$month)
datos_TM$day <- as.numeric(fechas_TM$day)
names(datos_TM)[which((names(datos_TM))=="Value")] <- "Agroipsa"
datos_TM$Agroipsa_aux <- datos_TM$Agroipsa
datos_TM$Date <- NULL
datos_TM$Datos <- NULL
datos_TM$Agroipsa_aux <- as.numeric(datos_TM$Agroipsa_aux)
datos_TM$Agroipsa <- as.numeric(datos_TM$Agroipsa)


 



#Parametros de RMWAGEN

year_max <- 2016
year_min <- 2015
origin <- "2015-01-1"
PREC_CLIMATE <- NULL
n_GPCA_iter <- 5
n_GPCA_iteration_residuals <- 5
p_test <- 1
p_prec <- 3
p_temp <- 10
exogen <- NULL
exogen_sim <- exogen
station <- c("Agroipsa_aux", "Agroipsa")
#station <- c("Agroipsa")
TN_CLIMATE <- NULL
TX_CLIMATE <- NULL
PREC_CLIMATE <- NULL





generacion_Datos_Temperatura <- ComprehensiveTemperatureGenerator( station = station,
                                                                   Tx_all = datos_TX,
                                                                   Tn_all = datos_TM,
                                                                   year_min = year_min,
                                                                   year_max = year_max,
                                                                   p= p_temp,
                                                                   n_GPCA_iteration = n_GPCA_iter,
                                                                   n_GPCA_iteration_residuals = n_GPCA_iteration_residuals,
                                                                   exogen= exogen,
                                                                   exogen_sim = exogen_sim,
                                                                   sample= "monthly",
                                                                   mean_climate_Tn=TN_CLIMATE,
                                                                   mean_climate_Tx=TX_CLIMATE)
#Extraer los valores del llenado

#Tomar los datos de los anos 2015 y 2016
datos_TX_2015_2016 <- subset(datos_TX, year >= 2015 & year <= 2016)
datos_TM_2015_2016 <- subset(datos_TM, year >= 2015 & year <= 2016)
datos_TM_2015_2016$Datos <- "Reales"
datos_TX_2015_2016$Datos <- "Reales"

#Indices de los faltantes
indices_TX <- which(as.vector(is.na(datos_TX_2015_2016$Agroipsa))== TRUE)
indices_TM <- which(as.vector(is.na(datos_TM_2015_2016$Agroipsa))== TRUE)

#Generado
datos_llenado_TX <- generacion_Datos_Temperatura$output$Tx_gen
datos_llenado_TM <- generacion_Datos_Temperatura$output$Tn_gen

#Escoger los datos que estan llenados

datos_TX_2015_2016$Agroipsa[indices_TX] <- datos_llenado_TX$Agroipsa_aux[indices_TX]
datos_TM_2015_2016$Agroipsa[indices_TM] <- datos_llenado_TM$Agroipsa_aux[indices_TM]

datos_TX_2015_2016$Datos[indices_TX] <- "Faltantes"
datos_TM_2015_2016$Datos[indices_TM] <- "Faltantes"

#Graficar

datos_TM_2015_2016$Date <- paste0(datos_TX_2015_2016$year, "-", datos_TX_2015_2016$month, "-", datos_TX_2015_2016$day)
datos_TM_2015_2016$Date <- as.Date(as.character(datos_TM_2015_2016$Date), format = "%Y-%m-%d" )

ggplot(datos_TM_2015_2016, aes(y=Agroipsa, x=Date, color = Datos)) + geom_point() + labs(title="Llenado de datos temperatura minima años 2015 a 2016", subtitle = "Chiapas estación Agroipsa")
ggsave("./Graficas/Tempera_Minima_llenado.pdf")


ggplot(datos_TM_2015_2016, aes(y=Agroipsa, x=Date, color = Datos)) + geom_point() + labs(title="Llenado de datos temperatura maxima años 2015 a 2016", subtitle = "Chiapas estación Agroipsa")
ggsave("./Graficas/Tempera_Maxima_llenado.pdf")



#Precipitacion 

fechas_P <- as.data.frame(str_split_fixed(datos_P$Date, "-", 3))
colnames(fechas_P) <- c("year", "month", "day")
datos_P$year <- as.numeric(as.character(fechas_P$year))
datos_P$month <- as.numeric(fechas_P$month)
datos_P$day <- as.numeric(fechas_P$day)
names(datos_P)[which((names(datos_P))=="Value")] <- "Agroipsa"
datos_P$Agroipsa_aux <- datos_P$Agroipsa
datos_P$Date <- NULL
datos_P$Datos <- NULL
datos_P$Agroipsa_aux <- as.numeric(datos_P$Agroipsa_aux)
datos_P$Agroipsa <- as.numeric(datos_P$Agroipsa)



generacion_Datos_Precipita <- ComprehensivePrecipitationGenerator(station=station,
                                                          prec_all=datos_P,
                                                          year_min=year_min,
                                                          year_max=year_max,
                                                          p=p_prec,
                                                          n_GPCA_iteration=n_GPCA_iter,
                                                          n_GPCA_iteration_residuals= n_GPCA_iteration_residuals,
                                                          exogen=exogen,
                                                          exogen_sim=exogen_sim,
                                                          sample="monthly",
                                                          mean_climate_prec=PREC_CLIMATE,
                                                          no_spline=FALSE)

#Extraer los valores del llenado

#Tomar los datos de los anos 2015 y 2016
datos_P_2015_2016 <- subset(datos_P, year >= 2015 & year <= 2016)
datos_P_2015_2016$Datos <- "Reales"

#Indices de los faltantes
indices_P <- which(as.vector(is.na(datos_P_2015_2016$Agroipsa))== TRUE)


#Generado
datos_llenado_P <- generacion_Datos_Precipita$prec_gen

#Escoger los datos que estan llenados
datos_P_2015_2016$Agroipsa[indices_P] <- datos_llenado_P$Agroipsa_aux[indices_P]
datos_P_2015_2016$Datos[indices_P] <- "Faltantes"


#Graficar
datos_P_2015_2016$Date <- paste0(datos_P_2015_2016$year, "-", datos_P_2015_2016$month, "-", datos_P_2015_2016$day)
datos_P_2015_2016$Date <- as.Date(as.character(datos_P_2015_2016$Date), format = "%Y-%m-%d" )


ggplot(datos_P_2015_2016, aes(y=Agroipsa, x=Date, color = Datos)) + geom_point() + labs(title="Llenado de datos precipitacion años 2015 a 2016", subtitle = "Chiapas estación Agroipsa")
ggsave("./Graficas/Precipit_llenado.pdf")





