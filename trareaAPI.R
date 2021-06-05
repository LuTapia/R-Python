library(httr)
library(jsonlite)
library(reshape2)
library(readxl)
library(ggplot2)
library(dplyr)

rm(list = ls())
##4.7

url2 <- 'http://hdr.undp.org/sites/default/files/2020_statistical_annex_table_1.xlsx'
GET(url2, write_disk(tf2 <- tempfile(fileext = ".xlsx")))
HDR2019 <- read_excel(tf2,
                      # Worksheet to import
                      sheet = "Table 1",
                      # Number of rows to skip
                      skip = 2)
head(HDR2019)
str(HDR2019)

# Renombra columnas de posición 1 y 2
names(HDR2019)[1] <- "HDI.rank"
names(HDR2019)[2] <- "Country"
# Renombra la última columna, que contiene el ranking del 2014
names(HDR2019)[names(HDR2019) == "...15"] <- 
  "HDI.rank.2019"
# Elimina filas bajo la condición que debe ser distinto de NA y "HDI rank".
HDR2019 <- subset(HDR2019,
                  !is.na(HDI.rank) & HDI.rank != "HDI rank")

# Check which variables do NOT (!) start with X_
#sel_columns <- !startsWith(names(HDR2019), "X_")
# Select the columns that do not start with X_
#HDR2019 <- subset(HDR2019, select = sel_columns)

HDR2019 <- HDR2019[,-c(4,14)]
str(HDR2019)

#Se cambia nombre de columnas
names(HDR2019)[3] <- "HDI"
names(HDR2019)[4] <- "LifeExp"
names(HDR2019)[6] <- "ExpSchool"
names(HDR2019)[8] <- "MeanSchool"
names(HDR2019)[10] <- "GNI.capita"
names(HDR2019)[12] <- "GNI.HDI.rank"

#Se cambia la estrctura de los datos (anteriormente 'chr')
HDR2019$HDI.rank <- as.numeric(HDR2019$HDI.rank)
HDR2019$Country <- as.factor(HDR2019$Country)
HDR2019$HDI <- as.numeric(HDR2019$HDI)
HDR2019$LifeExp <- as.numeric(HDR2019$LifeExp)
HDR2019$ExpSchool <- as.numeric(HDR2019$ExpSchool)
HDR2019$MeanSchool <- as.numeric(HDR2019$MeanSchool)
HDR2019$GNI.capita <- as.numeric(HDR2019$GNI.capita)
HDR2019$GNI.HDI.rank <- as.numeric(HDR2019$GNI.HDI.rank)
HDR2019$HDI.rank.2019 <- as.numeric(HDR2019$HDI.rank.2019)
str(HDR2019)

#Calculo del indicador HDI 
#form: (valor-min)/(max-min)
HDR2019$I.Health <- 
  (HDR2019$LifeExp - 20) / (85 - 20)
HDR2019$I.Education <- 
  ((pmin(HDR2019$ExpSchool, 18) - 0) / 
     (18 - 0) + (HDR2019$MeanSchool - 0) / 
     (15 - 0)) / 2
HDR2019$I.Income <-
  (log(HDR2019$GNI.capita) - log(100)) /
  (log(75000) - log(100))
HDR2019$HDI.calc <- 
  (HDR2019$I.Health * HDR2019$I.Education * 
     HDR2019$I.Income)^(1/3)

#Se comparan el HDI, con el HDI calculado (muestra ambas columnas)
HDR2019[, c("HDI", "HDI.calc")]

#4.8
##Primero obtenemos el data frame de la API de la pagina web indicada en la tarea
##http://hdr.undp.org/en/content/human-development-report-office-statistical-data-api

url = "http://ec2-54-174-131-205.compute-1.amazonaws.com/API/hdro_api_all.json"
datos <- GET(url)
datos <- fromJSON(content(datos,type="text"))
datos <- data.frame(datos)

#Guardamos un vector con el nombre de las variables que usaremos para el cálculo
indicators <- c(
  "Literacy rate, adult (% ages 15 and older)", 
  "Gross enrolment ratio, tertiary (% of tertiary school-age population)", 
  "Primary school teachers trained to teach (%)", 
  "Child malnutrition, stunting (moderate or severe) (% under age 5)", 
  "Mortality rate, female adult (per 1,000 people)", 
  "Mortality rate, male adult (per 1,000 people)")

#Guardamos las columnas que tengan igual nombre a datos del vector recién guardado
datosl <- datos[
  datos$indicator_name %in% indicators, ]
#Aplicamos dcast() para que los datos tomen la misma estructura que HDR2019
datosl <- dcast(datosl, indicator_id+indicator_name+country_code+country_name~year, value.var = "value")
#Se crea una columna en el dataframe con el promedio de los ultimos IDH registrados
datosl$"9999" <- apply(datosl[ ,c(12,19:23)], 1, mean, na.rm = TRUE)
#Se seleccionan solo las columnas de nuestro interés
datosl <- subset(datosl, 
                 select = c("indicator_name", "country_name", "9999"))
#Se reemplazan todos los valores string "NaN" por NA
datosl$"9999"[is.nan(datosl$"9999")]<-NA
#Se aplica dcast() para reordenar los datos
datosw <- dcast(datosl, country_name ~ indicator_name, 
                value.var = "9999")

# names(HDR2018)[1] <- "HDI.rank"
# Se acortan los nombres de las variables
names(datosw)[1] <- "Country"
names(datosw)[2] <- "Child.Malnu"
names(datosw)[3] <- "Tert.Enrol"
names(datosw)[4] <- "Adult.Lit"
names(datosw)[5] <- "Mortality.Female"
names(datosw)[6] <- "Mortality.Male"
names(datosw)[7] <- "Prim.Teacher"
str(datosw)
#Luego de verificar que los datos estan ok, se aplica summary() para obtener los mínimos y los máximos, para calcular nuestro propio IDH
summary(datosw)

#Obtenemos un indicador de educación que se usa para calcular el HDI
#el minimo se redondea hacia abajo y el máximo hacia arriba
datosw$I.Adult.Lit <-
  (datosw$Adult.Lit-24) / (100-24)
datosw$I.Tert.Enrol<-
  (datosw$Tert.Enrol-3) / (135-3) 
datosw$I.Prim.Teacher <-
  (datosw$Prim.Teacher-14) / (100-14)

datosw$I.Education.alt <- 
  (datosw$I.Adult.Lit +
     datosw$I.Tert.Enrol + 
     datosw$I.Prim.Teacher) / 3
summary(datosw$I.Education.alt)

#Índice de salud
datosw$I.Child.MalNu <-
  (datosw$Child.Malnu - 1) / (55 - 1)
datosw$I.Mortality.Female <-
  (datosw$Mortality.Female - 33) /
  (452 - 33) 
datosw$I.Mortality.Male <-
  (datosw$Mortality.Male - 58) /
  (553 - 58)
datosw$I.Health.alt <-
  (datosw$I.Child.MalNu + 
     datosw$I.Mortality.Female + 
     datosw$I.Mortality.Male) / 3
#Relación inversa entre valor y categoria en el ranking
datosw$I.Health.alt <- (1 - datosw$I.Health.alt)
summary(datosw$I.Health.alt)

#Se unen los dataframes
datos2018_19 <- merge(HDR2019, datosw)

#Se calcula y guarda el HDI propio
datos2018_19$IDH.propio <- 
  (datos2018_19$I.Health.alt *
     datos2018_19$I.Education.alt *
     datos2018_19$I.Income)^(1/3) 
summary(datos2018_19$IDH.propio)

#Se calculan los ranking de HDI y guardan en un nuevo dataframe
#Se eliminan los valores NA
datos2018_19_sub <- 
  subset(datos2018_19, !is.na(HDI) & !is.na(IDH.propio)) 

#Se crea un ranking de HDI propio y un Ranking del HDI real
datos2018_19_sub$IDH.propio.rank <-
  rank(-datos2018_19_sub$IDH.propio, na.last = "keep") 
datos2018_19_sub$IDH.rank <-
  rank(-datos2018_19_sub$HDI, na.last = "keep") 

#Se crean graficos de disperción para comparar el ranking de HDI propio vs HDI real
ggplot(datos2018_19_sub, aes(x = IDH.rank, y = IDH.propio.rank)) +
  geom_point(shape = 16) +
  labs(y = "Ranking de IDH alternativo", x = "Ranking de IDH real") +
  ggtitle("Comparacion ranking IDH real y IDH propio") +
  theme_bw()
ggplot(datos2018_19_sub, aes(x = IDH.rank, y = IDH.propio.rank)) +
  geom_point(shape = 16) +
  labs(y = "Ranking de IDH alternativo", x = "Ranking de IDH real") +
  ggtitle("Comparacion ranking IDH real y IDH propio") +
  theme_bw() + geom_smooth(span = 0.8, fill = "red", colour = "red4", 
                           lty = 2, size = 1)

#Tabla comparativa HDI
tablaIDH <- select(datos2018_19_sub, Country, HDI, IDH.rank, IDH.propio, IDH.propio.rank)
#Tabla HDI propio
tablaIDH1 <- select(datos2018_19_sub, Country, IDH.propio, I.Health.alt, I.Education.alt, I.Income)
#tablaIDH1 <- melt(tablaIDH1, id.vars = c("Country", "IDH.propio"))
#names(tablaIDH1)[3] <- "Índices"
#names(tablaIDH1)[4] <- "Valor"

#Comparativa entre los Índices utilizados para el calculo y el HDI
ggplot(tablaIDH1, aes(x = I.Health.alt, y = IDH.propio)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("HDI propio respecto al Índice de Salud") + 
  labs(x = "Índice de Salud", 
       y = "HDI propio")#+
  #facet_grid(~ NomCol) #Grafico de acuerdo a los valores de la variable "NomCol" (generalmente el "tipo" de algo)

ggplot(tablaIDH1, aes(x = I.Education.alt, y = IDH.propio)) + 
  geom_point() + 
  geom_smooth()+
  ggtitle("HDI propio respecto al Índice de Educación") + 
  labs(x = "Índice de Educación", 
       y = "HDI propio")

ggplot(tablaIDH1, aes(x = I.Income, y = IDH.propio)) + 
  geom_point() + 
  geom_smooth()+
  ggtitle("HDI propio respecto al Índice Económico") + 
  labs(x = "PIB", 
       y = "HDI propio")
#ggsave("mi_grafico.png") #guarda el ultimo grafico generado

tab1 <- select(datos2018_19_sub, Country, HDI, IDH.propio, I.Health.alt, I.Education.alt, I.Income)
tab1 <- melt(tab1, id.vars = c("Country", "HDI", "IDH.propio"))
names(tab1)[4] <- "Indices"
names(tab1)[5] <- "Valor"

#ggplot(tab1, aes(x = HDI, y = IDH.propio, colour= Indices)) + 
#  geom_point() + 
#  geom_smooth()+
#  ggtitle("HDI propio respecto al Índice Económico") + 
#  facet_grid(Indices ~.)

