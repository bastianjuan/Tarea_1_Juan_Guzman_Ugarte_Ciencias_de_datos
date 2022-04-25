# Instalamos y ejecutamos las librerÃ­as que nos permitirÃ¡n trabajar

library(rnaturalearth)
library(rnaturalearthdata)
library(mapview)
library(dplyr)
library(sf)
library(gstat)
library(sp)

# Definimos nuestra base de datos asociada al valor de la bigmac en el mundo por año
# El nombre de la base de datos es "big_mac_raw_index.csv" y se encuentra en la misma carpeta

datos= read.csv(file.choose(), sep = ",", header = TRUE, encoding = "UTF-8" ,fill = TRUE)

# Comenzamos con la limpieza de los datos
# Eliminamos las columnas currency_code, local_price, EUR, GBP, JPY y CNY

datos <- select(datos, -currency_code)
datos <- select(datos, -local_price, -EUR, -GBP, -JPY, -CNY)


library(countrycode)
datos$continent <- countrycode::countrycode(sourcevar = datos[, "name"],
                                            origin = "country.name",
                                            destination = "continent")

# Ahora descargamos las bases de datos asociadas a populated_places de Natural Earth en América del sur

pp120 <- ne_countries(scale = 'large' ,returnclass = "sf")

# Ahora, creamos un nuevo dataframe con los datos combinados mediante la asociaciÃ³n con la columna iso_a3 y sov_a3
DatosCombinados <- merge(datos, pp120, by.x="iso_a3", by.y="sov_a3")

# Luego filtramos la nueva base de datos

DataComb <- select(DatosCombinados, iso_a3, date, name.x, dollar_ex, dollar_price, USD, continent.x,  gdp_md_est, income_grp, geometry)

# Nos quedamos solamente con la data de America
DataComb <- DataComb[DataComb$continent.x=="Americas",]


Mapa <- st_as_sf(DataComb)

# Una vez que tenemos los datos en formato sf, podemos comenzar con la correlación espacial de la data y los gráficos

pacman::p_load(tmap, sf, tidyverse, mapview, RColorBrewer, viridis,
               leafpop, leaflet, leaflet.extras, mapdeck, plotly, ggmap,
               ggiraph, widgetframe)

# creamos una paleta que nos va a servir para los colores
pal <- magma(n = length(unique(Mapa$name.x)), direction = -1)


# graficamos usando funcion mapview

Mapa2019 <- mapview(Mapa[Mapa$date=="2019-01-01",], zcol = "dollar_price")
Mapa2009 <- mapview(Mapa[Mapa$date=="2009-07-01",], zcol = "dollar_price")

# Ahora hacemos un mapa interactivo que muestre los valores de la big mac en el año 2009 vs 2019
library(leaflet.extras2)
Mapa2009|Mapa2019

mylabel <- glue::glue("{Mapa$name.x} {Mapa$dollar_price}")

# Hagamos ahora el análisis de correlación
# Para ello, ocuparemos los datos de los países en el año 2019

datos2019 <- DatosCombinados[DatosCombinados$date=="2019-01-01",]
plot(datos2019$dollar_price~datos2019$gdp_md_est)
cor.test(datos2019$dollar_price, datos2019$gdp_md_est)
# Con el análisis de correlación, el valor de pearson nos entrega que no existe correlación entre ambas variables

modelo1 <- lm(datos2019$dollar_price~datos2019$gdp_md_est)
summary(modelo1)

# Como en el F-statistic el valor de p-value es > 0.05, aceptamos la hipótesis nula diciendo que nuestro modelo no es válido
# El R cuadrado es muy menor, por lo que la variabilidad de la variable "Precio de la big mac" no es explicada por el precio del Pib

modelo1$coefficients
plot(datos2019$dollar_price~datos2019$gdp_md_est)
abline(modelo1, col = "red")
