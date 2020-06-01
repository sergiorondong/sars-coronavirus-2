#------------------------------------------------------------------------------
# Se toman datos del European Centre for Disease Prevention and Control (ECDC),
# relacionados al número de contagios y muertes confirmadas de SARS-CoV-2, 
# luego se ordenan en un formato estandar en el objeto 'ecdcCovid'
#------------------------------------------------------------------------------

#-Paso 1: Importar librerias y datos ------------------------------------------

#-Librerias de R
nombres <- c("tibble", "countrycode", "ISOcodes")

sapply(X = nombres, FUN = require, character.only = TRUE)

#-Importar función necesaria
archivo <- "dir-name.R"

if (basename(getwd()) == "sars-coronavirus-2")
{
        archivo <- file.path("programas", archivo)
}

source(file = archivo)

#-Importar datos crudos
archivo <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

series <- read.csv(file = archivo, na.strings = "", fileEncoding = "UTF-8-BOM")

#-Paso 2: Se concatena información de las series de tiempo --------------------

#-Se unifican las regiones para seleccionar archivos
regiones <- levels(series[["countriesAndTerritories"]])

#-geoId: ISO_Alpha_2 para identificar las regiones geográficas
iso2 <- sapply(X = regiones, FUN = function (region)
{
	fila <- which(series[["countriesAndTerritories"]] == region)[1]

	as.character(series[fila, "geoId"])

}, USE.NAMES = FALSE)

#-iso3: ISO_Alpha_3 para identificar las regiones geográficas
iso3 <- sapply(X = iso2, FUN = function (id)
{
	if (id == "UK")
	{
		iso <- "GBR"

	} else if (id == "EL")
	{
		iso <- "GRC"

	} else if (id == "JPG11668")
	{
		iso <- "JPG11668"

	} else if (id == "XK")
	{
		iso <- "XKX"
	} else
	{
		iso <- countrycode(id, origin = "ecb", destination = "iso3c")
	}

}, USE.NAMES = FALSE)

#-Code3: Códigos de las regiones geográficas
code3 <- countrycode(iso3, origin = "iso3c", destination = "un")

#-Names: Nombres de las regiones geográficas
nombres <- sapply(X = iso2, FUN = function (id)
{
	if (id == "JPG11668")
	{
		nombre <- "Diamond Princess"

	} else if (id == "UK")
	{
		nombre <- "United Kingdom"

	} else if (id == "EL")
	{
		nombre <- "Greece"
	} else
	{
		nombre <- countrycode(sourcevar = id, origin = "ecb", 
				      destination = "ecb.name")
	}

	return (nombre)

}, USE.NAMES = FALSE)

#-Concatenar información
info <- tibble(iso2, iso3, code3, Names = nombres)

#-Paso 3: Crear objeto 'ecdcCovid' --------------------------------------------

#-Crear objeto 
ecdcCovid <- lapply(X = regiones, FUN = function (region)
{
	# seleccionar datos
	filas <- which(series[["countriesAndTerritories"]] == region)

	filas <- filas[order(filas, decreasing = TRUE)]

	columnas <- c("cases", "deaths")

	j <- which(regiones == region)
	
	# fechas
	fechas <- as.Date(x = series[filas, "dateRep"], format = "%d/%m/%Y")

	# datos
	diarios <- apply(X = series[filas, columnas], MARGIN = 2, 
			 FUN = as.numeric)

	acumulados <- apply(X = diarios, MARGIN = 2, FUN = cumsum)

	# poblacion
	poblacion <- as.numeric(series[filas, "popData2018"])

	# truncar los datos
	filas <- which(rowSums(acumulados) != 0)[1] - 1

	if (!is.na(filas) & filas != 0)
	{
		filas <- seq(filas)

		fechas <- fechas[-filas]

		diarios <- diarios[-filas, ]

		acumulados <- acumulados[-filas, ]

		poblacion <- poblacion[-filas]
	}

	serie <- tibble(as_tibble(info[j, ]), 
			Dates = fechas, 
			Cases = diarios[ , "cases"],
			Cum_Cases = acumulados[ , "cases"],
			Deaths = diarios[ , "deaths"],
			Cum_Deaths = acumulados[ , "deaths"],
			Population = poblacion)
})

#-Compactar series
ecdcCovid <- do.call(rbind, ecdcCovid)

#-Paso 4: Guardar objetos -----------------------------------------------------

#-Seleccionar elementos a guardar
seleccion <- c()

seleccion <- grepl(pattern = "Covid", x = ls())

#-Guardar en formato 'csv'
archivo <- file.path(DirName("datos"), "ecdc-covid.csv")

write.csv(x = ecdcCovid, file = archivo)

#-Guardar en formato 'RData'
archivo <- file.path(DirName("rdatos"), "ecdc-covid.RData")

save(x = ecdcCovid, file = archivo)

#-Eliminar objetos
rm(list = ls()[!seleccion])

