#------------------------------------------------------------------------------
# Se toman datos del Center for Systems Science and Engineering at Johns
# Hopkins University, relacionados al número de contagioas, muertes y 
# recuperaciones del SARS-CoV-2 confirmadas en los Estados Unidos y el mundo,
# para luego ser ordenados en un objeto llamado 'csseCovid'
#------------------------------------------------------------------------------

#-Paso 1: Importar librerias y datos ------------------------------------------

#-Librerias de R
nombres <- c("tibble")

sapply(X = nombres, FUN = require, character.only = TRUE)

#-Importar función necesaria
archivo <- "dir-name.R"

if (basename(getwd()) == "sars-coronavirus-2")
{
	archivo <- file.path("programas", archivo)
}

source(file = archivo)

#-Tabla con información de las series de tiempo
archivo <- list.files(path = DirName("submodulos"), pattern = "UID",
		      full.names = TRUE, recursive = TRUE)

tabla <- read.csv(file = archivo)

#-Series de tiempo
archivo <- list.files(path = DirName("submodulos"), pattern = "covid19",
		      full.names = TRUE, recursive = TRUE)

series <- lapply(X = archivo, FUN = read.csv)

names(series) <- basename(archivo)

#-Paso 2: Extraer información de las series -----------------------------------

zonas <- c("US", "global")

csseCovid <- lapply(X = zonas, FUN = function (zona)
{
	# se crean variables por zona
	nombres <- grep(pattern = zona, x = names(series), value = TRUE)

	for (nombre in nombres)
	{
		variable <- gsub(pattern = "time_series_covid19_",
				 replacement = "", x = nombre)

		variable <- gsub(pattern = paste0("_", zona, ".csv"),
				 replacement = "", x = variable)

		assign(x = variable, value = series[[nombre]])
	}

	# columnas con valores numéricos
	columnas <- grep(pattern = "X", x = colnames(confirmed), value = TRUE)

	# rango de fechas de las series
	fechas <- gsub(pattern = "X", replacement = "", x = columnas)

	fechas <- gsub(pattern = "[.]", replacement = "/", x = fechas)

	fechas <- as.Date(x = fechas, format = "%m/%d/%Y")

	# selección de series
	selec <- c("UID", "iso3", "code3", "Lat", "Long_", "Population",
		   "Admin2", "Province_State", "Country_Region")

	# función auxiliar
	CalculoBasico <- function(x, nombre)
	{
		x <- as.numeric(x)

		if (all(is.na(x)))
		{
			x <- rep(NA, NROW(fechas))

			x <- cbind(x, x)
		} else
		{
			x <- cbind(c(x[1], diff(x)), x)
		}


		colnames(x) <- c(nombre, paste("Cum", nombre, sep = "_"))

		return (x)
	}

	# datos de las series
	series <- lapply(X = 1:NROW(confirmed), FUN = function(i)
	{
		provincia <- as.character(confirmed[i, "Province.State"])

		pais <- as.character(confirmed[i, "Country.Region"])

		# información de las series
		if (zona == "US")
		{
			subconjunto <- tabla[["UID"]] == confirmed[i, "UID"]

		} else if (zona == "global")
		{
			if (provincia == "Hong Kong" | provincia == "Macau")
			{
				provincia <- paste(provincia, "SAR")
			}

			sub1 <- tabla[["Province_State"]] == provincia

			if (provincia == "")
			{
				sub2 <- tabla[["Country_Region"]] == pais

			} else if (provincia != "")
			{
				sub2 <- tabla[["Country_Region"]] != "US"
			}

			subconjunto <- sub1 & sub2
		}

		info <- subset(x = tabla, subset = subconjunto, select = selec)

		# datos de las series
		casos <- CalculoBasico(confirmed[i, columnas], "Cases")

		muertes <- CalculoBasico(deaths[i, columnas], "Deaths")

		datos <- tibble(casos, muertes)

		if (zona == "global")
		{
			s1 <- recovered[["Province.State"]] == provincia

			s2 <- recovered[["Country.Region"]] == pais

			if (any((s1 & s2) == TRUE))
			{
				j <- which(s1 & s2)

				recuperados <- recovered[i, columnas]
			} else
			{
				recuperados <- NA
			}

			recuperados <- CalculoBasico(recuperados, "Recovered")

			datos <- tibble(as_tibble(datos), 
					as_tibble(recuperados))
		}

		serie <- tibble(as_tibble(info), 
				Dates = fechas, as_tibble(casos),
				as_tibble(muertes))

		# truncar al primer valor distinto de cero
		columnas <- c("Cum_Deaths", "Cum_Cases")

		filas <- which(rowSums(serie[ , columnas]) != 0)[1] - 1

		if (!is.na(filas) & filas != 0)
		{
			filas <- seq(filas)
	
			serie <- serie[-filas, ] 
		}

		return (serie)
	})

	series <- do.call(rbind, series)
})

names(csseCovid) <- zonas

#-Paso 3: Guardar objetos -----------------------------------------------------

#-Seleccionar objetos
seleccion <- c()

seleccion <- grepl(pattern = "Covid", x = ls())

#-Guardar objeto 'csv' en el disco local
for (zona in zonas)
{
	archivo <- paste0("jhu-csse-covid-", zona, ".csv")

	archivo <- file.path(DirName("datos"), archivo)

	write.csv(x = csseCovid[[zona]], file = archivo, row.names = FALSE) 
}

#-Guardar objeto 'RData' en el disco local
archivo <- file.path(DirName("rdatos"), "jhu-csse-covid.RData")

save(x = csseCovid, file = archivo)

#-Eliminar objeto
rm(list = ls()[!seleccion])

