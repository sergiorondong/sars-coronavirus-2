DirName <- function(fichero)
{
	if (basename(getwd()) == fichero)
	{
		ruta <- "."

	} else if (basename(getwd()) == "programas")
	{
		ruta <- file.path("..", fichero)

	} else if (basename(getwd()) == "sars-coronavirus-2")
	{
		ruta <- fichero
	}

	if (ruta != ".")
	{
		dir.create(path = ruta, showWarnings = FALSE, recursive = TRUE)
	}

	return (ruta)
}

