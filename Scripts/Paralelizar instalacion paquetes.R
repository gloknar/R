# Para paralelizar la instalación de paquetes en R, modifica la variable make

getOption("Ncpus")
options("MAKE") <-  "-k -j 4"

Sys.getenv("MAKE", "make")

Ncpus = getOption("Ncpus", 2L)

make --help