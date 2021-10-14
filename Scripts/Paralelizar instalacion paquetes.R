# Para paralelizar la instalación de paquetes en R, modifica la variable make

# Con Ncpus
options(Ncpus = 4L)
getOption("Ncpus")


# Con make, no se si lo habré puesto bien
Sys.setenv("MAKE" = "make -k -j 4")
Sys.getenv("MAKE")
