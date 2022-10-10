##### UTF-8 Encoding


# Fuente: https://search.r-project.org/CRAN/refmans/coro/html/generator.html


library(coro)


# La funcion generator() del paquete coro crea un generador, cosa muy usada en
# DL. Es una funcion iteradora que puedes pausar con la funcion yield() y
# reanudar posteriormente. Para mas info, consulta la vinneta:
vignette("generator")


# Los generadores se pueden usar sobre bucles for. En este ejemplo vamos a
# iterar sobre las 3 primeras letras del abecedario, contenidas en la constante
# letters del paquete base:
generate_abc <- generator(function() {
  for (x in letters[1:3]) {
    yield(x)
  }
})


abc <- generate_abc()

abc()
#> [1] "a"

abc()
#> [1] "b"

abc()
#> [1] "c"


abc()
# > Generador exhausto




# Los generadores se usan en DL para pasarle a la red neuronal imagenes e inputs
# poco a poco, en vez de cargar todo el dataset en la RAM de golpe



# Mira la diferencia con esta funcion normal que nos devuelve todo el dataset a
# la vez
devolver_Dataset <- function() {
  for (x in letters[1:3]) {
    print(x)
  }
}

devolver_Dataset()


