# Si tengo 30000 de trigo, puedo cocinar x der cerveza con los ingredientes (sistema de ecuaciones) 
# Por si acaso, casteamos a numérico 




cerveza_recipe <- function() {
  cereales <- as.numeric(readline(prompt = "Por favor, introduce el nº de cereales a procesar: "))
  if(!is.na(cereales)) {
  
  # Calulamos la cantidad de ingredientes
  agua_mineral <- (cereales/5)*6
  azucar <- (cereales/5)
  levadura <- (cereales/5)*2
  return(c(cereales, agua_mineral, azucar, levadura))  
  } else if (is.na(cereales)) {
    print("El nº de cereales introducido es NA")
  }
}

ingredientes_cerve <- cerveza_recipe()

cat(paste0("Para convertir ", ingredientes_cerve[1]," cereales en cerveza necesitas: \n- ",ingredientes_cerve[2], " de agua mineral,\n- ", ingredientes_cerve[3]," de azucar\n- ", ingredientes_cerve[4]," de levadura"))



prueba <- NA
prueba
class(prueba)
typeof(prueba)





# ESTO ES LO DE LOS GENERADORES PARA PASAR UN STREAM DEFOTOS A UNA RED
# CONVOLUCIONAL, EN LIUGAR DE CARGAR TODO EL DATASET EN RAM DE GOLPE

generate_abc <- coro::generator(function() {
  for (x in letters[1:3]) {
    yield(x)
  }
})


# Create the iterator
abc <- generate_abc()

# Use the iterator by invoking it
abc()
#> [1] "a"

abc()
#> [1] "b"º