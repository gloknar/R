# Primero cargamos keras
# keras::install_keras()
library(keras)

# Añadir lo de añadir al path del sistema la ruta de CUDA/extra/lib64 y lo de el chart de compatibilidad de TF

install.packages("tensorflow")
tensorflow::install_tensorflow(version = "gpu")

tensorflow::tf_version()

library(tensorflow)
tf$config$list_physical_devices("GPU")

#Cargamos ahora el dataset MNIST
dataset_guapo <- dataset_mnist()




# Pasamos las fotos a un vector de dos dimensiones (nº foto, índice píxel) con array_reshape():

str(dataset_guapo)
dim(dataset_guapo$train$x)
dim(dataset_guapo$test$x)

dataset_guapo$train$x <- array_reshape(dataset_guapo$train$x, dim = c(60000, 28*28), order = "F")

dataset_guapo$test$x  <- array_reshape(dataset_guapo$test$x, dim = c(10000, 28*28), order = "F")


# Pasamos las clases a codificacion one-hot con to_categorical():

dim(dataset_guapo$train$y)
dim(dataset_guapo$test$y)

dataset_guapo$train$y <- to_categorical(dataset_guapo$train$y, num_classes = 10)

dataset_guapo$test$y <- to_categorical(dataset_guapo$test$y, num_classes = 10)


# Normalizamos la profundidad de color de los píxeles al dividirlos por su valor
# máximo (el color en estas fotos está codificado en 8 bits=256 valores,
# incluyendo el 0, luego el valor máximo será 255). Con esto normalizamos los pixeles del rango [0,255] al rango [0,1]

max(dataset_guapo$train$x)
max(dataset_guapo$test$x)


dataset_guapo$train$x <- dataset_guapo$train$x/255
dataset_guapo$test$x <- dataset_guapo$test$x/255





# Definimos el modelo con keras_model_sequential() y layer_(). activación
# softmax para la capa de salida; activación sigmoide para el resto:

modelo <- keras_model_sequential()



modelo %>%
  # Start with hidden 2D CNN layer 
  layer_dense(input_shape = 784, units = 80, activation = "sigmoid") %>%
  layer_dropout(0.25) %>%
  # and feed into dense layer
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  # Outputs from dense layer are projected onto 10 unit output layer
  layer_dense(10) %>%
  layer_activation("softmax")






# Compilamos el modelo con compile(). Aquí definimos la loss function, optimizer y métricas:

modelo %>% compile(loss = "categorical_crossentropy", optimizer = "SGD", metrics = c("AUC"))



# Entrenamos el modelo con fit():

red_neuronal_entrenada <- modelo %>% fit(x = dataset_guapo$train$x, y = dataset_guapo$train$y,
               epochs = 20, validation_data= dataset_guapo$test, verbose = 2,
               batch_size = 784)

plot(red_neuronal_entrenada)







############################
# Ejemplo para clasificar imágenes a color (dataset cifar10). Sacado de https://rpubs.com/HeatWave2019/537744


# Primero cargamos keras
library(keras)

#Cargamos ahora el dataset MNIST
dataset_guapo <- dataset_cifar10()


# Pasamos las fotos a un vector de dos dimensiones (nº foto, índice píxel) con array_reshape():
#
# str(dataset_guapo)
# dim(dataset_guapo$train$x)
# dim(dataset_guapo$test$x)
# 
# dataset_guapo$train$x <- array_reshape(dataset_guapo$train$x, dim = c(60000, 28*28), order = "F")
# 
# dataset_guapo$test$x  <- array_reshape(dataset_guapo$test$x, dim = c(10000, 28*28), order = "F")
# 




# Pasamos las clases a codificacion one-hot con to_categorical():

dim(dataset_guapo$train$y)
dim(dataset_guapo$test$y)

dataset_guapo$train$y <- to_categorical(dataset_guapo$train$y, num_classes = 10)

dataset_guapo$test$y <- to_categorical(dataset_guapo$test$y, num_classes = 10)




# Normalizamos la profundidad de color de los píxeles al dividirlos por su valor
# máximo (el color en estas fotos está codificado en 8 bits=256 valores,
# incluyendo el 0, luego el valor máximo será 255). Con esto normalizamos los pixeles del rango [0,255] al rango [0,1]

max(dataset_guapo$train$x)
max(dataset_guapo$test$x)


dataset_guapo$train$x <- dataset_guapo$train$x/255
dataset_guapo$test$x <- dataset_guapo$test$x/255





# Definimos el modelo con keras_model_sequential() y layer_(). activación
# softmax para la capa de salida; activación relu para el resto:


modelo = keras_model_sequential()

modelo %>%
  # Start with hidden 2D CNN layer 
  layer_conv_2d( filters = 32, kernel_size = c(3,3), padding = "same", input_shape = c(32, 32, 3) ) %>%
  layer_activation("relu") %>%
  # Second hidden layer
  layer_conv_2d(filters  = 32, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  # 2 additional hidden 2D CNN layers
  layer_conv_2d(filters = 32, kernel_size = c(3,3), padding = "same") %>%
  layer_activation("relu") %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%
  # Use max pooling 
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  # Flatten max filtered output into feature vector
  layer_flatten() %>%
  # and feed into dense layer
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  # Outputs from dense layer are projected onto 10 unit output layer
  layer_dense(10) %>%
  layer_activation("softmax")




modelo %>%
  # Start with hidden 2D CNN layer 
  layer_conv_2d( filters = 3, kernel_size = c(3,3), padding = "same", input_shape = c(32, 32, 3) ) %>%
  layer_activation("relu") %>%
  # Flatten max filtered output into feature vector
  layer_flatten() %>%
  # and feed into dense layer
  layer_dense(5) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  # Outputs from dense layer are projected onto 10 unit output layer
  layer_dense(10) %>%
  layer_activation("softmax")





# Compilamos el modelo con compile(). Aquí definimos la loss function, optimizer y métricas:

modelo %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 0.0001),
  metrics = "accuracy" )

summary(modelo)





# Entrenamos el modelo con fit():


start.time = Sys.time()

historial_entrenamiento_red_neuronal <- modelo %>% fit(
  x = dataset_guapo$train$x, y = dataset_guapo$train$y,
  batch_size = 5,
  epochs = 60,
  validation_data = list(dataset_guapo$test$x, dataset_guapo$test$y),
  shuffle = TRUE )

end.time = Sys.time() # Medimos tiempo que tarda en entrenarse la red

duracion_entrenamiento <- end.time-start.time

plot(red_neuronal_entrenada)


# Evaluamos la red neuronal en el set de test

evaluacion <- evaluate(modelo, x=dataset_guapo$test$x, dataset_guapo$test$y, batch_size = 32, verbose = 1,
         sample_weight = NULL, steps = NULL)


cat("la red puede clasificar imagenes a color con una precisión en el set de evaluación del", historial_entrenamiento_red_neuronal$metrics$val_accuracy[40], "y una precisión en el set de test del", evaluacion[2])
