# To esto pa hacer trabajo en cuda en R 

# Mira este tuto: https://tensorflow.rstudio.com/tutorials/beginners/
install.packages("tensorflow")

library(tensorflow)

install_tensorflow()

tf$constant("Hellow Tensorflow")



library(tensorflow)
library(tfautograph)
library(keras)
library(tfdatasets)

tf$executing_eagerly()

x <- matrix(2, ncol = 1, nrow = 1)
m <- tf$matmul(x, x)
m


a <- tf$constant(matrix(c(1,2,3,4), ncol = 2))
a

b <- tf$add(a, 1)
b



# Fetch and format the mnist data
mnist <- dataset_mnist()
dataset <- tensor_slices_dataset(mnist$train) %>% 
  dataset_map(function(x) {
    x$x <- tf$cast(x$x, tf$float32)/255
    x$x <- tf$expand_dims(x$x, axis = -1L)
    unname(x)
  }) %>% 
  dataset_shuffle(1000) %>% 
  dataset_batch(32)

dataset


mnist_model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation= "relu",
                input_shape = shape(NULL, NULL, 1)) %>% 
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = "relu") %>% 
  layer_global_average_pooling_2d() %>% 
  layer_dense(units = 10)


el <- dataset %>% 
  dataset_take(1) %>% 
  dataset_collect()
mnist_model(el[[1]])


optimizer <- optimizer_adam()
loss_object <- tf$keras$losses$SparseCategoricalCrossentropy(from_logits = TRUE)

loss_history <- list()



train_step <- function(images, labels) {
  with(tf$GradientTape() %as% tape, {
    logits <- mnist_model(images, training = TRUE)
    tf$debugging$assert_equal(logits$shape, shape(32, 10))
    loss_value <- loss_object(labels, logits)
  })
  loss_history <<- append(loss_history, loss_value)
  grads <- tape$gradient(loss_value, mnist_model$trainable_variables)
  optimizer$apply_gradients(
    purrr::transpose(list(grads, mnist_model$trainable_variables))
  )
}

train <- autograph(function() {
  for (epoch in seq_len(3)) {
    for (batch in dataset) {
      train_step(batch[[1]], batch[[2]])
    }
    tf$print("Epoch", epoch, "finished.")
  }
})

train()


history <- loss_history %>% 
  purrr::map(as.numeric) %>% 
  purrr::flatten_dbl()
ggplot2::qplot(x = seq_along(history), y = history, geom = "line")