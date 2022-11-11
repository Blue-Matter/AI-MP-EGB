# NN build function

build_model <- function(firsty=16,secondy=8,af="relu") {# "linear", "selu", "sigmoid"

  input <- layer_input_from_dataset(train_df %>% dplyr::select(-label))

  if(secondy>1){
    output <- input %>%
      layer_dense_features(dense_features(spec)) %>%
      layer_dense(units = firsty, activation = af) %>%
      layer_dense(units = secondy, activation = "relu") %>%
      layer_dense(units = 1)
  }else{
    output <- input %>%
      layer_dense_features(dense_features(spec)) %>%
      layer_dense(units = firsty, activation = af) %>%
      layer_dense(units = 1)
  }

  AIEGB <- keras_model(input, output)

  AIEGB %>%
    compile(
      loss = 'mean_squared_error',#"mse",#mae",#"mse",
      optimizer = 'adam',# 'sgd',#adam',#'ftrl',#'nadam',#'adamax',#'adadelta',# 'rmsprop',#'adam',#optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )

  AIEGB
}
