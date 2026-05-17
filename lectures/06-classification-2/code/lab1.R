rm(list = ls())
pacman::p_unload(pacman::p_loaded())

pacman::p_load(tidyverse, e1071, caret, ggsci, patchwork)

#### load and examine data ####
data(iris)
head(iris)
summary(iris)
str(iris)

#### visualize data ####
(iris |>
  ggplot(aes(x = Sepal.Width, y = Sepal.Length, color = Species, shape = Species)) +
  geom_point(size = 2) +
  theme_bw() +
  scale_color_aaas() +
  theme(legend.position = "bottom") +
  labs(x = "Width (sepal)", y = "Length (sepal)")) +
(iris |>
   ggplot(aes(x = Petal.Width, y = Petal.Length, color = Species, shape = Species)) +
   geom_point(size = 2) +
   theme_bw() +
   scale_color_aaas() +
   theme(legend.position = "bottom") +
   labs(x = "Width (petal)", y = "Length (petal)")) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = 'Sepal and petal measures for IRIS dataset',
    subtitle = 'Clear linear separation for petals but not for sepal'
  )  &
  theme(legend.position = "bottom")

#### create training and testing splits ####
set.seed(42)
train_indices <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
train_x <- as.matrix(iris |> dplyr::select(-Species))[train_indices, c("Sepal.Length", "Sepal.Width")]
train_y <- as.matrix(iris |> dplyr::select(Species))[train_indices, ]
test_x <- as.matrix(iris |> dplyr::select(-Species))[-train_indices, c("Sepal.Length", "Sepal.Width")]
test_y <- (iris |> dplyr::select(Species))[-train_indices, ]


#### fitting different SVM models ####
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

## linear
svm_linear <- train(x = train_x, y = train_y,
                            method = "svmLinear",
                            trControl = train_control,
                            preProcess = c("center","scale"),
                            tuneGrid = expand.grid(C = c(1/16, 1/8, 1/4, 1/2, 1, 2, 4)))
## polynomial
svm_poly <- train(x = train_x, y = train_y,
                    method = "svmPoly",
                    trControl = train_control,
                    preProcess = c("center","scale"),
                    tuneGrid = expand.grid(degree = c(1:4),
                                           scale = c(0.001, 0.010, 0.1, 1),
                                           C = c(1/16, 1/8, 1/4, 1/2, 1, 2, 4)))

## polynomial
svm_radial <- train(x = train_x, y = train_y,
                  method = "svmRadial",
                  trControl = train_control,
                  preProcess = c("center","scale"),
                  tuneGrid = expand.grid(sigma = c(0.001, 0.010, 0.1, 1),
                                         C = c(1/16, 1/8, 1/4, 1/2, 1, 2, 4)))

confusionMatrix(data = predict(svm_linear, newdata = test_x), reference = test_y)
confusionMatrix(data = predict(svm_radial, newdata = test_x), reference = test_y)
confusionMatrix(data = predict(svm_poly, newdata = test_x), reference = test_y)
