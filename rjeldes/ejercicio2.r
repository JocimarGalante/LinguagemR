# Especificar el espejo de CRAN
mirror <- "cran-r.c3sl.ufpr.br"

# Configurar el espejo y cargar paquetes necesarios
options(repos = mirror)

# Instalación y carga de paquetes
if (!requireNamespace("caTools", quietly = TRUE)) {
  install.packages("caTools")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}
if (!requireNamespace("kernlab", quietly = TRUE)) {
  install.packages("kernlab")
}
if (!requireNamespace("neuralnet", quietly = TRUE)) {
  install.packages("neuralnet")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caTools)
library(caret)

# Cargar el archivo Volumes.csv
url <- "http://www.razer.net.br/datasets/Volumes.csv"
volumes <- read.csv2(url, header = TRUE, sep = ";")

# Eliminar la columna NR
volumes <- volumes[, -1]

# Crear partición de datos: entrenamiento 80%, prueba 20%
set.seed(123)
indices <- createDataPartition(volumes$VOL, p = 0.8, list = FALSE)
train_data <- volumes[indices, ]
test_data <- volumes[-indices, ]

# Entrenar los modelos
# Random Forest
rf_model <- train(VOL ~ ., data = train_data, method = "rf")
# SVM
svm_model <- train(VOL ~ ., data = train_data, method = "svmRadial")
# Redes Neurales
nn_model <- train(VOL ~ ., data = train_data, method = "neuralnet")
# Modelo alométrico de Spurr
spurr_model <- nls(VOL ~ b0 + b1*DAP*DAP*HT, data = train_data, 
                   start = list(b0 = 0.5, b1 = 0.5))

# Hacer predicciones en los datos de prueba
rf_predictions <- predict(rf_model, newdata = test_data)
svm_predictions <- predict(svm_model, newdata = test_data)
nn_predictions <- predict(nn_model, newdata = test_data)
spurr_predictions <- predict(spurr_model, newdata = test_data)

#  Calcular las métricas de evaluación
# Función para calcular R²
calculate_r_squared <- function(observed, predicted) {
  1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
}
# Función para calcular Syx
calculate_syx <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}
# Coeficiente de determinación (R²) y Error estándar de la estimación (Syx)
metrics <- function(observed, predicted) {
  r2 <- calculate_r_squared(observed, predicted)
  syx <- calculate_syx(observed, predicted)
  syx_percent <- (syx / mean(observed)) * 100
  return(c(R2 = r2, Syx = syx, Syx_percent = syx_percent))
}

# Calcular las métricas para cada modelo
rf_metrics <- metrics(test_data$VOL, rf_predictions)
svm_metrics <- metrics(test_data$VOL, svm_predictions)
nn_metrics <- metrics(test_data$VOL, nn_predictions)
spurr_metrics <- metrics(test_data$VOL, spurr_predictions)

# ****************************** Mostrar los resultados ******************************
results <- data.frame(Modelo = c("Random Forest", "SVM", "Redes Neurais", "Modelo alométrico de Spurr"),
                      R2 = c(rf_metrics["R2"], svm_metrics["R2"], nn_metrics["R2"], spurr_metrics["R2"]),
                      Syx = c(rf_metrics["Syx"], svm_metrics["Syx"], nn_metrics["Syx"], spurr_metrics["Syx"]),
                      Syx_percent = c(rf_metrics["Syx_percent"], svm_metrics["Syx_percent"], nn_metrics["Syx_percent"], spurr_metrics["Syx_percent"]))
results
