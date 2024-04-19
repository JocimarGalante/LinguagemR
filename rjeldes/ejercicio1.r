
install.packages("randomForest")
install.packages("mlbench")
install.packages("caTools")
install.packages("caret")

install.packages("caret")
install.packages("e1071")
install.packages("neuralnet")
install.packages("mlbench")
install.packages("mice")
library(mlbench)
library(caret)
library(mice)
library(randomForest)



# Obter os dados
data(Satellite)
str(Satellite)

temp_dados <- Satellite
set.seed(123)
# library(caret)

# FORMA ENSENADA EN CLASES
indices <- createDataPartition(temp_dados$classes, p = 0.80, list = FALSE)
treino <- temp_dados[indices, ]
teste <- temp_dados[-indices, ]


# Treinar RF, SVM e RNA com a base de Treino
model_rf <- train(classes ~ ., data = treino, method = "rf")
model_svm <- train(classes ~ ., data = treino, method = "svmRadial")
model_nnet <- train(classes ~ ., data = treino, method = "nnet", trace = FALSE)

#  Evaluar modelos y elegir el mejor modelo basado en matrices de confusión
# Función para predecir clases y calcular la matriz de confusión
predict_and_confusion_matrix <- function(model, data) {
  predictions <- predict(model, newdata = data)
  confusion_matrix <- table(data$classes, predictions)
  return(confusion_matrix)
}



# Evaluar modelos en el conjunto de prueba
confusion_matrix_rf <- predict_and_confusion_matrix(model_rf, data_test)
confusion_matrix_svm <- predict_and_confusion_matrix(model_svm, data_test)
confusion_matrix_nnet <- predict_and_confusion_matrix(model_nnet, data_test)


#  Indicar el mejor modelo y la métrica utilizada
# Mostrar matrices de confusión
confusion_matrix_rf
confusion_matrix_svm
confusion_matrix_nnet


# Comparar la exactitud de los modelos
accuracy_rf <- sum(diag(confusion_matrix_rf)) / sum(confusion_matrix_rf)
accuracy_svm <- sum(diag(confusion_matrix_svm)) / sum(confusion_matrix_svm)
accuracy_nnet <- sum(diag(confusion_matrix_nnet)) / sum(confusion_matrix_nnet)

# Imprimir exactitud de los modelos
cat("Exactitud del modelo RandomForest:", accuracy_rf, "\n")
cat("Exactitud del modelo SVM:", accuracy_svm, "\n")
cat("Exactitud del modelo RNA:", accuracy_nnet, "\n")

# Modelo con la mayor exactitud
best_model <- ifelse(max(accuracy_rf, accuracy_svm, accuracy_nnet) == accuracy_rf, "RandomForest",
              ifelse(max(accuracy_rf, accuracy_svm, accuracy_nnet) == accuracy_svm, "SVM", "RNA"))

cat("El mejor modelo es:", best_model, "\n")
