mirror <- "cran-r.c3sl.ufpr.br"
options(repos = mirror)

# Instale o pacote mlbench se ainda não o tiver instalado
install.packages("mlbench")

# Carregue o pacote
library(mlbench)

# 1- Carregue a base de dados Satellite
data(Satellite)

# Visualize a estrutura da base de dados
str(Satellite)

# 2. Crie 2 partições contendo 80% para treino e 20% para teste
# Instale os pacotes necessários se ainda não os tiver instalado
install.packages("caret")

# Carregue o pacote
library(caret)

# Defina a semente para reprodução dos resultados
set.seed(123)

# Crie a partição
particao <- createDataPartition(Satellite$classes, p = 0.8, list = FALSE)

# Separe os dados de treinamento e teste
dados_treino <- Satellite[particao, ]
dados_teste <- Satellite[-particao, ]

# Verifique o tamanho dos dados de treinamento e teste
print(paste("Tamanho dos dados de treinamento:", nrow(dados_treino)))
print(paste("Tamanho dos dados de teste:", nrow(dados_teste)))

# 3- Treine modelos RandomForest, SVM e RNA para predição destes dados.
install.packages("neuralnet")
# Carregue os pacotes necessários
library(randomForest)
library(e1071)
library(neuralnet)

# 3.1 Treinamento do modelo Random Forest
modelo_rf <- randomForest(classes ~ ., data = dados_treino)

# 3.2 Treinamento do modelo SVM
modelo_svm <- svm(classes ~ ., data = dados_treino)

# 3.3 Treinamento do modelo RNA
modelo_rna <- neuralnet(classes ~ ., data = dados_treino, hidden = c(5, 2), linear.output = FALSE)

# Exiba os modelos treinados
print(modelo_rf)
print(modelo_svm)
print(modelo_rna)

# 4. Escolha o melhor modelo com base em suas matrizes de confusão.
# Carregue o pacote 'caret' para calcular a matriz de confusão
library(caret)

# Função para calcular métricas de desempenho
calcular_metricas <- function(matriz_confusao) {
  # Precisão (precision)
  precisao <- diag(matriz_confusao) / colSums(matriz_confusao)

  # Recall
  recall <- diag(matriz_confusao) / rowSums(matriz_confusao)

  # F1-score
  f1_score <- 2 * (precisao * recall) / (precisao + recall)

  # Retornar as métricas
  return(data.frame(precisao = precisao, recall = recall, f1_score = f1_score))
}

# Função para imprimir as métricas
imprimir_metricas <- function(nome_modelo, matriz_confusao) {
  cat("\nModelo:", nome_modelo, "\n")
  print(calcular_metricas(matriz_confusao))
}

# Função para plotar a matriz de confusão
plotar_matriz_confusao <- function(nome_modelo, matriz_confusao) {
  confusionMatrix(matriz_confusao, main = nome_modelo)
}

# Prever os rótulos usando cada modelo
predicoes_rf <- predict(modelo_rf, newdata = dados_teste)
predicoes_svm <- predict(modelo_svm, newdata = dados_teste)
predicoes_rna <- predict(modelo_rna, newdata = dados_teste)

# Prever as probabilidades usando a RNA
probabilidades_rna <- predict(modelo_rna, newdata = dados_teste)

# Obter os nomes das classes
nomes_classes <- levels(dados_teste$classes)

# Transformar probabilidades em rótulos de classe
predicoes_rna <- apply(probabilidades_rna, 1, function(x) nomes_classes[which.max(x)])

# Converter as predições em fatores
predicoes_rna <- factor(predicoes_rna, levels = nomes_classes)

# Calcular a matriz de confusão para cada modelo
matriz_confusao_rf <- confusionMatrix(predicoes_rf, dados_teste$classes)
matriz_confusao_svm <- confusionMatrix(predicoes_svm, dados_teste$classes)
matriz_confusao_rna <- confusionMatrix(predicoes_rna, dados_teste$classes)

# Imprimir as métricas de desempenho para cada modelo
imprimir_metricas("Random Forest", matriz_confusao_rf$table)
imprimir_metricas("SVM", matriz_confusao_svm$table)
imprimir_metricas("RNA", matriz_confusao_rna$table)

# Plotar as matrizes de confusão
plotar_matriz_confusao("Random Forest", matriz_confusao_rf$table)
plotar_matriz_confusao("SVM", matriz_confusao_svm$table)
plotar_matriz_confusao("RNA", matriz_confusao_rna$table)


# 5- Indique qual modelo dá o melhor o resultado e a métrica u2lizada
# Extrair os valores de F1-score para cada modelo
f1_rf <- calcular_metricas(matriz_confusao_rf$table)$f1_score
f1_svm <- calcular_metricas(matriz_confusao_svm$table)$f1_score
f1_rna <- calcular_metricas(matriz_confusao_rna$table)$f1_score

# Criar um data frame com os valores de F1-score
df_f1 <- data.frame(Modelo = c("Random Forest", "SVM", "RNA"),
                    F1_Score = c(f1_rf, f1_svm, f1_rna))

# Ordenar o data frame pelo F1-score
df_f1 <- df_f1[order(df_f1$F1_Score, decreasing = TRUE), ]

# Imprimir o data frame
print(df_f1)

# Identificar o melhor modelo
melhor_modelo <- df_f1[1, "Modelo"]
cat("\nO melhor modelo é:", melhor_modelo, "\n")


