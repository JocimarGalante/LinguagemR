# --- 00 Instalação e carregamento de pacotes necessários e funções básicas ---

# Função para log

log <- function(msg) {
  cat("\n", format(Sys.time(), "%d-%m-%Y %H:%M:%S"), "-", msg, "\n")
}

# Função para calcular métricas

calcular_metricas <- function(matriz_conf) {
  # Precisão (precision)
  precisao <- diag(matriz_conf) / colSums(matriz_conf)

  # Recall
  recall <- diag(matriz_conf) / rowSums(matriz_conf)
  
  # F1-score
  f1_score <- 2 * (precisao * recall) / (precisao + recall)
  
  # Retornar as métricas
  return(data.frame(precisao = precisao, recall = recall, f1_score = f1_score))
}

# Instalação dos pacotes necessários

mirror <- "cran-r.c3sl.ufpr.br"

log(paste("Instalando e carregando pacotes necessários. Mirror: ", mirror))

options(repos = mirror)
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
install.packages("mlbench")

# Carregamento dos pacotes

library("caret")
library("mlbench")

# --- 01 Carregue a base de dados Satellite ---

# Carregando a base de dados (ex 1)

log("Carregando base de dados Satellite")

data(Satellite)

# Visualizando a base de dados

log("Estrutura da base de dados Satellite")
str(Satellite)

log("Primeiras linhas da base de dados Satellite")
head(Satellite)

log("Sumário da base de dados Satellite")
summary(Satellite)

# --- 02 Crie partições contendo 80% para treino e 20% para teste ---

# Setando uma semente de aleatoriedade

set.seed(123)

# Criando índices para o treino

log("Particionando dados em treino e teste")

indices <- createDataPartition(Satellite$classes, p = 0.8, list = FALSE)

# Separando dados em treino e teste

dados_treino <- Satellite[indices, ]
dados_teste <- Satellite[-indices, ]

# Verificando quantas observações há em cada partição

paste("Observações nos dados de treinamento:", nrow(dados_treino))
paste("Observações nos dados de teste:", nrow(dados_teste))

# --- 03 Treine modelos RandomForest, SVM e RNA para predição destes dados ---

# Treinando os modelos

log("Treinando modelo Random Forest")
rf <- train(classes ~ ., data=dados_treino[, c(17, 18, 19, 20, ncol(dados_treino))], method="rf")

log("Treinando modelo SVM")
svm <- train(classes ~ ., data=dados_treino[, c(17, 18, 19, 20, ncol(dados_treino))], method="svmRadial")

log("Treinando modelo RNA")
rna <- train(classes ~ ., data=dados_treino[, c(17, 18, 19, 20, ncol(dados_treino))], method="nnet")

# Relizando as predições

log("Realizando predições")

predicoes_rf <- predict(rf, dados_teste)
predicoes_svm <- predict(svm, dados_teste)
predicoes_rna <- predict(rna, dados_teste)

# --- 04 Escolha o melhor modelo com base em suas matrizes de confusão ---

# Gerando matrizes de confusão

log("Verificando o melhor modelo com base na Accurary das matrizes de confusão")

matriz_confusao_rf <- confusionMatrix(predicoes_rf, dados_teste$classes)
matriz_confusao_svm <- confusionMatrix(predicoes_svm, dados_teste$classes)
matriz_confusao_rna <- confusionMatrix(predicoes_rna, dados_teste$classes)

# Criando um dataframe com os valores de accuracy para cada modelo

matriz_confusao_df <- data.frame(model=c("rf", "svm", "rna"), accuracy=c(matriz_confusao_rf$overall["Accuracy"], matriz_confusao_svm$overall["Accuracy"], matriz_confusao_rna$overall["Accuracy"]))

# Ordenando os modelos com base no valor de Accuracy das matrizes de confusão

matriz_confusao_df <- matriz_confusao_df[order(matriz_confusao_df$accuracy, decreasing=TRUE), ]

# Exibindo qual o modelo com maior acurácia

log("Modelos com base na acurácia (melhor para o pior):")
print(matriz_confusao_df)

# --- 05 Indique qual modelo dá o melhor o resultado e a métrica utilizada ---

# Calculando métricas dos modelos

metricas_rf <- calcular_metricas(matriz_confusao_rf$table)
metricas_svm <- calcular_metricas(matriz_confusao_svm$table)
metricas_rna <- calcular_metricas(matriz_confusao_rna$table)

media_metricas_df <- data.frame(model="rf", precisao=mean(metricas_rf$precisao), recall=mean(metricas_rf$recall), f1_score=mean(metricas_rf$f1_score))
media_metricas_df <- rbind(media_metricas_df, data.frame(model="svm", precisao=mean(metricas_svm$precisao), recall=mean(metricas_svm$recall), f1_score=mean(metricas_svm$f1_score)))
media_metricas_df <- rbind(media_metricas_df, data.frame(model="rna", precisao=mean(metricas_rna$precisao), recall=mean(metricas_rna$recall), f1_score=mean(metricas_rna$f1_score)))
media_metricas_df <- media_metricas_df[order(media_metricas_df$precisao, decreasing=TRUE), ]

log("Métricas com base na precisão (melhor para o pior):")
print(media_metricas_df)