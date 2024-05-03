# --- 00 Instalação e carregamento de pacotes necessários e funções básicas ---

# Função para log

log <- function(msg) {
  cat("\n", format(Sys.time(), "%d-%m-%Y %H:%M:%S"), "-", msg, "\n")
}

# Instalação dos pacotes necessários

mirror <- "cran-r.c3sl.ufpr.br"

log(paste("Instalando e carregando pacotes necessários. Mirror: ", mirror))

options(repos = mirror)
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("neuralnet")
install.packages("caret")

# Carregamento dos pacotes

library("neuralnet")
library("caret")

# --- 01 Carregar o arquivo Volumes.csv (http://www.razer.net.br/datasets/Volumes.csv) ---

url_dataset <- "http://www.razer.net.br/datasets/Volumes.csv"

# Carregando a base de dados

log(paste("Carregando base de dados de volumes de árvores. URL:", url_dataset))

dataset <- read.csv2(url_dataset, header = TRUE, sep = ";")

# --- 02 Eliminar a coluna NR, que só apresenta um número sequencial ---

dataset <- dataset[, !names(dataset) %in% "NR"]

# Visualizando a base de dados

log("Estrutura da base do dataset")
str(dataset)

log("Primeiras linhas da base do dataset")
head(dataset)

log("Sumário da base do dataset")
summary(dataset)

# --- 03 Criar partição de dados: treinamento 80%, teste 20% ---

# Setando uma semente de aleatoriedade

set.seed(123)

# Criando índices para o treino

log("Particionando dados em treino e teste")

indices <- createDataPartition(dataset$VOL, p = 0.8, list = FALSE)

# Separando dados em treino e teste

dados_treino <- dataset[indices, ]
dados_teste <- dataset[-indices, ]

# Verificando quantidade de observações há em cada partição

paste("Observações nos dados de treinamento:", nrow(dados_treino))
paste("Observações nos dados de teste:", nrow(dados_teste))

# --- 04 Usando o pacote "caret", treinar os modelos: Random Forest (rf), SVM (svmRadial), Redes Neurais (neuralnet) e o modelo alométrico de SPURR ---
# O modelo alométrico é dado por: Volume = b0 + b1 * dap ^ 2 * Ht

# Treinando os modelos

log("Treinando modelo Random Forest")
rf <- train(VOL ~ ., data = dados_treino, method = "rf")

log("Treinando modelo SVM")
svm <- train(VOL ~ ., data = dados_treino, method = "svmRadial")

log("Treinando modelo Neural Network")
rna <- train(VOL ~ ., data = dados_treino, method = "neuralnet")

log("Treinando modelo Alométrico de SPURR")
alom <- nls(VOL ~ b0 + b1 * (DAP ^ 2) * HT, data = dados_treino, start = list(b0 = 0.5, b1 = 0.5))

# --- 05 Efetue as predições nos dados de teste

log("Realizando predições")
predicoes_rf <- predict(rf, dados_teste)
predicoes_svm <- predict(svm, dados_teste)
predicoes_rna <- predict(rna, dados_teste)
predicoes_alom <- predict(alom, dados_teste)

# --- 06 Crie suas próprias funções (UDF) e calcule as seguintes métricas entre a predição e os dados observados ---

# Função para cálculo do coeficiente de determinação R2
calcular_coef_r2 <- function(observacoes, predicoes) {
  return(1 - sum((observacoes - predicoes) ^ 2) / sum((observacoes - mean(observacoes)) ^ 2))
}

# Função para erro padrão de estimativ: Syx
calcular_erro_syx <- function(observacoes, predicoes) {
  return(sqrt(sum((observacoes - predicoes) ^ 2) / (length(observacoes) - 2)))
}

# Função para o calculo da porcentagem de erro Syx
calcular_erro_syx_percent <- function(observacoes, predicoes) {
  return((calcular_erro_syx(observacoes, predicoes) / mean(observacoes)) * 100)
}

# Função para calcular um score com base no valor de R2 e Syx
calcular_score <- function(r2, syx) {
  return((r2 + (1 - syx)) / 2)
}

# Função para retornar as metricas de avaliação
calcular_metricas <- function(observacoes, predicoes, nome_modelo) {
  r2 <- calcular_coef_r2(observacoes, predicoes)
  syx <- calcular_erro_syx(observacoes, predicoes)
  syx_percent <- calcular_erro_syx_percent(observacoes, predicoes)
  score <- calcular_score(r2, syx_percent / 100)
  
  return(data.frame(model = nome_modelo, r2 = r2, syx = syx, syxPercentage = syx_percent, score = score))
}

# --- 07 Escolha o melhor modelo ---

log("Calculando métricas para os modelos")

metricas_df <- calcular_metricas(dados_teste$VOL, predicoes_rf, "rf")
metricas_df <- rbind(metricas_df, calcular_metricas(dados_teste$VOL, predicoes_svm, "svm"))
metricas_df <- rbind(metricas_df, calcular_metricas(dados_teste$VOL, predicoes_rna, "rna"))
metricas_df <- rbind(metricas_df, calcular_metricas(dados_teste$VOL, predicoes_alom, "alom"))

metricas_df <- metricas_df[order(metricas_df$score, decreasing=TRUE), ]

log("Métricas com base no score (melhor para o pior):")
print(metricas_df)