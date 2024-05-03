# endereco de diretorio da maquina)
setwd("C:/Users/jocim/Documents/Pós Graduação UFPR/LinguagemR/Estatística Aplicada I")

# Instalando o pacote para ler arquivos excel
install.packages("readxl")

# Carregando o pacote que le o arquivo excel. Toda vez que 
library(readxl)

# Instalando pacote manipula bases de dados (esse 
# pacote funciona junto ao pacote - abaixo - 
# ggplot2 que gera graficos)
 install.packages("tidyverse")

# Instalando pacote que produz graficos
 install.packages("ggplot2")

# Carregando os pacotes
library(ggplot2)
library(tidyverse)

# Instalando pacote
 install.packages("plotly")

# Carregando pacote que gera o grafico
library(plotly)

################################################################
#                        Histograma                            #   
################################################################

# Vamos utilizar o dataset "salarios.RData" que eh em formato
# nativo do R

# Carregando a base de dados "salarios" em formato R
load("salarios.RData")

# visualizando algumas estatisticas e informacoes das variaveis
summary(salarios)

# Vamos desenhar o histograma com o metodo de sturges (para a 
# quantidade de quebras) da variavel "husage" que eh a idade
# dos maridos
hist(salarios$husage, xlab="Idade dos Maridos", 
     ylab = "Frequency")

# Histograma para 5 quebras com a mesma variavel
hist(salarios$husage, breaks = 5, 
     xlab = "Idades dos Maridos",ylab = "Frequency")

# Vamos desenhar o histograma com o metodo de sturges (para a 
# quantidade de quebras) da variavel "age" que eh a idade
# dos maridos
hist(salarios$age, xlab="Idade das Esposas", 
     ylab = "Frequency")

# Histograma para 5 quebras com a mesma variavel
hist(salarios$age, breaks = 5, 
     xlab = "Idades das Esposas",ylab = "Frequency")

################################################################
#                           Boxplot                            #
################################################################

# Vamos continuar utilizando a base de dados "salarios"

# Instalando o pacote que gera o boxplot
 install.packages("car")

#Carregando o pacote que gera o boxplot
library (car)

# Gerando o boxplot para a variavel "age" que eh a idade das
# esposas
Boxplot( ~ age, data=salarios, id=list(method="y"), 
         ylab="Idade das esposas")

# Gerando o boxplot para a variavel "husage" que eh a 
# idade dos maridos
Boxplot( ~ husage, data=salarios, id=list(method="y"), 
         ylab="Idade dos maridos")
# Aqui vemos varios possiveis outliers na amostra, idades dos 
# maridos muito elevadas

###############################################################
#             Tabela de frequencia                            #
###############################################################

# Instalando pacote que gera a distribuicao de frequencia 
install.packages("fdth")

# Carregando pacote que calcula a distribuicao de frequencia
library(fdth)

# Calculando a distribuicao de frequencia e guardando no objeto
# "table"
tableAge <- fdt(salarios$age)

tableHusage <- fdt(salarios$husage)

# Mostrando a distribuicao de frequencia das Esposas
print (tableAge)

# Mostrando a distribuicao de frequencia dos Maridos
print (tableHusage)