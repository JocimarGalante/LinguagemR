##########################
## 1 Gráficos e tabelas ##
##########################

# a)(15 pontos) Elaborar os gráficos box-plot e histograma das variáveis “age” (idade da esposa) e “husage” (idade do marido) e comparar os resultados

# instalando pacotes boxplot e distribuicao de frequencia
install.packages("car")
install.packages("fdth")

# carregando pacotes
library (car)
library(fdth)

# carregando base de dados salarios nativo do R
load("salarios.RData")

# visualizando estatisticas e informacoes das variaveis
summary(salarios)

# histograma com metodo sturges da variavel "age" com 8 quebras (idade da esposa)
hist(salarios$age,  
     main = "Histograma Esposa", 
     xlab = "Idade da Esposa", ylab = "Freq. Absoluta", 
     breaks = 8,
     col = c("violet"), 
     border = FALSE, 
     xlim = c(0,90), ylim = c(0,1000),
     labels = TRUE)

# histograma com metodo sturges da variavel "husage" com 8 quebras (idade do marido)
hist(salarios$husage,  
     main = "Histograma Marido", 
     xlab = "Idade do Marido", ylab = "Freq. Absoluta",
     breaks = 8,
     col = c("blue"), 
     border = FALSE, 
     xlim = c(0,100), ylim = c(0,2000),
     labels = TRUE)

# histogramas sobrepostos
h1<-hist(salarios$age)
h2<-hist(salarios$husage)
plot(h1, col=rgb(0,0,1,1/4),
     main = "Idades das Esposas e Maridos",
     xlab = "Idades",
     ylim =c(min(min(h1$counts), min(h2$counts)), max(max(h1$counts, max(h2$counts)))),
     xlim=c(min(min(h1$breaks), min(h2$breaks)), max(max(h1$breaks, max(h2$breaks)))))
plot(h2, col=rgb(1,0,0,1/4), add=T) 


# Gerando o boxplot para a variavel "age" (idade das esposas)
Boxplot( ~ age, data=salarios, id=list(method="y"), 
         ylab="Idade das esposas",
         las = 1,
         col = "pink",
         notch=FALSE,
         boxwex = 0.8)
         # Add linha horizontal a media
         abline(h = mean(salarios$age),
                col = "blue",
                lwd = 1)
         # Add texto acima da linha da media
         text(y=mean(salarios$age)+2,
              x=0.6,
              paste("Mean:", round(mean(salarios$age),1)),
              col = "blue",
              cex = 0.8,
              pos = 4)
         # Add rotulos para os limites dos quartiles
         text(y=boxplot.stats(salarios$age)$stats,
              labels = round(boxplot.stats(salarios$age)$stats,0),
              x = 1.25,
              cex = 0.8,
              col ="red")

# Gerando o boxplot para a variavel "husage" (idade dos maridos)
Boxplot( ~ husage, data=salarios, id=list(method="y"), 
         ylab="Idade dos maridos",
         las = 1,
         col = "blue",
         notch=FALSE,
         boxwex = 0.8)
         abline(h = mean(salarios$husage),
                col = "green",
                lwd = 1)
         text(y=mean(salarios$husage)+2,
              x=0.5,
              paste("Mean:", round(mean(salarios$husage),1)),
              col = "green",
              cex = 0.8,
              pos = 4)
         text(y=boxplot.stats(salarios$husage)$stats,
              labels = round(boxplot.stats(salarios$husage)$stats,0),
              x = 1.25,
              cex = 0.8,
              col ="red")
         text(y=boxplot.stats(salarios$husage)$out,
              labels = round(boxplot.stats(salarios$husage)$out,0),
              x = 1.1,
              cex = 0.6,
              col ="blue")
        

# b)(15 pontos) Elaborar a tabela de frequencias das variáveis “age” (idade da esposa) e “husage” (idade do marido) e comparar os resultados
         
tableage <- fdt(salarios$age)
tablehusage <- fdt(salarios$husage)
         
# Mostrando a distribuicao de frequencia
print (tableage) 
print (tablehusage)


######################################
## 2 Medidas de posição e dispersao ##
######################################

#a) (15 pontos) Calcular a média, mediana e moda das variáveis “age” (idade da esposa) e “husage” (idade do marido) e comparar os resultados

## Media ##
# Calculando a media da idade das esposas
trunc(mean(salarios$age))
# Calculando a media da idade dos maridos
trunc(mean(salarios$husage))

# A media de idade das esposas na amostra e de 39 anos
# A media de idade dos maridos na amostra e de 42 anos


## Mediana ##
# Calculando a mediana de idade das esposas
median(salarios$age)
# Calculando a mediana de idade dos maridos
median(salarios$husage)

# A mediana de idade das esposas na amostra e de 39 anos
# A mediana de idade dos maridos na amostra e de 41 anos


## Moda ##
# Calculando as modas das idades das esposas e dos maridos
# Idade das esposas
table(salarios$age)
subset(table(salarios$age), 
       table(salarios$age) == max(table(salarios$age)))
# Idade dos maridos 
table(salarios$husage)
subset(table(salarios$husage), 
       table(salarios$husage) == max(table(salarios$husage)))

# A moda da idade das esposa e de 37 anos, com 217 pessoas
# A moda da idade dos maridos e de 44 anos, com 201 pessoas 



#b) (15 pontos) Calcular a variância, desvio padrão e coeficiente de variação das variáveis “age” (idade da esposa) e “husage” (idade do marido) e comparar os resultados

## Variancia ##
# Calculando a variancia da idade das esposas
var(salarios$age)
# Calculando a variancia da idade dos maridos
var(salarios$husage)

# A variancia da idade das esposas e 99.75
# A variancia da idade dos maridos e 126.07


## Desvio padrao ##
# Calculando o desvio padrao da idade das esposas:
sd(salarios$age)
# Calculando o desvio padrao da idade dos maridos:
sd(salarios$husage)

# O desvio padrao da idade das esposas e 9.98
# O desvio padrao da idade dos maridos e 11.22


## coef de variacao ##
# Calculando a media de idade das esposas e armazenando
meanIdadeE <- mean(salarios$age)
# Calculando a media de idade dos maridos e armazenando
meanIdadeM <- mean(salarios$husage)

# Calculando o desvio padrao de idade das esposas e armazenando
sdIdadeE <- sd(salarios$age)
# Calculando o desvio padrao de idade dos maridos e armazenando
sdIdadeM <- sd(salarios$husage)

# Calculando o coef de variacao de idade das esposas
cvIdadeE <- (sdIdadeE/meanIdadeE)*100
cvIdadeE

# Calculando o coeficiente de variacao de idade dos maridos
cvIdadeM <- (sdIdadeM/meanIdadeM)*100
cvIdadeM

# O coeficiente de variacao de idade das esposas e 25.33
# O coeficiente de variacao de idade dos maridos e 26.44

# CV <= 15% baixa dispersao -> dados homogeneos
# CV entre 15 e 30% media dispersao
# CV > 30% alta dispersao -> dados heterogeneos