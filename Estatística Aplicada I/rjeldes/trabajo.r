
# Este eh um teste e intervalo de confian�a para grandes amostras

# Instalando os pacotes necessarios para os testes
install.packages("carData")
install.packages("datasets")
install.packages("BSDA")
install.packages("nortest")
install.packages("stats")
install.packages("rcompanion")
install.packages("dplyr")
install.packages("tigerstats")
install.packages("misty")

# Carregando os pacotes necessarios para os testes
library(carData)
library(datasets)
library(BSDA)
library(nortest)
library(stats)
library(rcompanion)
library(dplyr)
library(tigerstats)
library(misty)

install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("datarium")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("nortest")
install.packages("ggplot2")
library(ggplot2)

install.packages("car")

library(car)
library("dplyr")
library("ggpubr")
library (dplyr)
library(multcomp)
library(car)
library(nortest)

# Carregando a base de dados "salarios.RData", o R vai buscar o
# arquivo onde tiver setado como diretorio padrao

load("C:/Users/acade/OneDrive/Documentos/UFPR/Estadistica1/Bases de Dados Usadas nas Aulas Práticas/salarios.RData")

# Configurando para nao aparecer notacao cientifica nos
# resultados
options(scipen = 999)

# Setando uma semente para geracao de numeros aleatorios: 
# set.seed(1234)
#            1 Gráficos e tabelas
# Gerando o boxplot para a variavel "age" que eh a idade das
# esposas
Boxplot( ~ age, data=salarios, id=list(method="y"), 
         ylab="Idade das esposas")

# idade dos maridos
Boxplot( ~ husage, data=salarios, id=list(method="y"), 
         ylab="Idade dos maridos")

# Crear un nuevo dataframe combinando las edades de la esposa y del esposo
salarios_combined <- data.frame(
  Edad = c(salarios$age, salarios$husage),
  Grupo = rep(c("Esposa", "Esposo"), each = nrow(salarios))
)
salarios_combined
summary(salarios_combined)

# Gráfico de caja combinado
#   A ) Elaborar os gráficos box-plot e histograma das variáveis “age” (idade da esposa) e
#     “husage” (idade do marido) e comparar os resultados
Boxplot(Edad ~ Grupo, data = salarios_combined, col = c("skyblue", "lightgreen"),
        main = "Box-Plot de la Edad de la Esposa y del Esposo")


edades <- data.frame( 
  group = rep(c("Woman", "Man")),
  edad = c(salarios$age, salarios$husage)
)
head(edades)
summary(edades)

# Vamos visualizar os dados usando box-plots
# Plotaremos o "weight" por groupo
ggboxplot(edades, x = "group", y = "edad", 
          color = "group", palette=c("#00AFBB", "#E7B800"),
          ylab = "edades", xlab = "Groups")

Boxplot(edad ~ group, data = edades, col = c("skyblue", "lightgreen"),
        main = "Box-Plot de la Edad de la Esposa y del Esposo")

#Formka 2
bxp <- ggboxplot(edades, x = "group", y = "edad", 
                 add = "point")
bxp


# Os outliers podem ser identificados usando o box-plot ou pela
# funcao "identify_outliers" do pacote "rstatix".
edades %>%
  group_by(group) %>%
  identify_outliers(edad)


# HISTOGRAMA

# Crear histograma para ambas columnas husage y age en el mismo gráfico
hist(c(salarios$husage, salarios$age), 
     xlab = "Edad", 
     ylab = "Frecuencia",
     col = c("lightblue", "lightgreen"),
     main = "Histograma de Edades de Maridos y Esposas")

# Agregar leyenda
legend("topright", 
       legend = c("Maridos", "Esposas"),
       fill = c("lightblue", "lightgreen"))


# Calcular la frecuencia máxima
frecuencia_maxima <- max(hist(c(salarios$husage, salarios$age), plot = FALSE)$counts)

frecuencia_maxima

# Agregar texto con la frecuencia máxima a la izquierda del gráfico
text(x = 0, y = frecuencia_maxima, 
     labels = paste("Frecuencia máxima:", frecuencia_maxima),
     adj = c(0, -0.5), col = "red", font = 2)


###############################################################
#             Tabela de Distribuicao de frequencia            #
###############################################################

# Instalando pacote que gera a distribuicao de frequencia 
install.packages("fdth")

# Carregando pacote que calcula a distribuicao de frequencia
library(fdth)

nuevo_dataframe <- data.frame(husage = salarios$husage, age = salarios$age)
head(nuevo_dataframe)

# Calculando a distribuicao de frequencia e guardando no objeto
# "table"
table2 <- fdt(nuevo_dataframe)

# Mostrando a distribuicao de frequencia
print (table2)

################################################################





# Agora a funcao do "dplyr" imprime 1000 observacoes aleatoriamente
dplyr::sample_n(weight, 1000)

# Calculemos algumas estatisticas por grupo-contagem, media e sd:
result <- edades %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(edad, na.rm = TRUE),
    mediana = median(edad, na.rm = TRUE),
    varianza = var(edad, na.rm = TRUE),
    sd = sd(edad, na.rm = TRUE),
    coef_variacion = sd(edad, na.rm = TRUE) / mean(edad, na.rm = TRUE) * 100
  )

result


# de los maridos es 9.98761
sdH <- sd(salarios$age)
sdH
# esposas 11.22817
sdW <- sd(salarios$husage)
sdW


  ((11.22817 / 9.98761  ) -1) * 100
  # el SD de las esposas es un 12% mayor que al de los hombres 

#Valores maximos 
summary(nuevo_dataframe$age)
summary(salarios$husage)

#################################################################
#                    Coeficiente de Variacao                    #
#################################################################

# Calculando a media dos pesos dos mujeres e guardando no
# objeto "meanMujeres" 42.45296
meanMujeres <- mean(nuevo_dataframe$husage)
meanMujeres 
# Calculando a media dos pesos das maridos e guardando no
# objeto "meanEsposos" 39.42758
meanEsposos <- mean(nuevo_dataframe$age)
meanEsposos
# Calculando o desvio padrao dos pesos dos esposas e 
# guardando no objeto sdMujeres  11.22817
sdMujeres <- sd(salarios$husage)
sdMujeres
# Calculando o desvio padrao dos pesos das maridos e
# guardando no objeto sdEsposos 9.98761
sdEsposos <- sd(salarios$age)
sdEsposos
# Calculando o coeficiente de variacao dos pesos dos
# maridos:
cvMujeres <- (sdMujeres/meanMujeres)*100
cvMujeres
# O coeficiente de variacao do pesos dos mujeres eh 26.44%

# Calculando o coeficiente de variacao dos pesos das 
# esposos:
cvEsposos <- (sdEsposos/meanEsposos)*100
cvEsposos
# O coeficiente de variacao do pesos das esposas eh 25.33%

# noa tem muita variacion 

# Regra de bolso:
# Quando o CV for:
# a) CV < 15% existe baixa dispersao: dados homogeneos
# b) 15% =< CV <= 30% existe media dispersao
# c) CV > 30% existe alta dispersao: dados heterogeneos


edades
# Agora vamos separar as mulheres em outro objeto:
edad_wife <- edades %>% filter(group == "Woman")

edad_Men <- edades %>% filter(group == "Man")

head(edad_wife)

# Vamos calcular algumas estatisticas descritivas das mulheres
mean(edad_wife$edad) # media 41.05325
sd(edad_wife$edad) # desvio padrao   10.75169

# Vamos calcular algumas estatisticas descritivas das hombres
mean(edad_Men$edad) # media 40.8273
sd(edad_Men$edad) # desvio padrao   10.7134 


# Vamos gerar um histograma
hist(edad_wife$edad)
# Vamos plotar a curva normal sobre o histograma
plotNormalHistogram <- function(data, prob = FALSE,
                    main = "Normal Distribution overlay on Histogram"
                    
                    ) {
    
normalidad <- lillie.test(data$edad)
  
  # Crear el histograma
 histograma <- ggplot(data, aes(x = edad)) +
    geom_histogram(aes(y=..density..), bins = 30, color="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    ggtitle(main) +
    xlab("Datos") +
    ylab("Densidad") +
    theme_minimal()

print('normalidade ')    
print( normalidad$p.value)

  # Agregar la línea de la distribución normal si los datos son aproximadamente normales
  if(normalidad$p.value > 0.05) {
    print('iff')
    histograma <- histograma + 
      stat_function(fun = dnorm, args = list(mean = mean(data), sd = sd(data)), color="blue") +
      labs(title = paste(main, "(p-value de Lilliefors:", round(normalidad$p.value, 4), ")"))
  } else {
    histograma <- histograma + 
      labs(title = paste(main, "(Los datos no son normalmente distribuidos - p-value de Lilliefors:", round(normalidad$p.value, 4), ")"))
  }
  
  # Mostrar el gráfico
print(histograma)

}
dataWoman <- data.frame(edad = edad_wife$edad)
dataMen <- data.frame(edad = edad_Men$edad)
head(dataWoman)
head(dataMen)
# Llamar a la función con tus datos y argumentos adicionales
plotNormalHistogram(dataWoman, prob = FALSE,
                    main = "Normal Distribution overlay on Histogram"
                   )

#las edades de la mujer no son normalmente distribuidos 
plotNormalHistogram(dataMen, prob = FALSE,
                    main = "Normal Distribution overlay on Histogram"
                    )

# los datos tanto para hombres y mujkerres no estan Distribuidos normalmente 

salarios_combined
summary(salarios_combined)
head(salarios$husage)
mean(salarios$husage)
summary(salarios$husage)
summary(salarios$age)
summary(edad_Men)

# Vamos executar o teste de t para a media dos idade das mulheres
# comparando com um valor hipotetico de 41 que eh o idade dos 
# homens

# Hipoteses:
# H0: A media do idade das mulheres eh estatisticamente igual a 41 
# Ha: A media do idade das mulheres nao eh estatisticamente igual 
#     a 41
result
head(edad_wife)
t.test(edad_wife$edad, y = NULL,
       alternative = c("two.sided"),
       mu = 41, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, data=edad_wife)

# Resultado do intervalo de confian�a:
# Isso quer dizer que a media dos idade das mulheres vai variar
# entre 40.77244 e 41.33406, com uma media de 41, com 95% de 
# confianca ou 5% de significancia.

# Resultado para o teste da media:
# Valor da Estatistica: t = 0.37174, com 5633 graus de liberdade

# Confrontamos esse valor com o valor tabelado da estatistica t
# para 95% de confianca ou 5% de significancia (usamos isso para
# obter os valores tabelados, conforme comandos abaixo)

# Obtendo os valores tabelados de "t" de student
qt(0.975, df=5633) # cauda superior 1.960
qt(0.025, df=5633) # cauda inferior

# Vamos construir o grafico
ptGC(c(-1.96,1.96),region="outside",df=5633, graph = TRUE)

# Como a estistica t calculada (0.37174) eh menor que a 
# estatistica tabelada (-1,96), portanto o valor de 0.37174 se
# encontra na regiao de rejeicao.
# Sendo assim, no rejeitamos a hipotese (H0) de que o valor
# verdadeiro da media eh estatisticamente igual a 41 (idade 
# dos homens).


# Mas, se a estatistica t calculada (no resultado do teste) se
# situasse entre -1.96 e 1.96 (valor tabelado) nao rejeitariamos
# H0. Neste caso, poderiamos considerar que a media da variavel
# seria estatisticamente igual a 41,00 com 95% de confianca ou
# 5% de significancia.



ci.sd(edad_wife$edad, method = "chisq")

View(edades)
dplyr::sample_n(weight, 1000)

with(dplyr::sample_n(edades, 5000), shapiro.test(edad[group == "Woman"]))     

with(dplyr::sample_n(edades, 5000), shapiro.test(edad[group == "Man"]))     

#para ambos casos es menor a 0,05 lo que indica que no estan normalmente distribuidos
























































































# Queremos saber se o peso mediano das mulheres difere do peso
# mediano dos homens.

# Vamos primeiro calcular um sumario estatistico
group_by(weight, group) %>%
  summarise(
    count = n(),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )


# Vamos visualizar os dados usando box-plots
# Plotaremos o "weight" por groupo
ggboxplot(weight, x = "group", y = "weight", 
          color = "group", palette=c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")
