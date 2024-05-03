##################################################################
#                              Media                             #
##################################################################

# Carregando a base de dados "salarios.RData"
load("salarios.RData")

# Calculando a media dos rendimentos dos maridos
mean(salarios$husearns)

# Calculando a media dos rendimentos das esposas
mean(salarios$earns)

# A media do rendimento dos maridos na amostra eh US$453,5406
# A media do rendimento das esposas na amostra eh US$232,833

# se dividirmos o rendimento medio dos maridos pelo das esposas
# temos
453.5406/232.833

# Portanto, o rendimento medio dos maridos eh quase o dobro do 
# rendimento medio das esposas (94,79% a mais)


##################################################################
#                              Mediana                           #
##################################################################

# Calculando a mediana dos rendimentos dos maridos
median(salarios$husearns)

# Calculando a mediana dos rendimentos das esposas
median(salarios$earns)

# A mediana do rendimento dos maridos eh de US$418,5
# A mediana do rendimento das esposas eh de US$185,0 

# Se dividirmos a mediana dos rendimentos dos maridos pelo das 
# esposas temos:
((418.5/185)-1)*100

# Em termos de mediana o rendimento dos maridos eh mais que o
# dobro do rendimento das esposas (126,22% a mais)


##################################################################
#                             Moda                               #
##################################################################

# Calculando as modas das idades dos maridos e esposas

# Para a idade dos maridos 
table(salarios$husage)
subset(table(salarios$husage), 
       table(salarios$husage) == 
         max(table(salarios$husage)))

# Para a idade das esposas
table(salarios$age)
subset(table(salarios$age), 
       table(salarios$age) == max(table(salarios$age)))

# A moda da idade dos maridos eh de 44 anos, com 201 pessoas 
# A moda da idade das esposa eh de 37 anos, com 217 pessoas

# Portanto, a moda da idade dos maridos eh maior que a das 
# esposas: 
44/37
# eh aproximadamente 18,92% maior.


##################################################################
#                             Variancia                          #
##################################################################

# Calculando a variancia dos rendimentos dos maridos
var(salarios$husearns)

# Calculando a variancia dos rendimentos das esposas
var(salarios$earns)

# A variancia do rendimento dos maridos eh US$165.639,1
# A variancia do rendimento das esposas eh US$69.340,83

# Se dividirmos a variancia dos rendimentos dos maridos pela
# das esposas:
((165639.1/69340.83)-1)*100

# Portanto, a variancia do rendimento dos maridos eh mais que o
# dobro (2X) a variancia dos rendimentos das esposas (138,88% a
# mais)

#################################################################
#                         Desvio Padrao                         #
#################################################################

# Calculando o desvio padrao dos rendimentos dos maridos:
sd(salarios$husearns)

# Calculando o desvio padr�o dos rendimentos das esposas:
sd(salarios$earns)

# O desvio padrao do rendimento dos maridos eh US$406,98 
# O desvio padrao do rendimento das esposas eh US$263,32

# Se dividirmos o desvio padrao dos rendimentos dos maridos pelo
# das esposas:
((406.9878/263.32650)-1)*100

# O desvio padrao dos rendimentos dos maridos eh mais que 50%
# superior ao das esposas (54,56% a mais)

#################################################################
#                    Coeficiente de Variacao                    #
#################################################################

# Calculando a media dos rendimentos dos maridos e guardando no
# objeto "meanM"
meanM <- mean(salarios$husearns)

# Calculando a media dos rendimentos das esposas e guardando no
# objeto "meanE"
meanE <- mean(salarios$earns)

# Calculando o desvio padrao dos rendimentos dos maridos e 
# guardando no objeto sdM
sdM <- sd(salarios$husearns)

# Calculando o desvio padrao dos rendimentos das esposas e
# guardando no objeto sdE
sdE <- sd(salarios$earns)

# Calculando o coeficiente de variacao dos rendimentos dos
# maridos:
cvM <- (sdM/meanM)*100
cvM
# O coeficiente de variacao do rendimento dos maridos eh 89,74%

# Calculando o coeficiente de variacao dos rendimentos das 
# esposas:
cvE <- (sdE/meanE)*100
cvE
# O coeficiente de variacao do rendimento das esposas eh 113,09%

# Isso quer dizer que o rendimento dos maridos e esposas variam
# muito na amostra. Ainda, pode-se concluir que os rendimentos
# das esposas variam mais que dos maridos.

# Quando o CV for:
# a) CV < 15% existe baixa dispersao: dados homogeneos
# b) 15% =< CV <= 30% existe media dispersao
# c) CV > 30% existe alta dispersao: dados heterogeneos