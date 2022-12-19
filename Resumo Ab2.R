###Intervalo de Confiança com Variancia Conhecida

rm(list = ls())

#Uma máquina produz rolamentos que apresentam desvio padrão de 0,042 polegadas 
#em seudiâmetro. Desejando-se conhecer o diâmetro médio dos rolamentos produzidos 
#por esta máquina, extraiu-seuma amostra de 100 rolamentos, observando-se uma 
#média igual a 0,824 polegadas. Obter o intervalo com 0,90 de confiança para o verdadeiro 
#diâmetro médio dos rolamentos.

desvio_padrao = 0.042
n = 100 #numero de amostras
media = 0.824 #media amostral
intervalo_confiança = 0.90
alpha = 1 - intervalo_confiança

zc = qnorm(1 - alpha/2, 0, 1)
zc = round(zc, 2)

erro = zc * desvio_padrao / sqrt(n)
erro = round(erro, 5)

#A media populacional esta nesse intervalo de confiança:
cat("[", media - erro, "," , media + erro ,"]")

###Intervalo de Confiança com variancia desconhecida###

#Tem q usar o devio padrao amostral em vez do desvio padrao populacional#
#ultiliza a distribuição t Student

#Em um processo para obtenção de compostos químicos de tintas, 
#obteve-se os seguintestempos: 90, 92, 92, 95, 98, 99, 100, 100, 100 e 117 segundos.
#a) Construir um intervalo com nível confiança de 95%.

amostra <- c(90, 92, 92, 95, 98, 99, 100, 100, 100, 117)
alpha = 0.05

n = length(amostra)
desvio = sd(amostra)
media = mean(amostra)

tc = qt(p = 1- alpha/2, df = n-1)#Distribuição de t studant   ##df = graus de liberdade
tc = round(tc, 2)

erro = tc * desvio / sqrt(n)
erro = round(erro, 3)

cat("[", media - erro, ",", media + erro, "]")

#outra alternativa e bem mais facil é a função t.test()
t.test(amostra, conf = 0.95)


###Proporção amostral###
#Considere testes de vazamentos identificados em dois municípios A e B.
#No município Aforam examinados 500 edifícios e em 100 apresentaram falhas.
#No B foram examinados 1000 edifícios e em 300 apresentaram falhas. 
#Construir um intervalo com nível confiança de 95% para o município B.#

#municipio A
alpha = 0.05
n = 500
p = 100/500   #numero de falhas da amostra sobre o numero total de amostras

zc = qnorm(1 - alpha / 2, 0, 1)
zc = round(zc, 2)

erro = zc * sqrt(p * (1 - p) / n)
erro = round(erro, 2)

cat("[", p - erro, ",", p + erro, "]")

#or
prop.test(x = 100, n = 500, conf.level = 0.95)
prop.test(x = 300, n = 1000, conf.level = 0.95)

###Descobrir tamanho da amostra exemplo###
#b tamanho da amostra para 95 de confiança e erro 0.08

alpha = 0.05
zc = qnorm(1- alpha/2,0, 1)
zc = round(zc, 2)

n = (zc * desvio / 0.08)^2
n




###Teste de Hipotese com variancia conhecida###

##Bilineares

#De uma população, com distribuição normal com média 45 e variância 36,
#tira-se uma amostraaleatória de tamanho 16, obtendo-se uma média de 43. 
#Ao nível de significância de 10%, testaras hipóteses:
media = 45
varriancia = 36
desvio = sqrt(36)
n = 16
media_n = 43
alpha = 0.1

z_critico = qnorm(1 - alpha/2, 0, 1)
z_critico

z_calculado = (media_n - media) / (desvio / sqrt(n))
z_calculado

##Monocaudal
media = 206
desvio = 12
n = 30
media_n = 210
alpha = 0.1

z_critico = qnorm(1-alpha, 0 ,1)
z_critico

z_calculado = (media_n - media) / (desvio / sqrt(n)) #pode afirmar que a resistencia foi aumentada? Resposta: SIm
z_calculado


###Teste de Hipotese com variancia desconhecida###
media = 115
n = 20
media_n = 118
desvio_n = 20
alpha = 0.05

t_critico = qt(p = 1- alpha/2, df = n-1)
t_critico

t_calculado = (media_n - media) / (desvio_n / sqrt(n))
t_calculado

# Outro exemplo

n = 16
variancia = 36
dp = sqrt(variancia)
mediaPopulacao = 45
media = 43
alfa = 0.1

z = (media - mediaPopulacao) / (dp / sqrt(n))
z

#or
valores <- c(125, 124, 125, 125, 125, 125, 124, 123, 122, 123, 123, 123, 123, 124, 124)

t.test(valores, conf = 0.95, mu = 127) #mu = media esperada


###Teste de Hipotese para proporção###
n = 300
confirmados = 160
p = 0.6
p1 = round(confirmados/n,2)
alpha = 0.05

z_calculado = (p1 - p) / sqrt(p * (1 - p) / n)
z_calculado

z_critico = qnorm(1 - alpha/2, 0, 1)
z_critico

pvalor = pnorm(z_calculado) * 2 #multiplica por dois caso seja bicaldal
pvalor

###Teste de Hipotese para proporção unicaudal###
n = 100
confirmados = 15
p = 0.2
p1 = round(confirmados/n,2)
alpha = 0.01

z_calculado = (p1 - p) / sqrt(p * (1 - p) / n)
z_calculado

z_critico = qnorm(1 - alpha/2, 0, 1)
z_critico

pvalor = pnorm(z_calculado) #multiplica por dois caso seja bicaudal
pvalor

###Teste de Hipotese para duas variancias###
cedrinho <- c(40.42, 32.64, 45.67, 41.62, 45.08, 34.73, 32.58, 38.96)
itauba <- c(54.07, 45.92, 44.10, 39.36, 38.46, 40.20, 40.93, 45.24)
var.test(cedrinho, itauba)      # Variâncias iguais
## Variancias desiguais
cedrinho <- c(40.42, 32.64, 45.67, 41.62, 45.08, 34.73, 32.58, 38.96)
itauba <- c(54.07, 45.92, 44.10, 39.36, 38.46, 40.20, 40.93, 45.24)
var.test(cedrinho, itauba, var.equal=T)      # Variâncias iguais

###Teste de Hipotese para duas medias###
## pareadas
antigo <- c(89, 84, 96, 82, 74, 92, 85, 91)
novo <- c(83, 83, 92, 84, 76, 91, 80, 91)

t.test(antigo, novo, mu=0, paired = TRUE, conf.level = 0.90, alternative = "greater")

# Exemplo 2 # As velocidades são iguais?

A <- c(22, 21, 28, 30, 33, 33, 26, 24, 31, 22)
B <- c(25, 28, 26, 36, 32, 39, 28, 33, 30, 27)
t.test(A, B, mu = 0, paired = TRUE, conf = 0.95)



###Teste de Hipotese para duas proporcoes###

n1 = 200
p1 = 0.3
n2 = 250
p2 = 0.38

prop.test(x = c(n1*p1, n2*p2), n = c(n1, n2), conf.level = 0.9, alternative = "two.sided")

###Teste de normalidade###  Testa se a amostra tem distribuição normal (H0: tem dist. normal)

cedrinho <- c(40.42, 32.64, 45.67, 41.62, 45.08, 34.73, 32.58, 38.96)
shapiro.test(cedrinho)
# Dependendo da taxa de significância, cedrinho tem ou não distribuição normal

### Analise de variância

l1 <- c(12, 10, 10, 15, 10, 13, 11, 12, 11, 13.29)
l2 <- c(10, 11, 12, 5, 11, 14, 8, 9, 11, 12.29)
l3 <- c(18, 15, 14, 17, 16, 18, 17, 15, 17, 16.29)
l4 <- c(10, 15, 10, 13, 12, 11, 12, 11, 13, 15.29)

tabelaJac <- data.frame(l1, l2, l3, l4)


### Correlação

# Exemplo de dataset .csv (só copiar e colar as colunas em variáveis)
pesoTotal <- c(10.47, 19.85, 21.25, 24.36, 27.38, 28.09, 33.61, 35.73, 38.33, 49.14)
pesoPapel <- c(2.43,  5.12, 6.88, 6.22, 8.84, 8.76, 7.54, 8.47, 9.55, 11.43)

tabelaCor <- data.frame(pesoTotal, pesoPapel)
cor (pesoTotal, pesoPapel)

### Outro exemplo
x = c(11.20,8.60,11.00,9.80,11.00,14.00,6.00,4.00,12.00,7.40,10.80,14.00)
y = c(9.50,6.60,7.60,8.80,8.30,9.90,7.25,4.16,10.80,4.50,8.25,28/3)
z = c(8.25,5.76,7.70,8.84,8.47,7.22,5.75,10.50,5.50,7.90,6.58,28/3)

tabelaCor <- data.frame(x, y, z)
cor(tabelaCor)


### Regressão linear 
# Exemplo de correlação
x = c(11.20,8.60,11.00,9.80,11.00,14.00,6.00,4.00,12.00,7.40,10.80,14.00)
y = c(9.50,6.60,7.60,8.80,8.30,9.90,7.25,4.16,10.80,4.50,8.25,28/3)
z = c(8.25,5.76,7.70,8.84,8.47,7.22,5.75,10.50,5.50,7.90,6.58,28/3)

tabelaCor <- data.frame(x, y, z)
cor(tabelaCor)
# Sabendo que a maior correlação é entre x e y, faremos a equação da reta
regressao = lm(x ~ y, data = tabelaCor)
regressao # Y = 1.2506x + 0.0836
# Intercept: coeficiente linear(valor sozinho), ponto que intercepta o eixo y.


### Resíduos são aderentes?
# Mesmo exemplo (precisamos da equaçao da reta de correlacao)
shapiro.test(rstudent(regressao)) # usa a função rstudent na regressão pra descobrir se ela é normal ou não.

shapiro.test(x)
shapiro.test(y)
shapiro.test(z)
# Normal: p-value > 0.05

summary(regressao)
