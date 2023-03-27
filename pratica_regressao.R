## Trabalho 
#
# Script de exemplo para o trabalho final de bioestat?stica 2022.2
# - An?lise de Regress?o Linear
# - An?lise de Regress?o Log?stica
#
# Eric Kauati Saito
# 31/10/2022

# Carregar dados do arquivo

dados <- read.table(file="healthcare-dataset-stroke-data.csv", sep=",", header=T)

# Trabalhando com os dados
## Primeiras an?lises
dados_reduzidos = dados[1:400,]

## An?lise de Regress?o
idade = c(21,19,27,23,20,31,30,37,36,40,39)
tempo = c(96,106,98,110,140,116,109,112,118,113,127)
### An?lise de Regress?o Linear

#### Gr?fico de dispers?o
plot(idade, tempo,
     ylim = c(85, 145),
     xlim = c(15, 45),
     main = "Tempo de rea??o ao est?mulo",
     xlab="Idade (ano)",
     ylab = "Tempo (s)")


#### Removendo outliers
#tempo2 = tempo[-5]
#idade2 = idade[-5]
#tabela = tabela[-c(5,7)]

remover = (tempo == 140)&(idade==20)
tempo2 = tempo[!remover]
idade2 = idade[!remover]
#tabela = tabela[!remover]

plot(idade2, tempo2,
     ylim = c(85, 145),
     xlim = c(15, 45),
     main = "Tempo de rea??o ao est?mulo",
     xlab = "Idade (ano)",
     ylab = "Tempo (s)")

#### Modelo 
modelo = lm(tempo2~idade2) #Tempo em rela??o a idade

#Para fazer em uma tabela direto, podemos fazer da seguinte forma
#Relacionando a idade com o n?vel de glicose, nos nosso dados reduzidos
#modelo = lm(avg_glucose_level ~ age, data = dados_reduzidos)

plot(idade2, tempo2,
     ylim = c(85, 145),
     xlim = c(15, 45),
     main = "Tempo de rea??o ao est?mulo",
     xlab = "Idade (ano)",
     ylab = "Tempo (s)")
abline(modelo) #Adiciona a reta do modelo de regress?o linear

#Resultados do modelo
summary(modelo)# O que significam esses resultados?

cor(idade2,tempo2) #Correla??o de Pearson
cor(idade2,tempo2)^2 #O que seria esse valor? O que significa esse resultado

#No manual tem as analises para avaliar o modelo de regress?o linear

### An?lise de Regress?o Log?stica
plot(dados_reduzidos$age, dados_reduzidos$heart_disease,
     xlab = "Idade (ano)",
     ylab = "Doen?a Card?aca")

#Modelo de regress?o log?stica
modelo_log = glm(heart_disease ~ age, data = dados_reduzidos, family = binomial)

#Resultado do modelo
summary(modelo_log) #O que significam esses resultados?

#Para podermos colocar no gr?fico o resultado e verificar os diferentes valores
#de probabilidade por idade, precisamos criar uma tabela com idade de 0 a 80
dados_plot = data.frame(age=seq(0, 80, by = 1)) #Veja que eu coloquei o nome da minha variável age
#e precisamos aplicar nosso modelo nessa tabela com as idades
dados_plot$heart_disease = predict(modelo_log, dados_plot, type = 'response')

#Agora podemos fazer nosso gr?fico com os resultados do nosso modelo
plot(dados_reduzidos$age, dados_reduzidos$heart_disease,
     xlab = "Idade (ano)",
     ylab = "Doen?a Card?aca")
lines(dados_plot$age, dados_plot$heart_disease)#Adiciona a linha do modelo de regress?o log?stica

#Analisando a tabela, podemos ver as diferentes idades e a probabilidade correspondente
dados_plot
