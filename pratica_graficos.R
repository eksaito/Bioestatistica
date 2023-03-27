## Trabalho 
#
# Script de exemplo para o trabalho final de bioestatística 2022.2
# - Gráficos
#
# Eric Kauati Saito
# 31/10/2022

# Carregar dados do arquivo

dados <- read.table(file="G:\\Outros computadores\\My Computer\\Macae Disciplinas\\2022_2\\R\\healthcare-dataset-stroke-data.csv", sep=",", header=T)

# Trabalhando com os dados
## Primeiras análises

# percebam que eu nomeie meus dados como "dados"
dados_reduzidos = dados[1:400,]
#cada nome que eu coloquei nesse programa, das variáveis, das colunas da tabela, são todas dos dados que eu estou utilizando
glicose = dados_reduzidos$avg_glucose_level #Podemos separar uma coluna da nossa tabela em uma nova variável

### Estatísticas
mean(dados_reduzidos$avg_glucose_level)
var(dados_reduzidos$avg_glucose_level)
sd(dados_reduzidos$avg_glucose_level)

media_glicose = mean(dados_reduzidos$avg_glucose_level) #salvando em uma varíavel que pode ser utilizada depois, por exemplo media_glicose + 10

summary(dados_reduzidos$avg_glucose_level) #5 estatísticas

## Gráficos
### Gráfico de Barras
table(dados_reduzidos$hypertension, dados_reduzidos$gender) #Agrupa todos em uma tabela

#Percebam que se queremos colocar mais de um grupo, precisamos utilizar table()
#Existem diversar formas de colocar legenda nos gráficos, uma forma é apresentada a seguir
#beside = coloca as barras um do lado do outro
#names = nome dos grupos
#col = cor
#ylim = limites do gráfixo no eixo y
barplot(table(dados_reduzidos$hypertension, dados_reduzidos$gender),
        beside = TRUE,
        names = c('Mulheres', "Homens"),
        col = c("orange","blue"),
        ylim = c(0,200)
        )

#para colocar a legenda precisamos primeiro criar o gráfico barplot()
#primeira parte da legenda é a posição que queremos colocar, nesse caso parte direita em cima
#legend = texto da legenda
#fill = coloca a cor na legenda (perceba que tem que ser o mesmo que foi utilizado no barplot)
#title = titulo da legenda, nem todas as vezes precisamos
#cex = tamanho da legenda
legend("topright", legend = c("Hipertenso","Sadios"),
       fill=c("orange","blue"), title = "Legenda", cex=1)

### Histograma
hist(dados_reduzidos$avg_glucose_level, breaks = 20,
     probability = TRUE,
     main = "Histograma do nível médio de glicose",
     xlab="Glicose (mg/dL)",
     ylab = "Frequência Relativa (%)"
     )

### Boxplot
boxplot(dados_reduzidos$avg_glucose_level,
        main = "Nível de glicose",
        ylab = "Nível de glicose (mg/dL)")

# Boxplot do nível de glicose em relação a hipertensão
boxplot(dados_reduzidos$avg_glucose_level ~ dados_reduzidos$hypertension,
        main = "Nível de glicose e Hipertensão",
        ylab = "Nível de glicose (mg/dL)",
        xlab= "Hipertensão",
        names = c('Não', 'Sim'),
        col = c('orange','blue')
        )

boxplot(dados_reduzidos$avg_glucose_level ~ dados_reduzidos$hypertension,
        main = "Nível de glicose e Hipertensão",
        ylab = "Nível de glicose (mg/dL)",
        xlab= "Hipertensão",
        names = c('Não', 'Sim'),
        col = c('orange','blue'),
        yaxt='n'
)
axis(side=2, at=seq(50, 300, by=20))

# boxplot(Dados$avg_glucose_level ~ Dados$stroke,
#         main = "Nível de glicose e Acidente Vascular Encefálico",
#         col = c("blue","orange"),
#         ylab = "Nível de glicose (mg/dL)",
#         xlab= "Ocorrência de Acidente Vascular Encefálico",
#         yaxt='n')
# axis(side=2, at=seq(50, 300, by=20))

## Análise de Regressão
idade = c(21,19,27,23,20,31,30,37,36,40,39)
tempo = c(96,106,98,110,140,116,109,112,118,113,127)
### Análise de Regressão Linear

#### Gráfico de dispersão
plot(idade, tempo,
     ylim = c(85, 145),
     xlim = c(15, 45),
     main = "Tempo de reação ao estímulo",
     xlab="Idade (ano)",
     ylab = "Tempo (s)")

#Retirando outlier, quando sabemos qual é a posição dele na lista de valores
#Estamos retirando o quinto valor na lista
tempo2 = tempo[-5]
idade2 = idade[-5]
#tabela = tabela[-c(5,7),] #Podemos retirar mais de um valor ao mesmo tempo de uma tabela


#Se a gente não sabe a posição na tabela, mas sabemos o valor que queremos retirar
#Podemos fazer o seguinte: 
#remover = (tempo == 140)&(idade==20)
#tempo2 = tempo[!remover]
#idade2 = idade[!remover]
#tabela = tabela[!remover,] #da mesma forma, podemos retirar de uma tabela

#Mais explicações de como retirar um outlier dos dados, me perguntem

plot(idade2, tempo2,
     ylim = c(85, 145),
     xlim = c(15, 45),
     main = "Tempo de reação ao estímulo",
     xlab = "Idade (ano)",
     ylab = "Tempo (s)")

plot(dados_reduzidos$avg_glucose_level, dados_reduzidos$bmi)
#Percebam que vai aparecer um error no console
#Isso acontece porque se vocês olharem a columa do bmi, vocês vão ver valores faltando
#para isso, é preciso remover eles
dados_reduzidos[dados_reduzidos$bmi=="N/A",] = NA
dados_reduzidos = dados_reduzidos[complete.cases(dados_reduzidos),]

plot(dados_reduzidos$avg_glucose_level, dados_reduzidos$bmi,
     ylab = "IMC",
     xlab = "Nivel de Glicose",
     main = "Relação do nível de glicose e IMC")


### Análise de Regressão Logística
plot(dados_reduzidos$age, dados_reduzidos$heart_disease,
     xlab = "Idade (ano)",
     ylab = "Doença Cardíaca")
