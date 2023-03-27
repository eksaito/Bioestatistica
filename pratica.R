## Trabalho 
#
# Script de exemplo para o trabalho final de bioestat�stica 2022.2
# - Testes estat�sticos
# - Analise de regress�o linear
# - Analise de regress�o log�stica
#
# Eric Kauati Saito
# 28/10/2022

# Carregar dados do arquivo

dados <- read.table(file="D:\\Macae Disciplinas\\2022\\R\\healthcare-dataset-stroke-data.csv", sep=",", header=T)

# Trabalhando com os dados
## Primeiras an�lises
dados_reduzidos = dados[1:400,]

## Testes estat�sticos

### Intervalo de confian�a
n_hipertenso = dados_reduzidos[dados_reduzidos$hypertension==0,]
hipertenso = dados_reduzidos[dados_reduzidos$hypertension==1,]

t.test(hipertenso$avg_glucose_level, conf.level = 0.9) #Intervalo de confian�a para 90%

### teste t
t.test(n_hipertenso$avg_glucose_level, hipertenso$avg_glucose_level, alternative = "two.sided") #less, greater
#ou ent�o, podiamos ter feito sem precisar separar as colunas
#t.test(dados_reduzidos$avg_glucose_level ~ dados_reduzidos$hypertension, alternative = "two.sided")

boxplot(dados_reduzidos$avg_glucose_level ~ dados_reduzidos$hypertension,
        main = "N�vel de glicose e Hipertens�o",
        ylab = "N�vel de glicose (mg/dL)",
        xlab= "Hipertens�o",
        names = c('N�o', 'Sim'),
        col = c('orange','blue'))

### teste t pareado
# N�o vai funcionar porque o tamanho das amostras s�o diferentes (e n�o s�o dados pareados)
# t.test(hipertenso$avg_glucose_level, n_hipertenso$avg_glucose_level, alternative = "two.sided", paired = TRUE)


### Wilcoxon rank sum test
# Testes n�o-param�trico para duas amostras INDEPENDENTES
# Wilcoxon signed-rank test -> n�o param�trico, pareado
wilcox.test(hipertenso$avg_glucose_level, n_hipertenso$avg_glucose_level, alternative = "two.sided")

### ANOVA
#bmi -> erro
oneway.test(age ~ smoking_status, data=dados_reduzidos, var.equal=T)
#### post-hoc
model <- aov(age ~ smoking_status, data=dados_reduzidos)
TukeyHSD(model, conf.level=.95)

### Teste para 2 propor��es
tabela = table(dados_reduzidos$hypertension, dados_reduzidos$Residence_type)
n_hipertensos = c(41,42)
residencia = c(194,206) #41+153, 42+164

prop.test(n_hipertensos, residencia, alternative = "two.sided",correct = FALSE)

barplot(tabela, beside = TRUE,
        legend = c("Press�o normal","Press�o alta"),
        ylim=c(0,200))

## An�lise de Regress�o
idade = c(21,19,27,23,20,31,30,37,36,40,39)
tempo = c(96,106,98,110,140,116,109,112,118,113,127)
### An�lise de Regress�o Linear

#### Gr�fico de dispers�o
plot(idade, tempo,
     ylim = c(85, 145),
     xlim = c(15, 45),
     main = "Tempo de rea��o ao est�mulo",
     xlab="Idade (ano)",
     ylab = "Tempo (s)")

boxplot(tempo)

tempo2 = tempo[-5]
idade2 = idade[-5]
#tabela = tabela[-c(5,7)]

remover = (tempo == 140)&(idade==20)
tempo2 = tempo[!remover]
idade2 = idade[!remover]
#tabela = tabela[!remover]

plot(idade2, tempo2,
     ylim = c(85, 145),
     xlim = c(15, 45),
     main = "Tempo de rea��o ao est�mulo",
     xlab = "Idade (ano)",
     ylab = "Tempo (s)")

#### Modelo 
modelo = lm(tempo2~idade2) #Tempo em rela��o a idade
#modelo = lm(avg_glucose_level ~ age, data = dados_reduzidos)

plot(idade2, tempo2,
     ylim = c(85, 145),
     xlim = c(15, 45),
     main = "Tempo de rea��o ao est�mulo",
     xlab = "Idade (ano)",
     ylab = "Tempo (s)")
abline(modelo) #Adiciona a reta do modelo de regress�o linear

summary(modelo)

cor(idade2,tempo2) #Correla��o de Pearson

### An�lise de Regress�o Log�stica

plot(dados_reduzidos$age, dados_reduzidos$heart_disease,
     xlab = "Idade (ano)",
     ylab = "Doen�a Card�aca")

modelo_log = glm(heart_disease ~ age, data = dados_reduzidos, family = binomial)
summary(modelo_log)

dados_plot = data.frame(age=seq(0, 80))
dados_plot$heart_disease = predict(modelo_log, dados_plot, type = 'response')

plot(dados_reduzidos$age, dados_reduzidos$heart_disease,
     xlab = "Idade (ano)",
     ylab = "Doen�a Card�aca")
lines(dados_plot$age, dados_plot$heart_disease)