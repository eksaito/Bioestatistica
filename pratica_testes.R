## Trabalho 
#
# Script de exemplo para o trabalho final de bioestat�stica 2022.2
# - Testes estat�sticos
#
# Eric Kauati Saito
# 31/10/2022

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