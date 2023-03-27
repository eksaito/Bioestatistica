## Trabalho 
#
# Script de exemplo para o trabalho final de bioestatística 2022.2
# - Testes estatísticos
#
# Eric Kauati Saito
# 31/10/2022

# Carregar dados do arquivo

dados <- read.table(file="D:\\Macae Disciplinas\\2022\\R\\healthcare-dataset-stroke-data.csv", sep=",", header=T)

# Trabalhando com os dados
## Primeiras análises
dados_reduzidos = dados[1:400,]

## Testes estatísticos

### Intervalo de confiança
n_hipertenso = dados_reduzidos[dados_reduzidos$hypertension==0,]
hipertenso = dados_reduzidos[dados_reduzidos$hypertension==1,]

t.test(hipertenso$avg_glucose_level, conf.level = 0.9) #Intervalo de confiança para 90%

### teste t
t.test(n_hipertenso$avg_glucose_level, hipertenso$avg_glucose_level, alternative = "two.sided") #less, greater
#ou então, podiamos ter feito sem precisar separar as colunas
#t.test(dados_reduzidos$avg_glucose_level ~ dados_reduzidos$hypertension, alternative = "two.sided")

boxplot(dados_reduzidos$avg_glucose_level ~ dados_reduzidos$hypertension,
        main = "Nível de glicose e Hipertensão",
        ylab = "Nível de glicose (mg/dL)",
        xlab= "Hipertensão",
        names = c('Não', 'Sim'),
        col = c('orange','blue'))

### teste t pareado
# Não vai funcionar porque o tamanho das amostras são diferentes (e não são dados pareados)
# t.test(hipertenso$avg_glucose_level, n_hipertenso$avg_glucose_level, alternative = "two.sided", paired = TRUE)


### Wilcoxon rank sum test
# Testes não-paramétrico para duas amostras INDEPENDENTES
# Wilcoxon signed-rank test -> não paramétrico, pareado
wilcox.test(hipertenso$avg_glucose_level, n_hipertenso$avg_glucose_level, alternative = "two.sided")

### ANOVA
#bmi -> erro
oneway.test(age ~ smoking_status, data=dados_reduzidos, var.equal=T)
#### post-hoc
model <- aov(age ~ smoking_status, data=dados_reduzidos)
TukeyHSD(model, conf.level=.95)

### Teste para 2 proporções
tabela = table(dados_reduzidos$hypertension, dados_reduzidos$Residence_type)
n_hipertensos = c(41,42)
residencia = c(194,206) #41+153, 42+164

prop.test(n_hipertensos, residencia, alternative = "two.sided",correct = FALSE)

barplot(tabela, beside = TRUE,
        legend = c("Pressão normal","Pressão alta"),
        ylim=c(0,200))