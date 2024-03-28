# INTRODUÇÃO ####

# Curso: Big Data Analytics com R e Microsoft Azure
# Machine Learning
# Data Science Academy

# Projeto 01
# Machine Learning em Logística Prevendo o Consumo
# de Energia de Carros Elétricos

# Muda o diretório de trabalho
setwd('../Projeto01')
getwd()

# Caso seja necessário instalar um pacote
#install.packages('pacote')

# Carrega os pacotes
library(readxl)
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(knitr)
library(car)
library(MASS)
library(neuralnet)
library(fastDummies)

# PREPARANDO OS DADOS ####

# Carrega dataframe
df <- read_excel('dataset/FEV-data-Excel.xlsx')

# Dimensões
dim(df)

# Visualiza os dados
View(df)

# Tipos de dados
str(df)

# Verificando valores NA
sum(is.na(df))
colSums(is.na(df))

# Existem 30 valores NA. Como o objetivo é obter um modelo de 
# Machine Learning, optou-se por remover estes valores e salvar
# em um novo dataframe.
df1 <- na.omit(df)
sum(is.na(df1))
dim(df1)
(nrow(df) - nrow(df1))/nrow(df) # Percentual de linhas removidas.

# Ao total, 9 observações com valores NA foram removidas,
# representando 20,7% do dataframe original.

# Extraindo nomes das colunas
colunas <- colnames(df1)
colunas

# Renomeando colunas
colunas[1] <- 'CarName'
colunas[4] <- 'MinPrice'
colunas[5] <- 'EnginePower'
colunas[6] <- 'MaxTorque'
colunas[7] <- 'BrakesType'
colunas[8] <- 'DriveType'
colunas[9] <- 'BattCap'
colunas[10] <- 'Range'
colunas[11] <- 'Wheelbase'
colunas[12] <- 'Length'
colunas[13] <- 'Width'
colunas[14] <- 'Height'
colunas[15] <- 'MinEmptyWeight'
colunas[16] <- 'PermGrossWeight'
colunas[17] <- 'MaxLoadCap'
colunas[18] <- 'Seats'
colunas[19] <- 'Doors'
colunas[20] <- 'TireSize'
colunas[21] <- 'MaxSpeed'
colunas[22] <- 'BootCap'
colunas[23] <- 'Acc100'
colunas[24] <- 'MaxDCCharg'
colunas[25] <- 'EnergyConsumption'
colnames(df1) <- colunas
remove(colunas)

# Convertendo as variáveis do tipo caracter para fator.
df1 <- df1 %>% 
  mutate_if(is.character, factor)

# Convertendo algumas variáveis do tipo numérica para fator.
unique(df1$Seats)
df1$Seats <- as.factor(df1$Seats)
unique(df1$Doors)
df1$Doors <- as.factor(df1$Doors)

# Tipos dos dados
str(df1)

# Visualiza os dados
View(df1)

# Resumo estatístico
summary(df1)

# A variável alvo é a 'EnergyConsumption'
png('plots/plot1.png', width = 600, height = 480) # Salvando plot em um PNG
plot(x=c(1:42), y=df1$EnergyConsumption, 
     main = 'Consumo de Energia Médio em kWH/100km',
     xlab = 'Nº do Carro (42 ao total)',
     ylab = 'Consumo de Energia (kWH/100km)')
dev.off()

# ANÁLISE EXPLORATÓRIA ####

# ANÁLISE UNIVARIADA ####

# Histograma do consumo de energia
Hist_Consumo <- ggplot(df1, aes(x=EnergyConsumption, fill=DriveType)) +
  geom_histogram(binwidth = 2.5) +
  labs(title = 'Consumo de Energia por Marca', 
       y = 'Consumo de Energia (kWH/100km)',
       x = 'Marca',
       fill = 'Tipo de Tração')
png('plots/plot2.png', width = 600, height = 480)
Hist_Consumo
dev.off()


# Boxplot de consumo de energia por marca e por tipo de tração
Consumo_Marcas <- ggplot(df1, aes(x=Make, y=EnergyConsumption, fill=DriveType)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Marca', 
       y = 'Consumo de Energia (kWH/100km)',
       x = 'Marca',
       fill = 'Tipo de Tração') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
png('plots/plot3.png', width = 600, height = 480) # Salvando plot em um PNG
Consumo_Marcas
dev.off()

# Percebe-se que algumas marcas (Audi e Porsche) consomem mais energia
# ao mesmo tempo que percebe-se que seus tipos de tração são de 4 rodas.


# Boxplot de consumo de energia por marca dividido em janelas de tração
Consumo_Marcas_por_Tracao <- ggplot(df1, aes(x=Make, y=EnergyConsumption, 
                                             fill=DriveType)) +
  geom_boxplot()  + 
  coord_cartesian(ylim = c(13, 30)) +
  labs(title = 'Consumo de Energia por Marca', 
       y = 'Consumo de Energia (kWH/100km)',
       x = 'Marca',
       fill = 'Tipo de Tração') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~DriveType, scale="free")
png('plots/plot4.png', width = 600, height = 480) # Salvando plot em um PNG
Consumo_Marcas_por_Tracao
dev.off()

# Neste plot percebe-se que cada tipo de tração possui consumos de energia
# semelhantes. A marca Citroën é um outlier, pois consome muito além da média
# dos carros com tração dianteira.

# Boxplot de consumo de energia por freio
Consumo_Tracao <- ggplot(df1, aes(x=DriveType, y=EnergyConsumption, 
                                  fill=DriveType)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Tração', 
       y = 'Consumo de Energia (kWH/100km)',
       x = 'Tipo de Tração',
       fill = 'Tipo de Tração')
png('plots/plot5.png', width = 600, height = 480) # Salvando plot em um PNG
Consumo_Tracao
dev.off()

# Como visto acima, os carros com tração nas 4 rodas consomem mais
# energia comparado aos de tração em 2 rodas. O plot abaixo resume
# o plot anterior.

# Boxplot do consumo de energia de acordo com tipo de freio (a disco
# frontal + traseiro e a disco frontal + tambor traseiro) e os tipos
# de tração.
Consumo_Freio <- ggplot(df1, aes(x=BrakesType, y=EnergyConsumption,
                                 fill=DriveType)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Freio', 
       y = 'Consumo de Energia (kWH/100km)',
       x = 'Tipo de Freio',
       fill = 'Tração')
png('plots/plot6.png', width = 600, height = 480) # Salvando plot em um PNG
Consumo_Freio
dev.off()

# Boxplot do consumo de energia de acordo com número de assentos.
Consumo_Assentos <- ggplot(df1, aes(x=Seats, y=EnergyConsumption, fill=Seats)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Assentos', 
       y = 'Consumo de Energia (kWH/100km)',
       x = 'Numero de Assentos',
       fill = 'Nº Assentos')
png('plots/plot7.png', width = 600, height = 480) # Salvando plot em um PNG
Consumo_Assentos
dev.off()

# Boxplot do consumo de energia de acordo com número de portas.
Consumo_Portas <- ggplot(df1, aes(x=Doors, y=EnergyConsumption, fill=Doors)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Portas', 
       y = 'Consumo de Energia (kWh/100km)',
       x = 'Numero de Portas',
       fill = 'Nº Portas')
png('plots/plot8.png', width = 600, height = 480) # Salvando plot em um PNG
Consumo_Portas
dev.off()

# Agrupando dados do tipo fator para gerar gráficos do tipo pizza
# mostrando o número total de cada tipo.
Brakes_Type_Total <- df1 %>%
  group_by(BrakesType) %>%
  summarise(total_type=n())

plot1 <- ggplot(Brakes_Type_Total, aes(x="", y=total_type, fill=BrakesType))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_minimal() +
  geom_text(aes(label=total_type), position = position_stack(vjust = 0.5)) +
  labs(title = 'Distribuição de Acordo com Freio', 
       y = NULL,
       x = NULL,
       fill = 'Tipo de Freio') +
  theme_void()

Drive_Type_Total <- df1 %>%
  group_by(DriveType) %>%
  summarise(total_type=n())

plot2 <- ggplot(Drive_Type_Total, aes(x="", y=total_type, fill=DriveType))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  geom_text(aes(label=total_type), position = position_stack(vjust = 0.5)) +
  labs(title = 'Distribuição de Acordo com Tração', 
       y = NULL,
       x = NULL,
       fill = 'Tipo de Tração') +
  theme_void()

Seats_Total <- df1 %>%
  group_by(Seats) %>%
  summarise(total_type=n())

plot3 <- ggplot(Seats_Total, aes(x="", y=total_type, fill=Seats))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  geom_text(aes(label=total_type), position = position_stack(vjust = 0.5)) +
  labs(title = 'Distribuição de Acordo com Assentos', 
       y = NULL,
       x = NULL,
       fill = 'Nº de Assentos') +
  theme_void()

Doors_Total <- df1 %>%
  group_by(Doors) %>%
  summarise(total_type=n())

plot4 <- ggplot(Doors_Total, aes(x="", y=total_type, fill=Doors))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  geom_text(aes(label=total_type), position = position_stack(vjust = 0.5)) +
  labs(title = 'Distribuição de Acordo com Portas', 
       y = NULL,
       x = NULL,
       fill = 'Nº de Portas') +
  theme_void()

# Gráficos de pizza agrupados
png('plots/plot9.png', width = 600, height = 480) # Salvando plot em um PNG
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
dev.off()

# ANÁLISE MULTIVARIADA ####

# Gerando um Plot de Correlação entre as Variáveis Numéricas
nums <- unlist(lapply(df1, is.numeric), use.names = FALSE)
corr <- cor(df1[,nums])
png('plots/plot10.png', width = 600, height = 480)
corrplot(corr, type="lower", tl.col="black", order='FPC',
         diag=FALSE)
dev.off()

# Correlação com a variável alvo
df2 <- df1[, nums]
View(df2)
sort(cor(df2[-18], df2$EnergyConsumption), decreasing = TRUE)

# ANÁLISE DE MODELOS PREDITIVOS ####

# Modelo 01

# O método utilizado será de regressão linear.
formula1 <- EnergyConsumption ~ . -CarName-Make-Model
modelo_v1 <- lm(formula1, data = df1)
summary(modelo_v1)

# O valor de R^2 é alto, de 98%, o que é bom, porém o modelo leva em conta
# muitas variáveis, tornando-o complexo.

# Detectando Colinearidade
kable(vif(modelo_v1), align = 'c')

# Considerando a última coluna da função acima, eliminaremos as variáveis
# com valores acima de 10, que são consideradas variâncias altas.

# Modelo 02 

# Removendo algumas variáveis com variâncias altas encontradas
# na função VIF acima.
formula2 <- EnergyConsumption ~ MinPrice+MaxTorque+BrakesType+DriveType+Range+Width+Height+MaxLoadCap+Seats+Doors+TireSize+BootCap+Acc100+MaxDCCharg
modelo_v2 <- lm(formula2, data = df1)
summary(modelo_v2)

# O modelo ainda apresenta um valor R^2 elevado de 94%, agora com menos 
# variáveis, o que deixa o modelo menos complexo e mais generalizável.

# Checando colinearidade do modelo 02
kable(vif(modelo_v2), align = 'c')

# Os valores estão entre 1 e 5, então pode-se deixá-los como variáveis
# explicativas do modelo.

# Modelo 03

# Verificação das variáveis mais relevantes para o modelo em regressão linear
# utilizando random forest para análise. Inicialmente foram removidas variáveis
# que não serão levadas em consideração (nome do carro, marca e modelo).
varimp <- randomForest(EnergyConsumption ~ MinPrice+MaxTorque+BrakesType+DriveType+Range+Width+Height+MaxLoadCap+Seats+Doors+TireSize+BootCap+Acc100+MaxDCCharg,
                       data = df1, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)
varImpPlot(varimp)

# Reduzindo algumas variáveis de acordo com o nível de importância delas.
formula3 <- EnergyConsumption ~ MinPrice+MaxTorque+DriveType+Width+MaxLoadCap+BootCap+Acc100
modelo_v3 <- lm(formula3, data = df1)
summary(modelo_v3)

# O valor de R^2 do modelo 03 é de 86%. 

# Coeficiente AIC
step <- stepAIC(modelo_v3, direction = "both", trace = FALSE)
summary(step)
summary(step)$coeff
summary(step)$r.squared

# Pode-se notar que pela AIC, as variáveis que menos perdem informação para
# explicar a variável alvo são MaxTorque, DriveType, MaxLoadCap e Acc100.

# Modelo 04

# Utilizando as variáveis encontradas na AIC.
formula4 <- EnergyConsumption ~ MaxTorque+DriveType+MaxLoadCap+Acc100
modelo_v4 <- lm(formula4, data = df1)
summary(modelo_v4)

# Com isto, o modelo consegue explicar cerca de 86% da variável alvo com o
# mínimo de variáveis explicatórias. Este é um modelo robusto.

# Detectando colinearidade
kable(vif(modelo_v4), align = 'c')

# Plotando gráfico de resíduos versus os valores ajustados
modelo_v4$fitted.values
png('plots/plot11.png', width = 600, height = 480)
qqPlot(modelo_v4, main = "Normal Q-Q plot")
dev.off()

# Modelo 05

# Utilizando Random Forest
modelo_v5 <- randomForest(formula1,
                          data=df1,
                          ntree = 500,
                          nodesize = 10,
                          importance = FALSE)
modelo_v5

# Este modelo com Random Forest consegue explicar até 83% da variância.

# Modelo 06

# Normalizando variáveis
func_normaliza <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

df3 <- as.data.frame(lapply(df1[,-c(1:4,7:9, 18:20)], FUN = func_normaliza))
df3_final <- data.frame(df3, df1[,c(7:9, 18:20)])
View(df3_final)

# Criando variáveis dummy
df4 <- dummy_cols(df3_final[c(2,10,13,15,17)])
View(df4)
colnames(df4)
colnames(df4)[6] <- "DriveType_2WD_front"
colnames(df4)[7] <- "DriveType_2WD_rear"
colnames(df4)

# Utilizando Redes Neurais
modelo_v6 <- neuralnet(EnergyConsumption ~ MaxTorque+DriveType_2WD_front+DriveType_2WD_rear+DriveType_4WD+MaxLoadCap+Acc100,
                       data=df4,
                       hidden=4)
plot(modelo_v6)
previsoes <- predict(modelo_v6, df4[,-c(4:5)])

# Desnormalizando variáveis
func_desnormaliza <- function(n){
  return (n*(max(df1$EnergyConsumption) - min(df1$EnergyConsumption)) + min(df1$EnergyConsumption))
}

# Encontrando os valores de previsão final sem normalização e
# comparando com valores do dataset original.
previsoes_desn <- as.data.frame(sapply(previsoes, FUN = func_desnormaliza))
colnames(previsoes_desn) <- 'EnergyConsumption'
previsoes_desn <- data.frame(previsoes_desn, df1$EnergyConsumption)
View(previsoes_desn)

# Percebe-se que este modelo com Redes Neurais não perfomou bem ao tentar
# prever os valores numéricos.

# Para este projeto, o modelo 04 será o escolhido como melhor modelo
# por sua boa capacidade de previsão, explicando 86% da variância do dataset
# e por ser simples, com poucas variáveis explicatórias.
summary(modelo_v4)

# Fim do projeto.
