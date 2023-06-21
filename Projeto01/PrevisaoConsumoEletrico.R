# Curso: Big Data Analytics com R e Microsoft Azure
# Machine Learning
# Data Science Academy

# Projeto 01
# Machine Learning em Logística Prevendo o Consumo
# de Energia de Carros Elétricos

# Muda o diretório de trabalho
setwd('C:/FCD/BigDataRAzure/Projetos/Projeto_1/')
getwd()

# Carrega os pacotes
library(readxl)
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(randomForest)

# Carrega dataframe
df <- read_excel('FEV-data-Excel.xlsx')

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
(nrow(df) - nrow(df1))/nrow(df)

# Ao total, 9 observações com valores NA foram removidas,
# representando 20,7% do dataframe original.

# Nomes das colunas
colunas <- colnames(df1)
colunas

# Renomeando colunas
colunas[1] <- 'Car_Name'
colunas[4] <- 'Min_Price'
colunas[5] <- 'Engine_Power'
colunas[6] <- 'Max_Torque'
colunas[7] <- 'Brakes_Type'
colunas[8] <- 'Drive_Type'
colunas[9] <- 'Batt_Cap'
colunas[10] <- 'Range'
colunas[11] <- 'Wheelbase'
colunas[12] <- 'Length'
colunas[13] <- 'Width'
colunas[14] <- 'Height'
colunas[15] <- 'Min_Empty_Weight'
colunas[16] <- 'Perm_Gross_Weight'
colunas[17] <- 'Max_Load_Cap'
colunas[18] <- 'Seats'
colunas[19] <- 'Doors'
colunas[20] <- 'Tire_Size'
colunas[21] <- 'Max_Speed'
colunas[22] <- 'Boot_Cap'
colunas[23] <- 'Acc_0_100'
colunas[24] <- 'Max_DC_Charg'
colunas[25] <- 'Energy_Consumption'
colnames(df1) <- colunas
remove(colunas)

# Visualiza os dados
View(df1)

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

# A variável alvo é a 'Energy_Consumption'
hist(df1$Energy_Consumption, 
     main = 'Consumo de Energia Médio em kWH/100km',
     xlab = 'Consumo de Energia',
     ylab = 'Frequência',
     breaks = 15)

# ANÁLISE EXPLORATÓRIA #


# Percebe-se que algumas marcas (Audi e Porsche) consomem mais energia
# ao mesmo tempo que percebe-se que seus tipos de tração são de 4 rodas.
Consumo_Marcas <- ggplot(df1, aes(x=Make, y=Energy_Consumption, fill=Drive_Type)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Marca', 
       y = 'Consumo de Energia',
       x = 'Marca',
       fill = 'Tração') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
Consumo_Marcas

# Neste plot percebe-se que cada tipo de tração possui consumos de energia
# semelhantes. Uma marca (Citroën) é um outlier.
Consumo_Marcas_por_Tracao <- ggplot(df1, aes(x=Make, y=Energy_Consumption, 
                                             fill=Drive_Type)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Marca', 
       y = 'Consumo de Energia',
       x = 'Marca',
       fill = 'Tração') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~Drive_Type, scale="free")
Consumo_Marcas_por_Tracao

# Como visto acima, os carros com tração nas 4 rodas consomem mais
# energia comparado aos de tração em 2 rodas. O plot abaixo resume
# o plot anterior.
Consumo_Tracao <- ggplot(df1, aes(x=Drive_Type, y=Energy_Consumption, 
                                  fill=Drive_Type)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Tração', 
       y = 'Consumo de Energia',
       x = 'Tipo de Tração',
       fill = 'Tração')
Consumo_Tracao

# Boxplot do consumo de energia de acordo com tipo de freio (a disco
# frontal + traseiro e a disco frontal + tambor traseiro) e os tipos
# de tração.
Consumo_Freio <- ggplot(df1, aes(x=Brakes_Type, y=Energy_Consumption,
                                 fill=Drive_Type)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Freio', 
       y = 'Consumo de Energia',
       x = 'Tipo de Freio',
       fill = 'Tração')
Consumo_Freio

# Boxplot do consumo de energia de acordo com número de assentos.
Consumo_Assentos <- ggplot(df1, aes(x=Seats, y=Energy_Consumption, fill=Seats)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Assentos', 
       y = 'Consumo de Energia',
       x = 'Numero de Assentos',
       fill = 'Nº Assentos')
Consumo_Assentos

# Boxplot do consumo de energia de acordo com número de portas.
Consumo_Portas <- ggplot(df1, aes(x=Doors, y=Energy_Consumption, fill=Doors)) +
  geom_boxplot() +
  labs(title = 'Consumo de Energia por Portas', 
       y = 'Consumo de Energia',
       x = 'Numero de Assentos',
       fill = 'Nº Portas')
Consumo_Portas

# Agrupando dados do tipo fator para gerar gráficos do tipo pizza
# mostrando o número total de cada tipo.
Brakes_Type_Total <- df1 %>%
  group_by(Brakes_Type) %>%
  summarise(total_type=n())

plot1 <- ggplot(Brakes_Type_Total, aes(x="", y=total_type, fill=Brakes_Type))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  labs(title = 'Distribuição de Acordo com Freio', 
       y = NULL,
       x = NULL,
       fill = 'Tipo de Freio')

Drive_Type_Total <- df1 %>%
  group_by(Drive_Type) %>%
  summarise(total_type=n())

plot2 <- ggplot(Drive_Type_Total, aes(x="", y=total_type, fill=Drive_Type))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  labs(title = 'Distribuição de Acordo com Tração', 
       y = NULL,
       x = NULL,
       fill = 'Tipo de Tração')

Seats_Total <- df1 %>%
  group_by(Seats) %>%
  summarise(total_type=n())

plot3 <- ggplot(Seats_Total, aes(x="", y=total_type, fill=Seats))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  labs(title = 'Distribuição de Acordo com Assentos', 
       y = NULL,
       x = NULL,
       fill = 'Nº de Assentos')

Doors_Total <- df1 %>%
  group_by(Doors) %>%
  summarise(total_type=n())

plot4 <- ggplot(Doors_Total, aes(x="", y=total_type, fill=Doors))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  labs(title = 'Distribuição de Acordo com Portas', 
       y = NULL,
       x = NULL,
       fill = 'Nº de Portas')

# Gráficos de pizza agrupados
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)


# ANÁLISE DE REGRESSÃO LINEAR #

# O método utilizado será de regressão linear por se tratar de uma variável
# numérica e não categórica.

# Verificação das variáveis mais relevantes para o modelo em regressão linear
# utilizando random forest para análise. Inicialmente foram removidas variáveis
# que não serão levadas em consideração (nome do carro, marca e modelo).
formula <- Energy_Consumption ~ . - Car_Name - Make - Model

modelo <- randomForest(formula, 
                       data = df1, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)

varImpPlot(modelo)

# H0: Não há efeito das variáveis escolhidas em Energy_Consumption.
# H1: Há efeito das variáveis escolhidas em Energy_Consumption.

summary(lm(formula, data = df1))

# O valor p neste modelo ficou abaixo de 0,05. Rejeita-se a hipótese nula.

# Após a primeira verificação, foram removidas outras variáveis que tem pouca
# ou nenhuma influência para o modelo.
formula2 <- Energy_Consumption ~ . - Car_Name - Make - Model - Doors - Batt_Cap - Seats - Height - Brakes_Type - Range - Acc_0_100 - Max_DC_Charg - Max_Speed - Width - Drive_Type - Boot_Cap

modelo2 <- randomForest(formula2, 
                       data = df1, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)

varImpPlot(modelo2)

summary(lm(formula2, data = df1))

# O valor p neste modelo2 ficou abaixo de 0,05. Rejeita-se a hipótese nula.

# Reduzindo mais um pouco o número de variáveis e fazendo algumas combinações
# entre elas, encontraram-se as variáveis abaixo que melhor explicam o modelo.
formula3 <- Energy_Consumption ~ Max_Torque + Length + Perm_Gross_Weight + Max_Load_Cap + Tire_Size

modelo3 <- randomForest(formula3, 
                        data = df1, 
                        ntree = 100, 
                        nodesize = 10,
                        importance = TRUE)

varImpPlot(modelo3)

summary(lm(formula3, data = df1))

# O valor p neste modelo3 ficou abaixo de 0,05. Rejeita-se a hipótese nula.

# Gravação do modelo em uma variável e teste de predição do modelo. Resultado
# gravado em uma nova coluna para comparativo.
rlmodelo <- lm(formula3, data = df1)

df2 <- df1 # Criado uma duplicata do dataset
df2$Previsao <- predict(rlmodelo, data = df1)

df2 <- df2[order(df2$Energy_Consumption),] # Ordenação dos dados para melhor visualização.

# Plot comparando a variável alvo Energy_Consumption e a variável de previsão
# do modelo.
Comparativo_Modelos <- ggplot(df2) +
  geom_line(aes(x = c(1:42), y = Energy_Consumption, colour="Consumo_Dataset_Original"), size = 1.25) + 
  geom_line(aes(x = c(1:42), y = Previsao, colour="Consumo_Previsao"), size = 1.25) +
  scale_color_manual(name = "Legenda", values = c("Consumo_Dataset_Original" = "darkblue", "Consumo_Previsao" = "red")) +
  labs(title = 'Comparação entre variável alvo e variável prevista', 
       y = 'Consumo de Energia',
       x = 'Carros')
Comparativo_Modelos
  
# Como vimos no gráfico anterior, o modelo de machine learning
# conseguiu obter resultados aproximados ao dataset inicial. Assim, o modelo
# obteve grau satisfatório.