# ------- Importando datasets e bibliotecas ------- #
play <-read.csv("C:\\Users\\pasto\\Documents\\Engenharia de Software\\Graduação PUCC\\6o sem\\IA e Sistemas Inteligentes\\Projetos\\Projeto_Final\\googleplaystore.csv", sep = ",", header = TRUE, na.strings = '')
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(viridis) 
library(tidyverse)
library(BBmisc)
library(factoextra)
library(class)
library(rpart)
library(rpart.plot)

# -------------- Entendimento e Limpeza dos dados --------------#

sapply(play, class)

# Remoção de possíveis elementos ou linhas duplicadas 
play <- unique(play)

# Removendo dados com no mínimo um NA
play1 <- play[complete.cases(play),] 

# Contando valores únicos que existem em cada coluna 
apply(play1, 2, function(x) length(unique(x)))

# Listando os valores únicos da coluna Category
y <- play1$Category
unique(y)

# Listando os valores únicos da coluna Content.Rating
w <- play1$Content.Rating
unique(w)


# -------------- Tratamento dos Dados ---------------#

# Adicionando uma nova coluna com apenas o ano da última atualização
library(stringr)
play2 <- play1 %>% mutate(Year = str_sub(play1$Last.Updated, start= -4))

# Duplicando a coluna Year
play2 <- play2 %>% mutate(IndYear = play2$Year)

# Transformando colunas Year e IndYear para numérica
play2$Year <- as.numeric(as.character(play2$Year))
play2$IndYear <- as.numeric(as.character(play2$IndYear))
sapply(play2, class)


# Duplicando a coluna Category
play2 <- play2 %>% mutate(IndCateg = play2$Category)

# Duplicando a coluna Content.Rating
play2 <- play2 %>% mutate(IndContent = play2$Content.Rating)

# Removendo o símbolo + da coluna Installs 

play2 <- play2 %>% mutate(Installs = str_sub(play2$Installs, end=-2))

# Removendo a vírgula da coluna Installs
play2$Installs <- gsub("[[:punct:]]", "", play2$Installs)


# Duplicando a coluna Installs
play2 <- play2 %>% mutate(IndInstalls = play2$Installs)

# Indexando o Installs (IndInstalls)
# Installs de 0 a 4 ---- 0:ruim, 1:regular, 2:bom, 3:muito bom, 4:excelente

for (i in 1:nrow(play2)){
  instalacoes <- play2$IndInstalls[i]
  
  if (instalacoes >= 100000000){
    indice_installs <- paste("4", sep = "")
    play2$IndInstalls[i] <- indice_installs
  }
  
  else if (instalacoes >= 1000000){
    indice_installs <- paste("3", sep = "")
    play2$IndInstalls[i] <- indice_installs
  }
  
  else if (instalacoes >= 10000){
    indice_installs <- paste("2", sep = "")
    play2$IndInstalls[i] <- indice_installs
  }
  
  else if (instalacoes >= 1000){
    indice_installs <- paste("1", sep = "")
    play2$IndInstalls[i] <- indice_installs
  }
  else if (instalacoes < 1000){
    indice_installs <- paste("0", sep = "")
    play2$IndInstalls[i] <- indice_installs
  }
}

sapply(play2, class)

# Transformando coluna IndInstalls (character) para numérica
play2$IndInstalls <- as.numeric(as.character(play2$IndInstalls))
sapply(play2, class)

# Para que a conversão de char pra int não venha em notação científica
options(scipen = 999)

# Transformando coluna Review (character) para numérica
play2$Reviews <- as.numeric(as.character(play2$Reviews))
sapply(play2, class)


# Transformando coluna Installs (character) para numérica
play2$Installs <- as.numeric(as.character(play2$Installs))
sapply(play2, class)

# Removendo o cifrão da coluna Price e transformando-a para numérica
play2$Price <- as.numeric(sub('$','',as.character(play2$Price),fixed=TRUE))
sapply(play2, class)

# Removendo valores "Varies with device" da coluna Size
play_tratado <- play2[play2$Size != "Varies with device", ]

# Padronizando valores de size para estarem todos em MB

for (i in 1:nrow(play_tratado)){
  unidade_medida <- substr(play_tratado$Size[i], nchar(play_tratado$Size[i]), nchar(play_tratado$Size[i]))
  
  if (unidade_medida == "k"){
    valor <- as.numeric(substr(play_tratado$Size[i], 1, nchar(play_tratado$Size[i])-1))/1000
    size_novo <- paste(valor, "M", sep = "")
    play_tratado$Size[i] <- size_novo
  }
}


# Removendo a letra M da coluna Size 
play_tratado <- play_tratado %>% mutate(Size = str_sub(play_tratado$Size, end=-2))
#sapply(play_tratado, class)


# Transformando coluna Size (character) para numérica
play_tratado$Size <- as.numeric(as.character(play_tratado$Size))
sapply(play_tratado, class)


# Indexando o ano (IndYear)

for (i in 1:nrow(play_tratado)){
  ano <- play_tratado$IndYear[i]
  
  if (ano == "2010"){
    indice_ano <- paste("0", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
  
  else if (ano == "2011"){
    indice_ano <- paste("1", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
  
  else if (ano == "2012"){
    indice_ano <- paste("2", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
  
  else if (ano == "2013"){
    indice_ano <- paste("3", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
  else if (ano == "2014"){
    indice_ano <- paste("4", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
  else if (ano == "2015"){
    indice_ano <- paste("5", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
  else if (ano == "2016"){
    indice_ano <- paste("6", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
  else if (ano == "2017"){
    indice_ano <- paste("7", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
  else if (ano == "2018"){
    indice_ano <- paste("8", sep = "")
    play_tratado$IndYear[i] <- indice_ano
  }
}



# Indexando a Category (IndCateg)

for (i in 1:nrow(play_tratado)){
  categoria <- play_tratado$IndCateg[i]
  
  if (categoria == "ART_AND_DESIGN"){
    indice_categoria <- paste("1", sep = "")
    play_tratado$IndCateg[i] <- indice_ano
  }
  else if (categoria == "AUTO_AND_VEHICLES"){
    indice_categoria <- paste("2", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "BEAUTY"){
    indice_categoria <- paste("3", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "BOOKS_AND_REFERENCE"){
    indice_categoria <- paste("4", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "BUSINESS"){
    indice_categoria <- paste("5", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "COMICS"){
    indice_categoria <- paste("6", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "COMMUNICATION"){
    indice_categoria <- paste("7", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "DATING"){
    indice_categoria <- paste("8", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "EDUCATION"){
    indice_categoria <- paste("9", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "ENTERTAINMENT"){
    indice_categoria <- paste("10", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "EVENTS"){
    indice_categoria <- paste("11", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "FINANCE"){
    indice_categoria <- paste("12", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "FOOD_AND_DRINK"){
    indice_categoria <- paste("13", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "HEALTH_AND_FITNESS"){
    indice_categoria <- paste("14", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "HOUSE_AND_HOME"){
    indice_categoria <- paste("15", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "LIBRARIES_AND_DEMO"){
    indice_categoria <- paste("16", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "LIFESTYLE"){
    indice_categoria <- paste("17", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "GAME"){
    indice_categoria <- paste("18", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "FAMILY"){
    indice_categoria <- paste("19", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "MEDICAL"){
    indice_categoria <- paste("20", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "SOCIAL"){
    indice_categoria <- paste("21", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "SHOPPING"){
    indice_categoria <- paste("22", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "PHOTOGRAPHY"){
    indice_categoria <- paste("23", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "SPORTS"){
    indice_categoria <- paste("24", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "TRAVEL_AND_LOCAL"){
    indice_categoria <- paste("25", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "TOOLS"){
    indice_categoria <- paste("26", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "PERSONALIZATION"){
    indice_categoria <- paste("27", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "PRODUCTIVITY"){
    indice_categoria <- paste("28", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "PARENTING"){
    indice_categoria <- paste("29", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "WEATHER"){
    indice_categoria <- paste("30", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "VIDEO_PLAYERS"){
    indice_categoria <- paste("31", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "NEWS_AND_MAGAZINES"){
    indice_categoria <- paste("32", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
  else if (categoria == "MAPS_AND_NAVIGATION"){
    indice_categoria <- paste("33", sep = "")
    play_tratado$IndCateg[i] <- indice_categoria
  }
}

# Transformando coluna IndCateg (character) para numérica
play_tratado$IndCateg <- as.numeric(as.character(play_tratado$IndCateg))
sapply(play_tratado, class)


# Indexando o ano (IndContent)

for (i in 1:nrow(play_tratado)){
  content <- play_tratado$IndContent[i]
  
  if (content == "Unrated"){
    indice_content <- paste("0", sep = "")
    play_tratado$IndContent[i] <- indice_content
  }
  
  else if (content == "Adults only 18+"){
    indice_content <- paste("1", sep = "")
    play_tratado$IndContent[i] <- indice_content
  }
  
  else if (content == "Mature 17+"){
    indice_content <- paste("2", sep = "")
    play_tratado$IndContent[i] <- indice_content
  }
  
  else if (content == "Teen"){
    indice_content <- paste("3", sep = "")
    play_tratado$IndContent[i] <- indice_content
  }
  else if (content == "Everyone 10+"){
    indice_content <- paste("4", sep = "")
    play_tratado$IndContent[i] <- indice_content
  }
  else if (content == "Everyone"){
    indice_content <- paste("5", sep = "")
    play_tratado$IndContent[i] <- indice_content
  }
}

# Transformando coluna IndContent (character) para numérica
play_tratado$IndContent <- as.numeric(as.character(play_tratado$IndContent))
sapply(play_tratado, class)

# ------- Histogramas ----- #

# histograma de valores das colunas

for (i in 3:6){
  hist(play_tratado[,i], xlab = names(play_tratado)[i], main = paste("Histogram of", names(play_tratado)[i]),col = magma(10))
}

# -------- Gráficos de barras ------------- #

# ----- CATEGORY ----- #
# (obs: las = 2 para legendas verticais do eixo x)
table(play_tratado$Category)

# Modificando tamanho da margem
par(mar=c(14,4,4,2))

barplot(table(play_tratado$Category), col = magma(20), 
        main = "Quantidade de Apps por Categoria", 
        xlab = "", 
        ylab = "Quantidade",
        las = 2,
        font=2)


# ---- RATING ---- #

par(mar=c(4,4,2,2))

barplot(table(play_tratado$Rating), col = magma(10), 
        main = "Quantidade de Apps por Avaliação", 
        xlab = "", 
        ylab = "Quantidade",
        las = 2,
        font=2)

# ---- REVIEWS ---- #

par(mar=c(4,4,2,2))

barplot(table(play_tratado$Reviews), col = magma(10), 
        main = "Quantidade de Apps por Reviews", 
        xlab = "", 
        ylab = "Quantidade",
        las = 2,
        font=2,
        xlim=c(0,500))


# ---- TYPE ---- # 

par(mar=c(4,8,8,8))

barplot(table(play_tratado$Type), col = magma(3), 
        main = "Quantidade de Apps por Tipo", 
        xlab = "", 
        ylab = "Quantidade",
        las = 1,
        font=2)


# ---- PRICE ---- # 
play_pagos <- filter(play_tratado, Price !="0")

par(mar=c(4,4,4,1))

barplot(table(play_pagos$Price), col = magma(20), 
        main = "Quantidade de Apps Pagos e seus Preços", 
        xlab = "", 
        ylab = "Quantidade",
        las = 2,
        font=2)

# ---- CONTENT RATING ---- # 
table(play_tratado$Content.Rating)

par(mar=c(8,4,8,1))

barplot(table(play_tratado$Content.Rating), col = magma(6), 
        main = "Quantidade de Apps por Content Rating", 
        xlab = "", 
        ylab = "Quantidade",
        las = 2,
        font=2)

# ---- ANDROID VERSION ---- # 

par(mar=c(10,4,6,1))

barplot(table(play_tratado$Android.Ver), col = magma(30), 
        main = "Quantidade de Apps por Versão do Android", 
        xlab = "", 
        ylab = "Quantidade",
        las = 2,
        font=2)


# ---- LAST UPDATE ---- # 

par(mar=c(8,6,6,1))

barplot(table(play_tratado$Year), col = magma(12), 
        main = "Quantidade de Apps por Ano de Última Atualização", 
        xlab = "", 
        ylab = "Quantidade",
        las = 2,
        font=2)


# ---- SIZE ---- # 

par(mar=c(4,4,2,2))

barplot(table(play_tratado$Size), col = magma(30), 
        main = "Quantidade de Apps por Tamanho", 
        xlab = "", 
        ylab = "Quantidade",
        las = 2,
        font=2)


# -------- GRÁFICOS DE PONTOS E BOXPLOT ---------- #

# Boxplot de Instalações por Ano
boxplot(play_tratado$Installs~play_tratado$Year, main = "Instalações por ano de última atualização", xlab = "Ano", ylab = "Instalações",ylim = range(0:10000000),col = magma(10))

# Normalização da coluna installs para facilitar visualização
Installs <- play_tratado$Installs
Installs <- normalize(Installs, method = 'range')

play.norm.installs <- play_tratado
play.norm.installs$Installs <- Installs

# Gráfico de pontos entre Rating, Installs, Type e Category
ggplot(play.norm.installs, aes(Installs, Rating, color = Category)) +
  geom_point() + facet_grid(.~Type) + ggtitle("Gráfico de pontos entre Rating, Installs e Type") +
  xlab("Installs") + ylab("Rating") 

# Gráfico de pontos entre Size, Installs, Type e Category
ggplot(play.norm.installs, aes(Installs, Size, color = Category)) +
  geom_point() + facet_grid(.~Type) + ggtitle("Gráfico de pontos entre Size, Installs e Type") +
  xlab("Installs") + ylab("Size") 

# Gráfico de pontos entre Android Version, Last Updated, Type e Category
ggplot(play_tratado, aes(Year, Android.Ver, color = Category)) +
  geom_point() + facet_grid(.~Type) + ggtitle("Gráfico de pontos entre Android Version, Last Update, Category e Type") +
  xlab("Year") + ylab("Android Version")


# Gráfico de pontos entre Android Version, Size, Type e Category
ggplot(play_tratado, aes(Size, Android.Ver, color = Category)) +
  geom_point() + facet_grid(.~Type) + ggtitle("Gráfico de pontos entre Android Version, Size, Category e Type") +
  xlab("Size") + ylab("Android Version")


# Gráfico de pontos entre Installs e Category
ggplot(play_tratado, aes(Category, Installs, color = Category)) +
  geom_point() + ggtitle("Gráfico de pontos entre Installs e Category") +
  xlab("Category") + ylab("Installs")

# ---- RECEITA ---- #

play_pagos <- play_pagos %>% mutate(Receita = Price * Installs)
play_pagos_norm <- play.norm.installs %>% mutate(Receita = Price * Installs)

# Gráfico de pontos entre Rating, Installs, Type e Category
ggplot(play_pagos, aes(Installs, Receita, color = Category)) +
  geom_point() + ggtitle("Gráfico de pontos entre Receita, Installs e Type") +
  xlab("Installs") + ylab("Receita") + xlim(0, 1000000) + ylim(0,45000000)


# ------------------------ MACHINE LEARNING ---------------------------- #

play_tratado <- play_tratado %>% mutate(Receita = Price * Installs)

# -------- REGRESSÃO entre RATING E INSTALLS ------- #

#dividindo o dataset em treino e teste (80% treino e 20% teste)
set.seed(200)
index <- sample(1:nrow(play_tratado),round(0.80*nrow(play_tratado))) 
train <- play_tratado[index, ]
test <- play_tratado[-index, ]


#código para modelo de regressão linear com dataset de treino
l=lm(Rating~Installs, train)

plot(x = train$Installs, y = train$Rating, main = "Regressão do conjunto de treino Installs x Rating")
abline(l)

#verificando o modelo
summary(l)

#realizando a previsão de valores para o dataset de teste
predict(l, test)

#plot de reta de regressão em dataset de teste
plot(x=test$Installs, y= test$Rating, main = "Regressão do conjunto de teste Installs x Rating")
abline(l)

# -------- REGRESSÃO entre PRICE e INSTALLS ------- #

#dividindo o dataset em treino e teste (80% treino e 20% teste)
set.seed(200)
index_pagos <- sample(1:nrow(play_pagos),round(0.80*nrow(play_pagos))) 
train_pagos <- play_pagos[index_pagos, ]
test_pagos <- play_pagos[-index_pagos, ]


#código para modelo de regressão linear com dataset de treino
l=lm(Price~Installs, train_pagos)

plot(x = train_pagos$Installs, y = train_pagos$Price, xlim=c(0,2000000), main = "Regressão do conjunto de treino Installs x Price")
abline(l)


#verificando o modelo
summary(l)

#realizando a previsão de valores para o dataset de teste
predict(l, test_pagos)

#plot de reta de regressão em dataset de teste
plot(x=test_pagos$Installs, y= test_pagos$Price, main = "Regressão do conjunto de teste Installs x Price")
abline(l)

# -------- REGRESSÃO entre REVIEWS E INSTALLS ------- #

#dividindo o dataset em treino e teste (80% treino e 20% teste)
set.seed(200)
index <- sample(1:nrow(play_tratado),round(0.80*nrow(play_tratado))) 
train <- play_tratado[index, ]
test <- play_tratado[-index, ]


#código para modelo de regressão linear com dataset de treino
l=lm(Reviews~Installs, train)

plot(x = train$Installs, y = train$Reviews, main = "Regressão do conjunto de treino Installs x Reviews")
abline(l)

#verificando o modelo
summary(l)

#realizando a previsão de valores para o dataset de teste
predict(l, test)

#plot de reta de regressão em dataset de teste
plot(x=test$Installs, y= test$Reviews, main = "Regressão do conjunto de teste Installs x Reviews")
abline(l)

# -------------------------------- Preparo para K-means ---------------------------- #

# ordenando o dataset
play_order <- play.norm.installs[order(play.norm.installs$Type),]

# Separando a coluna de Type (se é Free ou Paid) para a criação da matriz de confusão
classesPlay <- play_order[,7]

sapply(play_order, class)

# Redefinindo IndYear para numeric
play_order$IndYear <- as.numeric(as.character(play_order$IndYear))
sapply(play_order, class)

# Removendo colunas não desejáveis

#remove App, Category,Type,ContentRating até Year
play_numeric = select(play_order, -1,-2,-6,-7,-9:-14) 

# remove App, Category,Price, Installs (números muito grandes), Price até Year
play_numeric_arvore_type = select(play_order, -1,-2,-6,-8:-14)

# remove App, Category, Installs, Type, Content Rating até Year
play_numeric_arvore_installs = select(play_order, -1,-2,-6,-7,-9:-14) 
 

# ---------------- Aplicando o k-means ----------------- #

set.seed(123)

# k = 2

# printando o vetor de clusterização para k = 2
kmeans(play_numeric, 2, nstart = 3000, iter.max = 1000)


play.km2 <- kmeans(play_numeric, 2, nstart = 3000, iter.max = 1000)

fviz_cluster(play.km2, data = play_numeric,
             palette = c("#836FFF", "#FF0000"), 
             geom = c("point"),
             shape = NULL,
             ellipse.type = "norm",
             ggtheme = theme_bw()
)


# Verificando taxa de acerto para k=2 em relação a coluna Type

table(play.km2$cluster)
table(classesPlay)
table(classesPlay, play.km2$cluster)



# -------- Elbow Method para achar o número ótimo de clusters ------ #
set.seed(123)

fviz_nbclust(play_numeric, kmeans, method = "wss", k.max = 10) 


# Largura da Silhueta média para o k-means
fviz_nbclust(play_numeric, kmeans, method = "silhouette", k.max = 10)


# ----------- ÁRVORE -------- #
sapply(play_numeric_arvore_type, class)

# -----Dividindo o dataset em treino e teste para TYPE ----------

# Selecionando 80% da base original 
smp_size <- floor(0.8*nrow(play_numeric_arvore_type))

# Dessa amostragem de 80% (smp_size), armazena os índices do primeiro até a última instância (seq_len)
train_ind <- sample(seq_len(nrow(play_numeric_arvore_type)), size = smp_size)

# Conjunto de treino correspondendo a 80% da base original
train1 <- play_numeric_arvore_type[train_ind,]
# Conjunto de teste correspondendo ao resto, ou 20% da base original (pega os índices restantes)
test1 <- play_numeric_arvore_type[-train_ind,]


# Aplicação da árvore ~ TYPE

# Para que a conversão de char pra int não venha em notação científica
options(scipen = 999)

modelo1 <- rpart(Type~. , train1, method = "class", control = rpart.control(minsplit = 1))
plot <- rpart.plot(modelo1, type = 3,digits = -2)
pred <- predict(modelo1, test1, type = "class")
table(classesPlay, pred)

# -----Dividindo o dataset em treino e teste para INDINSTALLS ----------

# Selecionando 80% da base original 
smp_size1 <- floor(0.8*nrow(play_numeric_arvore_installs))

# Dessa amostragem de 80% (smp_size), armazena os índices do primeiro até a última instância (seq_len)
train_ind1 <- sample(seq_len(nrow(play_numeric_arvore_installs)), size = smp_size1)

# Conjunto de treino correspondendo a 80% da base original
train2 <- play_numeric_arvore_installs[train_ind1,]
# Conjunto de teste correspondendo ao resto, ou 20% da base original (pega os índices restantes)
test2 <- play_numeric_arvore_installs[-train_ind1,]

# Aplicação da árvore ~ INSTALLS

# Para que a conversão de char pra int não venha em notação científica
options(scipen = 999)


modelo2 <- rpart(IndInstalls~. , train2, method = "class", control = rpart.control(minsplit = 1))
plot <- rpart.plot(modelo2, type = 3,digits = -2)
pred1 <- predict(modelo2, test2, type = "class")
table(classesPlay, pred1)
