###############################################################
################                         ######################
################        D a t a          ######################
################    W r a n g l i g      ######################
################                         ###################### 
###############################################################

###############################################################
#########    M e x e r   a q u i    ###########################
######## ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇ #######################

# Expanção (xlsx / csv / txt)
# local_file -> nome do arquivo
# folder -> pasta onde está o arquivo 
# se o folder = getwd() apenas deixar "" 
#            Caso preciso usar pastas utilizar "/"

extension <-"" 
local_file <- ''
folder <-"" 

######## ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆ #################### 
###############################################################

###############################################################
##########  Carregando Biblioteca   ###########################
###############################################################

# Apenas Rodar o arquivo por coleto

# bibiotecas

needed_libs <- c("naniar","lattice","DMwR","data.table","e1071",
                 "UpSetR","VIM","mice","ggplot2","visdat","pROC","cluster",
                 "rms","Hmisc","tidyverse","simputation",
                 "rpart","rpart.plot","GGally")

install_missing <- function(lib){
  install.packages(lib,repos="https://cran.r-project.org/", dependencies = TRUE); 
  library(lib, character.only = TRUE)}
for (lib in needed_libs) 
  tryCatch(library(lib, character.only=TRUE), error = function(e) install_missing(lib))


# Criando o path para carregar o arquivo
path <- ifelse(folder =="",  str_replace_all(
  paste(folder,local_file,".",extension),"\\ ",""),
  str_replace_all(paste(folder,"/",local_file,".",extension),"\\ ",""))


# Função distinguir o arquivo
f <- function(path) {
  extension <- (strsplit(path, "\\.") %>%
                  unlist())[2]
  
  switch(extension,
         "xlsx" = (
           return(read.excel(path))
         ),
         "csv" = (
           return(read.csv(path))
         ),
         "txt" = (
           return( read.table(path, header = T))
         )
  )
}

df <- f(path)

head(df)
dim(df)
ncol(df)
nrow(df)

#################################################################
################ Reconhimento do Data. Frame   ##################
#################################################################

summary(df)
str(df)
skimr::skim(df)
dplyr::glimpse(df)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(size = 2.48, colour = "#A9A9A9") +
    geom_smooth(method=loess, fill = "red", color="red", ...)+
    geom_smooth(method=lm, fill = "blue", color = "blue", ...)
  p
}


# Gráfico ggpairs
ggpairs(df, lower = list(continuous = my_fn)) +
  labs(title = "Correlação entre as variáveis",
       caption = "by Ai Tendency")


print(paste("O conjunto de dados analisado neste DataWrangling foi o",
            str_replace_all(paste("'",local_file,".",extension,"'"),"\\ ",""), "com ", ncol(df), "variáveis e com ",
            nrow(df), " observações"))

Respotsa_sapply <- sum(sapply(df, function(x) sum(is.na(x))))

print(ifelse(Respotsa_sapply == 0, "O dataset NÃO possui dados missing, DESCONSIDERE o próximo módulo.",
             "O dataset POSSUI valores missing, devendo realizar o PRÓXIMO MÓDULO para reconhecimento desses valores faltantes e a impultação de variáveis."))

#################################################################
##############  1ª Parte - Valores Missing   ####################
#################################################################

# Quantidade de Colunas com números na e a quantidade de variáveis
sapply(df, function(x) sum(is.na(x)))

# Gráfico de Variáveis, o número missing
vis_dat(df)


gg_miss_var(df) + 
  labs(title = "Variáveis Missing",
       caption = "by AI Tendency")

vis_miss(airquality) + 
  labs(title = "Variáveis Missing",
       caption = "by AI Tendency")

# proporção de valores na

# percentual de número missing em relação a todas informações
# Qtde. col x Qtde. Linhas 
perc<- round((n_miss(df)/n_complete(df))*100, digits = 2)

# percentual de número missing em relação a todas as observações
# Qtde. Observações
perc1 <- round(pct_miss_case(df), digits = 2)

print(paste("No dataset observado, em relação a todo o conjunto de dados,",n_complete(df),", (quantidade de variáveis multiplicado pela quantidade de observações) temos equivalente a ", perc,"% de números fantantes, porém em relação a quantidade de observações (linhas) ", nrow(df), ", temos equivalente a ", perc1,"% de números fantantes."))

names(df)

#############################################################
#########    M e x e r   a q u i    #########################
######## ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇ #####################

# Removemos a variável com ID do clientese o df existir
# colocar na função qual a coluna que queira realizar 
# a remoção,

df <- df[-1]

###############################################################


###############################################################
##########  2ª Parte - Variáveis Categóricas   ################
###############################################################

###############################################################
#########    M e x e r   a q u i    ###########################
######## ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇ #######################


# Lista de varáveis categóricas
# escrever todas as variáveis que são categóricas

categorical.vars <- c('', '', '')

######## ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆ #################### 
###############################################################

# Função para Fatorização de variáveis categóricas
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(paste(df[[variable]]))
  }
  return(df)
}

# Fatorizando as variáveis categóricas (alterando as variáveis 
# categóricas para fatores)

df <- to.factors(df = df, variables = categorical.vars)

dim(df)

# Limpando as variáveis categóricas

to.categorical <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(paste(df[[variable]]))
  }
  return(df)
}


################################################################
############  3ª Parte - Imputação de Valores   ################
################################################################

# Aplicando Imputação em Valores Missing Usando Método PMM (Predictive Mean Matching)

# Checando valores missing
sapply(df, function(x)sum(is.na(x)))
sum(is.na(df))

# A correspondência média preditiva (rf) é uma maneira atraente 
# de fazer imputação múltipla para dados ausentes, especialmente 
# para imputar variáveis quantitativas. 

# Descobrindo os números das colunas das variáveis fatores, 
# para excluí-las da imputação

fac_col <- as.integer(0)
facnames <- names(Filter(is.factor, df))
k = 1

for(i in facnames){
  while (k <= length(facnames)){
    grep(i, colnames(df))
    fac_col[k] <- grep(i, colnames(df))
    k = k + 1
    break
  }
}

# Coluna que são do tipo fator
fac_col

# Imputação

regra_imputacao <- mice((df[,-c(fac_col)]), 
                        meth = 'rf')

# Aplicando a regra de imputação

total_data <- complete(regra_imputacao, 1)

# Junta novamente as variáveis categóricas
df <- cbind(total_data, df[,c(fac_col)])


# Dimensões
dim(df)

# Checando Valores missing
sapply(df, function(x) sum(is.na(x)))
sum(is.na(df))

###############################################################


################################################################
###############  4ª Parte - Normalização   #####################
################################################################

# função de normalização

normalize <- function(x) {
  return((x - min(x)) / max(x) - min(x))
}

head(df)

# Aplicando a função de normalização a todo o dataset

df <- as.data.frame(lapply(df, normalize))


################################################################
###########  5ª Parte - Teste / Treino / Valid   ###############
################################################################

# Índice de divisão dos dados

train <- 0.7
test <- 0.2
valid <- 0.1


indice_divide_dados <- sample(x = nrow(df),
                              size = train * nrow(df),
                              replace = FALSE)


indice_divide_dados2 <- df[-indice_divide_dados,]


indice_divide_dados3 <- sample(x = nrow(indice_divide_dados2),
                              size = (test * nrow(df)/(nrow(df)*(1-train))) * nrow(indice_divide_dados2),
                              replace = FALSE)



df_train <- df[indice_divide_dados,]
df_test <- indice_divide_dados2[indice_divide_dados3,]
df_valid <- indice_divide_dados2[-indice_divide_dados3,]



dim(df_train)

dim(df_test)
dim(df_valid)

###############################################################


###############################################################
############  6ª Parte - Coluna Tárget   #####################
###############################################################

###############################################################
#########    M e x e r   a q u i    ###########################
######## ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇  ⬇ #######################

# Colocar o nome da coluna target dentro do [" "]

target <- df_train[""]



######## ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆  ⬆ #################### 
###############################################################

# modificando o nome da variável target no df_train
setnames(df_train, target, "target")

prop.table(table(df_train[,"target"])) * 100

###############################################################
#########  7ª Parte - Balanciamento da Tárget   ###############
###############################################################

# SMOTE - Synthetic Minority Oversampling Technique

# passar o nome da variável target como "target"

df_train <- SMOTE(target ~., df_train)

prop.table(table(df_train$target)) * 100


###############################################################
############  7ª Parte - salvando o arquivo   #################
###############################################################

write.csv(df_train_balanced, "dados/dados_train.csv")
write.csv(df_test, "dados/dados_test.csv")
write.csv(df_valid, "dados_valid.csv")




###############################################################
###############  8ª Parte - Modelo SVM   ######################
###############        1ª Modelo         ######################
###############################################################


# modificando o nome da variável target no df_test

modelo_v1 <- svm(target ~ ., data = df_train, 
                 na.action = na.omit, 
                 scale = TRUE)

summary(modelo_v1)


# Fazendo previsões com o modelo
previsoes_v1 <- predict(modelo_v1, newdata = df_valid)

# Matriz de Confusão
caret::confusionMatrix(previsoes_v1, df_valid$target)

# Curva Roc

curva_roc <- multiclass.roc(response = df_valid$target, 
                            predictor = as.numeric(as.factor(previsoes_v1)))

# Score AUC (Area Under The Curve)
curva_roc$auc


# Juntando valores reais e previstos na mesma tabela

# Previsões
valores_previstos <- data.frame(as.numeric(as.factor(previsoes_v1)))
colnames(valores_previstos) <- ("previsão")

# Valores reais
valores_reais <- data.frame(as.numeric(as.factor(df_valid$target)))
colnames(valores_reais) <- ("valor_real")

# Dataframe final
final_df <- cbind(valores_reais, valores_previstos)
head(final_df)


###############################################################
###############  8ª Parte - Modelo SVM   ######################
###############        2ª Modelo         ######################
###############################################################

modelo_grid1 <- tune(svm, 
                     target ~ ., 
                     data = df_train, 
                     kernel = 'linear',
                     ranges = list(cost = c(0.05, 0.1, 0.5, 1, 2))) 

summary(modelo_grid1)

# Parâmetros do melhor modelo
modelo_grid1$best.parameters

# Melhor modelo
modelo_grid1$best.model 
modelo_v2 <- modelo_grid1$best.model 
summary(modelo_v2)

# Previsões
previsoes_v2 <- predict(modelo_v2, df_valid)

# Matriz de Confusão e Score AUC
caret::confusionMatrix(previsoes_v2, df_valid$target)
curva_roc <- multiclass.roc(response = df_valid$target, 
                            predictor = as.numeric(as.factor(previsoes_v2)))

curva_roc$auc


###############################################################
###############  9ª Parte - Modelo SVM   ######################
###############   Comparando Modelos     ######################
###############################################################

# Resultados do Modelo 1
resultados_v1 <- caret::confusionMatrix(previsoes_v1, df_valid$target)
resultados_v1$overall
resultados_v1$byClass

# Medidas Globais do Modelo 1
acuracia_v1 <- resultados_v1$overall['Accuracy']
curva_roc <- multiclass.roc(response = df_valid$target,
                            predictor = as.numeric(as.factor(previsoes_v1)))

score_auc_v1 <- curva_roc$auc

# Vetor com os resultados de avaliação do Modelo v1
vetor_modelo1 <- c("Modelo1 Kernel RBF", round(acuracia_v1, 4),
                   round(score_auc_v1, 4))

# Medidas Globais do Modelo 2
resultados_v2 <- caret::confusionMatrix(previsoes_v2, df_valid$target)
acuracia_v2 <- resultados_v2$overall['Accuracy']
curva_roc <- multiclass.roc(response = df_valid$target, predictor = as.numeric(as.factor(previsoes_v2)))
score_auc_v2 <- curva_roc$auc

# Vetor com os resultados de avaliação do Modelo v2
vetor_modelo2 <- c("Modelo2 Kernel Linear", round(acuracia_v2, 4), round(score_auc_v2, 4))

# Concatenando os resultados
# Dataframe para os resultados dos modelos

compara_modelos <- rbind(vetor_modelo1, vetor_modelo2)

colnames(compara_modelos) <- c("Modelo", "Acuracia", "AUC")

View(compara_modelos)


# Plot

compara_modelos <- as.data.frame(compara_modelos)

ggplot(compara_modelos, aes(x = Modelo, y = Acuracia, fill = Modelo)) + 
  geom_bar(stat = "identity") + 
  labs(title = " Comparação de Acurácia")

# AUC
ggplot(compara_modelos, aes(x = Modelo, y = AUC, fill = Modelo)) + 
  geom_bar(stat = "identity") + 
  labs(title = " Comparação de AUC")


###############################################################
###############  10ª  Parte  -  Modelo    #####################
###############    Salvando o modelo      #####################
###############################################################


# Salvando o modelo selecionado
saveRDS(modelo_v2, "modelo_v2.rds")

# Carregando o modelo salvo
modelo_svm <- readRDS("modelo_v2.rds")
print(modelo_v2)



###############################################################
###############  11ª  Parte  -  Modelo    #####################
###############       Clusterização       #####################
###############################################################

clusplot(df, df$target, color = TRUE, shade = TRUE, labels = 0, lines = 0, )





