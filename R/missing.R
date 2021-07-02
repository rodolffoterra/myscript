any(is.na(dados$AMOUNT))

# valores missing
sapply(df, function(x) sum(is.na(x)))

#Remove as linhas que tenha NA daquela coluna
df.merge <- df.merge[complete.cases(df.merge$salario_hora),]

# No lugar de NA colocar 0
actual$faturamento_2015[is.na(actual$faturamento_2015)] = 0

       
       
       
# Aplicando a Estratégia 1

# Calculando o percentual de valores ausentes por coluna
percentual_valores_ausentes = (colSums(is.na(dados_full)) / nrow(dados_full)) * 100

# Dataframe com o resultado anterior para criar o plot
df_percent_NA = data.frame(colnames(dados_full), percentual_valores_ausentes)
colnames(df_percent_NA) <- c("Variavel", "Percentual_Ausente")
df_percent_NA = df_percent_NA[order(df_percent_NA$Percentual_Ausente, decreasing = TRUE), ]

# Visualiza
View(df_percent_NA)

# Plot
plot(df_percent_NA$Percentual_Ausente, 
     ylab = "% de Valores Ausentes", 
     main = "Percentual de Valores Ausentes")

# Vamos remover as colunas com mais de 50% de valores ausentes
dim(dados_full)
dados_full <- dados_full[percentual_valores_ausentes < 50]
dim(dados_full)

# Aplicando as Estratégias 2 e 3

# Colunas ainda com valores ausentes
outrasNAcol <- (dados_full)[!colSums(is.na(dados_full))==0]
outrasNAcol <- colnames(outrasNAcol)

# Vamos colocar o valor "Desconhecido" onde estiver NA se for variável qualitativa
# Para variáveis quantitativas substituímos NA pela méda
for(f in outrasNAcol){
  
  if(any(is.na(dados_full[[f]]))){
    
    if(is.factor(dados_full[,f])){
      
      dados_full[,f] <- as.character(dados_full[,f])

           
#remover algumas colunas de uma única vez
excluir <- c("d","m","y")

ico <- ico[,!(names(ico)%in% excluir)]      
      # Estratégia 2
      dados_full[,f][is.na(dados_full[,f])] <- "Desconhecido"
      dados_full[,f] <- factor(dados_full[,f])
      
    }
    else{
      
      # Estratégia 3
      dados_full[is.na(dados_full[,f]),f] <- mean(dados_full[,f], na.rm = TRUE)
    }
  }
}

# Verifica o dataframe
str(dados_full)
names(dados_full)
dim(dados_full)
sum(is.na(dados_full))
