any(is.na(dados$AMOUNT))

# valores missing
sapply(df, function(x) sum(is.na(x)))

#Remove as linhas que tenha NA daquela coluna
df.merge <- df.merge[complete.cases(df.merge$salario_hora),]

# No lugar de NA colocar 0
actual$faturamento_2015[is.na(actual$faturamento_2015)] = 0
