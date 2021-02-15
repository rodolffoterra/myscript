# SQL Analytics

# 0.0 Soucer ----
# https://www.w3schools.com/sql/sql_select.asp

# 1.0 Load Package ----
library(sqldf)

# 2.0 Load Data ----

tb_func <- read.csv('funcionarios.csv')


# 3.0 DQL (Data Query Language) Instructions ----

# Select: Select table
sqldf("select * from  tb_func")

# order by  
# GREATER FOR SMALLER
sqldf("select * from  tb_func ORDER BY salario_hora ASC")

# Order by
# Smaller for Greater
sqldf("select * from  tb_func ORDER BY salario_hora DESC")
sqldf("select * from  tb_func ORDER BY salario_hora DESC, reg_procedencia DESC")



# Select: Select number of line
sqldf("select count(*) from  tb_func")

# Select: Select number of line with condition
sqldf("select count(*) from  tb_func where idade = 40")
sqldf("select count(*) from  tb_func where idade != 40")
sqldf("select count(*) from  tb_func where idade > 40")
sqldf("select count(*) from  tb_func where idade >= 40")

# Logical operator &b
sqldf("select * from  tb_func where idade < 30 and estado_civil = 'casado'")

# Logical operator & with filter column
sqldf("select (salario_hora) from  tb_func where idade < 30 and estado_civil = 'casado'")

# Logical operator or
sqldf("select * from  tb_func where idade < 30 or estado_civil = 'casado'")

# between
sqldf('select * from tb_func where idade >= 30 and idade <= 35')
sqldf('select * from tb_func where idade between 30 and 35')


# Like (coja encontra tal caracter)
sqldf("SELECT * from tb_func WHERE grau_instrucao LIKE '%medio' ")
sqldf("SELECT * from tb_func WHERE grau_instrucao LIKE '%medio%' ")
sqldf("SELECT * from tb_func WHERE grau_instrucao LIKE 'ensino%' ")

# in (cujo o valor está dentro da opção)
sqldf("SELECT * from tb_func WHERE idade IN (20,30,40,50) ")

# NULL
sqldf("SELECT * from 'tb_func' WHERE numero_filhos IS null")
sqldf("SELECT * from 'tb_func' WHERE numero_filhos IS NOT null")

# update
tb_func <- sqldf(c("update tb_func set numero_filhos = 25 where numero_filhos IS null",
              "select * from main.tb_func"))

tb_func <- sqldf(c("update tb_func set numero_filhos = null where numero_filhos IS 25",
                   "select * from main.tb_func"))

# Delete
tb_func <- sqldf(c("DELETE from 'tb_func' WHERE numero_filhos IS null",
                   "SELECT * from main.tb_func"))

# insert 
tb_func <- sqldf(c(" INSERT INTO tb_func (
            ID, estado_civil, grau_instrucao, numero_filhos, salario_hora, idade, reg_procedencia)
            values (38, 'casado', 'ensino medio', 3, 4.5, 30, 'capital')",
              "select * from main.tb_func"))

# Minimun, maximun and average value
sqldf("select MIN(salario_hora), MAX(salario_hora), AVG(salario_hora), count(*), SUM(salario_hora) from  tb_func ")


# distinct
sqldf("SELECT DISTINCT reg_procedencia from  tb_func")


# Group By
sqldf("SELECT round(AVG(salario_hora),2) Média, reg_procedencia região
      FROM tb_func
      GROUP BY reg_procedencia")

sqldf("SELECT round(AVG(salario_hora),2), reg_procedencia, grau_instrucao
      FROM tb_func
      GROUP BY reg_procedencia, grau_instrucao")


# 4.1 Exercise ----

# Crie uma instrução SQL que retorna a média de idade, número de filhos e grau de 
# instrução dos funcionários cujo salário_hora estiver acima da média de todos os funcionários.

# Retorne os dados somente de funcionários da capital e estado civil casado, 
# com ordem decrescente da média de idade

sqldf("select round(AVG(idade)), numero_filhos, grau_instrucao
      FROM tb_func
      WHERE reg_procedencia ='capital' 
        and  estado_civil = 'casado'
        and salario_hora > (SELECT AVG(salario_hora) FROM tb_func)
      GROUP BY numero_filhos, grau_instrucao
      ORDER BY round(AVG(idade)) DESC")

# 4.2 Exercise ----

# Retorne todos os registro dos funcionários com 2 filhos

sqldf("SELECT * FROM tb_func 
    WHERE cast(numero_filhos AS integer) = 2")

# or

sqldf("SELECT * FROM tb_func 
    WHERE cast(numero_filhos AS character) = 2")


# 4.3 Exercise ----

# Retorno a média de salário hora or estado


tb_endereco <- data.frame("id_end" = c(1001:1005),
                          "rua" = c('Jaguar', 'Mercedes Benz','BMW','Ferrari','McLaren'),
                          "numero" = c(40, 140, 20, 32, 45),
                          "bairro"= c('Tijuca','Centro','Tijuca','Centro','Centro'),
                          "cep" = c('24239-900','12098-900',"23232-900",'99872-900','43982-900'),
                          "estado" = c('Rio de Janeiro','Minas Gerais','Rio de Janeiro','Minas Gerais','Minas Gerais'),
                          "pais" =c('Brasil', 'Brasil', 'Brasil', 'Brasil', 'Brasil'),
                          "id_func" = c(2, 6, 3, 11, 17))


sqldf("select * from  tb_endereco")

# Join

sqldf("SELECT round(AVG(f.salario_hora),2) AVG, e.estado Estado
      FROM tb_func f, tb_endereco e
      WHERE f.id = e.id_func
      GROUP BY e.Estado")















