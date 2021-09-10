import pandas as pd
alunos = pd.DataFrame({'Nome': ['Ary', 'Cátia', 'Denis', 'Beto', 'Bruna', 'Dara', 'Carlos', 'Alice'], 
                        'Sexo': ['M', 'F', 'M', 'M', 'F', 'F', 'M', 'F'], 
                        'Idade': [15, 27, 56, 32, 42, 21, 19, 35], 
                        'Notas': [7.5, 2.5, 5.0, 10, 8.2, 7, 6, 5.6]}, 
                        columns = ['Nome', 'Idade', 'Sexo', 'Notas'])
alunos

# Criando uma função para gerar um status de cada aluno
def add_obs(aluno):
    obs = aluno['Nome'] + ' teve a nota ' + str(aluno['Notas'])

    if aluno['Notas'] >= 7:
        obs = obs + ' e foi aprovado!'
    else:
        obs = obs + ' e foi reprovado!'

    return obs

# Aplicando a função acima para cada linha do dataframe
# axis=1 indica que queremos executar a função nas linhas
alunos.apply(add_obs, axis=1)

alunos['Notas-Média(Notas)'] = alunos.Notas - alunos.Notas.mean()

alunos['Notas-Média(Notas)'] = alunos['Notas'].apply(lambda x: x - alunos['Notas'].mean())

alunos['Faixa Etária'] = alunos['Idade'].apply(lambda x: 'Menor que 20 anos' 
                                               if x < 20 
                                               else 'Entre 20 e 40 anos' if (x >=20 and x <= 40)
                                               else 'Acima de 40 anos')
