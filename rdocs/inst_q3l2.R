
# 1)

# dividir em teste e treino

# dividir treino em 10 blocos <=> cv E {1,2,...,10}

# considerar valores de gamma e sigma

# expand.grid

# loop sobre valores
## laço sobre cv 
### rodar SVM em treino [-cv,]
### aplicar o classificador em cv, para calcular o erro (e salva)
## calcular média dos erros e salvar

# escolher o par (gamma*;sigma*) com menor erro

# rodar SVM no treino inteiro


