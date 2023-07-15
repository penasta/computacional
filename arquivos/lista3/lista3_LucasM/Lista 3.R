setwd("C:/Users/Acer/Documents/UnB/Computacional/Lista 3")
#_______________________________________________________________________________
#_______________________________________________________________________________

# QUESTAO 1

library(MASS)

# Set the mean vector and covariance matrix
mu <- c(0, 1, 2)
Sigma <- matrix(c(1.0, -0.5, 0.5, -0.5, 1.0, -0.5, 0.5, -0.5, 1.0), nrow = 3)

# Generate random observations
n <- 200
set.seed(321)  

rmvn.cholesky <-
  function(n, mu, Sigma) {
    p <- length(mu)
    Q <- chol(Sigma)
    Z <- matrix(rnorm(n*p), nrow=n, ncol=p)
    X <- Z %*% Q + matrix(mu, n, p, byrow=TRUE)
    X
  }

a3 <- rmvn.cholesky(200, mu, Sigma)
plot(a3, xlab = "x", ylab = "y", pch = 20)
# print(colMeans(a3))
print(cor(a3))# se aproximam

# Create the pairs plot
pairs(a3, main = "Scatter Plots of Random Observations",
      pch = 19, col = "red")


#_______________________________________________________________________________
#_______________________________________________________________________________

# QUESTAO 2

# Definir a função que será integrada
funcao <- function(x) {
  return(exp(1)^x)
}

# Set the number of iterations
N <- 100000

# Define the function to estimate θ

funcao <- function(x) {
  return(exp(1)^x)
}


# Simple Monte Carlo method
simple_mc <- function(N) {
  u <- runif(N)  # Generate N random numbers from a uniform distribution
  theta <- funcao(u)  # Estimate θ using the function
  estimate <- mean(theta)  # Calculate the mean of θ
  return(estimate)
}

# Antithetic variate method
antithetic_mc <- function(N) {
  u <- runif(N/2)  # Generate N/2 random numbers from a uniform distribution
  u_antithetic <- 1 - u  # Generate the antithetic variates
  u_combined <- c(u, u_antithetic)  # Combine the variates
  theta <- funcao(u_combined)  # Estimate θ using the function
  estimate <- mean(theta)  # Calculate the mean of θ
  return(estimate)
}

# Estimar θ usando o simples Monte Carlo 
estimate_simple_mc <- simple_mc(N)

# Estimate θ using the antithetic variate method
estimate_antithetic_mc <- antithetic_mc(N)

# Print the results
print(paste("θ estimando usando o simples método de Monte Carlo:", estimate_simple_mc))# mais longe
print(paste("θ estimando usando método 'antithetic variate' :", estimate_antithetic_mc))# mais perto
print(paste("Valore real de θ :", integrate(funcao,0,1)[1]))



#_______________________________________________________________________________
#_______________________________________________________________________________

# QUESTAO 3

# detach(package:e1071)
# unloadNamespace("e1071")
# install.packages("e1071")
# install.packages("mlbench")

library(e1071)# Pacote contendo a implementação do SVM
library(mlbench)
data(Glass, package="mlbench")
## split data into a train and test set
index <- 1:nrow(Glass)
N <- trunc(length(index)/3)
set.seed(123)
testindex <- sample(index, N)
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
## svm
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 0.1)
svm.model
svm.pred <- predict(svm.model, testset[,-10])
svm.pred
## compute svm confusion matrix
matriz_confusao = table(pred = svm.pred, true = testset[,10])
matriz_confusao = as.matrix(matriz_confusao)

# install.packages("caret")
library(caret)

# Assuming you have a confusion matrix named "confusion_matrix"

# Set the range of gamma values to evaluate
gammas <- c(0.01, 0.1 ,1)

# Perform k-fold cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(seq(1:6), k = 10)

erros = numeric()
for (gamma in gammas) {
  erro1 <- numeric()  # Variável para armazenar o erro para cada fold
  erro2 <- numeric()  # Variável para armazenar o erro para cada fold
  erro3 <- numeric()  # Variável para armazenar o erro para cada fold
  
  for (i in 1:length(folds)) {
    # Divide os dados em conjunto de treinamento e teste para o fold atual
    train_data <- matriz_confusao[-folds[[i]], ]
    test_data <- t(as.matrix(matriz_confusao[folds[[i]], ]))
    
    # Extrai as variáveis de entrada (X) e de saída (Y) dos dados de treinamento e teste
    train_X <- train_data[, -ncol(train_data)]
    train_Y <- as.vector(train_data[, ncol(train_data)])
    test_X <- as.vector(test_data[, -ncol(test_data)])
    test_Y <- as.vector(test_data[, ncol(test_data)])
    
    # Treina o modelo SVM com o valor de gamma atual
    modelo1 <- svm(Type ~ ., data = trainset, cost = 100, gamma = gammas[1])
    modelo2 <- svm(Type ~ ., data = trainset, cost = 100, gamma = gammas[2])
    modelo3 <- svm(Type ~ ., data = trainset, cost = 100, gamma = gammas[3])
    
    # Realiza a predição nos dados de teste
    predicao1 <- predict(modelo1)
    predicao2 <- predict(modelo2)
    predicao3 <- predict(modelo3)
    
    # Calcula a taxa de erro no fold atual
    erro1[i] <- sum(predicao1 != test_Y) / length(test_Y)
    erro2[i] <- sum(predicao2 != test_Y) / length(test_Y)
    erro3[i] <- sum(predicao3 != test_Y) / length(test_Y)
    
    }
  # Calcula o erro médio para o valor de gamma atual
  avg_erro <- c(mean(erro1),mean(erro2),mean(erro3))
  erros <- c(erros, avg_erro)
}

# Encontra o valor de gamma com o menor erro
melhor_gamma <- gammas[which.min(erros)]
melhor_error <- min(erros)

cat("Melhor valor de gamma:", best_gamma, "\n")
cat("Erro de teste correspondente:", best_error, "\n")

# Se mudarmos os erros para um valor menor, teremos um erro melhor

# novo gamma:
# Set the range of gamma values to evaluate
gammas <- c(0.0001, 0.001 ,0.01)

# percebe-se que a partir de 0.01 o erro converge para 132.833333 
# e qualquer valor abaixo seria suficiente para avaliar a afirmação

gammas <- c(1,10,100)

# o mesmo serve para gammas maiores, caso utilize valores maiores percebemos
# esse mesmo comportamento, contudo convergindo para um valor um pouco maior
# sendo 134.5 p qlqr valor acima de 1.





#_______________________________________________________________________________
#_______________________________________________________________________________

# QUESTAO 4

  # 9.3_________________________________________________________________________


# Criar o vetor de valores de x
x <- seq(-10, 10, length.out = 1000)

# Definir a função densidade da distribuição Cauchy

densidade_cauchy <- function(x, locacao, escala) {
  return(1 / (3.141593 * escala * (1 + ((x - locacao) / escala)^2)))
}

# Definir os parâmetros da distribuição Cauchy
locacao <- 0  # Parâmetro de localização
escala <- 1  # Parâmetro de escala = teta

# Número de interações
N <- 10000

# Número de amostras descartadas
descarte <- 1000

# Iniciar a corrente
chain <- numeric(N + descarte)
chain[1] <- 0  # Starting value for the chain

# distribuição proposta incialmente (e.g., normal distribution)
dp_prop <- 1  # Desvio padrao da distribuição proposta incialmente

# Definir a desidade esperada (cauchy padrao)
target_density <- function(x) {
  return(1 / (pi * (1 + x^2)))
}

# Algoritmo de Metropolis-Hastings 
for (i in 2:(N + descarte)) {
  # Generate a candidate sample from the proposal distribution
  candidato <- rnorm(1, mean = chain[i - 1], sd = dp_prop)
  
  # Calculate the acceptance ratio
  acceptance_ratio <- target_density(candidato) / target_density(chain[i - 1])
  
  # Aceitar ou rejeitar a amostra do candidato
  if (runif(1) < acceptance_ratio) {
    chain[i] <- candidato  # Aceite o candidato
  } else {
    chain[i] <- chain[i - 1]  # rejeitar o candidato e manter o valor anterior
  }
}

# Desccarte das amostras
chain <- chain[(descarte + 1):(N + descarte)]

# Calcular os decis gerados
decis_gerado <- quantile(chain, probs = seq(0.1, 0.9, by = 0.1))

# Calcular os decis da distribuição
decis_cauchy <- qcauchy(seq(0.1, 0.9, by = 0.1))

# Valor dos decis
print("Decis das observações geradas:")
print(decis_gerado)

print("Decis da Cauchy padrão:")
print(decis_cauchy)


curve(densidade_cauchy(x, locacao, escala), from = -10, to = 10,
      xlab = "x", ylab = "Densidade", main = "Função densidade da distribuição Cauchy")
# Create the density plot
plot(density(chain), main = "Simulação", xlab = "x", ylab = "Density", col = "blue",
     xlim = c(-10,10))


# Calcular os valores da função densidade para cada valor de x
densidades <- densidade_cauchy(x, locacao, escala)

# Criar o gráfico usando a função plot()
plot(x, densidades, type = "l",
     xlab = "x", ylab = "Densidade",
     main = "Função densidade da distribuição Cauchy e dos valores gerados")
lines(density(chain), col = "blue")
legend( legend = c("Gerada", "Cauchy Padrão"),
       col = c("blue", "black"), lty = 1, pch =1, cex = 0.8, x = 2.3, y = 0.3)


  # 9.7_________________________________________________________________________

# Set the number of iterations
N <- 1000

# Set the correlation coefficient
rho <- 0.9

# Initialize the chains
X <- numeric(N)
Y <- numeric(N)

# Set the initial values for X and Y
X[1] <- 0
Y[1] <- 0

# Run the Gibbs sampler
for (t in 2:N) {
  # Sample X[t] given Y[t-1]
  X[t] <- rnorm(1, mean = rho * Y[t - 1], sd = sqrt(1 - rho^2))
  
  # Sample Y[t] given X[t]
  Y[t] <- rnorm(1, mean = rho * X[t], sd = sqrt(1 - rho^2))
}

# Set the burn-in length
burn_in <- 100

# Discard the burn-in samples
X_discarded <- X[(burn_in + 1):N]
Y_discarded <- Y[(burn_in + 1):N]

# Plot the bivariate normal chain after burn-in
plot(X_discarded, Y_discarded, type = "l", xlab = "X", ylab = "Y", main = "Bivariate Normal Chain (After Burn-In)")

# Aplicando a Regressao

Banco = as.data.frame(Y_discarded)
Banco$X_discarded = X_discarded
names(Banco) <- c("Y","X")

# Passo 2: Criação do modelo linear
modelo <- lm(Y ~ X, data = Banco)

# Passo 3: Conferir a normalidade dos dados pelos quantis
# Q-Q plot
qqnorm(modelo$residuals)
qqline(modelo$residuals)

# Shapiro-Wilk teste
shapiro.test(modelo$residuals)# normal

# Passo 4: Confira os resídios para avaliar a variancia constante
# Plot dos residuos pelos valores preditos
plot(modelo$fitted.values, modelo$residuals,
     xlab = "Valores Preditos", ylab = "Resíduos")
abline(h = 0, col = "red", lty = 2)
# aparentemente existe uma dispersão ok dos dados


# Gráfico de dispersão
plot(X, Y, main = "Regressão Linear", xlab = "X", ylab = "Y")

# Linha de regressão
abline(modelo, col = "red")
