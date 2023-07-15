# --------------------------------------------------------------------------- #
seed <- 150167636
pacman::p_load(tidyverse,car,mvtnorm,e1071,mlbench,caret)
# --------------------------------------------------------------------------- #

# 1) Rizzo – 3.14

n <- 200
mu <- c(0,1,2)
set.seed(seed)
Sigma <- matrix(c(1, -.5, .5,
                  -.5,1,-.5,
                  .5,-.5,1), 
                nrow = 3, ncol = 3)


r <- rmvnorm(n, mean = mu, sigma=Sigma, method="chol")

pairs(r)
colMeans(r)
cor(r)

# Notamos que tanto o vetor mu quanto a matriz sigma aparentam convergir para o verdadeiro parâmetro especificado.

# --------------------------------------------------------------------------- #

# 2) Rizzo – 5.7 (não precisa comparar com o valor teórico de 5.6)

m <- seed
a <- - 12 + 6 * (exp(1) - 1)
U <- runif(m)
T1 <- exp(U) #simple MC
T2 <- exp(U) + a * (U - 1/2) #controlled

mean(T1)
mean(T2)

(var(T1) - var(T2)) / var(T1)

# --------------------------------------------------------------------------- #

# 3) Validação Cruzada (usa os códigos no final para iniciar)

data(Glass, package="mlbench")
## split data into a train and test set
index <- 1:nrow(Glass)
N <- trunc(length(index)/3)
set.seed(seed)
testindex <- sample(index, N)
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
## svm
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 0.1)
svm.pred <- predict(svm.model, testset[,-10])
## compute svm confusion matrix
matriz_confusao <- table(pred = svm.pred, true = testset[,10])
(matriz_confusao <- as.matrix(matriz_confusao))

# a. Fixando o cost = 100 (penalidade para furar a margem do SVM), determine via k-fold 
# validação cruzada com k=10, o melhor valor para gamma (a largura de banda do kernel)
# e reporta esse valor e o erro de teste.

ctrl <- trainControl(method = "cv", number = 10)

model <- train(Type ~ RI + Na + Mg + Al + Si + K + Ca + Ba + Fe, data = Glass, trControl = ctrl)
print(model)

model$finalModel

# Ou seja, o melhor valor para gamma aparenta ser entre 0 e 1; enquanto o erro do teste ficou na casa de 20%.

# b. Agora otimiza o custo e gamma simultaneamente usando a mesma validação cruzada.
# Reporta os valores do custo, do gamma e do erro de teste.

#predict(model)

svm(Type ~ ., data = Glass, cost = 100, gamma = c(0.01, 0.1 ,1))
svm(Type ~ ., data = Glass, cost = 100, gamma = c(0.001, 0.05 ,0.15))

# Ou seja, o melhor valor de gama:
svm(Type ~ ., data = Glass, cost = 100, gamma = 0.1)

# --------------------------------------------------------------------------- #

# 4) Rizzo – 9.3 e 9.7

# 9.3:

rw.Metropolis <- function(x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rt(1, 1)
    if (u[i] <= (dcauchy(y) / dcauchy(x[i-1])))
      x[i] <- y else {
        x[i] <- x[i-1]
        k <- k + 1
      }
  }
  return(list(x=x, k=k))
}

teste <- rw.Metropolis(0,2000)
hist(teste[["x"]][1000:2000])
quantile(teste[["x"]][1000:2000])
cauchy <- rcauchy(1000)
quantile(cauchy)

plot(density(teste$x), main = "Valores gerados", xlab = "x", ylab = "Density", col = "#a11d21",
     xlim = c(-10,10))

set.seed(seed)
plot(density(rcauchy(1000, location = 0, scale = 1)), main = "Valores gerados", xlab = "x", ylab = "Density", col = "#a11d21",
     xlim = c(-10,10))

x <- seq(-10, 10, length.out = 2000)
plot(x,dcauchy(x, location = 0, scale = 1), main = "Valores gerados", xlab = "x", ylab = "Density", col = "#a11d21",
     xlim = c(-10,10))


set.seed(seed)
plot(density(rcauchy(2000, location = 0, scale = 1)), type = "l",
     xlab = "x", ylab = "Densidade",
     main = "Função densidade da distribuição Cauchy e dos valores gerados",
     xlim = c(-10,10))
lines(density(teste$x), col = "#a11d21")
lines(density(dcauchy(x, location = 0, scale = 1)), col = "blue")
legend( legend = c("Simulação Cauchy Metropolis-Hasting",
                   "Quantis teóricos da Cauchy padrão", "Simulação Cauchy Padrão `rcauchy`"),
        col = c("#a11d21","blue", "black"), lty = 1, cex = .75, x = 1.3, y = .95)

# 9.7:

N <- 5000
burn <- 1000
X <- matrix(0, N, 2)
rho <- -.9
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2
X[1, ] <- c(mu1, mu2)
for (i in 2:N) {
  x2 <- X[i-1, 2]
  m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
  X[i, 1] <- rnorm(1, m1, s1)
  x1 <- X[i, 1]
  m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
  X[i, 2] <- rnorm(1, m2, s2)
}
b <- burn + 1
X <- X[b:N, ]

df <- data.frame(X)
fit <- lm(df$X2 ~ df$X1)
summary(fit)

shapiro.test(fit$residuals) # normalidade ok

plot(fit$fitted.values, fit$residuals,
     xlab = "Valores ajustados", ylab = "Resíduos") # variância ok

# Gráfico de dispersão + linha de mínimos quadrados (regressão)
plot(df$X1, df$X2, main = "Regressão Linear", xlab = "X", ylab = "Y")
abline(fit, col = "blue")

# --------------------------------------------------------------------------- #

# Bônus:

# --------------------------------------------------------------------------- #