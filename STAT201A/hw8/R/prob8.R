library(mvtnorm)


muX <- 500
muY <- 480
sX <- 90
sY <- 100
rho <- 0.5

mu <- c(muX,muY)
covXY <- rho*sX*sY

sigma <- matrix(c(sX^2, covXY, covXY, sY^2), nrow=2, byrow=TRUE)
test <- rmvnorm(n=1e7, mean=mu, sigma=sigma)
test <- data.frame(X=test[,1],Y=test[,2])

t1 <- subset(test, (549.9 < X & X < 550.1))
t2 <- subset(t1, (X > Y))
t3 <- subset(test, (549.9 < X & X < 550.1 & Y < X))
sample <- (test[,1] > 549.99 & test[,1] < 550.01 )

pXY <- (1e-6)*sum(as.numeric(test[,1] > test[,2]))

standardizeX <- function(x,muX,sX) (x-muX)/sX

integrand <- function(x) dnorm(x,mean=muX,sd=sX)*pnorm((x-rho*(sY/sX)*(x-muX)-muY)/(sY*sqrt(1-rho^2)))

pXY <- integrate(f=integrand,lower=-Inf,upper=Inf)

muY2 <- standardizeX(x=550,muX,sX)*sY*rho+muY

pXY_theory <- integrate(f=integrand,lower=-Inf,upper=Inf)

library(ggplot2)

set.seed(112)

r <- round(runif(n=1,min=2,max=5),digits=2)
lambda <- round(runif(n=1,min=1,max=2),digits=2)

n <- 100
X <- rgamma(n=n,shape=r,rate=lambda)

# hist(simulation, freq=FALSE, main="Simulated Density of Gamma(2.19,1.69)",xlab="X",col="grey")
# curve(dgamma(x,shape=r,rate=lambda),xname="x",from=0,to=4,ylab="frequency")
#+ scale_colour_manual(name = 'Density', values = c('red', 'blue')) 
#opts(legend.position = c(0.85, 0.85))

p1 <- ggplot(data.frame(x=c(0,5))) + geom_histogram(aes(x = X, y = ..density..), alpha = 0.25, binwidth=0.25) + stat_function(fun=dgamma, arg=list(shape=r,rate=lambda),colour="darkgreen") 

print(p1)

mu_hat_1 <- (1/n)*sum(X^1)
mu_hat_2 <- (1/n)*sum(X^2)

r_hat <-  (mu_hat_1^2)/(mu_hat_2 - mu_hat_1^2)
lambda_hat <- mu_hat_1/(mu_hat_2 - mu_hat_1^2)

generateEstimators <- function(n,r,lambda) {
  X <- rgamma(n=n,shape=r,rate=lambda)
  mu_hat_1 <- (1/n)*sum(X^1)
  mu_hat_2 <- (1/n)*sum(X^2)
  r_hat <-  (mu_hat_1^2)/(mu_hat_2 - mu_hat_1^2)
  lambda_hat <- mu_hat_1/(mu_hat_2 - mu_hat_1^2)
  return(c(r_hat,lambda_hat))
}

times <- 999

more_estimators <- t(mapply(FUN=generateEstimators, n=rep(n,times=times),r=rep(r,times=times),lambda=rep(lambda,times=times)))

r_hat <- c(r_hat,more_estimators[,1])
lambda_hat <- c(lambda_hat, more_estimators[,2])

M <- function(msat) (msat-500)/90
V <- function(vsat) (vsat-480)/100

M(550)
