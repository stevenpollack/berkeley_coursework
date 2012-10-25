### R code from vignette source './tex/stat201a_hw4.Rnw'

###################################################
### code chunk number 1: prepare_data
###################################################
calculateDistance <- function(X1,X2) { #L1 distance
  return(sum(abs(X1-X2)))
}

findL1DistanceOfBinomialAndPoisson <- function(n,p) {
  X <- dbinom(x=0:n,size=n,prob=p) # vector of P(X=x), 0 <= x <= n
  Y <- dpois(x=0:n,lambda=n*p) # vector of P(Y=x), 0 <= x <= n
  return(0.5*calculateDistance(X,Y)) #scale according to definition of d(P_1,P_2)
}

findL1DistanceOfBinomialAndNormal <- function(n,p) {
  X <- dbinom(x=0:n,size=n,prob=p)
  Z <- ( pnorm(q=seq(from=0.5, to=n+0.5, by=1),
               mean=n*p, sd=sqrt(n*p*(1-p))
               ) - pnorm(q=seq(from=-0.5,to=n-0.5,by=1),
                         mean=n*p, sd=sqrt(n*p*(1-p)) ) )
  return(0.5*calculateDistance(X,Z)) 
}

findSupNormOfBinomialAndPoisson <- function(n,p) {
  X <- dbinom(x=0:n,size=n,prob=p)
  Y <- dpois(x=0:n,lambda=n*p)
  return(max(abs((X-Y))))
}

findSupNormOfBinomialAndNormal <- function(n,p) {
  X <- dbinom(x=0:n,size=n,prob=p)
  Z <- ( pnorm(q=seq(from=0.5, to=n+0.5, by=1),
               mean=n*p, sd=sqrt(n*p*(1-p)) 
               ) - pnorm(q=seq(from=-0.5, to=n-0.5,by=1),
                         mean=n*p, sd=sqrt(n*p*(1-p)) ) )
  return(max(abs((X-Z))))
}


# set up grid
num_of_trials <- seq(from=20,to=10**3,by=10)
p_values <- seq(from=10**-16, to=0.5,
  length.out=length(num_of_trials))

surfaces <- expand.grid(n=num_of_trials, p=p_values, 
                        KEEP.OUT.ATTRS=FALSE)

#calculate all the distances
surfaces$L1_distance_poisson <- with(surfaces,mapply(
    findL1DistanceOfBinomialAndPoisson, n=n, p=p))

surfaces$sup_norm_poisson <- with(surfaces, mapply(
  findSupNormOfBinomialAndPoisson, n=n, p=p))

surfaces$L1_distance_normal <- with(surfaces, mapply(
  findL1DistanceOfBinomialAndNormal, n=n, p=p))

surfaces$sup_norm_normal <- with(surfaces, mapply(
  findSupNormOfBinomialAndNormal, n=n, p=p))


###################################################
### code chunk number 2: plot_L1_poisson
###################################################
library(ggplot2)

maximum <- max(surfaces$L1_distance_poisson)
minimum <- min(surfaces$L1_distance_poisson)

d <- ggplot() + layer(data=surfaces, geom="tile",
                      mapping=aes(x=n, y=p, 
                                   fill=L1_distance_poisson))

d <- d + scale_fill_gradientn(
  colours=rainbow(7), breaks=c(seq(from=minimum,to=.1,
                                   length.out=6),maximum))

d <- d + opts(
  panel.background=theme_rect(fill="white", colour="black"),
  panel.grid.major=theme_line(colour = "grey90"),
  title="L1 distance between \n Poisson and Binomial",
  legend.title=theme_blank()) 

print(d)


###################################################
### code chunk number 3: plot_L1_normal
###################################################
maximum <- max(surfaces$L1_distance_normal)
minimum <- min(surfaces$L1_distance_normal)

d <- ggplot() + layer(data=surfaces, geom="tile",
                 mapping=aes(x=n, y=p, fill=L1_distance_normal))
d <- d + scale_fill_gradientn(
  colours=rainbow(7),breaks=c(seq(from=minimum,to=.1,
                                  length.out=6),maximum)) 
d <- d + opts(
  panel.background=theme_rect(fill="white",colour="black"),
  panel.grid.major=theme_line(colour = "grey90"),
  title="L1 distance between \n Normal and Binomial",
  legend.title=theme_blank())

print(d)


###################################################
### code chunk number 4: plot_sup_norm_poisson
###################################################
maximum <- max(surfaces$sup_norm_poisson)
minimum <- min(surfaces$sup_norm_poisson)

d <- ggplot() + layer(data=surfaces, geom="tile",
                      mapping=aes(x=n, y=p, fill=sup_norm_poisson)) 

d <- d + scale_fill_gradientn(
  colours=rainbow(7),
  breaks=c(seq(from=minimum,to=.015,length.out=6),.02,.035,maximum))

d <- d + opts(
  panel.background=theme_rect(fill="white",colour="black"),
  panel.grid.major=theme_line(colour = "grey90"),
  title="Max distance between \n Poisson and Binomial",
  legend.title=theme_blank()) 

print(d)


###################################################
### code chunk number 5: plot_sup_norm_normal
###################################################
maximum <- max(surfaces$sup_norm_normal)
minimum <- min(surfaces$sup_norm_normal)

d <- ggplot() + layer(data=surfaces, geom="tile",
                      mapping=aes(x=n, y=p, fill=sup_norm_normal))

d <- d + scale_fill_gradientn(
  colours=rainbow(7),
  breaks=c(seq(from=minimum,to=.04,length.out=5),.07,.1,.13,maximum))

d <- d + opts(
  panel.background=theme_rect(fill="white",colour="black"),
  panel.grid.major=theme_line(colour = "grey90"),
  legend.position="right",
  title="Max distance between \n Normal and Binomial",
  legend.title=theme_blank())

print(d)


