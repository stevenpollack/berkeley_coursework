library(ggplot2)
library(reshape2)
library(rgl)

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
  Z <- ( pnorm(q=seq(from=0.5, to=n+0.5, by=1), mean=n*p, sd=sqrt(n*p*(1-p)) )
        + (-1)*pnorm(q=seq(from=-0.5,to=n-0.5,by=1), mean=n*p, sd=sqrt(n*p*(1-p)) ) )
  return(0.5*calculateDistance(X,Z)) 
}

findSupNormOfBinomialAndPoisson <- function(n,p) {
  X <- dbinom(x=0:n,size=n,prob=p)
  Y <- dpois(x=0:n,lambda=n*p)
  return(max(abs((X-Y))))
}

findSupNormOfBinomialAndNormal <- function(n,p) {
  X <- dbinom(x=0:n,size=n,prob=p)
  Z <- ( pnorm(q=seq(from=0.5, to=n+0.5, by=1), mean=n*p, sd=sqrt(n*p*(1-p)) )
         + (-1)*pnorm(q=seq(from=-0.5,to=n-0.5,by=1), mean=n*p, sd=sqrt(n*p*(1-p)) ) )
  return(max(abs((X-Z))))
}

rescaleVector <- function(vector) {
  maximum <- max(vector)
  minimum <- min(vector)
  (vector-minimum)/(maximum-minimum)
}

# set up grid
num_of_trials <- seq(from=20,to=10**3,by=10)
p_values <- seq(from=10**-16,to=0.5,length.out=length(num_of_trials))
surfaces <- expand.grid(n=num_of_trials, p=p_values, KEEP.OUT.ATTRS=FALSE)

#calculate all the distances
surfaces$L1_distance_poisson <- with(surfaces, mapply(findL1DistanceOfBinomialAndPoisson, n=n, p=p))

surfaces$sup_norm_poisson <- with(surfaces, mapply(findSupNormOfBinomialAndPoisson, n=n, p=p))

surfaces$L1_distance_normal <- with(surfaces, mapply(findL1DistanceOfBinomialAndNormal, n=n, p=p))

surfaces$sup_norm_normal <- with(surfaces, mapply(findSupNormOfBinomialAndNormal, n=n, p=p))

# save the max and mins of the distances for analysis

surfaces$L1_poisson_max <- max(surfaces$L1_distance_poisson)
surfaces$L1_poisson_min <- min(surfaces$L1_distance_poisson)

surfaces$L1_normal_max <- max(surfaces$L1_distance_normal)
surfaces$L1_normal_min <- min(surfaces$L1_distance_poisson)

surfaces$sup_poisson_max <- max(surfaces$sup_norm_poisson)
surfaces$sup_poisson_min <- min(surfaces$sup_norm_poisson)

surfaces$sup_normal_max <- max(surfaces$sup_norm_normal)
surfaces$sup_normal_min <- min(surfaces$sup_norm_normal)

#rescale distances to [0,1] 
surfaces$L1_distance_poisson <- rescaleVector(surfaces$L1_distance_poisson)

surfaces$L1_distance_normal <- rescaleVector(surfaces$L1_distance_normal)

surfaces$sup_norm_poisson <- rescaleVector(surfaces$sup_norm_poisson)

surfaces$sup_norm_normal <- rescaleVector(surfaces$sup_norm_normal)


# make plots

plot_L1_poisson <- ggplot() + layer(data=surfaces, geom="tile", mapping=aes(x=n, y=p, fill=L1_distance_poisson)) + scale_fill_gradientn(colours=c("light yellow","brown"),breaks=seq(from=0,to=1,length.out=5)) + opts(panel.background=theme_rect(fill="white",colour="black"), panel.grid.major=theme_line(colour = "grey90"),title="L1 distance between \n Poisson and Binomial") + guides(fill=FALSE)

plot_L1_normal <- ggplot() + layer(data=surfaces, geom="tile", mapping=aes(x=n, y=p, fill=L1_distance_normal)) + scale_fill_gradientn(colours=c("light yellow","brown"),breaks=seq(from=0,to=1,length.out=5)) + opts(panel.background=theme_rect(fill="white",colour="black"), panel.grid.major=theme_line(colour = "grey90"), title="L1 distance between \n Normal and Binomial") + guides(fill=FALSE)

plot_sup_norm_poisson <- ggplot() + layer(data=surfaces, geom="tile", mapping=aes(x=n, y=p, fill=sup_norm_poisson)) + scale_fill_gradientn(colours=c("light yellow","brown"),breaks=seq(from=0,to=1,length.out=5)) + opts(panel.background=theme_rect(fill="white",colour="black"), panel.grid.major=theme_line(colour = "grey90"),title="Max distance between \n Poisson and Binomial") + guides(fill=FALSE)

plot_sup_norm_normal <- ggplot() + layer(data=surfaces, geom="tile", mapping=aes(x=n, y=p, fill=sup_norm_normal)) + scale_fill_gradientn(colours=c("light yellow","brown"),breaks=seq(from=0,to=1,length.out=5)) + opts(panel.background=theme_rect(fill="white",colour="black"), panel.grid.major=theme_line(colour = "grey90"), legend.position="bottom", title="Max distance between \n Normal and Binomial", legend.title=theme_blank())

arrange(ncol=2, plot_L1_poisson, plot_L1_normal, plot_sup_norm_poisson, plot_sup_norm_normal)








# 
# (plot_L1_poisson <- ggplot(data=surfaces, aes(x=n,y=p)) 
#  + geom_tile(aes(fill=poisson_L1_quality)) + scale_colour_gradient(low="red", high="purple")
#  + opts(title="L1 distance between \n Poisson and Binomial"))
# 
# (plot_L1_normal <- ggplot(data=surfaces, aes(x=n,y=p)) 
#  + geom_tile(aes(fill=normal_L1_quality)) + guides(fill=FALSE)
#  + opts(title="L1 distance between \n Normal and Binomial"))
# 
# (plot_sup_norm_poisson <- ggplot(data=surfaces, aes(x=n,y=p)) 
#  + geom_tile(aes(fill=poisson_sup_norm_quality)) + guides(fill=FALSE)
#  + opts(title="Max distance between \n Poisson and Binomial"))
# 
# (plot_sup_norm_normal <- ggplot(data=surfaces, aes(x=n,y=p)) 
#  + geom_tile(aes(fill=normal_sup_norm_quality)) + guides(fill=FALSE)
#  + opts(title="Max distance between \n Normal and Binomial"))
# 
# arrange(ncol=2, plot_L1_poisson, plot_L1_normal, plot_sup_norm_poisson, plot_sup_norm_normal)
# 
# surfaces$normal_L1_quality <- "1 < e"
# surfaces[surfaces$L1_distance_normal < 1,]$normal_L1_quality <- ".1 < e <= 1"
# surfaces[surfaces$L1_distance_normal < 10**-1,]$normal_L1_quality <- ".01 < e <= .1"
# surfaces[surfaces$L1_distance_normal < 10**-2,]$normal_L1_quality <- ".001 < e <= .01"
# surfaces[surfaces$L1_distance_normal < 10**-3,]$normal_L1_quality <- ".0001 < e <= .001"
# surfaces[surfaces$L1_distance_normal < 10**-4,]$normal_L1_quality <- ".00001 < e <= .0001"
# surfaces[surfaces$L1_distance_normal < 10**-5,]$normal_L1_quality <- " e <= .00001"
# 
# #mapply( assignLevel, surfaces$sup_norm_normal, 
# 
# ggplot(data=surfaces, aes(x=n,y=p)) + geom_tile(aes(fill=L1_distance_poisson))
# 
# matrix4error1 <- with(surfaces, matrix(mapply(findDistanceOfBinomialAndPoisson, n=n, p=p),+
#   nrow=length(num_of_trials)))
# 
# matrix4error2 <- with(surfaces, matrix(mapply(findDistanceOfBinomialAndNormal, n=n, p=p),+
#   nrow=length(num_of_trials)))
# 
# 
# 
# with(surfaces, plot3d(x=n, y=p, z=error1, type="s",col="red",size=1))
# with(surfaces, plot3d(x=n,y=p,z=error2,type="s",col="green",size=1))
# 
# 
# intersection <- with(surfaces, surfaces[abs(error1-error2)<10**(-4),])
# intersection$status <- "intersection"
# 
# poisson_better <- with(surfaces, surfaces[error1<=error2,])
# poisson_better$status <- "better"
# 
# poisson_significantly_better <- with(surfaces, surfaces[error1<=(error2-10**(-2)),])
# poisson_significantly_better$status <- "better than 10**-2"
# 
# poisson_significantly_even_better <- with(surfaces, surfaces[error1<=(error2-10**(-1)),])
# poisson_significantly_even_better$status <- "better than 10**-1"
# 
# analysis <- rbind(intersection, poisson_better,poisson_significantly_better,poisson_significantly_even_better)
# melted_analysis <- melt(data=analysis, measure.vars=c("n","p"), id.vars=c("status"))
# 
# surfaces$dist=surfaces$error1-surfaces$error2
# surfaces$test=rnorm(nrow(surfaces))
# scat <- ggplot(data=surfaces, aes(x=n,y=p))
# scat + geom_tile(aes(fill=dist))+
# scale_fill_gradient(limits=c(min(surfaces$dist), max(surfaces$dist)), low="red", high="blue") 
# 
# scat + geom_tile(aes(fill=error1))+
#   scale_fill_gradient(limits=c(min(surfaces$error1), max(surfaces$error1)), low="red", high="blue") 
# 
# scat + geom_tile(aes(fill=error2))+
#   scale_fill_gradient2(low="red", high="blue") 
# 
# ggplot() + geom_point(data=poisson_better, aes(x=n,y=p), color="red") + geom_point(data=poisson_significantly_better, aes(x=n,y=p), color="green")
# 
# # CLT: X_1, ..., X_n ~ F, E[X_i] = u, Var[X_i] = s^2 then \sqrt{n}(1/n \bar{X}_n - u) -> N(0, s^2)
# # 1/n \bar{X}_n - u -> N(0,s^2/n),
# # \bar{X}_n -> N(nu,ns^2)