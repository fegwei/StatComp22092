## -----------------------------------------------------------------------------
head(iris)
tail(iris)

## -----------------------------------------------------------------------------
summary(iris)

## -----------------------------------------------------------------------------
# library
library(ggplot2)
library(ggExtra)

# classic plot
p <- ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Petal.Width, color = Species), alpha = 0.6, shape = 16) +  
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(x = "Petal Length", y = "Petal Width") 
ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)

## -----------------------------------------------------------------------------
library(xtable)
xtable(tail(iris))

## -----------------------------------------------------------------------------
set.seed(22092)
n<-1000
u<-runif(n)
x<-2/sqrt(1-u)
hist(x,prob =TRUE,main = expression(F(x)==1-(frac(2,x))^2))
y<-seq(min(x),max(x),length=n)
lines(y,(2/y)^3)

## -----------------------------------------------------------------------------
randomNumber_beta<-function(n,a,b){
  j<-k<-0
  y<-numeric(n)
  while(k<n){
    u<-runif(1)
    j<-j+1
    x<-runif(1)
    if(x^(a-1)*(1-x)^(b-1)>u*((a-1)/(a+b-2))^(a-1)*((b-1)/(a+b-2))^(b-1)){
      k<-k+1
      y[k]<-x
    }
  }
  return(y)
}

## -----------------------------------------------------------------------------
y<-randomNumber_beta(1000,3,2)

## -----------------------------------------------------------------------------
hist(y,prob =TRUE,main = expression(f(x)==frac(Gamma(a+b),Gamma(a) *Gamma(b))* x^{a-1}(1-x)^{b-1}))
z<-seq(0,1,.01)
lines(x=z,y=dbeta(z,3,2))

## ----echo=FALSE---------------------------------------------------------------
qqplot(y,rbeta(n,3,2),xlab='Accpetance-rejection',ylab='rbeta')
abline(0,1,col='blue',lwd=2)

## -----------------------------------------------------------------------------
r <- 4
beta <- 2
n <- 1e4
lambda <- rgamma(n,r,beta)
x <- rexp(n,lambda)
x[1:20]

## -----------------------------------------------------------------------------
r <- 4
beta <- 2
n <- 1e4
lambda <- rgamma(n,r,beta)
x <- rexp(n,lambda)
hist(x,probability = TRUE,main=expression(F(x)==1-(frac(beta,beta+y))^gamma))
y<-seq(min(x),max(x),length=n)
lines(y,64/(2+y)^5)

## -----------------------------------------------------------------------------
quick_sort <- function(x){
  num<-length(x)
  if(num==0||num==1){
    return(x)
  }else{
    sentinel<-sample(1:num,1)#random pick one number
    a<-x[sentinel]
    y<-x[-sentinel]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))
  }
}

## -----------------------------------------------------------------------------
i =1 #Fill the data in row i of the matrix
m = matrix(nrow =5,ncol = 100)
for (n in c(10^4,2*10^4,4*10^4,6*10^4,8*10^4)) {
  for ( j in 1:100) {
    test <- sample(1:n)
    m[i,j]<-system.time(quick_sort(test))[1]
  }
  i = i+1;
}
a_n<-rowMeans(m)
a_n

## -----------------------------------------------------------------------------
t_n <- c(10^4,2*10^4,4*10^4,6*10^4,8*10^4)*log(c(10^4,2*10^4,4*10^4,6*10^4,8*10^4))
plot(a_n,t_n,main="Regression for Algorithm complexity with running time")
abline(lm(t_n~a_n),col='red')

## -----------------------------------------------------------------------------
x<-runif(10000)
cov <- cov(exp(x),exp(1-x))
var <- var(exp(x)+exp(1-x))
cov;var

## -----------------------------------------------------------------------------
m<-1e4
x<-runif(m)
theta.hat <-mean(exp(x))
theta.hat

## -----------------------------------------------------------------------------
u<-runif(m/2)
v<-1-u
u<-c(u,v)
g<-exp(u)
theta_hat<-mean(g)
theta_hat

## -----------------------------------------------------------------------------
MC.Phi <- function(R = 10000, antithetic = FALSE) {
  u <- runif(R/2)
  if (antithetic) v <- 1 - u else v <- runif(R/2)
  u <- c(u, v)
  g <-exp(u) 
  theta<- mean(g)
  theta
}

m <- 1000
MC1 <- MC2 <- numeric(m)
x <- 1
for (i in 1:m) {
  MC1[i] <- MC.Phi(R = 1000, antithetic = FALSE)
  MC2[i] <- MC.Phi(R = 1000, antithetic = TRUE)
}
var(MC1);var(MC2);1-var(MC2)/var(MC1)

## -----------------------------------------------------------------------------
x <- seq(0, 1, 0.01)
w <- 2

g <- x^2*exp(-x^2/2) / sqrt(2*pi)
f1 <- 4 / ((1 + x^2) * pi)
f2 <- exp(-x)
f3 <- x*exp(-x^2/2)/(1-exp(-1/2))
gs <- c(expression(g(x)==x^2*e^{-x^2/2}/sqrt(2*pi)),
           expression(f[1](x)==4/((1+x^2)*pi)),
            expression(f[2](x)==e^{-x}),
            expression(f[3](x)==x*e^{-x^2}/(1-e^{-1/2}))
   )
    #for color change lty to col
par(mfrow=c(1,1))
    #figure (a)
plot(x, g, type = "l", ylab = "",
         ylim = c(0,2.5), lwd = w,col=1,main='dentisy fucntion')
    lines(x, f1, lty = 2, lwd = w,col=2)
    lines(x, f2, lty = 3, lwd = w,col=3)
    lines(x, f3, lty = 4, lwd = w,col=4)
    legend("topleft", legend = gs,
           lty = 1:4, lwd = w, inset = 0.02,col=1:4)


## -----------------------------------------------------------------------------
plot(x, g/f1, type = "l", ylab = "",
        ylim = c(0,3.2), lwd = w, lty=2,col=2 ,main='g/f')
lines(x, g/f2, lty = 3, lwd = w,col=3)
lines(x, g/f3, lty = 4, lwd = w,col=4)
legend("topright", legend = gs[-1],
           lty = 2:4, lwd = w, inset = 0.02,col=2:4)


## -----------------------------------------------------------------------------
library(knitr)
m <- 1e4
est <- sd <- numeric(3)
g <- function(x) {
  exp(-x^2/2)*x^2/sqrt(2*pi) * (x > 0) * (x < 1)
  }
u <- runif(m) #f1, inverse transform method
x <- tan(pi * u / 4)
fg <- g(x) / (4 / ((1 + x^2) * pi))
est[1] <- mean(fg)
sd[1] <- sd(fg)
x <- rexp(m, 1) #using f2
fg <- g(x) / exp(-x)
est[2] <- mean(fg)
sd[2] <- sd(fg)
u <- runif(m) #f3, inverse transform method
x <-  sqrt(-2*log(1-(1-exp(-1/2))*u))
fg <- g(x) / (x*exp(-x^2/2))*(1-exp(-1/2))
est[3] <- mean(fg)
sd[3] <- sd(fg)

## -----------------------------------------------------------------------------
res <- rbind(est=round(est,3), sd=round(sd,3))
colnames(res) <- paste0('f',1:3)
knitr::kable(res, format = "markdown",align='c')

## -----------------------------------------------------------------------------
M<-10000
k<-5
r<-M/k
N<-50
T2<-numeric(k)
est<-rep(0,N)
g<-function(x){1/(1+x^2)}
for (i in 1:N) {
  for (j in 1:k) {
    U<-runif(M/k)
    a<- exp(-(j-1)/k)-exp(-j/k)
    x<- -log(exp(-(j-1)/5)-a*U)
    T2[j]<-mean(g(x)*a)
  }
  est[i] <- sum(T2)
}
round(mean(est),4)
round(sd(est),5)

## -----------------------------------------------------------------------------
set.seed(20221009)
remove(list=ls()) # clear up memory
n<-20
alpha<-0.05
m<-10000
LCL <-UCL<- numeric(m)
for (i in 1:m) {
  x<-rnorm(n,mean = 2,sd=2)
  LCL[i]<-sum(x)/n +sd(x)*qt(alpha/2,df=n-1)/sqrt(n)
  UCL[i]<-sum(x)/n -sd(x)*qt(alpha/2,df=n-1)/sqrt(n)
}
sum(LCL<2 & UCL>2)
mean(LCL<2 & UCL>2)

## -----------------------------------------------------------------------------
count5test <- function(x,y){
  X<-x-mean(x)
  Y<-y-mean(y)
  outx <- sum(X>max(Y)) + sum(X<min(Y))
  outy <- sum(Y>max(X)) + sum(Y<min(X))
  return (as.integer(max(c(outx,outy))>5))
}


## -----------------------------------------------------------------------------
sigma1 <-1
sigma2 <-1.5
m<-c(20,100,500,1000,5000,10000)
power<-FResult<-numeric(length(m))
z <- 1
for (i in m) {
  test1<-numeric(10000)
  test2<-numeric(10000)
  for (j in 1:10000) {
    x<-rnorm(i,0,sigma1)
    y<-rnorm(i,0,sigma2)
    test1[j] <- count5test(x,y)
    x<-x-mean(x)
    y<- y-mean(y)
    test2[j] <- var.test(x,y)$p.value<0.055
  }
  power[z]<-mean(test1)
  FResult[z]<-mean(test2)
  z <- z+1
}
res <- rbind(size=m,count5Test=power,Ftest=FResult)
res

## -----------------------------------------------------------------------------
remove(list = ls())
dt <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
xbar <- mean(dt)
lambda <- 1/xbar
lambda

## -----------------------------------------------------------------------------
bootstrap1 <- function(x){
  B<- 1e4
  set.seed(20221014)
  lambdaStar <- numeric(B)
  for (b in 1:B) {
    xstar<-sample(x,replace = TRUE)
    lambdaStar[b] <- 1/mean(xstar)
  }
  return(lambdaStar)
}

lambdaStar <- bootstrap1(dt)
round(c(bias = mean(lambdaStar)-lambda,se.boot = sd(lambdaStar)),3)


## -----------------------------------------------------------------------------
library(boot)
lambda.boot <- function(dat,ind){
  1/mean(dat[ind])
}
boot.obj <- boot(dt,statistic=lambda.boot,R=2000)
print(boot.ci(boot.obj,
type = c("basic", "norm", "perc",'bca')))

## -----------------------------------------------------------------------------
hist(boot.obj$t,probability = TRUE)

## -----------------------------------------------------------------------------
rm(list = ls())

skewness <- function(x,i) {
  #computes the sample skewness coeff.
  x_bar <- mean(x[i])
  x_bar
}

Sample3 <- function(n, mea, sd){
  samp <- rnorm(n, mea, sd)
  samp
}

Analysis3 <- function(m, func, Rr, n, mea, sd){
  library(boot)
  nornorm <- matrix(0, m, 2)
  norbasi <- matrix(0, m, 2)
  norperc <- matrix(0, m, 2)
  for (i in 1:m) {
    Samp <- Sample3(n, mea, sd)
    Skew <- boot(Samp, statistic = func, R=Rr)
    Nor <- boot.ci(Skew, type=c("norm","basic","perc"))
    nornorm[i,] <- Nor$norm[2:3]
    norbasi[i,] <- Nor$basic[4:5]
    norperc[i,] <- Nor$percent[4:5]
  }
  #Calculate the coverage probability of a normal distribution
  norm <- mean(nornorm[,1] <= s & nornorm[,2] >= s)
  basi <- mean(norbasi[,1] <= s & norbasi[,2] >= s)
  perc <- mean(norperc[,1] <= s & norperc[,2] >= s)
  #Calculate the probability of the left side of the normal distribution
  normleft <- mean(nornorm[,1] >= s )
  basileft <- mean(norbasi[,1] >= s )
  percleft <- mean(norperc[,1] >= s )
  #Calculate the right side probability of a normal distribution
  normright <- mean(nornorm[,2] <= s )
  basiright <- mean(norbasi[,2] <= s )
  percright <- mean(norperc[,2] <= s )
  analyresu <- c(norm, basi, perc, normleft, basileft, percleft, normright, basiright, percright)
  analyresu
}

Result3 <- function(sd, analyresu){
  dnam <- paste("N ( 0 ,", as.character(sd^2),")",seq="")
  Distribution <- c(dnam)
  Type <- c("basic", "norm", "perc")
  Left <- analyresu[4:6]
  Right <- analyresu[7:9]
  P.coverage <- analyresu[1:3]
  result <- data.frame(Distribution, Type, Left, Right, P.coverage)
  result
}

## -----------------------------------------------------------------------------
s <- 0
n <- 20
m <- 1000
R <- 1000

mea <- 0
sd <- 3 


set.seed(1234)
library(boot)

Analyresu <- Analysis3(m, skewness, R, n, mea, sd)
Resu <- Result3(sd, Analyresu)

knitr::kable (Resu, align="c")

## -----------------------------------------------------------------------------
remove(list = ls())
thetaTrue <- function(x){
  sigma <- cov(x)
  ei <- eigen(sigma)
  lambda <- ei$values[order(-(ei$values))]
  lambda[1]/sum(lambda)
}

## -----------------------------------------------------------------------------
jack <- function(x,theta.hat,n){
  for (i in 1:n) {
    x.jack <- scor[-i,]
    theta.hat[i] <- thetaTrue(x.jack)
  }
  theta.hat
}

## -----------------------------------------------------------------------------
library(bootstrap)
n <- length(scor[,1])
theta.hat <- numeric(n)
theta <- thetaTrue(scor)
theta.hat <- jack(scor,theta.hat,n)
bias.jack <- (n-1)*(mean(theta.hat-theta))
se.jack <- sqrt((n-1)*mean((theta.hat-theta)^2))
round(c(original.theta = theta,bias.jack = bias.jack,se.jack = se.jack),4)

## -----------------------------------------------------------------------------
remove(list=ls())
e <- function(n,data1,data2){
  e1 <- e2 <- e3 <- e4 <- matrix(0,nrow=n*(n-1)/2,ncol = 2)
  k =1 
  for (i in 1:n) {
    j = i+1
    while (j<=n) {
       y<- data1[c(-i,-j)]
       x<- data2[c(-i,-j)]
       
       J1 <- lm(y~x)
       yhat1_i <- J1$coef[1] + J1$coef[2] * data2[i]
       yhat1_j <- J1$coef[1] + J1$coef[2] * data2[j]
       e1[k,1] <- data1[i] - yhat1_i
       e1[k,2] <- data1[j] - yhat1_j
       
       J2 <- lm(y ~ x + I(x^2))
       yhat2_i <- J2$coef[1] + J2$coef[2] * data2[i] +J2$coef[3] * data2[i]^2
       yhat2_j <- J2$coef[1] + J2$coef[2] * data2[j] +J2$coef[3] * data2[j]^2
       e2[k,1] <- data1[i] - yhat2_i
       e2[k,2] <- data1[j] - yhat2_j
       
       J3 <- lm(log(y) ~ x)
       logyhat3_i <- J3$coef[1] + J3$coef[2] * data2[i]
       logyhat3_j <- J3$coef[1] + J3$coef[2] * data2[j]
       yhat3_i <- exp(logyhat3_i)
       yhat3_j <- exp(logyhat3_j)
       e3[k,1] <- data1[i] - yhat3_i
       e3[k,2] <- data1[j] - yhat3_j
       
       J4 <- lm(log(y) ~ log(x))
       logyhat4_i <- J4$coef[1] + J4$coef[2] * log(data2[i])
       logyhat4_j <- J4$coef[1] + J4$coef[2] * log(data2[j])
       yhat4_i <- exp(logyhat4_i)
       yhat4_j <- exp(logyhat4_j)
       e4[k,1] <- data1[i] - yhat4_i
       e4[k,2] <- data1[j] - yhat4_j
       
       k <- k+1
       j <- j+1
    }
  }
  return (list(e1=e1,e2=e2,e3=e3,e4=e4,J1=J1,J2=J2,J3=J3,J4=J4))
}

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n<- length(magnetic)
err <- e(n,magnetic,chemical)
e1 <- err$e1
e2 <- err$e2
e3 <- err$e3
e4 <- err$e4
c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2))
detach(ironslag)

## -----------------------------------------------------------------------------
err$J2

## -----------------------------------------------------------------------------
remove(list = ls())
x <- c(2,-2,-13,8,9,6)
y <- c(0,-1,-4,20,9,4)
cor.test(x,y,method = 'spearman')

## -----------------------------------------------------------------------------
rankx<-rank(x)
ranky<-rank(y)
pearson <- cor(x,y)
spearman <- cor(rankx,ranky)
N<-100000
rho_per <- rep(0,N)
for (i in 1:N) {
  x_per<-sample(x)
  rankx_per<- rank(x_per)
  y_per<-sample(y)
  ranky_per<-rank(y_per)
  rho_per[i] <- cor(rankx_per,ranky_per)
}
mean(abs(rho_per)>=abs(spearman))

## -----------------------------------------------------------------------------
randomWalk <- function(sigma,x0,N){
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1,x[i-1],sigma)
    if(u[i]<=((0.5*exp(-abs(y)))/(0.5*exp(-abs(x[i-1]))))){
      x[i] <- y
    }else{
      x[i] <- x[i-1]
      k <- k+1
    }
  }
  return(list(x=x,k=k))
}

## -----------------------------------------------------------------------------
set.seed(22092)
N <- 2000
sigma <- c(0.05,0.50,2.00,5.00,10.00,20.00)
x0 <- 5
rw1 <- randomWalk(sigma[1],x0,N)
rw2 <- randomWalk(sigma[2],x0,N)
rw3 <- randomWalk(sigma[3],x0,N)
rw4 <- randomWalk(sigma[4],x0,N)
rw5 <- randomWalk(sigma[5],x0,N)
rw6 <- randomWalk(sigma[6],x0,N)
accept.rate <- c(1-(rw1$k)/N, 1-(rw2$k)/N, 1-(rw3$k)/N, 1-(rw4$k)/N,1-(rw5$k)/N,1-(rw6$k)/N)
accept <- data.frame(sigma = sigma, no.reject=c(rw1$k, rw2$k, rw3$k, rw4$k,rw5$k,rw6$k), accept.rate = accept.rate)
knitr::kable(accept)

## -----------------------------------------------------------------------------
 par(mar=c(1,1,1,1))
 par(mfrow=c(3,2))  #display 4 graphs together
    refline <- c(log(0.05), -log(0.05))
    rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x,rw5$x,rw6$x)
    for (j in 1:6) {
        plot(rw[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rw[,j]))
        abline(h=refline)
    }
    par(mar=c(1,1,1,1))
    par(mfrow=c(1,1))


## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  #psi[i,j] is the statistic psi(X[i,1:j]) for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  
  psi.means <- rowMeans(psi)  #row means
  B <-  n * var(psi.means)   #between variance est
  psi.w <- apply(psi,1,'var') #within variance
  W <- mean(psi.w)
  v.hat <- W*(n-1)/n + (B/n) #upper variance est
  r.hat <- v.hat/W
  return(r.hat)
}


## -----------------------------------------------------------------------------
r <- function(sigma){
  #choose overdispersed initial values
  x0 <- c(-10,-5,5,10)
  
  #generate the chains
  X<- matrix(0,nrow=k,ncol = n)
  for (i in 1:k) {
    X[i,] <- randomWalk(sigma,x0[i],n)$x
  }
  
  #compute diagnostic statistics
  psi <- t(apply(X, 1,cumsum))
  for (i in 1:nrow(psi)) {
    psi[i,] <- psi[i,]/(1:ncol(psi))
  }
  return(psi)
}

## -----------------------------------------------------------------------------
k <- 4           #number of chains to generate
n <- 10000       #length of chains
b <- 1000        #burn-in length

psi <- array(0,dim=c(k,n,length(sigma)))
for (i in 1:length(sigma)) {
  psi[,,i] <- r(sigma[i])
}


rhat <- matrix(0,length(sigma),n)
for (i in 1:length(sigma)) {
  for (j in (b+1):n) {
    rhat[i,j] <- Gelman.Rubin(psi[,1:j,i])
  }
}
jpeg('picture.jpg',width=200*4,height=200*3)
par(mfrow = c(3,2))

for (i in 1:length(sigma)) {
  plot(rhat[i,(b+1):n],type='l',xlab='',ylab='R',ylim=c(1.0,1.8))
  abline(h=1.2,lty=2)
}

## -----------------------------------------------------------------------------
set.seed(22092)
N<-5000
burn <- 1000
X <- matrix(0,N,2)

rho <- 0.9
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
s1 <- s2<- sqrt(1-rho^2)*sigma1

X[1,] <- c(mu1,mu2)  #initialize

for (i in 2:N) {
  x2 <- X[i-1,2]
  m1 <- mu1 + rho*(x2-mu2)*sigma1/sigma2
  X[i,1] <- rnorm(1,m1,s1)
  x1 <- X[i,1]
  m2 <- mu2 + rho*(x1-mu1)*sigma2/sigma1
  X[i,2] <- rnorm(1,m2,s2)
}

b <- burn + 1
x <- X[b:N,]

## -----------------------------------------------------------------------------
colMeans(x)

## -----------------------------------------------------------------------------
cov(x)

## -----------------------------------------------------------------------------
cor(x)

## -----------------------------------------------------------------------------
dt = as.data.frame(x)
linReg = lm(V2~V1,data=dt)
summary(linReg)

## -----------------------------------------------------------------------------
qqnorm(residuals(linReg))
qqline(residuals(linReg))

## -----------------------------------------------------------------------------
bivarchain <- function(X,N){
  for (i in 2:N){
    x2 <- X[i-1,2]
    m1 <- mu1+rho*(x2-mu2)*sigma1/sigma2
    X[i,1] <- rnorm(1,m1,s1)
    x1 <- X[i,1]
    m2 <- mu2+rho*(x1-mu1)*sigma2/sigma1
    X[i,2] <- rnorm(1,m2,s2)
  }
  return(X)
}

G.R <- function(B) {
  B <- as.matrix(B)
  n <- ncol(B)
  k <- nrow(B)
  
  B.means <- rowMeans(B)    
  Z <- n * var(B.means)      
  B.w <- apply(B, 1, "var") 
  W <- mean(B.w)               
  v.hat <- W*(n-1)/n + (Z/n)     
  r.hat <- v.hat / W             
  return(r.hat)
}

## -----------------------------------------------------------------------------
N <- 5000; burn <- 1000; X <- matrix(0,N,2) 
k <- 4 #number of chains to generate
s1 <- sqrt(1-rho^2)*sigma1;s2 <- sqrt(1-rho^2)*sigma2
n <- 5000       #length of chains
b <- 1000        #burn-in length

set.seed(123)
X1 <- matrix(0,N,2) 
mu1 <- mu2 <- c(-5,-1,1,5) 
x1 <-y1 <- matrix(0,4,N)

for(i in 1:4){
  X1[1,] <- c(mu1[i], mu2[i])
  X1 <- bivarchain(X1,N)
  x1[i,] <- X1[,1]
  y1[i,] <- X1[,2]
}

B1 <- t(apply(x1, 1, cumsum))
B2 <- t(apply(y1, 1, cumsum))
for(i in 1:nrow(B1)){
  B1[i,] <- B1[i,]/(1:ncol(B1))
}
for(i in 1:nrow(B2)){
  B2[i,] <- B2[i,]/(1:ncol(B2))
}

rhatx <- rhaty <-rep(0,N)
for(j in b:N){
  rhatx[j] <- G.R(B1[,1:j])
  rhaty[j] <- G.R(B2[,1:j])
}
par(mfrow=c(1,2))
plot(rhatx[b:N], type="l", xlab="", ylab="R")
plot(rhaty[b:N], type="l", xlab="", ylab="R")

## -----------------------------------------------------------------------------
# generate sample
generateSample <- function(n,alpha,beta){
  X <- rnorm(n,0,1)
  gamma <-a_M<-a_Y<- 1;
  M <- a_M+alpha*X+rnorm(n)
  Y <- a_Y+beta*M+gamma*X+rnorm(n)
  return(list(X,M,Y))
}


## -----------------------------------------------------------------------------
# The function of test statistics computation
Ttest <- function(X,M,Y){
  fit1 <- summary(lm(M~X))
  fit2 <- summary(lm(Y~X+M))
  a <- fit1$coefficients[2,1]
  sea <- fit1$coefficients[2,2]
  b <- fit2$coefficients[3,1]
  seb <- fit2$coefficients[3,2]
  return(a*b/((a*seb)^2+(b*sea)^2)^0.5)
}

## -----------------------------------------------------------------------------
Imptest <- function(N,n,X,M,Y,T0){
  T1 <- T2 <- T3 <- numeric(N)
  # Condition 1
  for(i in 1:N){
    n1 <- sample(1:n, size=n, replace=FALSE)
    n2 <- sample(1:n, size=n, replace=FALSE)
    X1 <- X[n1];M1 <- M[n2];Y1 <- Y[n2]
    T1[i] <- Ttest(X1,M1,Y1)
  }
  # Condition 2
  for(i in 1:N){
    n1 <- sample(1:n, size = n, replace = FALSE)
    n2 <- sample(1:n, size = n, replace = FALSE)
    X2 <- X[n1];M2 <- M[n1];Y2 <- Y[n2]
    T2[i] <- Ttest(X2,M2,Y2)
  }
  # Condition 3
  for(i in 1:N){
    n1 <- sample(1:n, size = n, replace = FALSE)
    n2 <- sample(1:n, size = n, replace = FALSE)
    M3 <- M[n1];X3 <- X[n2];Y3 <- Y[n2]
    T3[i] <- Ttest(X3,M3,Y3)
  }
  # The p-value of Condition1
  p1 <- mean(abs(c(T0,T1))>abs(T0))
  # The p-value of Condition2
  p2 <- mean(abs(c(T0,T2))>abs(T0))
  # The p-value of Condition3
  p3 <- mean(abs(c(T0,T3))>abs(T0))
  return(c(p1,p2,p3))
}

N <- 1000 # The number of simulation
n <- 100 # The number of random sample
T0 <- numeric(3)
p <- matrix(0,3,3)
# The real values of parameters
alpha <- c(0,0,1);beta <- c(0,1,0)

for(i in 1:3){
  result <- generateSample(n,alpha[i],beta[i])
  X <- result[[1]]
  M <- result[[2]]
  Y <- result[[3]]
  # The original value of test statistics
  T0[i] <- Ttest(X,M,Y)
  p[i,] <- Imptest(N,n,X,M,Y,T0[i])
}

## -----------------------------------------------------------------------------
colnames(p) <- c("Condition 1","Condition 2","Condition 3")
rownames(p) <- c("alpha=0,beta=0","alpha=0,beta=1","alpha=1,beta=0")
p

## -----------------------------------------------------------------------------
solveAlpha <- function(N,b1,b2,b3,f0){
  g <- function(alpha){
    tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
    p <- 1/(1+tmp)
    mean(p) - f0
  }
  a <- -10
  b <- 10
  while (g(a)>0) {
    a <- a*10
  }
  while (g(b)<0) {
    b <- b*10
  }
  solution <- uniroot(g,c(a,b))
  return(solution$root)
}

## -----------------------------------------------------------------------------
N <- 1e6;b1<-0;b2<-1;b3<--1
f0 <- c(0.1,0.01,0.001,0.0001)
x1 <- rpois(N,1);x2<-rexp(N);x3 <- sample(0:1,N,replace=TRUE)
alphaPred <- rep(0,length(f0))
for (i in 1:length(f0)) {
  alphaPred[i] <- solveAlpha(N,b1,b2,b3,f0[i])
}
alphaPred

## -----------------------------------------------------------------------------
f0<-rep(0,10)
f0[1] <- 0.1
alphaPred <- rep(0,10)
for (i in 2:10) {
  f0[i] <-f0[i-1]/2
}
for (i in 1:length(f0)) {
  alphaPred[i] <- solveAlpha(N,b1,b2,b3,f0[i])
}
plot(-log(f0),alphaPred,main='f0 vs alpha')

## -----------------------------------------------------------------------------
dt <- matrix(c(11,12,8,9,27,28,13,14,16,17,0,1,23,24,10,11,24,25,2,3),nrow = 10,ncol=2,byrow = TRUE)

## -----------------------------------------------------------------------------
lkhood <- function(lambda){
  sm <- 0
  for (i in 1:nrow(dt)) {
    sm <- sm + (dt[i,1]*exp(-lambda*dt[i,1]) - dt[i,2]*exp(-lambda*dt[i,2]))/(exp(-lambda*dt[i,1]) - exp(-lambda*dt[i,2]))
  }
  sm
}
lambdaM <- uniroot(lkhood,c(0.1,0.05))
lambdaM$root

## -----------------------------------------------------------------------------
EMAlgorithm <- function(dt,lambda){
  x_hat <- rep(0,nrow(dt))
  for (i in 1:nrow(dt)) {
    x_hat[i] <- 1/lambda + (dt[i,1]*exp(-lambda*dt[i,1]) - dt[i,2]*exp(-lambda*dt[i,2]))/(exp(-lambda*dt[i,1]) - exp(-lambda*dt[i,2]))
  }
  return(nrow(dt)/sum(x_hat))
}

## -----------------------------------------------------------------------------
eps <- 1e-4
lambda0 <- 1
lambda1<- EMAlgorithm(dt,lambda0)
while (abs(lambda1-lambda0)>eps) {
  lambda0 <- lambda1
  lambda1 <- EMAlgorithm(dt,lambda0)
}
lambda1

## -----------------------------------------------------------------------------
a <- list(1,2,list(4,5))
b <- c(unlist(a))
b

## -----------------------------------------------------------------------------
as.vector(a)

## -----------------------------------------------------------------------------
c(1 == '1',-1<FALSE,'one' < 2)

## -----------------------------------------------------------------------------
dim(c(1,2,4,5))

## -----------------------------------------------------------------------------
ma <- matrix(1:12,3,4)
is.matrix(ma)

## -----------------------------------------------------------------------------
is.array(ma)

## -----------------------------------------------------------------------------
df <- data.frame(col1 <- 1:3,col2=c('x','y','z'))
attributes(df)

## -----------------------------------------------------------------------------
data.frame(a=c(),b=character())

## -----------------------------------------------------------------------------
data.frame(row.names = 1:5)

## -----------------------------------------------------------------------------
data.frame()

## -----------------------------------------------------------------------------
remove(list = ls())
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
data.frame(lapply(mtcars, function(x) if( is.numeric(x)) round(scale01(x),4) else x))

## -----------------------------------------------------------------------------
head(mtcars)

## -----------------------------------------------------------------------------
vapply(mtcars, sd, numeric(1))

## -----------------------------------------------------------------------------
head(iris)

## -----------------------------------------------------------------------------
vapply(iris[vapply(iris,is.numeric,logical(1))],sd,numeric(1))

## -----------------------------------------------------------------------------
library(Rcpp)
cppFunction('NumericMatrix gibbsC(int N){
    NumericMatrix X(N,2);

    double mu1 = 0,mu2=0;
    double sigma1= 1,sigma2 = 1;
    double rho = 0.9;
    double s1 = sqrt(1-rho*rho)*sigma1;
    double s2 = sqrt(1-rho*rho)*sigma1;

    X(0,0) = 0;
    X(0,1) = 0;
    for (int i = 1; i < N; i++)
    {
        for (int j = 0; j < 2; j++)
        {
            double x2 = X(i - 1, 1);
            double m1 = mu1 + rho * (x2 - mu2) * sigma1 / sigma2;
            X(i,0) = R::rnorm(m1,s1);
            double x1 = X(i - 1, 0);
            double m2 = mu1 + rho * (x2 - mu2) * sigma1 / sigma2;
            X(i,1) = R::rnorm(m2,s2);
        }
    }
    return X;
}')

## -----------------------------------------------------------------------------
set.seed(20221118)
GibbsR <- function(N){
  N<-N
  X <- matrix(0,N,2)
  
  rho <- 0
  mu1 <-mu2<-0;
  sigma1 <-sigma2<-1
  s1 <- s2<- sqrt(1-rho^2)*sigma1
  
  X[1,] <- c(mu1,mu2)  #initialize
  for (i in 2:N) {
    x2 <- X[i-1,2]
    m1 <- mu1 + rho*(x2-mu2)*sigma1/sigma2
    X[i,1] <- rnorm(1,m1,s1)
    x1 <- X[i,1]
    m2 <- mu2 + rho*(x1-mu1)*sigma2/sigma1
    X[i,2] <- rnorm(1,m2,s2)
  }
  return(X)
}

## -----------------------------------------------------------------------------
burn <- 1000
gxC <- gibbsC(5000)[(burn+1):5000,1]
gxR <- GibbsR(5000)[(burn+1):5000,1]
qqplot(gxC,gxR)

## -----------------------------------------------------------------------------
burn <- 1000
gyC <- gibbsC(5000)[(burn+1):5000,2]
gyR <- GibbsR(5000)[(burn+1):5000,2]
qqplot(gyC,gyR)

## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(gibbR = GibbsR(1000),gibbc = gibbsC(1000))
summary(ts)[,c(1,3,5,6)]

