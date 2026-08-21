###((n-1)S2)/(sigma2)-chisq(n-1)
mu=3
sigma=2
n=50
x<-function()
{
  a<-rnorm(n,mu,sigma)
  y<-((n-1)*var(a))/(sigma^2)
  return(y)
}
x()
mauX<-function(k)
{
  replicate(k,x())
}
mauX(5)
y=mauX(1000)
hist(y,freq = FALSE)
curve(dchisq(x,df=n-1),col="red",add=TRUE)
######
### cho X~B(1,p)
### X.bar~N(1*p,1*p(1-p)/n)
p=0.4
n=50
x<-function()
  {
    a<-rbinom(n,1,p)
    y<-mean(a)
    return(y)
}
x()
mauX<-function(k)
{
  replicate(k,x())
}
y=mauX(1000)
hist(y,freq = FALSE,breaks = 30)
curve(dnorm(x,mean=p,sd=sqrt(p*(1-p)/n)),col="red",add=TRUE)
########
### ((n-1)f.bar*(1-f.bar))/(p(1-p))~chisq(n-1)
p=0.4
n=50
x<-function()
{
  a<-rbinom(n,1,p)
  y<-mean(a)
  return(y)
}
x()
mauX<-function(k)
{
  replicate(k,x())
}
y=mauX(1000)
hist(y,freq = FALSE,breaks = 30)
curve(dnorm(x,mean=p,sd=sqrt(p*(1-p)/n)),col="red",add=TRUE)
########
### ((n-1)*f.bar*(1-f.bar))/(p(1-p))~chisq(n-1)
p=0.8
n=100
x<-function()
{
  a<-rbinom(n,1,p)
  y<-((n-1)*var(a))/(p*(1-p))
  return(y)
}
x()
mauX<-function(k)
{
  replicate(k,x())
}
y=mauX(1000)
hist(y,freq = FALSE,breaks = 50)
curve(dchisq(x,df=n-1),col="red",add=TRUE)
########## lay mau tu phan phoi Poisson
### X~P(lambda)
### X.bar~N(lambda,lambda/n)
### (X.bar-lambda)/sqrt(lambda/n)~N(0,1)
### [(n-1)*S2/(lambda)]~chisq(n-1)

n=100
X<-function(n){
  x<-rpois(n,lambda)
  (n-1)*var(x)/lambda
}
samplemeanX<-function(m) {replicate(m,X(n))}
m=1000
hist(samplemeanX(m),freq=0,breaks=40)
curve(dchisq(x,n-1),col="green",add=TRUE)
