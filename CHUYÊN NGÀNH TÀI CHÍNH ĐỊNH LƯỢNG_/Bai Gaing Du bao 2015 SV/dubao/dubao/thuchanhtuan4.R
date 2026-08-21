#### X~N(0,1)==> X2~chisp(1)
n=1000
x<-rnorm(n,0,1)
y<-x^2
hist(y,freq=FALSE,breaks=40)
curve(dchisq(x,df=1),col="red",add=TRUE)
curve(dchisq(x,df=5),col="blue",add=TRUE)

### X~chisp(m),Y~chisp(n), X+Y~chisp(m+n)

n<-10000
x<-rchisq(n,df=10)
y<-rchisq(n,df=20)
z<-x+y
hist(z,freq=0,breaks=40)
curve(dchisq(x,df=30),col="red",add=TRUE)

### X~N(0,1), Y~chisq(m), Z=(X/sqrt(Y/m))~St(m)

n<-10000
x<-rnorm(n)
y<-rchisq(n,df=10)
z<-x/sqrt(y/10)
hist(z,freq=0,breaks=40)
curve(dt(x,df=10),col="red",add=TRUE)

### X~chisq(m), Y~chisq(n), ==>(X/m)/(Y/n)~F(m,n)
n<-1000
X<-rchisq(n,df=10)
Y<-rchisq(n,df=20)
Z<-(X/10)/(Y/20)
hist(Z,freq=0,breaks=40)
curve(df(x,df1=10,df2=20),col="red",add =TRUE )

#### X~N(mu,sigma2) ==>X.bar~N(mu,sigma2/n)
mu=2
sigma=3
n=50
m=1000
mauX<-function()
{
  x<-rnorm(n,mu,sigma)
  y<-mean(x)
  return(y)
}
mauX()
mauY<-function(m)
{
  y<-replicate(m,mauX())
  return(y)
}
mauY(5)
Z<-mauY(m)
hist(Z,freq=FALSE,breaks = 50)
curve(dnorm(x,mean=mu,sd=sigma/sqrt(n)),col="red",add=TRUE)
curve(dnorm(x,mean=mu,sd=sigma/n),col="blue",add=TRUE)

### voi X.bar~N(mu,sigma2/n)==> kiem tra tinh chat khi chuan hoa X.bar

mu=2
sigma=3
n=50
m=1000
mauX<-function()
{
  x<-rnorm(n,mu,sigma)
  y<-(mean(x)-mu)/(sigma/sqrt(n))
  return(y)
}
mauX()
mauY<-function(m)
{
  y<-replicate(m,mauX())
  return(y)
}
mauY(5)
Z<-mauY(m)
hist(Z,freq=FALSE,breaks = 50)
curve(dnorm(x,mean=0,sd=1),col="red",add=TRUE)






