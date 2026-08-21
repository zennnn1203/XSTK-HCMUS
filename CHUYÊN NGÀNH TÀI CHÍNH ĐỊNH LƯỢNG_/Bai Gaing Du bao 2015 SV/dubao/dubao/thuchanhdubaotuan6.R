n<-100
p<-0.8
gen.f<-function()
{
  x<-rbinom(n,1,prob = p)
  return(mean(x)) 
}
gen.f()
gen.x<-function(k)
{
  y<-replicate(k,gen.f())
  return (y)
}
gen.x(10)

### f~=N(p,p*(1-p)/n))
f<-gen.x(1000)
hist(f,freq= FALSE,breaks = 30)
curve(dnorm(x,mean=p,sd=sqrt(p*(1-p)/n)),add= TRUE)

####
n<-100
p<-0.8
gen.f<-function()
{
  x<-rbinom(n,1,prob = p)
  f<-mean(x)
  a<-(n-1)*f*(1-f)/(p*(1-p))
  return(a) 
}
gen.f()
gen.x<-function(k)
{
  y<-replicate(k,gen.f())
  return (y)
}
gen.x(10)

### f~=N(p,p*(1-p)/n))
f<-gen.x(1000)
hist(f,freq= FALSE,breaks = 30)
curve(dchisq(x,df=n-1),add= TRUE)

###
#### KTC cho mu: x.bar-c.s/sqrt(n);x.bar+c.s/sqrt(n)
mu<-5
sigma<-2
gamma<-0.9
CI<-function(n)
{
  x<-rnorm(n,mean=mu,sd=sigma)
  x.bar<-mean(x)
  s<-sd(x)
  C<-qt(1-(1-gamma)/2,df=n-1)
  L<-x.bar-C*s/sqrt(n)
  U<-x.bar+C*s/sqrt(n)
  ci<-c(L,U)
  return(ci)
}
CI(100)

###  
n<-100
countCI<-function(k)
{
  count<-0
  for (i in 1:k)
  {
    CI (n)
    if ((CI(n)[1]<=mu)&(CI(n)[2]>=mu))
      count=count+1
  }
  return(count/k)
}
countCI(1000)
###
gamma<-0.9
p<-0.8
CI.p<-function(k)
{
  x<-rbinom(k,1,prob=p)
  f<-mean(x)
  d<-qnorm(1-(1-gamma)/2)
  L<-f-d*sqrt(f*(1-f)/n)
  U<-f+d*sqrt(f*(1-f)/n)
  ci<-c(L,U)
  return(ci)
}
CI.p(100)



