###cac loai phan phoi:unif, hyper, pois, norm,exp, chisp
### d, p,q,r
##d:pdf - p(X=k)
##p:cdf - p(X<=k)
##q: quantile
##r: tra ve gia tri ngau nhien thoa loai phan phoi
#### X~B(10,0.7)
### tinh xac suat X=5
dbinom(5,10,0.7)
### tinh xac suat X<=5
dbinom(0,10,0.7)+dbinom(1,10,0.7)+dbinom(2,10,0.7)+dbinom(3,10,0.7)+dbinom(4,10,0.7)+dbinom(5,10,0.7)
pbinom(5,10,0.7)
### VD2: tao ket qua tung dong xu 100 lan
rbinom(100,1,0.5)
### VD3: tao 100 gia tri cua bien( bien ngau nhien ch??? so lan sap trong 10 lan tung)
rbinom(100,10,0.5)
### VD4: X la bnn tuan theo phan phoi chuan, mu=5, sigma=2
qnorm(0.5,mean = 5,sd=2)
### tinh chat 1: X~H(N,K,n), N lon so voi n, X~B(n,p),p=K/N
###vd : trong hop bi có 200 vien bi, 90 bi trang , rut ra 30 bi

### tao bien X co 1000 quan sat
nn<-1000
m<-230
n<-400-230
k<-30
x<- rhyper(nn,m,n,k)
par(mfrow=c(1,2))
hist(x,freq=FALSE)
### tao bien Y co 1000 quan sat, Y~B(30,p=m/(m+n))
y<-rbinom(1000,30,m/(m+n))
hist(y,freq=FALSE)        

#### Tinh chat 2: X~B(n,p),n lon, X~P(n*p)
###vd2: X~B(10,0.7), Y~P(10*0.7), tao 1000 quan sat
nn<-1000
n<-2000
p<-0.02
par(mfrow=c(1,2))
x<-rbinom(1000,n,p)
hist(x,freq = FALSE)
y<-rpois(1000,n*p)
hist(y,freq = FALSE)

### Tinh chat 3: X~B(n,p),X~N(n*p,n*p(1-p)), n lon, np,n(1-p)>5
###VD3: X~B(100,0.6)
n<-100
p<-0.6
x<-rbinom(1000,100,0.6)
hist(x,freq = FALSE, breaks = 30)
curve(dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p))),add=TRUE)
curve(dnorm(x,mean=n*p,sd=sqrt(n*p*p*(1-p))),col="red",add=TRUE)














