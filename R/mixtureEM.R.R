mixtureEM.R <-
function(dat)
{
dat=as.matrix(dat, ncol=2)
x=dat[,1];y=dat[,2]
z=y
### Set Initial values as Dimitris et al 2003
d.mean=sum(x*z)/sum(z)
d.var=sum((x-d.mean)^2*z)/sum(z)
w=.5
# Method Of Moment
lambda2=(abs(d.var-d.mean+.5))^.5+d.mean-1
lambda11=2*d.mean-(lambda2+1)
lambda1=ifelse(lambda11<=0,0.01,lambda11)
pars=c(lambda1,lambda2,w)
parsinit=pars
done = TRUE ;n = length(x)
while(done==TRUE)
{
parsold = pars
## E step
s1a = pars[3]*dpois(x,pars[1])
s2a= (1-pars[3])*dpois(x-1,pars[2])
s3a=s1a+s2a;
ind=which(s3a>0)
s3a=s3a[ind];s1a=s1a[ind];s2a=s2a[ind]
## M step
x=x[ind];y=y[ind]
pars[1] = sum(s1a*x*y/sum(y)/s3a)/(sum(s1a/s3a*y/sum(y)))
pars[2] = sum(s2a*(x-1)*y/sum(y)/s3a)/(sum(s2a/s3a*y/sum(y)))
pars[3] = sum(s1a/s3a*y/sum(y))
delta =max(abs(pars-parsold))
done=ifelse(delta> 0.00001, TRUE,FALSE)
}   
structure(list(pars = pars,parsinit=parsinit))
}

