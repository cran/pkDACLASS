Baseline.R <-
function(dat,w,p=TRUE,...)
{
colnames(dat) = c("m.z","y")
fcut.dat = dat[dat$m.z>0,]
dat1 = matrix(bslnoff(fcut.dat,method="approx",plot=p,bw=w),ncol=2)
dat1 = data.frame(dat1);colnames(dat1) = c("m.z","y")
return(dat1)
}

