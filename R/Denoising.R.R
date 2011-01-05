Denoising.R <-
function(dat,h,...) 
{
colnames(dat) = c("m.z","y")
dat1 = matrix(cbind(dat$m.z,ifelse(dat$y-h>0,dat$y,0)),ncol=2)
dat1 = data.frame(dat1[dat1[,2]>0,]);colnames(dat1) = c("m.z","y")
return(dat1)
}

