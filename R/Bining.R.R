Bining.R <-
function(dat,INDEX,...)
{
colnames(dat) =c("m.z","y")
dat1 = tapply(dat$y, INDEX=round(dat$m.z),max)
dat1 = matrix(cbind(as.numeric(dimnames(dat1)[[1]]),as.vector(dat1)),ncol=2)
colnames(dat1) = c("m.z","y")
return(dat1)
}

