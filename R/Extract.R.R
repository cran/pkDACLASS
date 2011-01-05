Extract.R <-
function(dat,...)
{
colnames(dat) =c("m.z","y")
omit=NULL
for(i in 1:length(dat$m.z)-1)
{
omit[i]=dat$m.z[i+1]-dat$m.z[i]
index=which(omit!=1)
}
d=list(dat[1:index[1],])
d2=lapply(2:length(index)-1,function(i) na.omit(dat[(index[i]+1):index[i+1],]))
d3=list(na.omit(dat[max(index)+1: dim(dat)[1],]))
dat1=unlist(list(d,d2,d3),recursive=FALSE)
return(dat1)
}

