align.R <-
function(dat)
{
mz<-dat[,1]
y<-dat[,2]
n<-length(mz)
nn<-ny<-yy<-nwty<-nwtyy<-double(n)
for(i in 1:n){
nn[i]<-mz[i]-floor(mz[i])
         if(nn[i]==0){
         ny[i]<-mz[i]
         yy[i]<-y[i]
          } else 
                if(nn[i]==0.5){
                  ny[i]<-ceiling(mz[i])
                  yy[i]<-nn[i]*y[i]
                  nwty[i]<-floor(mz[i])
                  nwtyy[i]<-(1-nn[i])*y[i]
               }else
                   if(nn[i]>0 & nn[i]<0.5){ 
                   ny[i]<-floor(mz[i])
                   yy[i]<-(1-nn[i])*y[i]
                   nwty[i]<-ceiling(mz[i])
                   nwtyy[i]<-nn[i]*y[i]
               }else
                    if(nn[i]>0.5){
                    ny[i]<-ceiling(mz[i])
                    yy[i]<-nn[i]*y[i]
                    nwty[i]<-floor(mz[i])
                    nwtyy[i]<-(1-nn[i])*y[i]
                }               
}
wt.data<-cbind(ny,yy)
nwt.data<-cbind(nwty,nwtyy)
req.data<-rbind(wt.data,nwt.data)
d.ord<-req.data[order(req.data[,1]),]
ind<-rowSums(d.ord==0) !=ncol(d.ord)
d.data<-d.ord[ind,]
colnames(d.data)<-c("mz","y")
d.data <- data.frame(d.data)
library(plyr)
d.data<-ddply(d.data, .(mz), summarise, y=sum(y))
return(d.data)
}

