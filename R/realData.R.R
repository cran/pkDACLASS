realData.R <-
function(d,h,alpha,B,INDEX)
{
set.seed(1500)
colnames(d)=c("m.z", "y")
d2=Baseline.R(d,0.0251,FALSE)
d3 = Denoising.R(d2,h)
d4=data.frame(Bining.R(d3,INDEX))
d5=Extract.R(d4)
d6 = RedIso.R(d5,4)
d7=NULL
for(i in 1:length(d6)) d7[[i]]=cbind(d6[[i]][,1]-d6[[i]][1,1],d6[[i]][,2])
d8=matrix(t(sapply(1:length(d7),function(i) mixtureEM.R(d7[[i]])$pars)),ncol=3)
pvalues1=unlist(sapply(1:length(d7), function(i) pValue1.R(d7[[i]],d8[i,],B)$p1val))
index=which(pvalues1>alpha)
mono.y=unlist(lapply(1:length(index),function(i) max(d6[[i]]$y)))
pos.mono=unlist(lapply(1:length(mono.y),function(i) d3$m.z[d3$y==mono.y[[i]]]))
return(list(data=d6,pval=pvalues1,mono.mz=pos.mono,mono.y=mono.y))
}

