RedIso.R <-
function(dat,c=3,...)
{
x=NULL
for(i in 1:length(dat)) x[i]=nrow(dat[[i]]<=c) 
omit=which(x<=c)
while(length(omit)>=1)
{
dat[[max(omit)]]=NULL
omit=omit[-length(omit)]
}
return(dat)
}

