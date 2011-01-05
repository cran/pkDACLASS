dat.star <-
function(dat,pars,B,...) 
lapply(1:B,function(i) dataGen(sum(dat[,2]),pars[1],pars[2],pars[3]))

